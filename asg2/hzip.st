#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Author: Adam Henry, adlhenry@ucsc.edu"
"$Id: hzip.st,v 1.1 2014-10-21 17:27:14-07 - - $"

execname := 'hzip.st'.
nl := Character nl.

"Load the frequency table from a file."
FileStream extend [
   loadCharFreq: freqtable [
      [self atEnd not] whileTrue: [
         |ordChar|
         ordChar := self next asInteger.
         freqtable incr: ordChar.
      ]
   ]
]

"Print a character or its ASCII code."
Character extend [
   isPrint [
      ^ ($! <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ self asString]
                   ifFalse: [^ self asciiValue printStringRadix: 16]
   ]
]

"Frequency table array object."
Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
 
]

"Leaf object."
Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   nl := Character nl.
   postorder [
      stdout << '0' << self char << nl.
   ]

]

"Leaf tree object."
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   nl := Character nl.
   postorder [
      left postorder.
      right postorder.
	  stdout << '1' << self char << nl.
   ]
   
]

"Set program arguments."
opts := (Smalltalk getArgv: 1).
filenameIn := (Smalltalk getArgv: 2).
filenameOut := (Smalltalk getArgv: 3).

"Check program usage."
Smalltalk arguments size < 2
   ifTrue: [ 
		stderr << 'Usage: ' << execname 
		<< ' -dtcu inputfile [outputfile]' << nl.
		ObjectMemory quit.
   ].
   

"1. Load frequency table."
freqtable := ZeroArray new: 256.
[
   |infile|
   infile := FileStream open: filenameIn
						mode: FileStream read.
   infile loadCharFreq: freqtable.
   infile close.
] on: SystemExceptions.FileError do: [:signal |
   |errno|
   errno := File errno.
   stdout flush.
   stderr << execname << ': ' << filenameIn << ': '
		  << signal messageText << nl.
   stderr flush.
].

"2. Load priority queue from frequency table."
pqueue := SortedCollection new.
freqtable keysAndValuesDo: [:key :value |
  (value notNil and: [value > 0]) ifTrue: [
		newTree := Leaf new: (key asCharacter) count: value.
		pqueue add: newTree.
  ]
].
opts =~ 'd' ifMatched: [pqueue inspect.].

"3. Unload priority queue into Huffman tree."
[pqueue notEmpty] whileTrue: [
	tree := pqueue removeFirst.
	pqueue size = 0 ifTrue: [^ tree].
	rtree := pqueue removeFirst.
	newTree := Tree new: tree char count: (tree count + rtree count) 
	left: tree right: rtree.
	pqueue add: newTree.
].
opts =~ 'd' ifMatched: [stdout << tree << nl.].

"4. Traverse Huffman tree into encoding array."
encodings := Array new: 256.
tree inorder: [:char :encoding |
   encodings at: char asInteger put: encoding.
] prefix: ''.

"5. Print out frequency and encoding table."
opts =~ 't' ifMatched: [
	freqtable keysAndValuesDo: [:key :value |
		(value notNil and: [value > 0]) ifTrue: [
		 |character|
		 character := (key asCharacter visible).
		 character asString size to: 3 do: [:skip| stdout << ' '].
		 stdout << character << ' '. 
		 value asString size to: 5 do: [:skip| stdout << ' '].
		 stdout << value << ' '.
		 stdout << (encodings at: key) << nl.
		]
	]
].

"Write the Huffman tree into the outfile."
opts =~ 'c' ifMatched: [
	tree postorder.
].

"Compress file."

"Reconstruct Huffman tree from compressed file."

"Decompress file."
