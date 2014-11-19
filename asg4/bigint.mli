(* Author: Adam Henry, adlhenry@ucsc.edu *)
(* $Id: bigint.mli,v 1.1 2014-11-18 15:54:59-08 - - $ *)

module Bigint : sig
	type bigint
	val bigint_of_string : string -> bigint
	val string_of_bigint : bigint -> string
	val add : bigint -> bigint -> bigint
	val sub : bigint -> bigint -> bigint
	val mul : bigint -> bigint -> bigint
	val div : bigint -> bigint -> bigint
	val rem : bigint -> bigint -> bigint
	val pow : bigint -> bigint -> bigint
	val zero : bigint
end
