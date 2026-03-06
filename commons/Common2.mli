(* Yoann Padioleau
 *
 * Copyright (C) 1998-2009 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*###########################################################################*)
(* Globals *)
(*###########################################################################*)

val get_value: string -> 'a
val write_value: 'a -> string -> unit

(*###########################################################################*)
(* Types *)
(*###########################################################################*)

type filename = string
type dirname = string

(* file or dir *)
type path = string

type float_time = float

(** file position as line number (1-based) *)
type filepos = int

type 'a pair = 'a * 'a

type 'a matrix = 'a array array

type 'a set = 'a list
type ('a, 'b) assoc = ('a * 'b) list

type 'a stack = 'a list

type 'a tree2 = Tree of 'a * 'a tree2 list
type ('a, 'b) tree = Node of 'a * ('a, 'b) tree list | Leaf of 'b

(*###########################################################################*)
(* Scoring / regression testing *)
(*###########################################################################*)

type score_result = Ok | Pb of string
type score = (string, score_result) Hashtbl.t

val empty_score : unit -> score
val regression_testing : score -> string (* best_score_file *) -> unit

(*###########################################################################*)
(* Modules *)
(*###########################################################################*)

module StringSet : sig
  include Set.S with type elt = string

  val of_list : string list -> t
  val to_list : t -> string list
end

(*###########################################################################*)
(* Debugging/logging *)
(*###########################################################################*)

val pr2_no_nl : string -> unit
val pr2_xxxxxxxxxxxxxxxxx : unit -> unit

val log2 : string -> unit

val example : bool -> unit
val error_cant_have : 'a -> 'b
val warning : string -> 'a -> 'a
val mk_pr2_wrappers : bool ref -> (string -> unit) * (string -> unit)

(*###########################################################################*)
(* Exn *)
(*###########################################################################*)

val exn_to_s_with_backtrace : exn -> string

(*###########################################################################*)
(* Comparison *)
(*###########################################################################*)

type compare = Equal | Inf | Sup
val ( <=> ) : 'a -> 'a -> compare

(*###########################################################################*)
(* Regexp *)
(*###########################################################################*)

val ( ==~ ) : string -> Str.regexp -> bool

(*###########################################################################*)
(* Basic *)
(*###########################################################################*)

val once : bool ref -> (unit -> unit) -> unit
val do_n : int -> (unit -> unit) -> unit
val add1 : int -> int

val y_or_no : string -> bool

(*###########################################################################*)
(* Math *)
(*###########################################################################*)

val pi : float
val square : float -> float
val borne : min:'a -> max:'a -> 'a -> 'a

val sum : int list -> int
val sum_int : int list -> int
val sum_float : float list -> float

val pourcent : int -> int -> int
val pourcent_float : int -> int -> float

module ArithFloatInfix : sig
  val ( + ) : float -> float -> float
  val ( - ) : float -> float -> float
  val ( / ) : float -> float -> float
  val ( * ) : float -> float -> float
  val ( +.. ) : int -> int -> int
  val ( -.. ) : int -> int -> int
  val ( /.. ) : int -> int -> int
  val ( *.. ) : int -> int -> int
  val ( += ) : float ref -> float -> unit
end

(*###########################################################################*)
(* Option *)
(*###########################################################################*)

val optionise : (unit -> 'a) -> 'a option

(*###########################################################################*)
(* Pair *)
(*###########################################################################*)

val pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val fst3 : 'a * 'b * 'c -> 'a
val thd3 : 'a * 'b * 'c -> 'c

(*###########################################################################*)
(* String *)
(*###########################################################################*)

val is_blank_string : string -> bool
val n_space : int -> string
val split_list_regexp : string -> string list -> (string * string list) list

val string_of_char : char -> string
val string_of_chars : char list -> string
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_option : ('a -> string) -> 'a option -> string
val list_of_string : string -> char list
val chop : string -> string
val strip : char -> string -> string
val string_match_substring : Str.regexp -> string -> bool

(*###########################################################################*)
(* Numeric *)
(*###########################################################################*)

val float_of_string_opt : string -> float option
val int64_of_string_opt : string -> int64 option
val int64_of_string_c_octal_opt : string -> int64 option

(*###########################################################################*)
(* Filename *)
(*###########################################################################*)

val chop_dirsymbol : string -> string
val relative_to_absolute : filename -> filename
val is_absolute : filename -> bool
val dirs_and_base_of_file : path -> string list * string
val inits_of_absolute_dir : dirname -> dirname list
val inits_of_relative_dir : dirname -> dirname list

(*###########################################################################*)
(* File system *)
(*###########################################################################*)

val mkdir : ?mode:Unix.file_perm -> string -> unit
val unix_lstat_eff : filename -> Unix.stats
val unix_stat_eff : filename -> Unix.stats
val filesize_eff : filename -> int
val lfile_exists_eff : filename -> bool
val readdir_to_dir_list : string -> dirname list
val readdir_to_file_list : string -> filename list
val common_prefix_of_files_or_dirs : path list -> dirname

val nblines : filename -> int
val nblines_eff : filename -> int
val nblines_with_wc : filename -> int

val files_of_dir_or_files_no_vcs :
  string (* extension *) -> string (* root *) list -> string (* filename *) list

val lines : string -> string list
val unlines : string list -> string
val lines_with_nl_either : string -> (string, unit) Either.t list

val glob : string -> string list
val unix_diff : string -> string -> string list

(*###########################################################################*)
(* Dates *)
(*###########################################################################*)

type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

type year = Year of int
type day = Day of int
type date_dmy = DMY of day * month * year
type days = Days of int

val mk_date_dmy : int -> int -> int -> date_dmy
val int_of_month : month -> int
val month_of_string : string -> month
val unixtime_to_dmy : Unix.tm -> date_dmy
val floattime_to_unixtime : float_time -> Unix.tm
val today : unit -> float_time
val month_before : float_time -> float_time
val rough_days_between_dates : date_dmy -> date_dmy -> days
val maximum_dmy : date_dmy list -> date_dmy
val minimum_dmy : date_dmy list -> date_dmy

(*###########################################################################*)
(* List *)
(*###########################################################################*)

val take_while : ('a -> bool) -> 'a list -> 'a list
val drop_while : ('a -> bool) -> 'a list -> 'a list
val splitAt : int -> 'a list -> 'a list * 'a list
val split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list
val split_gen_when : ('a list -> 'a list option) -> 'a list -> 'a list list
val span_tail_call : ('a -> bool) -> 'a list -> 'a list * 'a list
val head_middle_tail : 'a list -> 'a * 'a list * 'a
val list_last : 'a list -> 'a
val list_init : 'a list -> 'a list
val inits : 'a list -> 'a list list
val tails : 'a list -> 'a list list
val enum_safe : int -> int -> int list
val pack_safe : int -> 'a list -> 'a list list
val chunks : int -> 'a list -> 'a list list
val remove_first : 'a -> 'a list -> 'a list
val uniq_eff : 'a list -> 'a list
val uniq : 'a list -> 'a list
val map_flatten : ('a -> 'b list) -> 'a list -> 'b list
val acc_map : ('a -> 'b) -> 'a list -> 'b list
val map_eff_rev : ('a -> 'b) -> 'a list -> 'b list
val maximum : 'a list -> 'a
val minimum : 'a list -> 'a
val count_elements_sorted_highfirst : 'a list -> ('a * int) list
val or_list : bool list -> bool
val and_list : bool list -> bool
val return_when : ('a -> 'b option) -> 'a list -> 'b
val iter_with_previous_opt : ('a option -> 'a -> unit) -> 'a list -> unit
val repeat : 'a -> int -> 'a list
val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a
val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

val group_by_pre : ('a -> bool) -> 'a list -> 'a list * ('a * 'a list) list
val group_by_mapped_key : ('a -> 'b) -> 'a list -> ('b * 'a list) list

(*###########################################################################*)
(* Set (as list) *)
(*###########################################################################*)

val union_set : 'a set -> 'a set -> 'a set
val minus_set : 'a list -> 'a list -> 'a list
val ( $+$ ) : 'a set -> 'a set -> 'a set

(*###########################################################################*)
(* Assoc *)
(*###########################################################################*)

val keys : ('a * 'b) list -> 'a list
val sort_by_key_highfirst : ('a, 'b) assoc -> ('a * 'b) list

(*###########################################################################*)
(* Hash *)
(*###########################################################################*)

val hkeys : ('a, 'b) Hashtbl.t -> 'a list
val hmem : 'a -> ('a, 'b) Hashtbl.t -> bool
val hfind_option : 'a -> ('a, 'b) Hashtbl.t -> 'b option
val hupdate_default :
  'a -> update:('b -> 'b) -> default:(unit -> 'b) -> ('a, 'b) Hashtbl.t -> unit
val diff_set_eff : 'a list -> 'a list -> 'a list * 'a list * 'a list

val hash_with_default :
  (unit -> 'b) ->
  < add : 'a -> 'b -> unit
  ; to_list : ('a * 'b) list
  ; to_h : ('a, 'b) Hashtbl.t
  ; update : 'a -> ('b -> 'b) -> unit
  ; assoc : 'a -> 'b >

(*###########################################################################*)
(* Stack *)
(*###########################################################################*)

val top : 'a stack -> 'a

(*###########################################################################*)
(* Tree *)
(*###########################################################################*)

val tree2_iter : ('a -> unit) -> 'a tree2 -> unit

(*###########################################################################*)
(* Command line *)
(*###########################################################################*)

val cmdline_flags_devel : unit -> Arg_.cmdline_options
val cmdline_flags_verbose : unit -> Arg_.cmdline_options
val cmdline_flags_other : unit -> Arg_.cmdline_options
