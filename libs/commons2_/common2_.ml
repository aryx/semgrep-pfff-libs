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
open Common

(*###########################################################################*)
let rec (do_n : int -> (unit -> unit) -> unit) =
 fun i f ->
  if i =|= 0 then ()
  else (
    f ();
    do_n (i - 1) f)

let enum_safe x n = if x > n then [] else List_.enum x n

let rec list_last = function
  | [] -> raise Not_found
  | [ x ] -> x
  | _x :: y :: xs -> list_last (y :: xs)

let (list_of_string : string -> char list) = function
  | "" -> []
  | s -> List_.enum 0 (String.length s - 1) |> List_.map (String.get s)

let (lines : string -> string list) =
 fun s ->
  let rec lines_aux = function
    | [] -> []
    | [ x ] -> if x = "" then [] else [ x ]
    | x :: xs -> x :: lines_aux xs
  in
  Str.split_delim (Str.regexp "\r\n\\|\n") s |> lines_aux

let (with_open_stringbuf : ((string -> unit) * Buffer.t -> unit) -> string) =
 fun f ->
  let buf = Buffer.create 1000 in
  let pr s = Buffer.add_string buf (s ^ "\n") in
  f (pr, buf);
  Buffer.contents buf

let foldl1 p xs =
  match xs with
  | x :: xs -> List.fold_left p x xs
  | [] -> failwith "foldl1: empty list"

let repeat e n =
  let rec repeat_aux acc = function
    | 0 -> acc
    | n when n < 0 -> failwith "repeat"
    | n -> repeat_aux (e :: acc) (n - 1)
  in
  repeat_aux [] n

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

(* I used this in coccinelle where the huge logging of stuff ask for
 * a more organized solution that use more visual indentation hints.
 *
 * todo? could maybe use log4j instead ? or use Format module more
 * consistently ?
 *)

let _tab_level_print = ref 0
let _tab_indent = 5
let _prefix_pr = ref ""

let pr s =
  UStdlib.print_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.print_string " ");
  UStdlib.print_string s;
  UStdlib.print_string "\n";
  flush UStdlib.stdout

let pr_no_nl s =
  UStdlib.print_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.print_string " ");
  UStdlib.print_string s;
  flush UStdlib.stdout

let _chan_pr2 = ref (None : out_channel option)

let out_chan_pr2 ?(newline = true) s =
  match !_chan_pr2 with
  | None -> ()
  | Some chan ->
      output_string chan (s ^ if newline then "\n" else "");
      flush chan

let pr2 s =
  UStdlib.prerr_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.prerr_string " ");
  UStdlib.prerr_string s;
  UStdlib.prerr_string "\n";
  flush UStdlib.stderr;
  out_chan_pr2 s;
  ()

let pr2_no_nl s =
  UStdlib.prerr_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.prerr_string " ");
  UStdlib.prerr_string s;
  flush UStdlib.stderr;
  out_chan_pr2 ~newline:false s;
  ()

let pr_xxxxxxxxxxxxxxxxx () =
  pr "-----------------------------------------------------------------------"

let pr2_xxxxxxxxxxxxxxxxx () =
  pr2 "-----------------------------------------------------------------------"

let spf = Printf.sprintf

(* ---------------------------------------------------------------------- *)

let _chan = ref UStdlib.stderr

let start_log_file () =
  let filename = spf "/tmp/debugml%d:%d" (UUnix.getuid ()) (UUnix.getpid ()) in
  pr2 (spf "now using %s for logging" filename);
  _chan := UStdlib.open_out_bin filename

let dolog s =
  output_string !_chan (s ^ "\n");
  flush !_chan

let verbose_level = ref 1
let log s = if !verbose_level >= 1 then dolog s
let log2 s = if !verbose_level >= 2 then dolog s
let example b =
  if b then () else failwith ("ASSERT FAILURE: " ^ Printexc.get_backtrace ())

let once aref f =
  if !aref then ()
  else (
    aref := true;
    f ())

(* cache_file, cf below *)

let exn_to_s_with_backtrace exn =
  Printexc.to_string exn ^ "\n" ^ Printexc.get_backtrace ()

(* want or of merd, but cant cos cant put die ... in b (strict call) *)
let sum_float = List.fold_left ( +. ) 0.0
(* in prelude: let sum_int   = List.fold_left (+) 0 *)

let pi = 3.14159265358979323846
let square x = x *. x
let borne ~min ~max x = if x > max then max else if x < min then min else x

let sum xs = List.fold_left ( + ) 0 xs
type compare = Equal | Inf | Sup
let ( <=> ) a b = if a =*= b then Equal else if a < b then Inf else Sup

let pourcent x total = x * 100 / total
let pourcent_float x total = float_of_int x *. 100.0 /. float_of_int total
module ArithFloatInfix = struct
  let ( +.. ) = ( + )
  let ( -.. ) = ( - )
  let ( /.. ) = ( / )
  let ( *.. ) = ( * )
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( / ) = ( /. )
  let ( * ) = ( *. )
  let ( += ) ref v = ref := !ref + v
end

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

type 'a pair = 'a * 'a
let pair f (x, y) = (f x, f y)
let some = function
  | Some x -> x
  | _ -> failwith "some: None"

let optionise f =
  try Some (f ()) with
  | Not_found -> None

(* pixel *)
let split sep s = Str.split (Str.regexp sep) s

(*
let _ = example (split "/" "" =*= [])
let _ = example (split ":" ":a:b" =*= ["a";"b"])
*)
let join sep xs = String.concat sep xs
(*
let _ = example (join "/" ["toto"; "titi"; "tata"] =$= "toto/titi/tata")
*)

(*
let rec join str = function
  | [] -> ""
  | [x] -> x
  | x::xs -> x ^ str ^ (join str xs)
*)

let (split_list_regexp : string -> string list -> (string * string list) list) =
 fun re xs ->
  let rec split_lr_aux (heading, accu) = function
    | [] -> [ (heading, List.rev accu) ]
    | x :: xs ->
        if x =~ re then (heading, List.rev accu) :: split_lr_aux (x, []) xs
        else split_lr_aux (heading, x :: accu) xs
  in
  split_lr_aux ("__noheading__", []) xs |> fun xs ->
  if List_.hd_exn "unexpected empty list" xs =*= ("__noheading__", []) then
    List_.tl_exn "unexpected empty list" xs
  else xs

let chop_dirsymbol = function
  | s when s =~ "\\(.*\\)/$" -> matched1 s
  | s -> s

let is_blank_string s = s =~ "^\\([ \t]\\)*$"

(* src: lablgtk2/examples/entrycompletion.ml *)
type filename = string (* TODO could check that exist :) type sux *)

(* with sexp *)
type dirname = string

(* TODO could check that exist :) type sux *)
(* with sexp *)

(* file or dir *)
type path = string

module BasicType = struct
  type filename = string
end

let relative_to_absolute s =
  if s = "." then USys.getcwd ()
  else if Filename.is_relative s then USys.getcwd () ^ "/" ^ s
  else s

let is_relative s = Filename.is_relative s
let is_absolute s = not (is_relative s)

(* pre: prj_path must not contain regexp symbol *)
let grep_dash_v_str =
  "| grep -v /.hg/ |grep -v /CVS/ | grep -v /.git/ |grep -v /_darcs/"
  ^ "| grep -v /.svn/ | grep -v .git_annot | grep -v .marshall"

let arg_symlink b = if b then " -L " else ""

let files_of_dir_or_files_no_vcs ext xs =
  xs
  |> List_.map (fun x ->
         if USys.is_directory x then
           (* nosemgrep: forbid-exec *)
           UCmd.cmd_to_list
             ("find " ^ arg_symlink false ^ x ^ " -noleaf -type f -name \"*." ^ ext
            ^ "\"" ^ grep_dash_v_str)
         else [ x ])
  |> List_.flatten

(*****************************************************************************)
(* i18n *)
(*****************************************************************************)
type langage = English | Francais | Deutsch

(* gettext ? *)

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(* maybe I should use ocamlcalendar, but I don't like all those functors ... *)

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

type wday =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

type date_dmy = DMY of day * month * year
type hour = Hour of int
type minute = Min of int
type second = Sec of int
type time_hms = HMS of hour * minute * second
type full_date = date_dmy * time_hms

(* intervalle *)
type days = Days of int
type time_dmy = TimeDMY of day * month * year
type float_time = float

let check_date_dmy (DMY (_day, _month, _year)) = raise Common.Todo
let check_time_dmy (TimeDMY (_day, _month, _year)) = raise Common.Todo
let check_time_hms (HMS (_x, _y, _a)) = raise Common.Todo

(* ---------------------------------------------------------------------- *)

(* older code *)
let int_to_month i =
  assert (i <= 12 && i >= 1);
  match i with
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  (*
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
*)
  | _ -> raise Common.Impossible

let month_info =
  [
    (1, Jan, "Jan", "January", 31);
    (2, Feb, "Feb", "February", 28);
    (3, Mar, "Mar", "March", 31);
    (4, Apr, "Apr", "April", 30);
    (5, May, "May", "May", 31);
    (6, Jun, "Jun", "June", 30);
    (7, Jul, "Jul", "July", 31);
    (8, Aug, "Aug", "August", 31);
    (9, Sep, "Sep", "September", 30);
    (10, Oct, "Oct", "October", 31);
    (11, Nov, "Nov", "November", 30);
    (12, Dec, "Dec", "December", 31);
  ]

let week_day_info =
  [
    (0, Sunday, "Sun", "Dim", "Sunday");
    (1, Monday, "Mon", "Lun", "Monday");
    (2, Tuesday, "Tue", "Mar", "Tuesday");
    (3, Wednesday, "Wed", "Mer", "Wednesday");
    (4, Thursday, "Thu", "Jeu", "Thursday");
    (5, Friday, "Fri", "Ven", "Friday");
    (6, Saturday, "Sat", "Sam", "Saturday");
  ]

let i_to_month_h =
  month_info
  |> List_.map (fun (i, month, _monthstr, _mlong, _days) -> (i, month))

let s_to_month_h =
  month_info
  |> List_.map (fun (_i, month, monthstr, _mlong, _days) -> (monthstr, month))

let slong_to_month_h =
  month_info
  |> List_.map (fun (_i, month, _monthstr, mlong, _days) -> (mlong, month))

let month_to_s_h =
  month_info
  |> List_.map (fun (_i, month, monthstr, _mlong, _days) -> (month, monthstr))

let month_to_i_h =
  month_info
  |> List_.map (fun (i, month, _monthstr, _mlong, _days) -> (month, i))

let i_to_wday_h =
  week_day_info
  |> List_.map (fun (i, day, _dayen, _dayfr, _daylong) -> (i, day))

let wday_to_en_h =
  week_day_info
  |> List_.map (fun (_i, day, dayen, _dayfr, _daylong) -> (day, dayen))

let wday_to_fr_h =
  week_day_info
  |> List_.map (fun (_i, day, _dayen, dayfr, _daylong) -> (day, dayfr))

let month_of_string s = List.assoc s s_to_month_h
let string_of_month s = List.assoc s month_to_s_h
let month_of_int i = List.assoc i i_to_month_h
let int_of_month m = List.assoc m month_to_i_h
let days_month =
  [| 0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334 (*; 365*) |]

let rough_days_since_jesus (DMY (Day nday, month, Year year)) =
  let n = nday + days_month.(int_of_month month - 1) + (year * 365) in
  Days n

let is_more_recent d1 d2 =
  let (Days n1) = rough_days_since_jesus d1 in
  let (Days n2) = rough_days_since_jesus d2 in
  n1 > n2

let max_dmy d1 d2 = if is_more_recent d1 d2 then d1 else d2
let min_dmy d1 d2 = if is_more_recent d1 d2 then d2 else d1
let maximum_dmy ds = foldl1 max_dmy ds
let minimum_dmy ds = foldl1 min_dmy ds

let rough_days_between_dates d1 d2 =
  let (Days n1) = rough_days_since_jesus d1 in
  let (Days n2) = rough_days_since_jesus d2 in
  Days (n2 - n1)

let mk_date_dmy day month year =
  let date = DMY (Day day, month_of_int month, Year year) in
  (* check_date_dmy date *)
  date

(* ---------------------------------------------------------------------- *)
(* conversion to unix.tm *)

let dmy_to_unixtime (DMY (Day n, month, Year year)) =
  let tm =
    {
      Unix.tm_sec = 0;
      (* Seconds 0..60 *)
      tm_min = 0;
      (* Minutes 0..59 *)
      tm_hour = 12;
      (* Hours 0..23 *)
      tm_mday = n;
      (* Day of month 1..31 *)
      tm_mon = int_of_month month - 1;
      (* Month of year 0..11 *)
      tm_year = year - 1900;
      (* Year - 1900 *)
      tm_wday = 0;
      (* Day of week (Sunday is 0) *)
      tm_yday = 0;
      (* Day of year 0..365 *)
      tm_isdst = false (* Daylight time savings in effect *);
    }
  in
  Unix.mktime tm

let unixtime_to_dmy tm =
  let n = tm.Unix.tm_mday in
  let month = month_of_int (tm.Unix.tm_mon + 1) in
  let year = tm.Unix.tm_year + 1900 in

  DMY (Day n, month, Year year)

let floattime_to_unixtime sec = Unix.localtime sec
let today : unit -> float = fun () -> UUnix.time ()
let (unlines : string list -> string) = fun s -> String.concat "\n" s ^ "\n"

let n_space n = repeat " " n |> join ""

let nblines s = lines s |> List.length
(*
let _ = example (nblines "" =|= 0)
let _ = example (nblines "toto" =|= 1)
let _ = example (nblines "toto\n" =|= 1)
let _ = example (nblines "toto\ntata" =|= 2)
let _ = example (nblines "toto\ntata\n" =|= 2)
*)

(* old: fork sucks.
 * (* note: on MacOS wc outputs some spaces before the number of lines *)
 *)

(* from https://gist.github.com/jaspervdj/1162402 *)
(* Fold over a file in chunks *)
let fold_file f x file_name =
  let buffer = Bytes.create 1024 in
  let file = UStdlib.open_in file_name in
  let rec go a =
    let length = input file buffer 0 (Bytes.length buffer) in
    let a' = f a (Bytes.sub buffer 0 length) in
    if length > 0 then go a' else a'
  in
  let r = go x in
  close_in file;
  r

(* Count the number of newlines in a buffer *)
let count_newlines s =
  let rec go n i =
    try
      let i' = Bytes.index_from s i '\n' in
      go (n + 1) (i' + 1)
    with
    | Not_found -> n
  in
  go 0 0

(* Compose the previous two functions to count the lines in a file *)
let nblines_eff file = fold_file (fun x s -> x + count_newlines s) 0 file

(* old: this could generate some Sys_error "Out of memory" in stressful
 * conditions because of the repeated calls to input_line which on
 * huge files will allocate each time new memory. The GC will reclaim
 * it, but it may be too late and we reach the physical memory limit.
 *)
(*
let nblines_eff2 file =
  let res = ref 0 in
  let finished = ref false in
  let ch = open_in_bin file in
  while not !finished do
    try
      let _ = input_line ch in
      incr res
    with End_of_file -> finished := true
  done;
  close_in ch;
  !res
*)

(* could be in h_files-format *)
let lines_with_nl_either s =
  let xs = Str.full_split (Str.regexp "\n") s in
  xs
  |> List_.map (function
       | Str.Delim _s -> Either.Right ()
       | Str.Text s -> Either.Left s)

(*
let _ = example (lines_with_nl_either "ab\n\nc" =*=
    [Left "ab"; Right (); Right (); Left "c"])
*)

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)
let nblines_with_wc a = nblines_eff a

let _batch_mode = ref false

let y_or_no msg =
  pr2 (msg ^ " [y/n] ?");
  if !_batch_mode then true
  else
    let rec aux () =
      match UStdlib.read_line () with
      | "y"
      | "yes"
      | "Y" ->
          true
      | "n"
      | "no"
      | "N" ->
          false
      | _ ->
          pr2 "answer by 'y' or 'n'";
          aux ()
    in
    aux ()

let mkdir ?(mode = 0o770) file = UUnix.mkdir file mode

(* opti? use wc -l ? *)
let _hmemo_unix_lstat_eff = Hashtbl.create 101
let _hmemo_unix_stat_eff = Hashtbl.create 101

let unix_lstat_eff file =
  if is_absolute file then
    memoized _hmemo_unix_lstat_eff file (fun () -> UUnix.lstat file)
  else
    (* this is for efficieny reason to be able to memoize the stats *)
    failwith "must pass absolute path to unix_lstat_eff"

let unix_stat_eff file =
  if is_absolute file then
    memoized _hmemo_unix_stat_eff file (fun () -> UUnix.stat file)
  else
    (* this is for efficieny reason to be able to memoize the stats *)
    failwith "must pass absolute path to unix_stat_eff"

let filesize_eff file = (unix_lstat_eff file).st_size
let lfile_exists_eff filename =
  try
    match (unix_lstat_eff filename).st_kind with
    | Unix.S_REG
    | Unix.S_LNK ->
        true
    | _ -> false
  with
  | UUnix.Unix_error (Unix.ENOENT, _, _) -> false

let is_directory_eff file = (unix_lstat_eff file).st_kind =*= Unix.S_DIR
let capsule_unix f args =
  try f args with
  | UUnix.Unix_error (e, fm, argm) ->
      log
        (Printf.sprintf "exn Unix_error: %s %s %s\n" (Unix.error_message e) fm
           argm)

let readdir_to_kind_list (path : string) (kind : Unix.file_kind) : string list =
  USys.readdir path |> Array.to_list
  |> List.filter (fun s ->
         try
           let stat = UUnix.lstat (path ^ "/" ^ s) in
           stat.st_kind =*= kind
         with
         | UUnix.Unix_error _ ->
             pr2 ("EXN pb stating file: " ^ s);
             false)

let (readdir_to_dir_list : string -> string list) =
 fun path -> readdir_to_kind_list path Unix.S_DIR

let (readdir_to_file_list : string -> string list) =
 fun path -> readdir_to_kind_list path Unix.S_REG

let unixname () =
  let uid = UUnix.getuid () in
  let entry = UUnix.getpwuid uid in
  entry.pw_name

(* This regex matches the directory part a glob pattern
   used below. This way we are only trying to match
   files contained in the dir specified by the pattern or subdirs,
   instead of caluclating the contents of the entire
   working directory. I.e. tests/**/*.extension would
   result in tests/ *)
let dir_regex = Str.regexp "^[^\\*]*"

let glob pattern =
  Str.search_forward dir_regex pattern 0 |> ignore;
  let dir = Str.matched_string pattern in
  let regex = pattern |> Re.Glob.glob ~anchored:true |> Re.compile in
  (* TODO: should remove, but that would require modifying many call sites
   * so let's forge one for now
   *)
  let caps = Cap.readdir_UNSAFE () in
  let files = UFile.Legacy.dir_contents caps dir in
  files |> List.filter (fun s -> Re.execp regex s)

let sanity_check_files_and_adjust ext files =
  let files =
    files
    |> List.filter (fun file ->
           if not (file =~ ".*\\." ^ ext) then (
             pr2 ("warning: seems not a ." ^ ext ^ " file");
             false)
           else if UFile.is_dir ~follow_symlinks:true (Fpath.v file) then (
             pr2 (spf "warning: %s is a directory" file);
             false)
           else true)
  in
  files

let rec take_while p = function
  | [] -> []
  | x :: xs -> if p x then x :: take_while p xs else []

(* now in prelude: let rec drop n xs = ... *)
let rec drop_while p = function
  | [] -> []
  | x :: xs -> if p x then drop_while p xs else x :: xs

let rec group_by_mapped_key fkey l =
  match l with
  | [] -> []
  | x :: xs ->
      let k = fkey x in
      let xs1, xs2 =
        List.partition
          (fun x' ->
            let k2 = fkey x' in
            k =*= k2)
          xs
      in
      (k, x :: xs1) :: group_by_mapped_key fkey xs2

let (group_by_post : ('a -> bool) -> 'a list -> ('a list * 'a) list * 'a list) =
 fun f xs ->
  let rec aux_filter grouped_acc acc = function
    | [] -> (List.rev grouped_acc, List.rev acc)
    | x :: xs ->
        if f x then aux_filter ((List.rev acc, x) :: grouped_acc) [] xs
        else aux_filter grouped_acc (x :: acc) xs
  in
  aux_filter [] [] xs

let (group_by_pre : ('a -> bool) -> 'a list -> 'a list * ('a * 'a list) list) =
 fun f xs ->
  let xs' = List.rev xs in
  let ys, unclassified = group_by_post f xs' in
  ( List.rev unclassified,
    ys |> List.rev |> List_.map (fun (xs, x) -> (x, List.rev xs)) )

let rec (split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list) =
 fun p -> function
  | [] -> raise Not_found
  | x :: xs ->
      if p x then ([], x, xs)
      else
        let l1, a, l2 = split_when p xs in
        (x :: l1, a, l2)

let head_middle_tail xs =
  match xs with
  | x :: y :: xs ->
      let head = x in
      let reversed = List.rev (y :: xs) in
      let tail = List_.hd_exn "unexpected empty list" reversed in
      let middle = List.rev (List_.tl_exn "unexpected empty list" reversed) in
      (head, middle, tail)
  | _ -> failwith "head_middle_tail, too small list"

let rec remove_first e xs =
  match xs with
  | [] -> raise Not_found
  | x :: xs -> if x =*= e then xs else x :: remove_first e xs

(* now in prelude
   let exclude p xs =
   List.filter (fun x -> not (p x)) xs
*)
(* now in prelude
*)

let rec list_init = function
  | [] -> raise Not_found
  | [ _x ] -> []
  | x :: y :: xs -> x :: list_init (y :: xs)

(* now in prelude:
 * let rec list_last = function
 * | [] -> raise Not_found
 * | [x] -> x
 * | x::y::xs -> list_last (y::xs)
 *)

(* pixel *)
(* now in prelude
 *   let last_n n l = List.rev (take n (List.rev l))
 *   let last l = List_.hd_exn "unexpected empty list" (last_n 1 l)
 *)

(* todo: foldl, foldr (a more consistent foldr) *)

(* start pixel *)
let rec inits = function
  | [] -> [ [] ]
  | e :: l -> [] :: List_.map (fun l -> e :: l) (inits l)

let rec tails = function
  | [] -> [ [] ]
  | _ :: xs as xxs -> xxs :: tails xs

let maximum l = foldl1 max l
let minimum l = foldl1 min l

(* do a map tail recursive, and result is reversed, it is a tail recursive map => efficient *)
let or_list = List.fold_left ( || ) false
let and_list = List.fold_left ( && ) true

let rec (return_when : ('a -> 'b option) -> 'a list -> 'b) =
 fun p -> function
  | [] -> raise Not_found
  | x :: xs -> (
      match p x with
      | None -> return_when p xs
      | Some b -> b)

let rec splitAt n xs =
  if n =|= 0 then ([], xs)
  else
    match xs with
    | [] -> ([], [])
    | x :: xs ->
        let a, b = splitAt (n - 1) xs in
        (x :: a, b)

let rec pack_safe n xs =
  match xs with
  | [] -> []
  | _ :: _ ->
      let a, b = splitAt n xs in
      a :: pack_safe n b

let chunks n xs =
  let size = List.length xs in
  let chunksize = if size mod n =|= 0 then size / n else 1 + (size / n) in
  let xxs = pack_safe chunksize xs in
  if List.length xxs <> n then failwith "chunks: impossible, wrong size";
  xxs

let map_flatten f l =
  let rec map_flatten_aux accu = function
    | [] -> accu
    | e :: l -> map_flatten_aux (List.rev (f e) @ accu) l
  in
  List.rev (map_flatten_aux [] l)

(* now in prelude: let rec repeat e n *)

let sort_prof a b = List.sort a b

type order = HighFirst | LowFirst

let sort_by_key_highfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k2 k1) xs

type 'a matrix = 'a array array

type 'a set = 'a list
(* with sexp *)

let (insert_set : 'a -> 'a set -> 'a set) =
 fun x xs ->
  if List.mem x xs then
    (* let _ = print_string "warning insert: already exist" in *)
    xs
  else x :: xs

let empty_set : 'a set = []

let (set : 'a list -> 'a set) =
 fun xs -> xs |> List.fold_left (flip insert_set) empty_set |> List.sort compare

let (union_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 ->
  s2
  |> List.fold_left
       (fun acc x -> if List.mem x s1 then acc else insert_set x acc)
       s1

let ( $+$ ) = union_set

type ('a, 'b) assoc = ('a * 'b) list
(* with sexp *)

let assoc = List.assoc
let keys xs = List_.map fst xs
let hmem k h = Hashtbl.mem h k
let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] |> List.sort compare

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  (* replace or add? depends the semantic of hashtbl you want *)
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

(*
let _  =
  let h = Hashtbl.create 101 in
  Hashtbl.add h "toto" 1;
  Hashtbl.add h "toto" 1;
  assert(hash_to_list h =*= ["toto",1; "toto",1])
*)

let hfind_default key value_if_not_found h =
  try Hashtbl.find h key with
  | Not_found ->
      Hashtbl.add h key (value_if_not_found ());
      Hashtbl.find h key

(* not as easy as Perl  $h->{key}++; but still possible *)
let hupdate_default key ~update:op ~default:value_if_not_found h =
  let old = hfind_default key value_if_not_found h in
  Hashtbl.replace h key (op old)

let add1 old = old + 1
let hfind_option key h = optionise (fun () -> Hashtbl.find h key)

(* see below: let hkeys h = ... *)

let count_elements_sorted_highfirst xs =
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun e -> hupdate_default e (fun old -> old + 1) (fun () -> 0) h);
  let xs = hash_to_list h in
  (* not very efficient ... but simpler. use max_with_elem stuff ? *)
  List.sort (fun (_k1, v1) (_k2, v2) -> compare v2 v1) xs

let hkeys h =
  let hkey = Hashtbl.create 101 in
  h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
  hash_to_list hkey |> List_.map fst

let uniq_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun k -> Hashtbl.replace h k true);
  hkeys h

type ('a, 'b) hash_with_default =
  < add : 'a -> 'b -> unit
  ; to_list : ('a * 'b) list
  ; to_h : ('a, 'b) Hashtbl.t
  ; update : 'a -> ('b -> 'b) -> unit
  ; assoc : 'a -> 'b >

let hash_with_default fv =
  object
    val h = Hashtbl.create 101
    method to_list = hash_to_list h
    method to_h = h
    method add k v = Hashtbl.replace h k v
    method assoc k = Hashtbl.find h k
    method update k f = hupdate_default k ~update:f ~default:fv h
  end

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)
type 'a stack = 'a list
(* with sexp *)

let (empty_stack : 'a stack) = []

(*let (push: 'a -> 'a stack -> 'a stack) = fun x xs -> x::xs *)
let top (xs : 'a stack) : 'a = List_.hd_exn "unexpected empty stack" xs
type 'a tree2 = Tree of 'a * 'a tree2 list

let rec (tree2_iter : ('a -> unit) -> 'a tree2 -> unit) =
 fun f tree ->
  match tree with
  | Tree (node, xs) ->
      f node;
      xs |> List.iter (tree2_iter f)

type ('a, 'b) tree = Node of 'a * ('a, 'b) tree list | Leaf of 'b
(* with tarzan *)

type score_result = Ok | Pb of string

(* with sexp *)
type score = (string (* usually a filename *), score_result) Hashtbl.t

(* with sexp *)
type score_list = (string (* usually a filename *) * score_result) list
(* with sexp *)

let empty_score () : score = Hashtbl.create 101

let regression_testing_vs newscore bestscore =
  let newbestscore = empty_score () in

  let allres =
    hash_to_list newscore |> List_.map fst
    |> union_set (hash_to_list bestscore |> List_.map fst)
  in
  allres
  |> List.iter (fun res ->
         match
           ( optionise (fun () -> Hashtbl.find newscore res),
             optionise (fun () -> Hashtbl.find bestscore res) )
         with
         | None, None -> raise Common.Impossible
         | Some x, None ->
             UPrintf.printf "new test file appeared: %s\n" res;
             Hashtbl.add newbestscore res x
         | None, Some _x -> UPrintf.printf "old test file disappeared: %s\n" res
         | Some newone, Some bestone -> (
             match (newone, bestone) with
             | Ok, Ok -> Hashtbl.add newbestscore res Ok
             | Pb x, Ok ->
                 UPrintf.printf
                   "PBBBBBBBB: a test file does not work anymore!!! : %s\n" res;
                 UPrintf.printf "Error : %s\n" x;
                 Hashtbl.add newbestscore res Ok
             | Ok, Pb _x ->
                 UPrintf.printf "Great: a test file now works: %s\n" res;
                 Hashtbl.add newbestscore res Ok
             | Pb x, Pb y ->
                 Hashtbl.add newbestscore res (Pb x);
                 if not (x = y) then (
                   UPrintf.printf
                     "Semipb: still error but not same error : %s\n" res;
                   let s = "Old error: " ^ y in
                   UPrintf.printf "%s\n" (String.sub s 0 (String.length s - 1));
                   UPrintf.printf "New error: %s\n" x)));
  flush UStdlib.stdout;
  flush UStdlib.stderr;
  newbestscore

let get_value filename =
  let chan = UStdlib.open_in_bin filename in
  let x = UStdlib.input_value chan in
  (* <=> Marshal.from_channel  *)
  close_in chan;
  x

let write_value valu filename =
  let chan = UStdlib.open_out_bin filename in
  UStdlib.output_value chan valu;
  (* <=> Marshal.to_channel *)
  (* Marshal.to_channel chan valu [Marshal.Closures]; *)
  close_out chan

let regression_testing newscore best_score_file =
  pr2 ("regression file: " ^ best_score_file);
  let (bestscore : score) =
    if not (USys.file_exists best_score_file) then
      write_value (empty_score ()) best_score_file;
    get_value best_score_file
  in
  let newbestscore = regression_testing_vs newscore bestscore in
  write_value newbestscore (best_score_file ^ ".old");
  write_value newbestscore best_score_file;
  ()

let string_of_score_result v =
  match v with
  | Ok -> "Ok"
  | Pb s -> "Pb: " ^ s

let total_scores score =
  let total = hash_to_list score |> List.length in
  let good =
    hash_to_list score |> List.filter (fun (_s, v) -> v =*= Ok) |> List.length
  in
  (good, total)

let print_total_score score =
  pr2 "--------------------------------";
  pr2 "total score";
  pr2 "--------------------------------";
  let good, total = total_scores score in
  pr2 (Printf.sprintf "good = %d/%d" good total)

let print_score score =
  score |> hash_to_list
  |> List.iter (fun (k, v) ->
         pr2 (Printf.sprintf "%s --> %s" k (string_of_score_result v)));
  print_total_score score;
  ()
(*x: common.ml *)
(*****************************************************************************)
(* Scope managment (cocci) *)
(*****************************************************************************)

(* could also make a function Common.make_scope_functions that return
 * the new_scope, del_scope, do_in_scope, add_env. Kind of functor :)
 *)

let cmdline_flags_devel () =
  [
    ( "-debugger",
      Arg.Set Common.debugger,
      " option to set if launched inside ocamldebug" );
  ]

let cmdline_flags_verbose () =
  [
    ("-verbose_level", Arg.Set_int verbose_level, " <int> guess what");
  ]

let _check_stack = ref true

let cmdline_flags_other () =
  [
    ("-nocheck_stack", Arg.Clear _check_stack, " ");
    ("-batch_mode", Arg.Set _batch_mode, " no interactivity");
  ]

(* potentially other common options but not yet integrated:

   "-timeout",        Arg.Set_int timeout,
   "  <sec> interrupt LFS or buggy external plugins";

   (* can't be factorized because of the $ cvs stuff, we want the date
   * of the main.ml file, not common.ml
   *)
   "-version",   Arg.Unit (fun () ->
    pr2 "version: _dollar_Date: 2008/06/14 00:54:22 _dollar_";
    raise (Common.UnixExit 0)
    ),
   "   guess what";

   "-shorthelp", Arg.Unit (fun () ->
    !short_usage_func();
    raise (Common.UnixExit 0)
   ),
   "    see short list of options";
   "-longhelp", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
    ),
   "-help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
   ),
   " ";
   "--help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
   ),
   " ";
*)

let ( ==~ ) s re = Str.string_match re s 0

module Infix = struct
  let ( ==~ ) = ( ==~ )
end


(*---------------------------------------------------------------------------*)
(* Directories part 2 *)
(*---------------------------------------------------------------------------*)

(* todo? vs common_prefix_of_files_or_dirs? *)
let find_common_root files =
  let dirs_part = files |> List_.map fst in

  let rec aux current_candidate xs =
    try
      let topsubdirs =
        xs |> List_.map (List_.hd_exn "unexpected empty list") |> uniq_eff
      in
      match topsubdirs with
      | [ x ] ->
          aux (x :: current_candidate)
            (xs |> List_.map (List_.tl_exn "unexpected empty list"))
      | _ -> List.rev current_candidate
    with
    | _ -> List.rev current_candidate
  in
  aux [] dirs_part

(*
let _ = example
  (find_common_root
      [(["home";"pad"], "foo.php");
       (["home";"pad";"bar"], "bar.php");
      ]
    =*= ["home";"pad"])
*)

let dirs_and_base_of_file file =
  let dir, base = Filename_.db_of_filename file in
  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  (dirs, base)

(*
let _ = example
  (dirs_and_base_of_file "/home/pad/foo.php" =*= (["home";"pad"], "foo.php"))
*)

let inits_of_absolute_dir dir =
  if not (is_absolute dir) then
    failwith (spf "inits_of_absolute_dir: %s is not an absolute path" dir);
  if not (UFile.is_dir ~follow_symlinks:true (Fpath.v dir)) then
    failwith (spf "inits_of_absolute_dir: %s is not a directory" dir);
  let dir = chop_dirsymbol dir in

  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  inits dirs |> List_.map (fun xs -> "/" ^ join "/" xs)

let inits_of_relative_dir dir =
  if not (is_relative dir) then
    failwith (spf "inits_of_relative_dir: %s is not a relative dir" dir);
  let dir = chop_dirsymbol dir in

  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  inits dirs
  |> List_.tl_exn "unexpected empty list"
  |> List_.map (fun xs -> join "/" xs)

(*
let _ = example
  (inits_of_absolute_dir "/usr/bin" =*= (["/"; "/usr"; "/usr/bin"]))

let _ = example
  (inits_of_relative_dir "usr/bin" =*= (["usr"; "usr/bin"]))
*)

(* main entry *)
let common_prefix_of_files_or_dirs xs =
  let xs = xs |> List_.map relative_to_absolute in
  match xs with
  | [] -> failwith "common_prefix_of_files_or_dirs: empty list"
  | [ x ] -> x
  | _y :: _ys ->
      (* todo: work when dirs ?*)
      let xs = xs |> List_.map dirs_and_base_of_file in
      let dirs = find_common_root xs in
      "/" ^ join "/" dirs

(*
let _ =
  example
    (common_prefix_of_files_or_dirs ["/home/pad/visual";
                                     "/home/pad/commons";]
     =*= "/home/pad/pfff"
    )
*)
