let (matched : int -> string -> string) = fun i s -> Str.matched_group i s
let matched1 s = matched 1 s
let matched2 s = (matched 1 s, matched 2 s)
let matched3 s = (matched 1 s, matched 2 s, matched 3 s)
let matched4 s = (matched 1 s, matched 2 s, matched 3 s, matched 4 s)

let matched5 s =
  (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)

let matched6 s =
  (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)

let matched7 s =
  ( matched 1 s,
    matched 2 s,
    matched 3 s,
    matched 4 s,
    matched 5 s,
    matched 6 s,
    matched 7 s )

let _memo_compiled_regexp = Hashtbl.create 101

let candidate_match_func s re =
  let compile_re =
    try Hashtbl.find _memo_compiled_regexp re with
    | Not_found ->
        let v = Str.regexp re in
        Hashtbl.add _memo_compiled_regexp re v;
        v
  in
  Str.string_match compile_re s 0

let match_func s re = candidate_match_func s re
let ( =~ ) s re = match_func s re

module Operators = struct
  let ( =~ ) = ( =~ )
end
