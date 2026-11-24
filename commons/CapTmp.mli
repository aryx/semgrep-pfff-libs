(* Capability aware wrappers to UTmp.ml *)

val with_temp_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  Cap.FS_.tmp ->
  (Fpath.t -> 'a) ->
  'a

val get_temp_dir_name : Cap.FS_.tmp -> Fpath.t

val new_temp_file :
  ?prefix:string -> ?suffix:string -> ?temp_dir:Fpath.t -> Cap.FS_.tmp -> Fpath.t

val replace_named_pipe_by_regular_file_if_needed :
  Cap.FS_.tmp -> ?prefix:string -> Fpath.t -> Fpath.t option

val replace_stdin_by_regular_file :
  Cap.FS_.tmp -> ?prefix:string -> unit -> Fpath.t

val erase_temp_files : Cap.FS_.tmp -> unit
