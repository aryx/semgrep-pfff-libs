(* See also CapSys and CapUnix in TCB/.

   See also Cmd.ml which contains the "pure" stuff.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* claude: the exec capability is now a function of the command, so that
 * callers can restrict it dynamically (see Cap.mli in ocaml-caps). We
 * call it before running anything so such a restriction is honored.
 *)
let check (caps : < Cap.exec; .. >) (cmd_str : string) : unit =
  caps#exec cmd_str |> ignore

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let string_of_run caps ~trim ?env cmd =
  check caps (Cmd.to_string cmd);
  UCmd.string_of_run ~trim ?env cmd

let string_of_run_with_stderr caps ~trim ?env cmd =
  check caps (Cmd.to_string cmd);
  UCmd.string_of_run_with_stderr ~trim ?env cmd

let lines_of_run caps ~trim ?env cmd =
  check caps (Cmd.to_string cmd);
  UCmd.lines_of_run ~trim ?env cmd

let status_of_run caps ?quiet ?env cmd =
  check caps (Cmd.to_string cmd);
  UCmd.status_of_run ?quiet ?env cmd

(*****************************************************************************)
(* Deprecated *)
(*****************************************************************************)

(* DEPRECATED: you should use lines_of_run *)
let cmd_to_list caps ?verbose cmd_str =
  check caps cmd_str;
  UCmd.cmd_to_list ?verbose cmd_str

(* you should probably use of the xxx_of_run function above *)
let with_open_process_in caps cmd_str fchan =
  check caps cmd_str;
  UCmd.with_open_process_in cmd_str fchan

let open_process caps cmd =
  check caps cmd;
  UUnix.open_process cmd

let close_process (_caps : < Cap.exec; .. >) chans = UUnix.close_process chans
