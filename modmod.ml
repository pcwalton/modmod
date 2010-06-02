(* it's a mod mod *)

type sample = {
    sa_name: string;            (* 22 characters long *)
    sa_finetune: int;
    sa_volume: int;             (* 0x00-0x40 *)
    sa_loop_start: int;
    sa_loop_end: int;
    sa_data: string
};;

type row = {
    ro_instrument: int;
    ro_period: int;
    ro_effect: int
};;

type pattern = row array;;      (* 64 rows *)

type song = {
    so_title: string;           (* 20 characters long *)
    so_samples: sample array;   (* 31 samples long *)
    so_order: int array;
    so_patterns: pattern array
};;

let play_mod _ = () in

let load_stream (_:in_channel) = () in

let play_stream f = play_mod (load_stream f) in

let main () =
    let parse_command_line () : string =
        let optparser =
            OptParse.OptParser.make ~usage:"%prog [options] song.mod" ()
        in
        let paths = OptParse.OptParser.parse_argv optparser in
        if List.length paths != 1 then
            OptParse.OptParser.error optparser "one file must be specified";
        List.hd paths
    in

    let path = parse_command_line () in
    let f = open_in_bin path in
    Std.finally (fun () -> close_in f) play_stream f
in
main ()

