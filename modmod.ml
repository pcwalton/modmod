(* modmod *)

type sample_info = {
    si_name: string;            (* 22 characters long *)
    si_finetune: int;
    si_volume: int;             (* 0x00-0x40 *)
    si_loop_start: int;
    si_loop_end: int
};;

type sample = {
    sa_info: sample_info;
    sa_data: string
};;

type row = {
    ro_instrument: int;
    ro_period: int;
    ro_effect_cmd: int;
    ro_effect_data: int
};;

type pattern = row array;;      (* 64 rows *)

type song = {
    so_title: string;           (* 20 characters long *)
    so_samples: sample array;   (* 31 samples long *)
    so_order: int array;
    so_patterns: pattern array
};;

let play_mod _ = () in

let load_stream(f:in_channel) : song =
    let inf = IO.input_channel f in
    let trim = ExtString.String.strip ~chars:"\000" in
    let title = trim(IO.nread inf 20) in

    (* The sample data will be filled in later. *)
    let (sample_infos:((sample_info * int) array)) =
        let load_sample_info _ =
            let read_word_and_double() = 2 * IO.BigEndian.read_ui16 inf in

            let name = trim (IO.nread inf 22) in
            let len = read_word_and_double() in
            let finetune = (IO.read_byte inf) land 0x0f in
            let volume = IO.read_byte inf in
            let loop_start = read_word_and_double() in
            let loop_end = read_word_and_double() in
            let info = {
                si_name = name;
                si_finetune = finetune;
                si_volume = volume;
                si_loop_start = loop_start;
                si_loop_end = loop_end
            } in
            (info, len)
        in
        Array.init 31 load_sample_info
    in

    let order_len = IO.read_byte inf in
    ignore (IO.read_byte inf);  (* song loop byte *)
    let order = Array.init order_len (fun _ -> IO.read_byte inf) in

    ignore (IO.nread inf 4);    (* id *)

    let patterns =
        let load_pattern _ =
            let load_row _ =
                let a = IO.BigEndian.read_ui16 inf in
                let b = IO.read_byte inf in
                let instrument = ((a land 0xf000) lsr 8) lor (b lsr 4) in
                let period = a land 0xffff in
                let effect_cmd = b land 0x0f in
                let effect_data = IO.read_byte inf in
                {
                    ro_instrument = instrument;
                    ro_period = period;
                    ro_effect_cmd = effect_cmd;
                    ro_effect_data = effect_data
                }
            in
            Array.init 64 load_row
        in
        let pattern_count = 1 + (Array.fold_left max 0 order) in
        Array.init pattern_count load_pattern
    in

    let samples =
        let load_sample_data (info, len) =
            let data = IO.nread inf len in
            { sa_info = info; sa_data = data }
        in
        Array.map load_sample_data sample_infos
    in

    {
        so_title = title;
        so_samples = samples;
        so_order = order;
        so_patterns = patterns
    }
in

let play_stream f = play_mod(load_stream f) in

let main() =
    let parse_command_line() : string =
        let optparser =
            OptParse.OptParser.make ~usage:"%prog [options] song.mod" ()
        in
        let paths = OptParse.OptParser.parse_argv optparser in
        if List.length paths != 1 then
            OptParse.OptParser.error optparser "one file must be specified";
        List.hd paths
    in

    let path = parse_command_line() in
    let f = open_in_bin path in
    Std.finally (fun() -> close_in f) play_stream f
in
main()

