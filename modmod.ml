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

type note = {
    no_instrument: int;
    no_period: int;
    no_effect_cmd: int;
    no_effect_data: int
};;

type row = note array;;         (* 4 notes *)

type pattern = row array;;      (* 64 rows *)

type song = {
    so_title: string;           (* 20 characters long *)
    so_samples: sample array;   (* 31 samples long *)
    so_order: int array;
    so_patterns: pattern array
};;

type tempo = {
    te_tempo: int;              (* tempo in BPM (default 125) *)
    te_speed: int;              (* rows per beat (default 6) *)
};;

(* Constants *)

let frequency = 44100 in
let finetune_freqs = Array.map (( * ) 2) [|
    (* table from mikmod/mplayer/mloader.c *)
    8363; 8413; 8463; 8529; 8581; 8651; 8723; 8757;
    7895; 7941; 7985; 8046; 8107; 8169; 8232; 8280
|] in

let valid_ids = ExtHashtbl.Hashtbl.of_enum (ExtList.List.enum [
    ("M.K.", ());
    ("4CHN", ());
    ("6CHN", ());
    ("8CHN", ());
    ("FLT4", ());
    ("FLT8", ())
]) in

let play driver song =
    let rec play_row ~order:(order_no:int) ~row:(row_no:int) ~tempo:tempo =
        let len = frequency * tempo.te_speed * tempo.te_tempo / 60 in
        let render_note note =
            let sample = song.so_samples.(note.no_instrument) in
            failwith "TODO"
        in

        if order_no == Array.length song.so_order then () else
            let pat = song.so_patterns.(song.so_order.(order_no)) in
            if row_no == Array.length pat then
                play_row ~order:(order_no + 1) ~row:0 ~tempo:tempo
            else begin
                let row = pat.(row_no) in
                Printf.printf "%d:%d:" order_no row_no;
                Std.print row;
                Ao.play driver (render_note row.(0));
                while true do () done; (* TODO *)
                play_row ~order:order_no ~row:(row_no + 1) ~tempo:tempo
            end
    in
    play_row ~order:0 ~row:0 ~tempo:{ te_tempo = 125; te_speed = 6 }
in

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

    (* Read orders, then unused orders *)
    let order = Array.init order_len (fun _ -> IO.read_byte inf) in
    for i = order_len to 127 do ignore (IO.read_byte inf) done;

    let id = IO.nread inf 4 in
    if not (Hashtbl.mem valid_ids id) then
        failwith ("Unknown ID: \"" ^ id ^ "\"");

    let patterns =
        let load_pattern _ =
            let load_row _ =
                let load_note _ =
                    let a = IO.BigEndian.read_ui16 inf in
                    let b = IO.read_byte inf in
                    let instrument = ((a land 0xf000) lsr 8) lor (b lsr 4) in
                    let period = a land 0xffff in
                    let effect_cmd = b land 0x0f in
                    let effect_data = IO.read_byte inf in
                    {
                        no_instrument = instrument;
                        no_period = period;
                        no_effect_cmd = effect_cmd;
                        no_effect_data = effect_data
                    }
                in
                Array.init 4 load_note
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

let load_and_play_stream f =
    let song = load_stream f in
    let driver = Ao.open_live ~bits:16 ~rate:frequency ~channels:2 () in
    Std.finally (fun() -> Ao.close driver) (play driver) song
in

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
    Std.finally (fun() -> close_in f) load_and_play_stream f
in
main()

