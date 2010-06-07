(* modmod *)

type loop_info = {
    li_start: int;
    li_end: int
}

type sample_info = {
    si_name: string;            (* 22 characters long *)
    si_finetune: int;
    si_volume: int;             (* 0x00-0x40 *)
    si_loop: loop_info option;
};;

type sample = {
    sa_info: sample_info;
    sa_data: string
};;

type effect =
      EF_none
    | EF_arpeggio of (int * int)                (* 0; 1st half note add, 2nd *)
    | EF_slide_up of int                        (* 1; upspeed *)
    | EF_slide_down of int                      (* 2; downspeed *)
    | EF_portamento of int                      (* 3; up/downspeed *)
    | EF_vibrato of (int * int)                 (* 4; speed, depth *)
    | EF_portamento_and_slide of (int * int)    (* 5; upspeed, downspeed *)
    | EF_vibrato_and_slide of (int * int)       (* 6; upspeed, downspeed *)
    | EF_tremolo of (int * int)                 (* 7; speed, depth *)
    | EF_set_sample_offset of int               (* 9; offset *)
    | EF_volume_slide of (int * int)            (* A; upspeed, downspeed *)
    | EF_position_jump of int                   (* B; position *)
    | EF_set_volume of int                      (* C; volume from 00-40 *)
    | EF_pattern_break                          (* D *)
    | EF_set_speed of int                       (* Fxx < 20; speed *)
    | EF_set_tempo of int                       (* Fxx >= 20; tempo *)

type note = {
    no_instrument: int;
    no_period: int;
    no_effect: effect
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

let playback_freq = 44100 in
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
    let mix dest src =
        let (dest_in, src_in) = (IO.input_string dest, IO.input_string src) in
        let out = IO.output_string() in
        let len = (String.length src) / 2 in
        for i = 0 to len-1 do
            let result =
                let n = IO.read_i16 dest_in + IO.read_i16 src_in in
                if n < -32768 then -32768 else if n > 32767 then 32767 else n
            in
            IO.write_i16 out result
        done;
        IO.close_out out
    in

    let rec play_row ~order:(order_no:int) ~row:(row_no:int) ~tempo:tempo =
        let render_row row : string =
            let len =
                tempo.te_speed * playback_freq * 5 / (tempo.te_tempo * 2)
            in

            let render_note note : string =
                let sample = song.so_samples.(note.no_instrument) in
                let { sa_info = info; sa_data = data } = sample in
                let sample_len = String.length data in
                let sample_freq =
                    (* TODO: take period into account! *)
                    finetune_freqs.(info.si_finetune)
                in

                let out = IO.output_string() in     (* TODO: reuse a buffer *)
                for i = 0 to len - 1 do
                    let raw_pos = i * sample_freq / playback_freq in
                    let past_end = raw_pos >= sample_len in
                    if past_end && info.si_loop == None then
                        IO.write_i16 out 0
                    else
                        let pos =
                            if not past_end then raw_pos else
                                let loop = Option.get info.si_loop in
                                let loop_start = loop.li_start in
                                let loop_pos =
                                    let loop_len = loop.li_end - loop_start in
                                    (sample_len - raw_pos) mod loop_len
                                in
                                loop_pos + loop_start
                        in
                        let samp =
                            let b = Char.code data.[pos] in
                            if b < 128 then b else b - 256
                        in
                        IO.write_i16 out (samp lsl 8)
                done;
                IO.close_out out
            in

            let note_data = Array.map render_note row in
            let blank = String.make (len * 2) '\000' in
            Array.fold_left mix blank note_data 
        in

        (* Advances to the next row, whatever that may be. This is determined
         * by the effects. *)
        let advance row =
            let channels = Array.length row in
            let rec check_note_and_jump i =
                if i == channels then
                    if row_no == 63 then
                        play_row ~order:(order_no + i) ~row:0 ~tempo:tempo
                    else
                        play_row ~order:order_no ~row:(row_no + 1) ~tempo:tempo
                else
                    match row.(i).no_effect with
                          EF_pattern_break ->
                            play_row ~order:(order_no + 1) ~row:0 ~tempo:tempo
                        | EF_position_jump p ->
                            play_row ~order:p ~row:0 ~tempo:tempo
                        | _ ->
                            check_note_and_jump (i + 1)
            in
            check_note_and_jump 0
        in

        if order_no == Array.length song.so_order then () else
            let pat = song.so_patterns.(song.so_order.(order_no)) in
            let row = pat.(row_no) in
            Printf.printf "%d:%d:" order_no row_no;
            Std.print row;
            let pcm = render_row row in
            Ao.play driver pcm;
            advance row
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
                si_loop =
                    if loop_start == 0 then None else
                        Some { li_start = loop_start; li_end = loop_end }
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
                    let parse_effect cmd data =
                        let nibbles() = data lsr 4, data land 0xf in
                        match cmd with
                              0x0 when data == 0 -> EF_none
                            | 0x0 -> EF_arpeggio(nibbles())
                            | 0x1 -> EF_slide_up data
                            | 0x2 -> EF_slide_down data
                            | 0x3 -> EF_portamento data
                            | 0x4 -> EF_vibrato(nibbles())
                            | 0x5 -> EF_portamento_and_slide(nibbles())
                            | 0x6 -> EF_vibrato_and_slide(nibbles())
                            | 0x7 -> EF_tremolo(nibbles())
                            | 0x9 -> EF_set_sample_offset data
                            | 0xa -> EF_volume_slide(nibbles())
                            | 0xb ->
                                let (tens, ones) = nibbles() in
                                EF_position_jump(tens * 10 + ones)  (* silly *)
                            | 0xc -> EF_set_volume data
                            | 0xd -> EF_pattern_break
                            | 0xf when data <= 0x20 -> EF_set_speed data
                            | 0xf -> EF_set_tempo data
                            | _ -> EF_none      (* TODO: E-commands *)
                    in

                    let a = IO.BigEndian.read_ui16 inf in
                    let b = IO.read_byte inf in
                    let instrument = ((a land 0xf000) lsr 8) lor (b lsr 4) in
                    let period = a land 0xffff in
                    let effect_cmd = b land 0x0f in
                    let effect_data = IO.read_byte inf in
                    {
                        no_instrument = instrument;
                        no_period = period;
                        no_effect = parse_effect effect_cmd effect_data
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
    let driver = Ao.open_live ~bits:16 ~rate:playback_freq ~channels:2 () in
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

