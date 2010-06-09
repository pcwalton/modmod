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

type note_on = {
    no_instrument: int;
    no_period: int;
};;

type note =
      NO_none
    | NO_note_on of note_on
;;

type row = (note * effect) array;;  (* 4 notes *)

type pattern = row array;;          (* 64 rows *)

type song = {
    so_title: string;               (* 20 characters long *)
    so_samples: sample array;       (* 31 samples long *)
    so_order: int array;
    so_patterns: pattern array
};;

type tempo = {
    te_tempo: int;                  (* tempo in BPM (default 125) *)
    te_speed: int;                  (* rows per beat (default 6) *)
};;

type channel_audio = {
    ca_sample: int;
    ca_freq: int;
    mutable ca_pos: int
};;

(* Constants *)

let playback_freq = 44100 in
let finetune_freqs = Array.map (( * ) 2) [|
    (* table from mikmod/mplayer/mloader.c *)
    8363; 8413; 8463; 8529; 8581; 8651; 8723; 8757;
    7895; 7941; 7985; 8046; 8107; 8169; 8232; 8280
|] in
let c1_period = 856/2 in

let valid_ids = ExtHashtbl.Hashtbl.of_enum (ExtList.List.enum [
    ("M.K.", ());
    ("4CHN", ());
    ("6CHN", ());
    ("8CHN", ());
    ("FLT4", ());
    ("FLT8", ())
]) in

let get_s16 buf idx =
    let value = Char.code buf.[idx] lor ((Char.code buf.[idx + 1]) lsl 8) in
    if value > 32767 then value - 65536 else value
in
let set_s16 buf idx value =
    let value = if value < 0 then 65536 + value else value in
    buf.[idx] <- Char.chr (value land 0xff);
    buf.[idx + 1] <- Char.chr (value lsr 8);
in

(** [mix dest src] mixes the 16-bit little-endian audio buffer [src] into
    [dest]. *)
let mix dest src =
    if String.length src != String.length dest then
        invalid_arg "buffers must have the same length";

    let rec loop i =
        if i < String.length dest then
            let n = (get_s16 dest i) + (get_s16 src i) in
            let n = if n < -32768 then -32768
                    else if n > 32767 then 32767
                    else n
            in
            set_s16 dest i n;
            loop (i + 2)
        else ()
    in
    loop 0
in

let play driver song =
    let channels = Array.init 4 (fun _ -> ref None) in
    let rec play_row ~order:(order_no:int) ~row:(row_no:int) ~tempo:tempo =
        let render_row row : string =
            (* Play each note, updating the channels. *)
            let play_note (note, _) =
                match note with
                      NO_none -> None
                    | NO_note_on { no_instrument = inst; no_period = pd } ->
                        let info = song.so_samples.(inst).sa_info in
                        let c1_freq = finetune_freqs.(info.si_finetune) in
                        let freq = pd * c1_freq / c1_period in
                        Some { ca_sample = inst; ca_freq = freq; ca_pos = 0 }
            in
            ExtArray.Array.iter2
                begin
                    fun chan note ->
                        Option.may
                            (fun audio -> chan := Some audio)
                            (play_note note)
                end
                channels row;

            (* Create the buffers. TODO: reuse them. *)
            let len = playback_freq*tempo.te_speed*5 / (2*tempo.te_tempo) in
            let dest = String.make (len * 2) '\000' in
            let buf = String.create (len * 2) in

            (* Render each channel. *)
            let render_channel chan =
                String.fill buf 0 (len * 2) '\000';
                for i = 0 to len - 1 do
                    Option.may begin fun audio ->
                        let sample = song.so_samples.(audio.ca_sample) in
                        let { sa_info = info; sa_data = data } = sample in
                        let len = String.length data in
                        let pos =
                            Int64.to_int
                                (Int64.div
                                    (Int64.mul (Int64.of_int audio.ca_pos)
                                        (Int64.of_int audio.ca_freq))
                                    (Int64.of_int playback_freq))
                        in
                        let past_end = pos >= len in
                        if past_end && info.si_loop == None then
                            chan := None    (* past the end *)
                        else begin
                            let pos =
                                if not past_end then pos else
                                    let loop = Option.get info.si_loop in
                                    let loop_len =
                                        loop.li_end - loop.li_start
                                    in
                                    let loop_pos = (len - pos) mod loop_len in
                                    loop_pos + loop.li_start
                            in
                            let samp = Char.code data.[pos] in
                            let samp = if samp < 128 then samp else samp-256 in
                            let samp = samp lsl 8 in
                            let samp = samp * info.si_volume / 0x40 in
                            set_s16 buf (i * 2) samp;
                            audio.ca_pos <- audio.ca_pos + 1
                        end
                    end !chan
                done;
                mix dest buf;
            in
            Array.iter render_channel channels;
            dest
        in

        (* Advances to the next row (which row it is is determined by the
         * effects). *)
        let advance row =
            let channels = Array.length row in
            let rec check_note_and_jump i =
                if i == channels then
                    if row_no == 63 then
                        play_row ~order:(order_no + i) ~row:0 ~tempo:tempo
                    else
                        play_row ~order:order_no ~row:(row_no + 1) ~tempo:tempo
                else
                    match snd row.(i) with
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

                    let a = IO.read_byte inf in
                    let b = IO.read_byte inf in
                    let c = IO.read_byte inf in
                    let d = IO.read_byte inf in

                    let instrument = (a land 0xf0) lor (c lsr 4) in
                    let period = ((a land 0x0f) lsl 8) lor b in
                    let (effect_cmd, effect_data) = ((c land 0x0f), (d)) in

                    let note = 
                        if instrument == 0 then NO_none else
                            NO_note_on {
                                no_instrument = instrument;
                                no_period = period
                            }
                    in
                    let effect = parse_effect effect_cmd effect_data in
                    (note, effect)
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

