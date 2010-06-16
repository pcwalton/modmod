(*
 * modmod, a music module player.
 *
 * Copyright (c) 2010 Patrick Walton
 *)

type loop_info = {
    li_start: int;
    li_len: int
}

type sample_info = {
    si_name: string;            (* 22 characters long *)
    si_freq: int;               (* base frequency of C-2 *)
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
    | EF_pan of (int * int)                     (* 8; ??? *)
    | EF_set_sample_offset of int               (* 9; offset *)
    | EF_volume_slide of (int * int)            (* A; upspeed, downspeed *)
    | EF_position_jump of int                   (* B; position *)
    | EF_pattern_break                          (* D *)
    | EF_set_speed of int                       (* Fxx < 20; speed *)
    | EF_set_tempo of int                       (* Fxx >= 20; tempo *)

type note_on = {
    no_instrument: int option;
    no_period: int;
};;

type note =
      NO_none
    | NO_note_on of note_on
;;

type entry = {
    en_note: note;
    en_vol: int option;             (* 0x00-0x40 *)
    en_effect: effect
};;

type row = entry array;;            (* 4 entries *)

type pattern = row array;;          (* 64 rows *)

type song = {
    so_title: string;               (* 20 characters long *)
    so_samples: sample array;       (* 31 samples long *)
    so_order: int array;
    so_patterns: pattern array
};;

type tempo = {
    mutable te_tempo: int;          (* tempo in BPM (default 125) *)
    mutable te_speed: int;          (* rows per beat (default 6) *)
};;

type channel_audio = {
    ca_sample: int;
    ca_freq: int;
    mutable ca_vol: int;
    mutable ca_pos: int
};;

(* Constants *)

let playback_freq = 44100 in
let c2_period = 856/4 in

let get_s16 buf idx =
    let value = Char.code buf.[idx] lor ((Char.code buf.[idx + 1]) lsl 8) in
    if value > 32767 then value - 65536 else value
in
let set_s16 buf idx value =
    let value = if value < 0 then 65536 + value else value in
    buf.[idx] <- Char.chr (value land 0xff);
    buf.[idx + 1] <- Char.chr (value lsr 8);
in

(* Logging functions *)
let string_of_row row =
    let string_of_note note =
        match note with
              NO_none -> "--- --"
            | NO_note_on note_on ->
                let period = Printf.sprintf "%03d" note_on.no_period in
                let instr =
                    match note_on.no_instrument with
                          None -> "--"
                        | Some instr -> Printf.sprintf "%02x" instr
                in
                period ^ " " ^ instr
    in
    let string_of_vol vol =
        Option.map_default (Printf.sprintf "%02x") "--" vol
    in
    let string_of_effect effect =
        if effect = EF_none then "---" else
        let nibbles x y = (x lsl 4) lor y in
        let bytes =
            match effect with
                  EF_arpeggio(x, y) -> (0x0, nibbles x y)
                | EF_slide_up x -> (0x1, x)
                | EF_slide_down x -> (0x2, x)
                | EF_portamento x -> (0x3, x)
                | EF_vibrato(x, y) -> (0x4, nibbles x y)
                | EF_portamento_and_slide(x, y) -> (0x5, nibbles x y)
                | EF_vibrato_and_slide(x, y) -> (0x6, nibbles x y)
                | EF_tremolo(x, y) -> (0x7, nibbles x y)
                | EF_pan(x, y) -> (0x8, nibbles x y)
                | EF_set_sample_offset x -> (0x9, x)
                | EF_volume_slide (x, y) -> (0xa, nibbles x y)
                | EF_position_jump x -> (0xb, x)
                | EF_pattern_break -> (0xd, 0x00)
                | EF_set_speed x | EF_set_tempo x -> (0xf, x)
                | EF_none -> failwith "none?!"
        in
        Printf.sprintf "%03x" ((fst bytes lsl 8) lor snd bytes)
    in
    let string_of_entry ent =
        let note = string_of_note ent.en_note in
        let vol = string_of_vol ent.en_vol in
        let effect = string_of_effect ent.en_effect in
        String.concat " " [ note; vol; effect ]
    in
    String.concat "  " (List.map string_of_entry (Array.to_list row))
in

let die str = prerr_string "modmod: "; prerr_endline str; exit 1 in
let warn str = prerr_string "modmod: warning: "; prerr_endline str in
let warn_unless cond str = if not cond then warn str in

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
    let tempo = { te_tempo = 125; te_speed = 6 } in
    let channels = Array.init 4 (fun _ -> ref None) in
    let rec play_row ~order:(order_no:int) ~row:(row_no:int) =
        let update_tempo row =
            Array.iter begin fun ent ->
                match ent.en_effect with
                      EF_set_speed speed -> tempo.te_speed <- speed
                    | EF_set_tempo t -> tempo.te_tempo <- t
                    | _ -> ()
            end row;
        in

        let render_row row : string =
            (* Play each entry, updating the channels. *)
            let play_entry chan ent =
                begin
                    match ent.en_note with
                          NO_none -> ()
                        | NO_note_on note_on ->
                            let render inst =
                                let period = note_on.no_period in
                                let info = song.so_samples.(inst).sa_info in
                                let c2_freq = info.si_freq in
                                let freq = c2_period * c2_freq / period in
                                chan := Some {
                                    ca_sample = inst;
                                    ca_freq = freq;
                                    ca_vol = info.si_volume;
                                    ca_pos = 0
                                }
                            in

                            match (note_on.no_instrument, !chan) with
                                  (* change the instrument *)
                                  (Some inst, _) -> render inst
                                  (* keep the old channel instrument *)
                                | (None, Some audio) -> render audio.ca_sample
                                  (* ignore *)
                                | (None, None) -> ()
                end;

                (* Handle volume and middle-end effects. *)
                let render_middle_effects audio =
                    Option.may (fun vol -> audio.ca_vol <- vol) ent.en_vol
                in
                Option.may render_middle_effects !chan
            in
            ExtArray.Array.iter2 play_entry channels row;

            (* Create the buffers. TODO: reuse them. *)
            let len = playback_freq*tempo.te_speed*5 / (tempo.te_tempo*2) in
            let dest = String.make (len*4) '\000' in
            let buf = String.create (len*4) in

            (* Render each channel. *)
            let render_channel chan =
                String.fill buf 0 (len*4) '\000';
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
                        if past_end && Option.is_none info.si_loop then
                            chan := None    (* past the end *)
                        else begin
                            let pos =
                                if not past_end then pos else
                                    (* Determine where we are in the loop
                                     * section. *)
                                    let { li_start = lstart; li_len = llen } =
                                        Option.get info.si_loop
                                    in
                                    let lpos = (pos - len) mod llen in
                                    lpos + lstart
                            in
                            let samp = Char.code data.[pos] in
                            let samp = if samp < 128 then samp else samp-256 in
                            let samp = samp lsl 8 in
                            let samp = samp * audio.ca_vol / 0x40 / 2 in
                            set_s16 buf (i*4) samp;
                            set_s16 buf (i*4 + 2) samp;
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
                        play_row ~order:(order_no + 1) ~row:0
                    else
                        play_row ~order:order_no ~row:(row_no + 1)
                else
                    match row.(i).en_effect with
                          EF_pattern_break ->
                            play_row ~order:(order_no + 1) ~row:0
                        | EF_position_jump p ->
                            play_row ~order:p ~row:0
                        | _ ->
                            check_note_and_jump (i + 1)
            in
            check_note_and_jump 0
        in

        if order_no == Array.length song.so_order then () else
            let pat_no = song.so_order.(order_no) in
            let pat = song.so_patterns.(pat_no) in
            let row = pat.(row_no) in
            Printf.printf "%02d:%02d: %s" pat_no row_no (string_of_row row);
            print_newline();
            update_tempo row;
            let pcm = render_row row in
            Ao.play driver pcm;
            advance row
    in
    play_row ~order:0 ~row:0
in

let trim = ExtString.String.strip ~chars:"\000" in

let load_mod(f:in_channel) : song =
    let finetune_freqs = Array.map (( * ) 2) [|
        (* table from mikmod/mplayer/mloader.c *)
        8363; 8413; 8463; 8529; 8581; 8651; 8723; 8757;
        7895; 7941; 7985; 8046; 8107; 8169; 8232; 8280
    |] in
    let valid_ids = ExtHashtbl.Hashtbl.of_enum (ExtList.List.enum [
        ("M.K.", ()); ("4CHN", ()); ("FLT4", ());
    ]) in

    let inf = IO.input_channel f in
    let title = trim(IO.nread inf 20) in

    (* The sample data will be filled in later. *)
    let (sample_infos:((sample_info * int) array)) =
        let load_sample_info _ =
            let read_word_and_double() = 2 * IO.BigEndian.read_ui16 inf in

            let name = trim (IO.nread inf 22) in
            let len = read_word_and_double() in
            let finetune = (IO.read_byte inf) land 0x0f in
            let freq = finetune_freqs.(finetune) in
            let volume = IO.read_byte inf in
            let loop_start = read_word_and_double() in
            let loop_len = read_word_and_double() in
            let info = {
                si_name = name;
                si_freq = freq;
                si_volume = volume;
                si_loop =
                    if loop_len == 0 then None else
                        Some { li_start = loop_start; li_len = loop_len }
            } in
            (info, len)
        in
        Array.init 31 load_sample_info
    in
    Std.print sample_infos;

    let order_len = IO.read_byte inf in
    ignore (IO.read_byte inf);  (* song loop byte *)

    (* Read orders, then unused orders. *)
    let order = Array.init order_len (fun _ -> IO.read_byte inf) in
    for i = order_len to 127 do ignore (IO.read_byte inf) done;

    let id = IO.nread inf 4 in
    if not (Hashtbl.mem valid_ids id) then
        failwith ("Unknown ID: \"" ^ id ^ "\"");

    let patterns =
        let load_pattern _ =
            let load_row _ =
                let load_entry _ =
                    let parse_effect cmd data =
                        let nibbles() = data lsr 4, data land 0xf in
                        let vol = if cmd == 0xc then Some data else None in
                        let effect =
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
                                    EF_position_jump(tens*10 + ones) (* o_O *)
                                | 0xc -> EF_none    (* volume *)
                                | 0xd -> EF_pattern_break
                                | 0xf when data <= 0x20 -> EF_set_speed data
                                | 0xf -> EF_set_tempo data
                                | _ -> EF_none      (* TODO: E-commands *)
                        in
                        (vol, effect)
                    in

                    let a = IO.read_byte inf in
                    let b = IO.read_byte inf in
                    let c = IO.read_byte inf in
                    let d = IO.read_byte inf in

                    let instrument = (a land 0xf0) lor (c lsr 4) in
                    let instrument =
                        if instrument == 0 then
                            None
                        else
                            Some (instrument - 1)
                    in
                    let period = ((a land 0x0f) lsl 8) lor b in
                    let (effect_cmd, effect_data) = ((c land 0x0f), (d)) in

                    let note = 
                        if period == 0 then NO_none else
                            NO_note_on {
                                no_instrument = instrument;
                                no_period = period
                            }
                    in
                    let (vol, effect) = parse_effect effect_cmd effect_data in
                    { en_note = note; en_vol = vol; en_effect = effect }
                in
                Array.init 4 load_entry
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

let load_s3m f =
    let inf = IO.input_channel f in
    let read_parapointer _ = IO.read_ui16 inf * 16 in

    let name = trim(IO.nread inf 28) in
    assert (IO.read_byte inf == 0x1a);
    assert (IO.read_byte inf == 16);
    seek_in f 0x20;
    let order_count = IO.read_ui16 inf in
    let instr_count = IO.read_ui16 inf in
    let pat_count = IO.read_ui16 inf in
    ignore(IO.read_ui16 inf);   (* flags *)
    ignore(IO.read_ui16 inf);   (* creator code *)
    assert (IO.read_ui16 inf == 2);
    assert (IO.nread inf 4 == "SCRM");
    let global_volume = IO.read_byte inf in
    let initial_speed = IO.read_byte inf in
    let initial_tempo = IO.read_byte inf in
    let (master_volume, output_channels) =
        let b = IO.read_byte inf in (b land 0x7f, b lsr 7)
    in
    seek_in f 0x40;
    let order = Array.init order_count (fun _ -> IO.read_byte inf) in
    let instr_offsets = Array.init instr_count read_parapointer in
    let pat_offsets = Array.init pat_count read_parapointer in

    let read_instr offset =
        seek_in f offset;
        let t = IO.read_byte inf in
        assert (t == 1);    (* sample, not adlib melody or adlib drum *)
        ignore(IO.nread inf 12);    (* filename *)
        let samp_offset =
            let lobyte = IO.read_byte inf in
            lobyte lor ((IO.read_ui16 inf) lsl 8)
        in
        let len = IO.read_i32 inf in
        let loop_begin = IO.read_i32 inf in
        let loop_end = IO.read_i32 inf in
        let vol = IO.read_byte inf in   (* [0, 64) *)
        ignore(IO.read_byte inf);
        warn_unless (IO.read_byte inf == 0) "packed samples TODO";

        let flags = IO.read_byte inf in
        let loop = flags land 0x01 != 0 in
        warn_unless (flags land 0x02 == 0) "stereo samples TODO";
        warn_unless (flags land 0x04 == 0) "16-bit samples TODO";

        let freq = IO.read_i32 inf in   (* frequency of C-2 *)
        ignore(IO.nread inf 12);
        let name = trim(IO.nread inf 28) in
        assert (IO.nread inf 28 == "SCRS");

        let data = IO.nread inf len in

        let loop =
            if not loop then None else
            Some { li_start = loop_begin; li_len = loop_end - loop_begin }
        in
        let info =
            { si_name = name; si_freq = freq; si_volume = vol; si_loop = loop }
        in
        { sa_data = data; sa_info = info }
    in
    let instrs = Array.map read_instr instr_offsets in

    let read_pat offset =
        seek_in f offset;
        failwith "TODO"
    in
    let pats = Array.map read_pat pat_offsets in

    failwith "TODO"
in

let play_song song =
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

    let get_loader path =
        let loaders = ExtHashtbl.Hashtbl.of_enum (ExtList.List.enum [
            ("mod", load_mod);
            ("s3m", load_s3m)
        ]) in
        try
            let ext = ExtList.List.last (ExtString.String.nsplit path ".") in
            let ext = String.lowercase ext in
            try
                Hashtbl.find loaders ext
            with Not_found ->
                die("don't know how to play files of type \"" ^ ext ^ "\"")
        with ExtString.Invalid_string ->
                die "file has no extension; can't determine its type";
    in

    let path = parse_command_line() in
    let loader = get_loader path in
    let f = open_in_bin path in
    let song = Std.finally (fun() -> close_in f) loader f in
    play_song song
in
main()

