open Format

let usage = "usage: ./compilo [options] file.cid"
let preprocessing = ref false

let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".cid") then
        raise (Arg.Bad "no .cid extension");
      file := Some s
    in
    Arg.parse [("-pp", Arg.Set preprocessing, "Enable preprocessing for macros")] set_file usage;
    match !file with Some f -> f | None -> Arg.usage [] usage; exit 1


let preproc pp =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  match pp with
  | true ->
     let output_file = (Filename.chop_suffix file ".cid") ^ ".pp.cid" in
     Preprocessor.preprocessor output_file lb;
     open_in output_file
       
  | false -> c
     

let () =
  let c  = preproc !preprocessing in
  let lb = Lexing.from_channel c in
  Printf.printf "Preprocesseur fini \n";
  let prog = SourceParser.prog SourceLexer.token lb in
  close_in c;
  let prog_type_context = SourceTypeChecker.typecheck_program prog in
  let prog = SourceToImp.strip_program prog prog_type_context  in
  let prog = ImpToGoto.translate_program prog in
  let asm = GotoToMips.translate_program prog in
  let output_file = (Filename.chop_suffix file ".cid") ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
