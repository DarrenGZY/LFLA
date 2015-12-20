open Printf

exception Usage of string

let _ =
    let (in_file,out_file) = 
        if Array.length Sys.argv == 2 then 
            (Sys.argv.(1), "a.out")
        else if (Array.length Sys.argv == 4) && (Sys.argv.(2) = "-o") then
            (Sys.argv.(1), Sys.argv.(3))
        else raise (Usage("usage: ./LFLA [filename] or ./LFLA [filename] -o [output]"))
    in
    let s_length = String.length in_file in
    if (String.sub in_file (s_length-3) 3) <> ".la" then 
        raise(Usage("input file should have format filename.la"))
    else
    (* let length = String.length in_file in *)
    let lexbuf = Lexing.from_channel (open_in in_file) in
    (* let in_file_bytes = Bytes.of_string in_file in *)
    (* let out_file = (Bytes.sub_string in_file_bytes 0 (length-2)) ^ "py" in *)
    let program = Parser.program Scanner.token lexbuf in
        let python_program  = Translate.translate program in 
        let pyFile = open_out out_file in
             fprintf pyFile "%s\n" "#!/usr/bin/python";
             fprintf pyFile "%s\n" "import sys";
             fprintf pyFile "%s\n" "sys.path.append('./lib')";
             fprintf pyFile "%s\n" "from InSpace import *";
             fprintf pyFile "%s\n" "from AffSpace import *";
             fprintf pyFile "%s\n" "from Core import * \n";
             fprintf pyFile "%s\n"  (Compile.compile python_program);
             close_out pyFile;
             Sys.command ("chmod +x " ^ out_file) ;
    
