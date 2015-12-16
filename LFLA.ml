open Printf

let _ =
    let in_file = if Array.length Sys.argv == 2 then Sys.argv.(1)
                    else raise (Failure ("usage: ./LFLA [filename]"))
    in
    (* let length = String.length in_file in *)
    let lexbuf = Lexing.from_channel (open_in in_file) in
    (* let in_file_bytes = Bytes.of_string in_file in *)
    (* let out_file = (Bytes.sub_string in_file_bytes 0 (length-2)) ^ "py" in *)
    let program = Parser.program Scanner.token lexbuf in
        let python_program, env = Translate.translate program in 
        let pyFile = open_out "a.out" in
             fprintf pyFile "%s\n" "#!/usr/bin/python";
             fprintf pyFile "%s\n" "import sys";
             fprintf pyFile "%s\n" "sys.path.append('./lib')";
             fprintf pyFile "%s\n" "from InSpace import *";
             fprintf pyFile "%s\n" "from AffSpace import *\n";
             fprintf pyFile "%s\n"  (Compile.compile python_program);
             close_out pyFile;
             Sys.command ("chmod +x a.out") ;
    
