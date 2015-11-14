open Printf

let _ =
    let lexbuf = Lexing.from_channel stdin in 
    let program = Parser.program Scanner.token lexbuf in
        let pyFile = open_out "test.py" in
             fprintf pyFile "%s\n" "import numpy as np";
             fprintf pyFile "%s\n"  (Compile.compile program);
             close_out pyFile;
    
