open Core.Std

let _ =
    let lexbuf = Lexing.from_channel stdin in 
    let program = Parser.program Scanner.token lexbuf in
        let pyFile = Out_channel.create "test.py" in
             fprintf pyFile "%s\n" "import numpy as np";
             fprintf pyFile "%s\n"  (Compile.compile program);
             Out_channel.close pyFile 
    
