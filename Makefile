OBJS = ast.cmo past.cmo parser.cmo scanner.cmo translate_env.cmo check.cmo translate.cmo compile.cmo LFLA.cmo


# Choose one
YACC = ocamlyacc
# YACC = menhir --explain

TARFILES = Makefile scanner.mll parser.mly \
	ast.ml compile.ml LFLA.ml 

LFLA : $(OBJS)
	ocamlc  -o LFLA $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

microc.tar.gz : $(TARFILES)
	cd .. && tar czf microc/microc.tar.gz $(TARFILES:%=microc/%)

.PHONY : clean
clean :
	rm -f LFLA parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff *.pyc *.output *.log *.toc *.pdf *.synctex.gz *.aux 

# Generated by ocamldep *.ml *.mli
ast.cmo: 
ast.cmx: 
past.cmo:
past.cmx:
translate_env.cmo: ast.cmo
translate_env.cmx: ast.cmx
check.cmo: ast.cmo
check.cmx: ast.cmx
compile.cmo: past.cmo 
compile.cmx: past.cmx
translate.cmo: ast.cmo past.cmo check.cmo
translate.cmx: ast.cmx past.cmx check.cmx
LFLA.cmo: scanner.cmo parser.cmi compile.cmo \
    ast.cmo translate.cmo past.cmo
LFLA.cmx: scanner.cmx parser.cmx compile.cmx \
    ast.cmx transalte.cmx past.cmx
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
