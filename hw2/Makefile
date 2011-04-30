BIN = Typecheck 

OBJECTS = otree.cmo other_funcs.cmo minijava_lexar.cmo minijava.cmo table_visitor.cmo checker_visitor.cmo main.cmo 

CMIS = otree.cmi other_funcs.cmi minijava.cmi minijava_lexar.cmi table_visitor.cmi main.cmi minijava.mli checker_visitor.cmi

YACC = ocamlyacc
LEX = ocamllex
CC = ocamlc 

all : $(BIN) 

$(BIN) : $(OBJECTS)
	$(CC) -o $(BIN) $(OBJECTS) 
#	$(CC) $(OFLAGS) -o $(BIN) $(OBJECTS) 


main.cmo: main.ml otree.ml other_funcs.ml table_visitor.ml checker_visitor.ml
	$(CC) -c main.ml 

other_funcs.cmo: other_funcs.ml
	$(CC) -c other_funcs.ml 

table_visitor.cmo: table_visitor.ml otree.ml other_funcs.ml 
	$(CC) -c table_visitor.ml

checker_visitor.cmo: checker_visitor.ml otree.ml other_funcs.ml table_visitor.ml
	$(CC) -c checker_visitor.ml

otree.cmo: otree.ml
	$(CC) -c otree.ml

minijava_lexar.cmo: minijava_lexar.mll minijava.mly
	$(LEX) minijava_lexar.mll

minijava.cmo: minijava.mly minijava_lexar.mll
	$(YACC) minijava.mly
	$(CC) -c minijava.mli
	$(CC) -c minijava_lexar.ml
	$(CC) -c minijava.ml

clean:
	@- $(RM) $(OBJECTS) $(CMIS)


