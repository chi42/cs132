BIN = V2VM

OBJECTS = exceptions.cmo vtypes.cmo vlexar.cmo vparser.cmo 

CMIS = vlexar.cmi vparser.cmi vtypes.cmi

MLIS = vparser.mli

YACC = ocamlyacc
LEX = ocamllex
CC = ocamlc

all : $(BIN)

$(BIN) : $(OBJECTS)
	$(CC) -o $(BIN) $(OBJECTS)

exceptions.cmo: exceptions.ml
	$(CC) -c exceptions.ml

vtypes.cmo: vtypes.ml exceptions.ml
	$(CC) -c vtypes.ml

vlexar.cmo: vlexar.mll exceptions.ml
	$(LEX) vlexar.mll

vparser.cmo: vparser.mly vlexar.mll vtypes.ml exceptions.ml
	$(YACC) vparser.mly
	$(CC) -c vparser.mli
	$(CC) -c vlexar.ml
	$(CC) -c vparser.ml

clean:
	@- $(RM) $(OBJECTS) $(CMIS) $(MLIS)

flush:
	@- $(RM) $(OBJECTS) $(CMIS) $(MLIS) $(BIN) vparser.ml vlexar.ml

