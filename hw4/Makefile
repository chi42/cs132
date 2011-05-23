BIN = V2VM

OBJECTS = vtypes.cmo vlexar.cmo vparser.cmo 

CMIS = vlexar.cmi vparser.cmi vtypes.cmi

YACC = ocamlyacc
LEX = ocamllex
CC = ocamlc

all : $(BIN)

$(BIN) : $(OBJECTS)
	$(CC) -o $(BIN) $(OBJECTS)

vtypes.cmo: vtypes.ml 
	$(CC) -c vtypes.ml

vlexar.cmo: vlexar.mll vtypes.ml
	$(LEX) vlexar.mll

vparser.cmo: vparser.mly vlexar.mll vtypes.ml
	$(YACC) vparser.mly
	$(CC) -c vparser.mli
	$(CC) -c vlexar.ml
	$(CC) -c vparser.ml

clean:
	@- $(RM) $(OBJECTS) $(CMIS)
