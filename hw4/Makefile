BIN = V2VM

OBJECTS = vtokens.cmo vlexar.cmo vparser.cmo 

CMIS = vtokens.cmi vlexar.cmi vparser.cmi 

YACC = ocamlyacc
LEX = ocamllex
CC = ocamlc

all : $(BIN)

$(BIN) : $(OBJECTS)
	$(CC) -o $(BIN) $(OBJECTS)

vapor_tokens.cmo: vtokens.ml
	$(CC) -c vtokens.ml

vlexar.cmo: vlexar.mll vtokens.ml
	$(LEX) vlexar.mll
	$(CC) -c vlexar.ml

vparser.cmo: vparser.mly vlexar.mll vtokens.ml 
	$(YACC) vparser.mly
	$(CC) -c vparser.mli
	$(CC) -c vparser.ml

clean:
	@- $(RM) $(OBJECTS) $(CMIS)
