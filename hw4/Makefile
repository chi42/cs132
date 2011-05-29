BIN = V2VM

OBJECTS = exceptions.cmo vtypes.cmo vlexar.cmo vparser.cmo debug_printers.cmo basic_blocks.cmo main.cmo

CMIS = vlexar.cmi vparser.cmi vtypes.cmi exceptions.cmi main.cmi basic_blocks.cmi debug_printers.cmi


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

debug_printers.cmo: debug_printers.ml exceptions.ml vtypes.ml
	$(CC) -c debug_printers.ml

basic_blocks.cmo: basic_blocks.ml exceptions.ml vtypes.ml debug_printers.ml
	$(CC) -c basic_blocks.ml

main.cmo: main.ml vparser.mly vlexar.mll exceptions.ml vtypes.ml basic_blocks.ml debug_printers.ml
	$(CC) -c main.ml

clean:
	@- $(RM) $(OBJECTS) $(CMIS) $(MLIS)

purge:
	make clean
	@- $(RM) vparser.ml vlexar.ml vparser.output $(BIN)

