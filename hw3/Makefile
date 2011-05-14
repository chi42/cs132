BIN = J2V 

OBJECTS = tree_types.cmo minijava_lexar.cmo minijava.cmo table_builder.cmo printers.cmo ir_gen.cmo main.cmo

CMIS = minijava.cmi minijava_lexar.cmi minijava.mli tree_types.cmi table_builder.cmi main.cmi printers.cmi ir_gen.cmi

YACC = ocamlyacc
LEX = ocamllex
CC = ocamlc 

all : $(BIN) 

$(BIN) : $(OBJECTS)
	$(CC) -o $(BIN) $(OBJECTS) 

main.cmo: main.ml tree_types.ml minijava.mly minijava_lexar.mll table_builder.ml ir_gen.ml printers.ml 
	$(CC) -c main.ml 

ir_gen.cmo: ir_gen.ml
	$(CC) -c ir_gen.ml 

tree_types.cmo: tree_types.ml
	$(CC) -c tree_types.ml 

table_builder.cmo: table_builder.ml tree_types.ml
	$(CC) -c table_builder.ml 

minijava_lexar.cmo: minijava_lexar.mll minijava.mly 
	$(LEX) minijava_lexar.mll

minijava.cmo: minijava.mly minijava_lexar.mll tree_types.cmo
	$(YACC) minijava.mly
	$(CC) -c minijava.mli
	$(CC) -c minijava_lexar.ml
	$(CC) -c minijava.ml

printers.cmo: printers.ml
	$(CC) -c printers.ml

clean:
	@- $(RM) $(OBJECTS) $(CMIS)


