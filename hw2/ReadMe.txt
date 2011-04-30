-Written in ocaml, and requires ocamlyacc, ocamlex, ocamlc, and make to build. 

-To build:
  make

-I attempted to implement the visitor pattern in ocaml. In retrospect,
this was a bad idea and required forcing a functional language to work in 
ways it was not intended to. It also turned out to be much harder then I 
anticipated, thus the delay. Also, as I later found out, ocaml does not 
have function overloading (aside from virtual functions), thus the 
bloated and awkward code. 

-ocamlyacc finds 7 shift/reduce conflicts, these are resolved via precedence
