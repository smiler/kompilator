CC	= ghc
BINARY	= parser

all: parse

parse: 
	$(CC) --make -o $(BINARY) -fno-warn-missing-fields Main.hs ../lexer/UCLexer.hs

clean: 
	rm -f *.o *.hi $(BINARY)
