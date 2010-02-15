CC	= ghc
BINARY	= semantic

all: parse

parse: 
	$(CC) --make -o $(BINARY) -fno-warn-missing-fields Main.hs -i../parser -i../lexer

clean: 
	rm -f *.o *.hi $(BINARY)
