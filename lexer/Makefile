CC	= ghc
BINARY	= lexer

all: lex

lex: 
	alex -g -o UCLexer.hs uc.x
	$(CC) --make -o $(BINARY) Main.hs

clean: 
	rm -f *.o *.hi UCLexer.hs $(BINARY)
