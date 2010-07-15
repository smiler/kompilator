CC	= ghc
BINARY	= rtl

all: rtlol

rtlol: 
	$(CC) --make -o $(BINARY) -fno-warn-missing-fields Main.hs -i../parser -i../lexer -i../semantic

clean: 
	rm -f *.o *.hi $(BINARY)
