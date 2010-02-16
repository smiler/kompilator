CC	= ghc
BINARY	= rtl

all: rtl

rtl: 
	$(CC) --make -o $(BINARY) -fno-warn-missing-fields Main.hs -i../parser -i../lexer

clean: 
	rm -f *.o *.hi $(BINARY)
