CC	= ghc
BINARY	= mips

all: build

build: 
	$(CC) --make -o $(BINARY) -fno-warn-missing-fields Main.hs -i../parser -i../lexer -i../rtl

clean: 
	rm -f *.o *.hi $(BINARY)
