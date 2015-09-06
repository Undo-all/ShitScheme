FILES=$(shell echo *.hs)

all: $(FILES)
	ghc *.hs -O2
	rm *.hi *.o

