
all:
	ghc -O2 Suggest.hs

clean:
	rm -rf Suggest *.o *.hi

