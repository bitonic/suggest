
all:
	ghc -O2 -Wall Suggest.hs

clean:
	rm -rf Suggest *.o *.hi

