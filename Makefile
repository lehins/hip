all: Main

Main: Main.hs
	ghc $@ -i/home/lehins/github/hip/ -Wall -rtsopts -with-rtsopts=-N -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -optlo-O3
	./Main +RTS

debug: Main.hs
	ghc Main -i/home/lehins/github/hip/ -Wall
	ghc Main -i/home/lehins/github/hip/ -Wall -prof -auto-all -caf-all -osuf=p_o
	./Main +RTS -xc

clean:
	find . -type f -name '*.o' -delete
	find . -type f -name '*.p_o' -delete
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.dyn_*' -delete
	rm Main
