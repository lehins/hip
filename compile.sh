if [ $2 = "debug" ]; then
		ghc $1 -i/home/lehins/github/hip/ -Wall
		ghc $1 -i/home/lehins/github/hip/ -Wall -prof -auto-all -caf-all -osuf=p_o
else
		ghc $1 -i/home/lehins/github/hip/ -Wall -rtsopts -with-rtsopts=-N -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -optlo-O3 
fi
