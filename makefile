all:
	ghc -O2 --make -threaded -rtsopts Graphics/Image.hs && ghc -O2 --make -threaded -rtsopts -v examples.hs -hide-package unm-hip
