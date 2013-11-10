all:
	ghc -O2 --make -threaded -rtsopts Data/Image.hs && ghc -O2 --make -threaded -rtsopts -v examples.hs -hide-package unm-hip
