bus.o: Bus.hs
	rm *.hi *.o; ghc --make Bus.hs
