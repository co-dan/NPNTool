all:
	ghc -O2 --make -i../../src/ Main.hs -prof -auto-all -rtsopts

prof: Main
	./Main +RTS -p -hy -K64m

clean:
	rm *.o
	rm *.hi
	rm Main
