default:
	ghc ./Main.hs -o Main

runall:
	make all
	python geoParser.py
	gpu ./Main

run:
	make all
	gpu ./Main

all:
	rm ./Main
	rm ./Main.o
	rm ./Main.hi
	ghc ./Main.hs -o Main
