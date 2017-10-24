all:
	make clean
	ghc ./Main.hs -o Main

clean:
	@if [ -f ./Main ];\
		then\
		rm ./Main 2> /dev/null;\
	else\
		echo "./Main already clean";\
	fi

	@if [ -f ./Main.o ];\
		then\
		rm ./Main.o 2> /dev/null;\
	else\
		echo "./Main.o already clean";\
	fi	

run:
	make clean &&	make &&	gpu ./Main
