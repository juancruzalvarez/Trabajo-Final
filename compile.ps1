&gfortran -J obj/ -c src/quotient_difference.f90 -o obj/quotient_difference.o
&gfortran -J obj/ -c src/util.f90 -o obj/util.o
&gfortran -I obj/ -c src/main.f90 -o obj/main.o
&gfortran -o bin/quotient_difference.exe obj/main.o obj/util.o obj/quotient_difference.o