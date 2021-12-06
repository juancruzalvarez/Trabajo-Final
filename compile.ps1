&gfortran -J obj/ -c src/util.f90 -o obj/util.o
&gfortran -J obj/ -c src/bairstow.f90 -o obj/bairstow.o
&gfortran -J obj/ -c src/newton.f90 -o obj/newton.o
&gfortran -J obj/ -c src/pruebas.f90 -o obj/pruebas.o
&gfortran -J obj/ -c src/quotient_difference.f90 -o obj/quotient_difference.o
&gfortran -I obj/ -c src/main.f90 -o obj/main.o
&gfortran -o bin/quotient_difference.exe obj/main.o obj/util.o obj/bairstow.o obj/newton.o obj/pruebas.o obj/quotient_difference.o 