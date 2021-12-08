&gfortran -J obj/ -c src/util.f90 -o obj/util.o -fcheck=all -fbacktrace
&gfortran -J obj/ -c src/bairstow.f90 -o obj/bairstow.o -fcheck=all -fbacktrace
&gfortran -J obj/ -c src/newton.f90 -o obj/newton.o -fcheck=all -fbacktrace
&gfortran -J obj/ -c src/pruebas.f90 -o obj/pruebas.o -fcheck=all -fbacktrace
&gfortran -J obj/ -c src/quotient_difference.f90 -o obj/quotient_difference.o -fcheck=all -fbacktrace
&gfortran -I obj/ -c src/main.f90 -o obj/main.o -fcheck=all -fbacktrace
&gfortran -o bin/quotient_difference.exe obj/main.o obj/util.o obj/bairstow.o obj/newton.o obj/pruebas.o obj/quotient_difference.o -fcheck=all -fbacktrace