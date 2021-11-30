program main
   use quotient_difference
   use util
   implicit none

   integer           :: grado, iteraciones, i
   real, allocatable :: coeficientes(:)

   write(*,*) 'Ingrese el grado del polinomio:'
   read(*,*)  grado

   allocate(coeficientes(grado+1)) 

   write(*,*) 'Ingrese los coeficientes separados por un espacio, comenzando por el coeficiente principal:'
   read(*,*)  (coeficientes(i), i=1, grado+1)
   write(*,*) 'Ingrese la cantidad de iteraciones a realizar:'
   read(*,*) iteraciones

   call mostrar_raices_matriz(grado, coeficientes, iteraciones)
   read(*,*)
end program main