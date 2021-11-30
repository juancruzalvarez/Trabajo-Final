module quotient_difference
use util
implicit none
   
contains

   !dado un polinomio calcula y muestra la matriz del algoritmo qd y las raices.
   subroutine mostrar_raices_matriz(grado, coeficientes, iteraciones)
      implicit none

      integer                                   :: grado, iteraciones, i
      real, dimension(iteraciones, grado*2+1)   :: mat
      real, dimension(grado+1)                  :: coeficientes

      mat = matriz_qd(grado, coeficientes, iteraciones)
      write(*,*) 'Matriz del algoritmo qd:'
      call mostrar_matriz(mat, iteraciones, grado*2+1)
      write(*,*) 'Las raices calculadas del polinomio son:'
      do i = 2, grado*2+1, 2
         write(*, '(A,I2,A,F10.4)') 'x',i/2,': ', mat(iteraciones, i)
      end do
   end subroutine mostrar_raices_matriz

   !devuelve las raices del polinomio calculadas por el algoritmo qd. 
   function calcular_raices(grado, coeficientes, iteraciones)
      implicit none

      integer                                   :: grado, iteraciones, i
      real, dimension(iteraciones, grado*2+1)   :: mat
      real, dimension(grado+1)                  :: coeficientes
      real, dimension(grado)                    :: calcular_raices

      mat = matriz_qd(grado, coeficientes, iteraciones)
      do i = 2, grado*2+1, 2
         calcular_raices(i/2) = mat(iteraciones, i)
      end do

   end function calcular_raices

   !devuelve la matriz del algoritmo qd.
   function matriz_qd(grado, coeficientes, iteraciones)
      implicit none

      integer                                   :: grado, iteraciones, i
      real, dimension(iteraciones, grado*2 +1)  :: matriz_qd 
      real, dimension(grado+1)                  :: coeficientes

      matriz_qd(1, :) = iteracion_inicial_qd(grado, coeficientes)
      do i=2, iteraciones
         matriz_qd(i, :) = iteracion_qd(grado, matriz_qd(i-1, :))
      end do
      
   end function matriz_qd

   !devuelve la primer iteracion del algoritmo qd.
   function iteracion_inicial_qd(grado, coeficientes)
      implicit none

      integer                      :: grado
      real, dimension(grado*2 + 1) :: iteracion_inicial_qd
      real, dimension(grado +1)    :: coeficientes
      
      integer :: i

      iteracion_inicial_qd(1)         = 0
      iteracion_inicial_qd(2)         = -coeficientes(2) / coeficientes(1)
      iteracion_inicial_qd(grado*2+1) = 0

      do i=4, grado*2+1, 2
         iteracion_inicial_qd(i) = 0
      end do

      do i=3, grado*2, 2
         iteracion_inicial_qd(i) = coeficientes((i/2) + 2) / coeficientes((i/2) + 1)
      end do

   end function iteracion_inicial_qd

   !devuelve una iteracion del algoritmo qd dada la anterior.
   function iteracion_qd(grado, iteracion_anterior)
      implicit none

      integer                      :: grado, i
      real, dimension(grado*2 + 1) :: iteracion_qd, iteracion_anterior 

      iteracion_qd(1)         = 0
      iteracion_qd(grado*2+1) = 0

      do i = 2, grado*2+1, 2 
         iteracion_qd(i) = iteracion_anterior(i+1) - iteracion_anterior(i-1) + iteracion_anterior(i)
      end do

      do i = 3, grado*2 , 2
         iteracion_qd(i) = (iteracion_qd(i+1)/iteracion_qd(i-1))*iteracion_anterior(i)
      end do
   end function iteracion_qd

end module quotient_difference