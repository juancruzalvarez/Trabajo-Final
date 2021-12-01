module pruebas
   implicit none

   contains
   !genera un polinomio dadas sus raices, realizando repetidamente la operacion nuevoP = P * (x - x0) = P*x - Px0
   !se realiza cada una de las operaciones por separado
   !P*x simplemente corre todos los coeficientes a la izquierda una posicion
   !P*x0 multiplica todos los coeficientes por x0
   ! p1- p2 resta todos los coeficientes 
   ! ej: P(x) = 1*x2 + 2*x1 + 1
   !     nuevo P(x) = (1*x2 + 2*x1 + 1) * (x1-3)
   !     nuevo P(x) = (1*x2 + 2*x1 + 1) * x1   - (1*x2 + 2*x1 + 1) * 3
   !     nuevo P(x) =      polinomio_por_x     -   polinomio_por_x0
   !     nuevo P(x) =  1*x3 + 2*x2 + 1*x1 + 0 -  3*x2 + 6*x1 + 3
   !     nuevo P(x) =  1*x3 - 1*x2 -5*x1 -3
   function generar_polinomio(grado, raices)
      implicit none
      integer                           :: grado, i
      real (kind=8), dimension(grado+1) :: generar_polinomio, polinomio_por_x, polinomio_por_x0, polinomio_actual
      real (kind=8), dimension(grado)   :: raices
      do i=1, grado
         polinomio_actual(i) = 0
      end do
      polinomio_actual(grado+1) = 1                                  !P(x) = 1
      do i=1, grado
         polinomio_por_x  = eoshift (polinomio_actual, shift = 1)   !multiplicar por x
         polinomio_por_x0 = polinomio_actual * raices(i)            !multiplicar por x0
         polinomio_actual = polinomio_por_x - polinomio_por_x0      !restar coeficiente a coeficiente
      end do
      generar_polinomio = polinomio_actual
   end function
end module pruebas