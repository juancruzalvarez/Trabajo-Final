module newton
   use util
   implicit none

   integer, parameter :: MAX_ITER = 600
   private MAX_ITER

   contains

   !dado una polinomio y una raiz aproximada x0, devuelve una estimacion mejorada por el metodo de newton.
   !este metodo obtiene una nueva estimacion aproximando el polinomio linealmente mediante su derivada, e igualando esta aproximacion a 0
   function iteracion_nwt(grado, coeficientes, x0)
      implicit none
      integer                           :: grado
      real (kind = 8)                   :: iteracion_nwt, x0, y0, pendiente_tangente
      real (kind = 8), dimension(grado+1) :: coeficientes

      y0 = evaluar_polinomio(grado, coeficientes, x0)                                              ! y0 = P(x0)
      pendiente_tangente = evaluar_polinomio(grado-1, calcular_derivada(grado, coeficientes), x0)  ! pendiente = P'(x0)
      iteracion_nwt = x0 - y0 / pendiente_tangente                                                 ! P'(x0)*(x-x0) + y0 = 0 => x = x0 -y0/P'(x0), donde x es la nueva estimacion de la raiz.
   end function iteracion_nwt

   function mejorar_estimacion(grado, coeficientes, x0, cota_error)
      implicit none
      integer                             :: grado, i
      real (kind = 8)                     :: mejorar_estimacion, estimacion_actual, estimacion_anterior, x0, cota_error
      real (kind = 8), dimension(grado+1) :: coeficientes

      i=0
      estimacion_actual = x0
      estimacion_anterior = x0 + 2*cota_error
      do while(abs(estimacion_actual-estimacion_anterior) > cota_error .and. i<MAX_ITER)
         estimacion_anterior = estimacion_actual
         estimacion_actual = iteracion_nwt(grado, coeficientes, estimacion_actual)
         i = i+1
      end do
      mejorar_estimacion = estimacion_actual
      write(*,*) 'Iteraciones newton:'
      write(*,*) i
   end function mejorar_estimacion

end module newton