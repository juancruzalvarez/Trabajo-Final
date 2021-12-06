module newton
   use util
   implicit none

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

   function mejorar_estimacion(grado, coeficientes, x0, iteraciones)
      implicit none
      integer                           :: grado, iteraciones, i
      real (kind = 8)                   :: mejorar_estimacion, estimacion_actual, x0
      real (kind = 8), dimension(grado+1) :: coeficientes
      estimacion_actual = x0
      do i=0, iteraciones
         estimacion_actual = iteracion_nwt(grado, coeficientes, estimacion_actual)
      end do
      mejorar_estimacion = estimacion_actual
   end function mejorar_estimacion

end module newton