module bairstow
   implicit none

   integer, parameter :: MAX_ITER = 6000
   contains
   function aplicar_bairstow(grado, coeficientes, cota_error)
      implicit none

      integer                          :: grado, i
      real(kind=8), dimension(2)       :: aplicar_bairstow
      real(kind=8), dimension(3)       :: valores_actuales
      real(kind=8), dimension(grado+1) :: coeficientes
      real(kind=8)                     :: cota_error

      if(grado <3) then
         return
      endif

      if(coeficientes(grado) /= 0) then
         valores_actuales(1) = coeficientes(grado+1)/coeficientes(grado)
      else
         valores_actuales(1) = 1
      endif
      valores_actuales(2) = valores_actuales(1)
      valores_actuales(3) = 1000000
      i = 0
      do while(valores_actuales(3) > cota_error .and. i < MAX_ITER)
         valores_actuales = iteracion_bairstow(grado, coeficientes, valores_actuales)
         i = i+1
      end do
      write(*,*) 'Iteraciones baristow:'
      write(*,*) i
      aplicar_bairstow = -1 * valores_actuales(1:2)
   end function aplicar_bairstow

   function iteracion_bairstow(grado, coeficientes, uv)
      implicit none

      integer                          :: grado, i
      real(kind=8), dimension(3)       :: iteracion_bairstow, uv
      real(kind=8), dimension(grado+1) :: coeficientes, q, p
      real(kind=8)                     :: qm1, qm2, pm1, pm2, h, k
      qm1 = 0
      qm2 = 0
      pm1 = 0
      pm2 = 0
      do i=1, grado+1 
         q(i) = coeficientes(i) + uv(1)*qm1 + uv(2)*qm2
         p(i) = q(i) + uv(1)*pm1 +uv(2)*pm2
         qm2 = qm1
         qm1 = q(i)
         pm2 = pm1
         pm1 = p(i)
      end do


      h = (q(grado+1)*p(grado-2) - q(grado)*p(grado-1)) / (p(grado-1)**2 -p(grado)*p(grado-2))
      k = (q(grado)*p(grado)  -  q(grado+1)*p(grado-1)) / (p(grado-1)**2 -p(grado)*p(grado-2))

      iteracion_bairstow(1) = uv(1) + h
      iteracion_bairstow(2) = uv(2) + k
      iteracion_bairstow(3) = ( abs(q(grado+1)) + abs(q(grado)) )/2
      
   end function iteracion_bairstow

end module bairstow