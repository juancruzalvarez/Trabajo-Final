program main
   use bairstow
   use quotient_difference
   use pruebas
   use util
   implicit none

   integer                      :: grado, i, go
   real (kind = 8), allocatable :: raices(:), coeficientes(:), raices_estimadas(:)
   real (kind=8), dimension(2)  :: bar
   real(kind = 8) ::x0,x1, e0 = 0.0000001
   complex(kind = 8) disc
   

   write(*,*) 'Ingrese el grado del polinomio:'
   read (*,*)  grado

   allocate(coeficientes(grado+1), raices(grado), raices_estimadas(grado)) 

   write(*,*) 'Ingrese los coeficientes del polinomio:'
   read (*,*) (coeficientes(i), i=1, grado+1)

   !coeficientes = generar_polinomio(grado, raices)
   write(*,*) 'El polimonio del cual estimar las raices es:'
   call mostrar_polinomio(grado, coeficientes)
  
   go = 1
   do while(go == 1)
      bar = aplicar_bairstow(grado, coeficientes, e0)
      write(*,*) 'bar1'
      write(*,*) bar(1)
      write(*,*) 'bar2'
      write(*,*) bar(2)
   
      disc=bar(1)**2 -4*bar(2)
      write(*,*) (-bar(1)/2)+sqrt(disc)/2
      write(*,*) (-bar(1)/2)-sqrt(disc)/2
      coeficientes = dividir_por_factor_cuadratico(grado,coeficientes,bar(1),bar(2))
      grado = grado-2
      read (*,*)go
   end do

   !write(*,*) 'Las raices calculadas con 2 iteraciones de qd son:'
   !call mostrar_vector(calcular_raices_qd(grado, coeficientes, 2), grado, .true.)

    !write(*,*) 'Las raices calculadas con 5 iteraciones de qd son:'
   !call mostrar_vector(calcular_raices_qd(grado, coeficientes, 5), grado, .true.)

   !write(*,*) 'Las raices calculadas con 10 iteraciones de qd son:'
   !call mostrar_vector(calcular_raices_qd(grado, coeficientes, 5), grado, .true.)

   !write(*,*) 'Las raices calculadas con 50 iteraciones de qd son:'
   !call mostrar_vector(calcular_raices_qd(grado, coeficientes, 50), grado, .true.)

   !write(*,*) 'Las raices calculadas con 10 iteraciones de qd, y mejoradas con 5 iteraciones del metodo de newton son:'
   !call mostrar_vector(calcular_raices_qd_nwt(grado, coeficientes, 10, 5), grado, .true.)

 
end program main