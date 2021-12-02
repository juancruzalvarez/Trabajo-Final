program main
   use quotient_difference
   use pruebas
   use util
   implicit none

   integer                      :: grado, i
   real (kind = 8), allocatable :: raices(:), coeficientes(:), raices_estimadas(:)
   real (kind=8), dimension(3)  :: factor_cuad

   write(*,*) 'Ingrese el grado del polinomio:'
   read (*,*)  grado

   allocate(coeficientes(grado+1), raices(grado), raices_estimadas(grado)) 

   write(*,*) 'Ingrese las raices del polinomio:'
   read (*,*) (raices(i), i=1, grado)

   coeficientes = generar_polinomio(grado, raices)
   write(*,*) 'El polimonio del cual estimar las raices es:'
   call mostrar_polinomio(grado, coeficientes)


   write(*,*) 'Las raices calculadas con 2 iteraciones de qd son:'
   call mostrar_vector(calcular_raices_qd(grado, coeficientes, 2), grado, .true.)

    write(*,*) 'Las raices calculadas con 5 iteraciones de qd son:'
   call mostrar_vector(calcular_raices_qd(grado, coeficientes, 5), grado, .true.)

   write(*,*) 'Las raices calculadas con 10 iteraciones de qd son:'
   call mostrar_vector(calcular_raices_qd(grado, coeficientes, 5), grado, .true.)

   write(*,*) 'Las raices calculadas con 50 iteraciones de qd son:'
   call mostrar_vector(calcular_raices_qd(grado, coeficientes, 50), grado, .true.)

   write(*,*) 'Las raices calculadas con 10 iteraciones de qd, y mejoradas con 5 iteraciones del metodo de newton son:'
   call mostrar_vector(calcular_raices_qd_nwt(grado, coeficientes, 10, 5), grado, .true.)

   write(*,*) 'El polinomio dividido la primer raiz ingresada es:'
   call mostrar_polinomio(grado-1, dividir_por_factor_lineal(grado, coeficientes, raices(1)))

   factor_cuad = generar_polinomio(2,  raices(1:2))
   write(*,*) 'El polinomio dividido las primeras 2 raices ingresadas es:'
   call mostrar_polinomio(grado-2, dividir_por_factor_cuadratico(grado, coeficientes,factor_cuad(2),factor_cuad(3)))
   read (*,*)
end program main