module util

character(len=20), parameter :: FMT_NUMEROS = "(F12.8)"

contains
subroutine mostrar_vector(vec, n, mismaLinea)
   implicit none
   integer                       :: n, i
   real (kind = 8), dimension(n) :: vec
   logical                       :: mismaLinea !si es verdadero todos los elementos del vector, de ser posible se mostrar en una misma linea, sino se mostraran uno por linea

   do i=1, n
      if (mismaLinea) then 
         write (*, FMT_NUMEROS, advance = "no") vec(i)
      else 
         write (*,FMT_NUMEROS) vec(i)
      end if
   end do
   write(*,*) 

end subroutine

subroutine mostrar_matriz(mat, n, m)
   implicit none
   integer                         :: n, m, i, j
   real (kind = 8), dimension(n,m) :: mat

   do i=1, n
      call mostrar_vector(mat(i,:), m, .true.)
      write(*,*) 
   end do

end subroutine mostrar_matriz

subroutine mostrar_polinomio(grado, coeficientes)
   implicit none
   integer                             :: grado, i
   real (kind = 8), dimension(grado+1) :: coeficientes

   do i=1, grado+1
      if(coeficientes(i) .GE. 0) then
         write (*,'(A,F0.4,A,I0)', advance = "no") '+ ', coeficientes(i),'x^', grado+1-i
      else
         write (*,'(A,F0.4,A,I0)', advance = "no") ' ', coeficientes(i),'x^', grado+1-i
      endif
   end do
   write(*,*)

end subroutine


function calcular_derivada(grado, coeficientes)
   implicit none
   integer                             :: grado, i
   real (kind = 8), dimension(grado+1) :: coeficientes
   real (kind = 8), dimension(grado)   :: calcular_derivada

   do i=1, grado
      calcular_derivada(i) = coeficientes(i)*(grado-i +1)
   end do

end function calcular_derivada

function evaluar_polinomio(grado, coeficientes, x)
   implicit none
   integer                             :: grado, i
   real (kind = 8), dimension(grado+1) :: coeficientes
   real (kind = 8)                     :: x, valor, evaluar_polinomio

   do i=1, grado+1
      valor = valor + coeficientes(i)*(x**(grado+1-i)) 
   end do 
   evaluar_polinomio = valor;

end function evaluar_polinomio

! devuelve el resultado de dividir un polinomio por (x-x0), siendo x0 una de las raices del polinomio
! realiza la division por rufini
function dividir_por_factor_lineal(grado, coeficientes, x0)
   implicit none
   integer                           :: grado, i
   real (kind=8)                     :: x0, aux
   real (kind=8), dimension(grado)   :: dividir_por_factor_lineal
   real (kind=8), dimension(grado+1) :: coeficientes

   aux = 0
   do i=1, grado+1
      dividir_por_factor_lineal(i) = coeficientes(i) + aux
      aux = (coeficientes(i) + aux) * x0
   end do

end function dividir_por_factor_lineal

! devuelve el resultado de dividir un polinomio por (x^2 + bx + c)
function dividir_por_factor_cuadratico(grado, coeficientes, b, c)
   implicit none
   integer                           :: grado, i
   real (kind=8)                     :: b, c
   real (kind=8), dimension(grado-1) :: dividir_por_factor_cuadratico
   real (kind=8), dimension(grado+1) :: coeficientes, aux

   aux = coeficientes
   do i=1, grado-1
      dividir_por_factor_cuadratico(i) = aux(i)
      aux(i+1) = aux(i+1) - b*aux(i)
      aux(i+2) = aux(i+2) - c*aux(i)  
   end do

end function dividir_por_factor_cuadratico


end module util