module util

character(len=20), parameter :: FMT_NUMEROS = "(F10.4)"

contains

subroutine mostrar_matriz(mat, n, m)
   implicit none

   integer              :: n, m, i, j
   real, dimension(n,m) :: mat
   do i=1, n
      do j=1, m
         write (*,FMT_NUMEROS, advance = "no") mat(i, j)
      end do
      write(*,*) 
   end do
end subroutine mostrar_matriz

end module util