program lagrange
    implicit none
    real(8) :: x(4) = (/1.0, 2.0, 3.0, 5.0/)
    real(8) :: y(4) = (/3.0, 6.0, 19.0, 99.0/)
    real(8) :: xi, resultado
    integer :: n, i
  
    n = 4
    xi = 4.0  ! Valor a interpolar
  
    ! Llamamos a la función que calcula el polinomio de Lagrange
    resultado = interpolar_lagrange(x, y, n, xi)
  
    print *, "El valor interpolado en x = ", xi, " es: ", resultado
  
  contains
  
    ! Función que calcula la interpolación de Lagrange
    real(8) function interpolar_lagrange(x, y, n, xi)
      real(8), intent(in) :: x(n), y(n), xi
      integer, intent(in) :: n
      real(8) :: L, sum
      integer :: i, j
  
      sum = 0.0
  
      ! Calculamos la suma ponderada de los términos de Lagrange
      do i = 1, n
        L = 1.0
        do j = 1, n
          if (j /= i) then
            L = L * (xi - x(j)) / (x(i) - x(j))
          end if
        end do
        sum = sum + L * y(i)
      end do
  
      interpolar_lagrange = sum
    end function interpolar_lagrange
  
  end program lagrange
  