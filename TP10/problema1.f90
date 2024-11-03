program interpolacion_lagrange
    implicit none
    real(8) :: x(3) = (/1.0, 2.0, 2.5/)
    real(8) :: y(3)
    real(8) :: x_eval(2) = (/1.5, 1.2/)
    real(8) :: resultado(2), error(2)
    integer :: i, n
  
    ! Calculamos los valores de la función en los nodos
    y(1) = f(x(1))
    y(2) = f(x(2))
    y(3) = f(x(3))
  
    ! Evaluamos el polinomio en los puntos requeridos
    do i = 1, 2
      resultado(i) = interpolar(x, y, 3, x_eval(i))
      error(i) = abs(f(x_eval(i)) - resultado(i))
      print *, "Evaluación en x = ", x_eval(i), ": ", resultado(i), " Error: ", error(i)
    end do
  
  contains
  
    ! Función a interpolar: f(x) = x + 2/x
    real(8) function f(x)
      real(8), intent(in) :: x
      f = x + 2.0 / x
    end function f
  
    ! Función para interpolación de Lagrange
    real(8) function interpolar(x, y, n, xi)
      real(8), intent(in) :: x(n), y(n), xi
      real(8) :: L, sum
      integer :: i, j
  
      sum = 0.0
      do i = 1, n
        L = 1.0
        do j = 1, n
          if (j /= i) then
            L = L * (xi - x(j)) / (x(i) - x(j))
          end if
        end do
        sum = sum + L * y(i)
      end do
  
      interpolar = sum
    end function interpolar
  
  end program interpolacion_lagrange
  