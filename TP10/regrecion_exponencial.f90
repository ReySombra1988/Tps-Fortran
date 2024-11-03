program regresion_exponencial
    implicit none
    real(8) :: x(4) = (/1.0, 2.0, 3.0, 4.0/)
    real(8) :: y(4) = (/2.7, 7.3, 20.0, 54.6/)
    real(8) :: A, C
  
    ! Llamamos a la subrutina que realiza la regresión exponencial
    call ajuste_exponencial(x, y, 4, A, C)
  
    print *, "Curva ajustada: y = ", C, "*exp(", A, "*x)"
  
  contains
  
    ! Subrutina para ajuste exponencial
    subroutine ajuste_exponencial(x, y, n, A, C)
      real(8), intent(in) :: x(n), y(n)
      integer, intent(in) :: n
      real(8), intent(out) :: A, C
      real(8) :: sum_x, sum_y_ln, sum_xy_ln, sum_x2
      integer :: i
  
      sum_x = 0.0
      sum_y_ln = 0.0
      sum_xy_ln = 0.0
      sum_x2 = 0.0
  
      do i = 1, n
        sum_x = sum_x + x(i)
        sum_y_ln = sum_y_ln + log(y(i))
        sum_xy_ln = sum_xy_ln + x(i) * log(y(i))
        sum_x2 = sum_x2 + x(i)**2
      end do
  
      A = (n * sum_xy_ln - sum_x * sum_y_ln) / (n * sum_x2 - sum_x**2)
      C = exp((sum_y_ln - A * sum_x) / n)
    end subroutine ajuste_exponencial
  
  end program regresion_exponencial
  