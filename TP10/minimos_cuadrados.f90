program ajuste_minimos_cuadrados
    implicit none
    real(8) :: x(4) = (/1.0, 2.0, 3.0, 5.0/)
    real(8) :: y(4) = (/3.0, 6.0, 19.0, 99.0/)
    real(8) :: a, b
  
    ! Llamamos a la subrutina que realiza el ajuste
    call ajuste_lineal(x, y, 4, a, b)
  
    print *, "Recta ajustada: y = ", a, "*x + ", b
  
  contains
  
    ! Subrutina que realiza el ajuste lineal
    subroutine ajuste_lineal(x, y, n, a, b)
      real(8), intent(in) :: x(n), y(n)
      integer, intent(in) :: n
      real(8), intent(out) :: a, b
      real(8) :: sum_x, sum_y, sum_xy, sum_x2
      integer :: i
  
      sum_x = 0.0
      sum_y = 0.0
      sum_xy = 0.0
      sum_x2 = 0.0
  
      do i = 1, n
        sum_x = sum_x + x(i)
        sum_y = sum_y + y(i)
        sum_xy = sum_xy + x(i) * y(i)
        sum_x2 = sum_x2 + x(i)**2
      end do
  
      a = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x**2)
      b = (sum_y - a * sum_x) / n
    end subroutine ajuste_lineal
  
  end program ajuste_minimos_cuadrados
  