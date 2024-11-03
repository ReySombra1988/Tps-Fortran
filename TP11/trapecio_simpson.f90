program integracion
        implicit none
        integer :: i, n
        real(8) :: a, b, h, integral_trapecio, integral_simpson, x
      
        ! Definimos los límites y el número de subintervalos
        a = -1.0
        b = 1.0
        n = 10  ! Subintervalos para el método del trapecio
      
        ! Cálculo con la regla del trapecio
        integral_trapecio = metodo_trapecio_compuesto(a, b, n, f1)
      
        ! Cálculo con la regla de Simpson
        integral_simpson = metodo_simpson_compuesto(a, b, n / 2, f1)
      
        ! Mostramos los resultados
        print *, "Integral (Trapecio): ", integral_trapecio
        print *, "Integral (Simpson): ", integral_simpson
      
      contains
      
        ! Función 1: f(x) = 1 / (1 + x^2)
        real(8) function f1(x)
          real(8), intent(in) :: x
          f1 = 1.0 / (1.0 + x**2)
        end function f1
      
        ! Método del Trapecio Compuesto
        real(8) function metodo_trapecio_compuesto(a, b, n, f)
          real(8), intent(in) :: a, b
          integer, intent(in) :: n
          interface
            real(8) function f(x)
              real(8), intent(in) :: x
            end function f
          end interface
          real(8) :: h, sum, x
          integer :: i
      
          h = (b - a) / n
          sum = f(a) + f(b)
      
          do i = 1, n - 1
            x = a + i * h
            sum = sum + 2.0 * f(x)
          end do
      
          metodo_trapecio_compuesto = h * sum / 2.0
        end function metodo_trapecio_compuesto
      
        ! Método de Simpson Compuesto
        real(8) function metodo_simpson_compuesto(a, b, n, f)
          real(8), intent(in) :: a, b
          integer, intent(in) :: n
          interface
            real(8) function f(x)
              real(8), intent(in) :: x
            end function f
          end interface
          real(8) :: h, sum, x
          integer :: i
      
          h = (b - a) / (2 * n)
          sum = f(a) + f(b)
      
          do i = 1, n - 1
            x = a + 2 * i * h
            sum = sum + 2.0 * f(x)
          end do
      
          do i = 1, n
            x = a + (2 * i - 1) * h
            sum = sum + 4.0 * f(x)
          end do
      
          metodo_simpson_compuesto = h * sum / 3.0
        end function metodo_simpson_compuesto
      
end program integracion
