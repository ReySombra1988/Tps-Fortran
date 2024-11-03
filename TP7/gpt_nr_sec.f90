program root_finding
    implicit none
    real(8) :: tol, x0, x1, root
    integer :: max_iter
  
    ! Definición de la tolerancia y número máximo de iteraciones
    tol = 1.0e-6
    max_iter = 100
  
    ! Ejemplo de valores iniciales
    x0 = 1.5  ! Punto inicial para Newton-Raphson
    x1 = 1.0  ! Segundo punto para la Secante
  
    ! Llamada al método de Newton-Raphson
    print *, "Método de Newton-Raphson:"
    call newton_raphson(x0, tol, max_iter, root)
    print *, "Raíz encontrada (Newton-Raphson): ", root
  
    ! Llamada al método de la Secante
    print *, "Método de la Secante:"
    call secant_method(x0, x1, tol, max_iter, root)
    print *, "Raíz encontrada (Secante): ", root
  
  contains
  
    ! Función objetivo (f(x) = 0) - Cambiar según el problema
    real(8) function f(x)
      real(8), intent(in) :: x
      f = x**3 - 2.0 * x - 5.0  ! Ejemplo: f(x) = x^3 - 2x - 5
    end function f
  
    ! Derivada de la función objetivo - Cambiar según el problema
    real(8) function df(x)
      real(8), intent(in) :: x
      df = 3.0 * x**2 - 2.0  ! Ejemplo: f'(x) = 3x^2 - 2
    end function df
  
    ! Método de Newton-Raphson
    subroutine newton_raphson(x0, tol, max_iter, root)
      real(8), intent(in) :: x0, tol
      integer, intent(in) :: max_iter
      real(8), intent(out) :: root
      integer :: iter
      real(8) :: x, fx, dfx
  
      x = x0
      do iter = 1, max_iter
        fx = f(x)
        dfx = df(x)
        if (abs(dfx) < tol) then
          print *, "La derivada es muy pequeña, el método falla."
          root = x
          return
        end if
        x = x - fx / dfx
        if (abs(fx) < tol) then
          root = x
          return
        end if
      end do
      print *, "El método de Newton-Raphson no converge en ", max_iter, " iteraciones."
      root = x
    end subroutine newton_raphson
  
    ! Método de la Secante
    subroutine secant_method(x0, x1, tol, max_iter, root)
      real(8), intent(in) :: x0, x1, tol
      integer, intent(in) :: max_iter
      real(8), intent(out) :: root
      integer :: iter
      real(8) :: x_prev, x_curr, x_next, fx_curr, fx_prev
  
      x_prev = x0
      x_curr = x1
      do iter = 1, max_iter
        fx_curr = f(x_curr)
        fx_prev = f(x_prev)
        if (abs(fx_curr - fx_prev) < tol) then
          print *, "División por cero potencial, el método falla."
          root = x_curr
          return
        end if
        x_next = x_curr - fx_curr * (x_curr - x_prev) / (fx_curr - fx_prev)
        if (abs(f(x_next)) < tol) then
          root = x_next
          return
        end if
        x_prev = x_curr
        x_curr = x_next
      end do
      print *, "El método de la Secante no converge en ", max_iter, " iteraciones."
      root = x_curr
    end subroutine secant_method
  
  end program root_finding
  