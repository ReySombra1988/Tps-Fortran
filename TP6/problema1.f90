program Problema_1
    implicit none
    integer i
    real x, f, g, k

    x=0.5    !valor inicial
    
    k=0
    do i=1, 16  !while (criterio de corte)
        k=k+1
        x=x**2+x-4

        print*, x
    end do
    
end program Problema_1