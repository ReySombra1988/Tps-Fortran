program PUNTOFIJO
    implicit none
    integer i
    real x, f, g, k

    x=0.5    !valor inicial
    ! f=x-cos(x)=0
    ! g=cos(x)
    k=0
    do i=1, 16  !while (criterio de corte)
        k=k+1
        x=f(x)

        print*, x
    end do

end program PUNTOFIJO

    Function f(x)
    implicit none
    real f, x
    f=cos(x)
    end Function