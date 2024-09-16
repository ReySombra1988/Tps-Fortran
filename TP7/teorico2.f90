program SECANTE
    implicit none
    integer i
    real X, f, g, k, XA
    XA=1    !valor previo al inicial
    X=0.5    !valor inicial
    ! f=x**2
    ! f´=2x
    k=0
    do i=1, 16  !while (criterio de corte)
        k=k+1
        X = X-((X**2)*(X-XA))/(2*X-2*XA)

        print*, X
    end do
END PROGRAM SECANTE