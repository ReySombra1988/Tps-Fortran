program NEWTON_RAWSON
    implicit none
    integer i
    real X, f, g, k

    X=0.5    !valor inicial
    ! f=x**2
    ! f´=2x
    k=0
    do i=1, 16  !while (criterio de corte)
        k=k+1
        X = X-((X**2)/2*X)

        print*, X
    end do
END PROGRAM NEWTON_RAWSON