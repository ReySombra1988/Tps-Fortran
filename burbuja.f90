PROGRAM Burbuja
    implicit none
    integer, parameter:: n=5
    integer vector (n), i, j, aux
    logical acceso

    vector = (/2,5,-4,9,1/)

    DO i = 1, n-1
        acceso= .false.
        do j = 1, n-i
            if(vector(j)>vector(j+1)) then
                aux=vector(j)
                vector(j)=vector(j+1)
                vector(j+1)=aux
                acceso= .true.
            end if
        end do
        if(.not.(acceso)) exit
    END DO

    write(*,*) "vector ordenado =", vector

END PROGRAM Burbuja