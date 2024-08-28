PROGRAM Burbuja
    implicit none
    integer, parameter:: n=200
    integer i, j, aux, ndatos
    real*8 x(n)
    logical acceso

    open (11, file="vector1.txt")   
    read(11,*)
    read(11,*) ndatos
    read(11,*)
    do i = 1, ndatos
       read(11,*) x(i)
    end do
    close (11)

    DO i = 1, ndatos-1
        acceso= .false.
        do j = 1, ndatos-i
            if(x(j)>x(j+1)) then
                aux=x(j)
                x(j)=x(j+1)
                x(j+1)=aux
                acceso= .true.
            end if
        end do
        if(.not.(acceso)) exit
    END DO

    WRITE(*, '(A)') "vector ordenado ="

    do i = 1, ndatos
        WRITE(*, '(F8.2)') x(i)
     end do

END PROGRAM Burbuja