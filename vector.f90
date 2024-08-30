program random_number
    implicit none
    integer, parameter:: n=200
    integer i, j, ndatos, ios
    real*8 x(n)
    logical acceso
    real :: aleatorio_real, aux
    integer :: aleatorio_entero
    CHARACTER(LEN=1) :: dummy, adm, oo, aa
    CHARACTER(LEN=200) texto

    adm=char(173)
    oo=char(162)
    aa=char(160)
    ndatos=0
texto= "A continuaci" //oo// "n deber" //aa// " ingresar su vector en el archivo de texto que se le proporcionar" //aa 
print"(A)", texto
print"(A)","Guarde y cierre el archivo al terminar"
print*, "Ingrese cualquier letra para abrir el archivo"
read*, dummy
CALL SYSTEM('vector1.txt')
    open (11, file="vector1.txt")   
    read(11,*)
    do
        read(11,*, iostat=ios) aux
        if (ios /= 0) exit
       ndatos=ndatos+1
           end do 
close (11)

open (11, file="vector1.txt")   
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

    WRITE(*, '(A)') "Vector ordenado ="

    do i = 1, ndatos
        WRITE(*, '(F8.2)') x(i)
     end do
!comienzo a desordenar
do i = 1, ndatos
    CALL RANDOM_NUMBER(aleatorio_real)
    aleatorio_entero=Int(aleatorio_real*ndatos)+1
    aux=x(i)
    x(i)=x(aleatorio_entero)
    x(aleatorio_entero)=aux
end do

WRITE(*, '(A)') "Vector desordenado nuevamente ="

do i = 1, ndatos
    WRITE(*, '(F8.2)') x(i)
 end do
 texto= adm// "Gracias por usar el ordenador mezclador!"
print"(A)", texto
print*, "Ingrese cualquie letra para salir"
read*, dummy
    end program random_number