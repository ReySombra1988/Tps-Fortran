program txt_to_bin
    implicit NONE
    integer :: ios, ndatos,  i
    character(len=100) :: data
    ndatos=3
    open (11, file="vector3.txt", status = "old", action="read", iostat=ios)
if (ios /=0) then
    print*, "error al abrir el archivo de texto"
    stop
end if
open(12, file="texto.bin", form="unformatted", status="replace", action="write")

do i = 1, ndatos
    read(11, '(A)', iostat=ios) data
    if (ios /= 0) exit
    write(12) data
end do

close(11)
close(12)
    end program txt_to_bin