program txt_to_bin
    implicit NONE
    integer :: ios, i, ndatos
    real :: data
    ndatos=20
    open (11, file="vector2.txt", status = "old", action="read", iostat=ios)
if (ios /=0) then
    print*, "error al abrir el archivo de texto"
    stop
end if
open(12, file="datos.bin", form="unformatted", status="replace", action="write")

do i = 1, ndatos
    read(11, *, iostat=ios) data
    if (ios /= 0) exit
    write(12) data
end do

close(11)
close(12)
    end program txt_to_bin
