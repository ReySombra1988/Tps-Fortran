program bin_to_txt
    implicit NONE
    integer :: ios, i, ndatos
    real :: data
    ndatos=20
    open (11, file="datos.bin", form="unformatted", status = "old", action="read", iostat=ios)
if (ios /=0) then
    print*, "error al abrir el archivo de texto"
    stop
end if

open(12, file="datos.txt", status="replace", action="write")

do i = 1, ndatos
read(11, iostat=ios) data
if (ios /=0) exit
write(12, '(F8.2)') data
end do

close(11)
close(12)
    end program bin_to_txt