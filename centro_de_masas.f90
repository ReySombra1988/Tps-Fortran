program centro_de_masa
    integer, parameter::n=200
    real*8 x(n), y(n), m(n)
 open (11, file="datos.txt")   
 read(11,*)
 read(11,*) ndatos
 read(11,*)
 do i=1,ndatos
    read(11,*) x(i), y(i), m(i)
 end do
 close (11)
 print*, x(4)
 end program centro_de_masa

