program cantidad_datos
    implicit none
    integer ndatos
    real line
    integer ios
    ndatos=0
    open (11, file="ndatos.txt")   
    do
 read(11,*, iostat=ios) line
 if (ios /= 0) exit
ndatos=ndatos+1

    end do 
    close (11)
    print*, ndatos
end program cantidad_datos