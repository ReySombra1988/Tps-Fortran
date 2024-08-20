program syracuse
    implicit none
    real n 
    print*, "Ingrese un numero entero positivo"
    read*, n 
    do while (n/=1)
        if(MOD(n, 2)=0) then
        n=n/2
        print*, n 
        else if ( n==13 ) then
            print*, "El numero 13 es de mala suerte"
            n=1
        else
            n=n*3+1
            print*, n
        end if
    end do
end program syracuse
            

