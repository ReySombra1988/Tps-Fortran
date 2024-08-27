program par_impar
    real n
    print*, "Ingrese un numero entero"
    read*, n
    if (n==0) then 
        print*, "el 0 no es ni par ni impar"
    else if (mod(n,2.d0)==0) then
        print*, "par"
    else
        print*, "impar"
    end if
end program par_impar

