program raices
    implicit none
    real discriminante, x1, x2, x1i, x2i, a, b, c
    print*, "Ingrese los coeficientes del polinomio"
    print*, "Ingrese a"
    read*,  a
    print*, "Ingrese b"
    read*,  b
    print*, "Ingrese c"
    read*,  c
    discriminante = b**2-4*a*c 
    if ( discriminante>=0 ) then
        x1=(-b+sqrt(discriminante))/(2*a )
        x2=(-b-sqrt(discriminante))/(2*a )
        print*, "Las raices son:", x1, x2 
    else
        x1=((-b)/(2*a ))
        x1i=((sqrt(-discriminante))/(2*a ))
        x2i=((-sqrt(-discriminante))/(2*a ))

        print*, "Las raices son:", x1,"+", x1i, "i", x1, "+", x2i, "i" 
    end if
end program raices