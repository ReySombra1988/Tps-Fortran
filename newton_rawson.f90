program newton_rawson
    implicit none
    real xn, xn1, error, tolerancia, a
    tolerancia=1e-7
    print*, "Ingrese numero para calcularle su raiz"
    read*, a
    print*, "Ingrese su estimacion de la raiz"
    read*, xn
    do while (error>tolerancia)
        xn1=(xn+a/xn)/2
        error=abs(xn1-xn)
        xn=xn1
    end do
    print*, "la raiz de", a, "es:", xn
end program newton_rawson

    