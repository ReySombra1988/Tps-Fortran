program calculadora
    implicit none
    real a, b, c, discriminante, x1, x2, x1i, x2i, j
    character*10 m
    j=0
    do  while (j==0)
    print*, "Indique que operacion desea realizar:"
    print*, "SUMA, RESTA, PRODUCTO, DIVISION, POTENCIA", " y calculo de las RAICES de una ecuacion de 2do. grado"
    read*,  m
    select case (m)
    case ("suma")
        print*, "Ingrese el primer valor"
        read*,  a
        print*, "Ingrese el segundo valor"
        read*,  b
        x1=a+b
        print*, "El resultado es:", x1
        j=1
    case ("resta")
        print*, "Ingrese el primer valor"
        read*,  a
        print*, "Ingrese el segundo valor"
        read*,  b
        x1=a-b
        print*, "El resultado es:", x1
        j=1
    case ("producto")
        print*, "Ingrese el primer valor"
        read*,  a
        print*, "Ingrese el segundo valor"
        read*,  b
        x1=a*b
        print*, "El resultado es:", x1
        j=1
    case ("DIVISION")
        print*, "Ingrese el numerador"
        read*,  a
        print*, "Ingrese el denominador"
        read*,  b
        x1=a/b
        print*, "El resultado es:", x1
        j=1
    case ("potencia")
        print*, "Ingrese la base"
        read*,  a
        print*, "Ingrese la potencia"
        read*,  b
        x1=a**b
        print*, "El resultado es:", x1
        j=1
    case ("raices")
        print*, "Ingrese el valor de a"
        read*,  a
        print*, "Ingrese el valor de b"
        read*,  b
        print*, "Ingrese el valor de c"
        read*,  c
        j=1
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
    end select
    if (j==0) then
        print*, "Boluda, escribi bien"
    else
        print*, "Gracias por usar la calculadora"
    end if
end do
end program calculadora