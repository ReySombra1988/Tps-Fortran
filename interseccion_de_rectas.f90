program calculadora
    implicit none
    real y11, y12, y21, y22, m1, m2, x11, x12, x21, x22, b1, b2, x, y
print*, "Ingrese la componente en x del primer punto de la primera recta"
read*, x11
print*, "Ingrese la componente en y del primer punto de la primera recta"
read*, y11
print*, "Ingrese la componente en x del segundo punto de la primera recta"
read*, x21
print*, "Ingrese la componente en y del segundo punto de la primera recta"
read*, y21
print*, "Ingrese la componente en x del primer punto de la segunda recta"
read*, x12
print*, "Ingrese la componente en y del primer punto de la segunda recta"
read*, y12
print*, "Ingrese la componente en x del segundo punto de la segunda recta"
read*, x22
print*, "Ingrese la componente en y del segundo punto de la segunda recta"
read*, y22
if (y21-y11==0) then
    m1=0
else
m1=(x21-x11)/(y21-y11)
end if
if (y22-y12==0) then
    m2=0
else
m2=(x22-x12)/(y22-y12)
end if
if (m1==m2)  then
    print*, "Las rectas son paralelas"
else
    b1=y11-m1*x11
    b2=y22-m2*y22
    x=(b1-b2)/(m2-m1)
    y=m2*x+b2
    print*, b1, b2, m2, m1 
    print*, "El  punto de interseccion es:", "(", x, ",", y, ")"
end if
end program calculadora