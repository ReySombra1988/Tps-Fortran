program producto_punto
    implicit none
    integer, parameter :: i=10
    integer n, j, d, A(i), B(i), C(i)
A=0
B=0
c=0
    print*, "Ingrese la dimension de los vectores"
    read*, n
    do j=1, n
        print*, "ingrese el valor", j, "del vector A"
        read*, A(j)
    end do
    do j=1, n
        print*, "ingrese el valor", j, "del vector B"
        read*, B(j)
    end do
    print*, A
    print*, B
    C=A*B
    print*, C
    d=0
do j=1, n
d=d+C(j)
end do
print*, "El producto punto es:", d
d=0
d=dot_product(A,B)
print*, "Corroboro que el producto punto es", d


end program producto_punto