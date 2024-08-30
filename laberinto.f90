program laberinto
    implicit none
    integer, parameter :: n=6
    integer i, a
    integer :: random_value
    character(len=3) :: sal1(n), sal2(n), nom1(n), nom2(n), nom3(n), com(n), desc(n), hab(n), sal3(n), res, aux, j, current_pos
    hab= (/"1","2","3","4","5","6"/)
    sal2= (/"e", "1", "2", "3", "4", "5"/)
    sal3= (/"2", "3", "4", "5", "6", "s"/)
    com= (/"no", "su", "es", "oe", "ar", "ab"/)
    sal1=(/"n","n","n","n","n","n"/)

    current_pos="1"



     ! Asignar valores aleatorios al vector nom1
    do i = 1, n
        random_value = generate_random_integer()
        nom1(i) = hab(random_value)
    end do
nom2(1)="e"
    do i = 2, n
        random_value = generate_random_integer()
        nom2(i) = hab(random_value)
    end do
nom3(6)="s"
    do i = 1, n-1
        random_value = generate_random_integer()
        nom3(i) = hab(random_value)
    end do
    do i = 1, n
        select case (i)
        case(1)
            j="1"
        case(2)
            j="2"
        case(3)
            j="3"
        case(4)
            j="4"
        case(5)
            j="5"
        case(6)
            j="6"
            end select
        if (sal1(i)=="n") then
            do
            random_value = generate_random_integer()
            sal1(i) = hab(random_value)
            aux=sal1(i)
            select case (aux)
            case("1")
                a=1
            case("2")
                a=2
            case("3")
                a=3
            case("4")
                a=4
            case("5")
                a=5
            case("6")
                a=6
                end select
            if (sal1(a)=="n") then
            sal1(a)=j
            exit
            end if
            end do
        end if
    end do

    print*, "sal 1 es:", sal1, "asi"
    print*, nom1
    print*, nom2
    print*, nom3
    print*, hab
    print*, sal3
    print*, com
    print*, sal2

contains

function generate_random_integer() result(aleatorio_entero)
    implicit none
    integer :: aleatorio_entero
    real :: aleatorio_real

    call random_number(aleatorio_real)
    aleatorio_entero = int(aleatorio_real * n) + 1
end function generate_random_integer

end program laberinto