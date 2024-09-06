program laberinto
    implicit none
    integer, parameter :: n=6
    integer, parameter :: m=300
    integer i, a, current_pos, ios
    integer :: random_value, ndic, ndic2
    integer :: sal1(n), habi(n)
    CHARACTER(LEN=100) :: egresos (m)
    CHARACTER(LEN=100) :: ingresos (m)
    CHARACTER(LEN=100) :: comandos (m) 
    CHARACTER(LEN=200) :: frases (m)
    character(len=:), allocatable :: cadena_sin_espacios
    character(len=1) :: nom1(n), nom2(n), nom3(n)
    character(len=3) :: sal2(n), com(n), desc(n), sal3(n), hab(n), res, aux, j
    character(len=100) :: comand, descno, descsu, desces, descoe, descar, descab, irno, irsu, ires,  iroe, irar, irab, dummy
    integer :: ascii_val
    integer :: len_comand
    character(len=1) :: ene ! ñ
    character(len=1) :: preg ! ¿
    character(len=1) :: adm ! ¡
    character(len=1) :: aa ! á
    character(len=1) :: ee ! é
    character(len=1) :: ii ! í
    character(len=1) :: oo ! ó
    character(len=1) :: uu ! ú
    ene=char(164)
    preg=char(168)
    adm=char(173)
    aa=char(160)
    ee=char(130)
    ii=char(161)
    oo=char(162)
    uu=char(163)
    
    hab= (/"1","2","3","4","5","6"/)
    sal2= (/"e", "1", "2", "3", "4", "5"/)
    sal3= (/"2", "3", "4", "5", "6", "s"/)
    com= (/"no", "su", "es", "oe", "ar", "ab"/)
    sal1=(/99,99,99,99,99,99/)
    habi= (/ 1, 2, 3, 4, 5, 6/)
    ndic=0
    open (11, file="data/diccionario.txt")   
    do
        read(11,*, iostat=ios) aux
        if (ios /= 0) exit
       ndic=ndic+1
           end do 
           close (11)
           open (11, file="data/diccionario.txt") 
           do i = 1, ndic
            read(11, *) comandos (i), frases (i)
         end do
         close (11)
         ndic2=0
         open (12, file="data/comandos.txt")   
         do
             read(12,*, iostat=ios) aux
             if (ios /= 0) exit
            ndic2=ndic2+1
                end do 
                close (12)
                open (12, file="data/comandos.txt") 
                do i = 1, ndic2
                    read(12,*) ingresos (i), egresos (i)
              end do
              close (12)
     

    current_pos= 1
    descno = "Hay una salida hacia el norte"
    descsu = "Hay una salida hacia el sur"
    desces = "Hay una salida hacia el este"
    descoe = "Hay una salida hacia el oeste"
    descar = "Hay un agujero en el techo"
    descab = "Hay un agujero en el suelo"
    irno = "Te diriges por un pasadizo hacia el norte hasta llegar a una nueva sala"
    irsu = "Te diriges por un pasadizo hacia el sur hasta llegar a una nueva sala"
    ires = "Te diriges por un pasadizo hacia el este hasta llegar a una nueva sala"
    iroe = "Te diriges por un pasadizo hacia el oeste hasta llegar a una nueva sala"
    irar = "Trepas hasta la siguiente sala"
    irab = "Deciendes hasta la siguiente sala"



     ! Asignar valores aleatorios al vector nom1
    do i = 1, n
        random_value = generate_random_integer()
        nom1(i) = hab(random_value)
    end do
nom2(1)="e"
    do i = 2, n
        do
        random_value = generate_random_integer()
        if (nom1(i) /= hab(random_value)) then
        nom2(i) = hab(random_value)
        exit
        end if
        end do
    end do
nom3(6)="s"
    do i = 1, n-1
        do
        random_value = generate_random_integer()
        if (nom1(i) /= hab(random_value)) then
            if (nom2(i) /= hab(random_value)) then
            nom3(i) = hab(random_value)
            exit
            end if
        end if
        end do
    end do
    do i = 1, n

        if (sal1(i)==99) then
            do
            random_value = generate_random_integer()
            sal1(i) = habi(random_value)
            a=sal1(i)
            if (sal1(a)==99) then
            sal1(a)=i
            exit
            end if
            end do
        end if
    end do


print*
print*, "LABERINTO DE TEXTO"
print*, "Rey Sombra"
print*, "V1.02 5/SEP/2024"
print*
print*, "ADVERTENCIA: Cada vez que ingreses al laberinto este habra cambiado completamente"
print*, "Hay 131,621,703,842,267,136 combinaciones distintas del laberinto, y todas tienen solucion"
print*, "Se recomienda fuertemente el uso de papel y lapiz"
print*
print*
print*, "Te encuentras en un laberinto subterraneo lleno de salas identicas"
print*,  "No sabes como llegaste hasta aqui, solo te queda buscar una salida"
print*
comand="mirar"

DO ! loop principal
if (comand/="frase") then

    select case (nom2(current_pos))
    case ("e")
        print*, "En esta habitacion se encuentra la entrada que esta bloqueada"
    case ("1")
        print*, descno
    case ("2")
        print*, descsu
    case ("3")
        print*, desces
    case ("4")
        print*, descoe
    case ("5")
        print*, descar
    case ("6")
        print*, descab
    end select


        
        select case (nom1(current_pos))
        case ("1")
            print*, descno
        case ("2")
            print*, descsu
        case ("3")
            print*, desces
        case ("4")
            print*, descoe
        case ("5")
            print*, descar
        case ("6")
            print*, descab
        end select


        select case (nom3(current_pos))
                    
        case ("s")
            print*, adm// "En esta habitacion se encuentra la salida!, podes SALIR del laberinto"
        case ("1")
            print*, descno
        case ("2")
            print*, descsu
        case ("3")
            print*, desces
        case ("4")
            print*, descoe
        case ("5")
            print*, descar
        case ("6")
            print*, descab
        end select

end if
        print*

! el comando del jugador

    read"(A)", comand
comand= quitar_espacios(comand)

! Convertir el primer carácter de la cadena a su valor ASCII
ascii_val = iachar(comand(1:1))

! Verificar si es el signo de pregunta de apertura
if (ascii_val == 168) then
    
    ! Obtener la longitud de la cadena original
    len_comand = len_trim(comand)

    ! Desplazar el resto de la cadena (sin el primer carácter) a la variable comand
    comand = comand(2:len_comand)

end if


    DO i = 1, ndic
        IF (comand == comandos(i)) THEN
            print*
          print*, frases(i)
          comand="frase"
          EXIT
        END IF 
      END DO

      DO i = 1, ndic2
        IF (ingresos(i) == comand) THEN
          comand = egresos(i)
          EXIT
        END IF
      END DO

SELECT CASE (comand)

case("frase")

case ("norte")
    print*
If (nom1(current_pos)=="1") then
    print*, irno
    current_pos=sal1(current_pos)
else if (nom2(current_pos)=="1") then
        print*, irno
        SELECT case (sal2(current_pos))
        case ("1")
        current_pos=1
        case ("2")
        current_pos=2
        case ("3")
        current_pos=3
        case ("4")
        current_pos=4
        case ("5")
        current_pos=5
    end select
else if (nom3(current_pos)=="1") then
    print*, irno
    SELECT case (sal3(current_pos))
    case ("2")
    current_pos=2
    case ("3")
    current_pos=3
    case ("4")
    current_pos=4
    case ("5")
    current_pos=5
    case ("6")
    current_pos=6
    end select
else 
    PRINT*, "No puedes ir en esa direccion"
end if

case ("sur")
    print*
    If (nom1(current_pos)=="2") then
        print*, irsu
        current_pos=sal1(current_pos)
    else if (nom2(current_pos)=="2") then
            print*, irsu
            SELECT case (sal2(current_pos))
            case ("1")
            current_pos=1
            case ("2")
            current_pos=2
            case ("3")
            current_pos=3
            case ("4")
            current_pos=4
            case ("5")
            current_pos=5
        end select
    else if (nom3(current_pos)=="2") then
        print*, irsu
        SELECT case (sal3(current_pos))
        case ("2")
        current_pos=2
        case ("3")
        current_pos=3
        case ("4")
        current_pos=4
        case ("5")
        current_pos=5
        case ("6")
        current_pos=6
        end select
    else 
        PRINT*, "No puedes ir en esa direccion"
    end if

case ("este")
    print*
    If (nom1(current_pos)=="3") then
        print*, ires
        current_pos=sal1(current_pos)
    else if (nom2(current_pos)=="3") then
            print*, ires
            SELECT case (sal2(current_pos))
            case ("1")
            current_pos=1
            case ("2")
            current_pos=2
            case ("3")
            current_pos=3
            case ("4")
            current_pos=4
            case ("5")
            current_pos=5
        end select
    else if (nom3(current_pos)=="3") then
        print*, ires
        SELECT case (sal3(current_pos))
        case ("2")
        current_pos=2
        case ("3")
        current_pos=3
        case ("4")
        current_pos=4
        case ("5")
        current_pos=5
        case ("6")
        current_pos=6
        end select
    else 
        PRINT*, "No puedes ir en esa direccion"
    end if

case ("oeste")
    print*
    If (nom1(current_pos)=="4") then
        print*, iroe
        current_pos=sal1(current_pos)
    else if (nom2(current_pos)=="4") then
            print*, iroe
            SELECT case (sal2(current_pos))
            case ("1")
            current_pos=1
            case ("2")
            current_pos=2
            case ("3")
            current_pos=3
            case ("4")
            current_pos=4
            case ("5")
            current_pos=5
        end select
    else if (nom3(current_pos)=="4") then
        print*, iroe
        SELECT case (sal3(current_pos))
        case ("2")
        current_pos=2
        case ("3")
        current_pos=3
        case ("4")
        current_pos=4
        case ("5")
        current_pos=5
        case ("6")
        current_pos=6
        end select
    else 
        PRINT*, "No puedes ir en esa direccion"
    end if

case ("arriba")
    print*
    If (nom1(current_pos)=="5") then
        print*, irar
        current_pos=sal1(current_pos)
    else if (nom2(current_pos)=="5") then
            print*, irar
            SELECT case (sal2(current_pos))
            case ("1")
            current_pos=1
            case ("2")
            current_pos=2
            case ("3")
            current_pos=3
            case ("4")
            current_pos=4
            case ("5")
            current_pos=5
        end select
    else if (nom3(current_pos)=="5") then
        print*, irar
        SELECT case (sal3(current_pos))
        case ("2")
        current_pos=2
        case ("3")
        current_pos=3
        case ("4")
        current_pos=4
        case ("5")
        current_pos=5
        case ("6")
        current_pos=6
        end select
    else 
        PRINT*, "No puedes ir en esa direccion"
    end if

case ("abajo")
    print*
    If (nom1(current_pos)=="6") then
        print*, irab
        current_pos=sal1(current_pos)
    else if (nom2(current_pos)=="6") then
            print*, irab
            SELECT case (sal2(current_pos))
            case ("1")
            current_pos=1
            case ("2")
            current_pos=2
            case ("3")
            current_pos=3
            case ("4")
            current_pos=4
            case ("5")
            current_pos=5
        end select
    else if (nom3(current_pos)=="6") then
        print*, irab
        SELECT case (sal3(current_pos))
        case ("2")
        current_pos=2
        case ("3")
        current_pos=3
        case ("4")
        current_pos=4
        case ("5")
        current_pos=5
        case ("6")
        current_pos=6
        end select
    else 
        PRINT*, "No puedes ir en esa direccion"
    end if

case ("salir")
    if (nom3(current_pos)=="s") then
        print*
        print*, adm// "Ganaste, saliste del laberito!"
        print*, "Software desarrollado en FORTRAN por Lucas M. B. Borches"
        print*, "Ingrese cualquier tecla para salir"
        read*, dummy
        exit
    else
        print*
        print*, "La salida no se encuentra en esta habitacion"

    end if

case ("cerrar")
    exit

case ("mirar")
    print*
    print*, "Te encuentras en un laberinto subterraneo lleno de salas identicas"
    print*

    CASE DEFAULT
        print*
          PRINT *, "No entiendo ese comando"
          print*, "Podes usar los comandos ARRIBA, ABAJO, NORTE, SUR, ESTE, OESTE, MIRAR, SALIR y CERRAR"
          print*
end select


end do




contains

function generate_random_integer() result(aleatorio_entero)
    implicit none
    integer :: aleatorio_entero
    real :: aleatorio_real

    call random_number(aleatorio_real)
    aleatorio_entero = int(aleatorio_real * n) + 1
end function generate_random_integer

function quitar_espacios(str) result(str_sin_espacios)
    implicit none
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: str_sin_espacios
    integer :: i, n
    character(len=1) :: ch
    character(len=len(str)) :: temporal

    n = 0
    do i = 1, len(str)
        ch = str(i:i)
        if (ch /= ' ') then
            n = n + 1
            temporal(n:n) = ch
        end if
    end do

    ! Ajustar la longitud del resultado
    str_sin_espacios = temporal(1:n)
end function quitar_espacios

end program laberinto