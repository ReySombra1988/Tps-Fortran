program juegos_de_texto
    implicit none
    character(len=1) :: a

    do
    print*
    print*, "JUEGOS DE TEXTO"
    print*
    print*, "Escape de la habitacion Nivel 1", "          1"
    print*
    print*, "Escape de la habitacion Nivel 2", "          2"
    print*
    print*, "LABERINTO DE TEXTO", "                       3"
    print*
    print*, "Cerrar", "                                   0"
    print*
    print*, "Ingrese el numero del juego que desea jugar"
    print*

    read*, a

    select case (a)
    case ("1")
        CALL SYSTEM('juego_escape.exe')
    case("2")
        CALL SYSTEM('escaperoomlvl2')
    case("3")
        CALL SYSTEM('laberinto002.exe')
    case("0")
        exit
    end select
end do
end program juegos_de_texto
