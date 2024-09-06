program eliminar_espacios
    implicit none
    character(len=100) :: cadena = '  Hola   Mundo  '
    character(len=:), allocatable :: cadena_sin_espacios

    cadena_sin_espacios = quitar_espacios(cadena)
    print *, 'Cadena original: "', cadena, '"'
    print *, 'Cadena sin espacios: "', cadena_sin_espacios, '"'
    
contains
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
end program eliminar_espacios