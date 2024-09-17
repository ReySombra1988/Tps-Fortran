program bin_to_text
    implicit none
    integer :: ios, i, len_line
    integer :: ascii_val
    character(len=100) :: line
    integer :: j

    open(11, file="texto.bin", form="unformatted", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print*, "Error al abrir el archivo binario"
        stop
    end if

    open(12, file="texto.txt", status="replace", action="write")

    ! Leer longitud de cada línea y luego la línea en sí
    do
        read(11, iostat=ios) len_line
        if (ios /= 0) exit
        
        ! Leer cada valor ASCII y construir la línea
        line = ' '  ! Inicializar línea con espacios
        do j = 1, len_line
            read(11, iostat=ios) ascii_val
            if (ios /= 0) exit
            line(j:j) = achar(ascii_val)
        end do
        
        ! Escribir la línea en el archivo de texto
        write(12, '(A)') trim(line)
    end do

    close(11)
    close(12)
    print*, "Archivo de texto creado correctamente."
end program bin_to_text