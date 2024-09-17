program bin_to_text
    implicit none
    integer :: ios, i, ndatos, ascii_val
    character(len=100) :: data, line

    ndatos = 200  ! Ajusta este número según el número real de caracteres que esperas

    open(11, file="texto.bin", form="unformatted", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print*, "Error al abrir el archivo binario"
        stop
    end if

    open(12, file="texto.txt", status="replace", action="write")

    ! Leer valores ASCII y convertir a texto
    do i = 1, ndatos
        read(11, iostat=ios) ascii_val
        if (ios /= 0) exit
        write(12, '(A)', advance="no") achar(ascii_val)
    end do

    close(11)
    close(12)
    print*, "Archivo de texto creado correctamente."
end program bin_to_text