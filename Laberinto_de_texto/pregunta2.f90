program pregunta2
    implicit none
    character(len=100) :: comand
integer :: ascii_val
print*, "ingrese signo de pregunta"
! Leer un comando de entrada
read(*, '(A)') comand

! Convertir el primer carácter de la cadena a su valor ASCII
ascii_val = iachar(comand(1:1))

! Verificar si es el signo de pregunta de apertura
if (ascii_val == 168) then
    print*, "El comando empieza con el signo de pregunta de apertura ¿"
else
    print*, "No es el signo de apertura de pregunta"
end if
end program pregunta2