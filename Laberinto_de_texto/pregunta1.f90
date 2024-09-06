program pregunta1
    implicit none
    character(len=100) :: comand

! Leer un comando de entrada
    print*, "ingrese signo de pregunta"
read(*, '(A)') comand

! Verificar si el comando contiene el signo ¿
if (comand == "¿") then
    print*, "Leíste el signo de apertura de pregunta ¿"
else
    print*, "no se leyo el signo"
end if
end program