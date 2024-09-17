program ifselec
implicit none
INTEGER, PARAMETER :: ndatos = 4
CHARACTER(LEN=20) :: comando
CHARACTER(LEN=20) :: palabras (ndatos), comandos (ndatos)
integer i
open (11, file="pruebatxt.txt")   

do i = 1, ndatos
   read(11,*) palabras(i), comandos (i)
end do
close (11)
do while (comando/="salir")
  print*, "ingrese palabra"
READ(*, '(A)') comando
DO i = 1, ndatos
    IF (TRIM(palabras(i)) == TRIM(comando)) THEN
      comando = comandos(i)
      EXIT
    END IF
  END DO
  select case (TRIM(comando))
  case ("uno")
    print*, "caso uno"

  case ("dos")
    print*, "caso dos"
  end select
end do
end program ifselec