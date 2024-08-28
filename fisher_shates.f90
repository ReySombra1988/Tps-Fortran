PROGRAM FisherYatesShuffle
    IMPLICIT NONE
    INTEGER, PARAMETER :: n = 10
    INTEGER :: A(n), i, j, temp, n, random_number
  
    ! Inicializar el arreglo A con valores del 1 al 10
    A = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/)
  n=10
    ! Llamar a la rutina de desordenamiento Fisher-Yates
    CALL Shuffle(A, n)
  
    ! Imprimir el arreglo desordenado
    PRINT *, 'Arreglo desordenado:', A
  
  CONTAINS
  
    SUBROUTINE Shuffle(A, n)
      INTEGER, INTENT(INOUT) :: A(n)
      INTEGER :: i, j, temp
  
      ! Desordenar el arreglo
      DO i = n, 2, -1
        j = INT(RANDOM_NUMBER() * i) + 1  ! Generar índice aleatorio
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
      END DO
    END SUBROUTINE Shuffle
  
  END PROGRAM FisherYatesShuffle