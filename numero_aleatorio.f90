program random_number
    IMPLICIT NONE
    real :: aleatorio_real
    integer :: aleatorio_entero
    CALL RANDOM_NUMBER(aleatorio_real)
    aleatorio_entero=Int(aleatorio_real*10)
    PRINT *, "Numero aleatorio generado: ", aleatorio_entero
    end program random_number