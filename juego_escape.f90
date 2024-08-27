PROGRAM Escape_room
    IMPLICIT NONE
  
    CHARACTER(LEN=20) :: comando, dummy
    CHARACTER(LEN=20) :: inventario(3)
    LOGICAL :: llave_encontrada = .FALSE.
    LOGICAL :: puerta_abierta = .FALSE.
    INTEGER :: i
  
    ! Inicializa el inventario vacío
    DO i = 1, 3
       inventario(i) = ""
    END DO
  
    PRINT *, "Estas en una habitacion oscura!"
    PRINT *, "Hay una puerta cerrada."
    PRINT *, "Podes ver una mesa con un cajon."
    PRINT *, "Tal vez encuentres algo util."
  
    ! Bucle principal del juego
    DO
       PRINT *, "Que queres hacer? (mirar, abrir cajon, agarrar llave, abrir puerta, inventario, salir)"
       READ(*, '(A)') comando
  
       SELECT CASE (TRIM(comando))
  
       CASE ('mirar')
          PRINT *, "Estas en una habitacion asfixiante y oscura."
          PRINT *, "Ves una puerta y una mesa con un cajon."
  
       CASE ('abrir cajon')
          IF (.NOT. llave_encontrada) THEN
             PRINT *, "Abris el cajon y encontras una llave."
             llave_encontrada = .TRUE.
          ELSE
             PRINT *, "El cajon esta vacio."
          END IF
  
       CASE ('agarrar llave')
          IF (llave_encontrada) THEN
             PRINT *, "Agarras la llave y la guardas en tu inventario."
             inventario(1) = 'llave'
             llave_encontrada = .FALSE.
          ELSE
             PRINT *, "No hay nada que agarrar."
          END IF
  
       CASE ('abrir puerta')
          IF (puerta_abierta) THEN
             PRINT *, "La puerta ya esta abierta."
          ELSEIF (inventario(1) == 'llave') THEN
             PRINT *, "Usas la llave para abrir la puerta. Escapaste!"
             puerta_abierta = .TRUE.
          ELSE
             PRINT *, "La puerta esta cerrada con llave. Necesitas encontrar una llave."
          END IF
  
       CASE ('inventario')
          PRINT *, "Tu inventario contiene:"
          DO i = 1, 3
             IF (inventario(i) /= "") THEN
                PRINT *, "- ", TRIM(inventario(i))
             ELSE
                PRINT *, "- (vacio)"
             END IF
          END DO
  
       CASE ('salir')
          PRINT *, "Gracias por jugar. Chau!"
          EXIT
  
       CASE DEFAULT
          PRINT *, "No entiendo ese comando."
       
       END SELECT
  
       IF (puerta_abierta) then
       print*, "precione cualquier tecla para salir"
       read*, dummy
        EXIT
       end if
    END DO
  
  END PROGRAM escape_room