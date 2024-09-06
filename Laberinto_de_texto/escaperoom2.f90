PROGRAM Escape_room
    IMPLICIT NONE
  
    CHARACTER(LEN=20) :: comando, dummy
    CHARACTER(LEN=20) :: inventario(3)
    LOGICAL :: puerta_abierta = .FALSE.
    LOGICAL :: cuadro_agarrado = .FALSE.
  LOGICAL :: codigo_visible = .FALSE.
  INTEGER :: i
  INTEGER :: intentos_codigo = 0
  
    ! Inicializa el inventario vacío
    DO i = 1, 3
       inventario(i) = ""
    END DO
  
    PRINT *, "Estas en una habitacion oscura!"
    PRINT *, "Hay una puerta cerrada."
    PRINT *, "Puedes ver una mesa con un cajon y un cuadro en la pared"
  
    ! Bucle principal del juego
    DO
       PRINT *, "Que queres hacer?"
       READ(*, '(A)') comando
  
       SELECT CASE (TRIM(comando))
  
       CASE ('mirar')
          PRINT *, "Estas en una habitacion asfixiante y oscura."
          PRINT *, "Ves una puerta un cuadro en la pared y una mesa con un cajon."

        CASE ('mirar cuadro')
        PRINT *, "El cuadro muestra una cara burlona. Parece que no hay nada especial a simple vista."

        CASE ('patear puerta')
        print*, "Te rompiste la pierna"

    CASE ('romper cuadro')
        print*, "Te rompiste la mano"

        CASE ('pararse en la mesa')
        print*, "Te ves ridiculo"

        CASE ('agarrar cuadro')
        IF (.NOT. cuadro_agarrado) THEN
           PRINT *, "Agarras el cuadro y detras encuentras un codigo escrito: 840."
           cuadro_agarrado = .TRUE.
           codigo_visible = .TRUE.
           inventario(1) = 'cuadro'
        ELSE
           PRINT *, "Ya agarraste el cuadro."
        END IF

       CASE ('abrir cajon')
             PRINT *, "Abris el cajon y esta vacio!"
  
       CASE ('agarrar llave')
             PRINT *, "No hay nada que agarrar."
  
       CASE ('abrir puerta')
        IF (puerta_abierta) THEN
           PRINT *, "La puerta ya esta abierta."
        ELSEIF (codigo_visible) THEN
           PRINT *, "Introduce el codigo para abrir la puerta: "
           READ(*, '(A)') comando
           IF (TRIM(comando) == '840') THEN
              PRINT *, "Codigo correcto. La puerta se abre. escapaste!"
              puerta_abierta = .TRUE.
           ELSE
              PRINT *, "Codigo incorrecto."
              intentos_codigo = intentos_codigo + 1
              IF (intentos_codigo >= 3) THEN
                 PRINT *, "Demasiados intentos. Fin del juego!"
                 EXIT
              END IF
           END IF
        ELSE
           PRINT *, "La puerta esta cerrada y tiene un teclado numerico. Necesitas encontrar un codigo."
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
  
       CASE ('cerrar')
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