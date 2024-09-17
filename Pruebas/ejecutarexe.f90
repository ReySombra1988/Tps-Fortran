PROGRAM ejecutarexe
    integer :: aleatorio_entero
    INTEGER :: exit_status
    CHARACTER(len=100) :: comando
    comando = 'numero_aleatorio.exe'
    CALL SYSTEM('numero_aleatorio.exe')
    CALL EXECUTE_COMMAND_LINE(TRIM(comando), EXITSTAT=exit_status)
    print*, "mezclador=", aleatorio_entero
END PROGRAM ejecutarexe