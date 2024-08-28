PROGRAM CrearArchivoTXT
    IMPLICIT NONE
    INTEGER :: unit_number
    CHARACTER(len=100) :: filename
  
    ! Especificar el nombre del archivo
    filename = 'archivo_salida.txt'
  
    ! Asignar un número de unidad para el archivo
    unit_number = 10
  
    ! Abrir el archivo para escritura
    OPEN(UNIT=unit_number, FILE=filename, STATUS='UNKNOWN', ACTION='WRITE')
  
    ! Escribir algunas líneas en el archivo
    WRITE(unit_number, *) 'Este es un archivo de texto creado con Fortran.'
    WRITE(unit_number, *) 'Cada línea es escrita con una instrucción WRITE.'
    WRITE(unit_number, *) 'Fortran puede manejar archivos fácilmente.'
  
    ! Cerrar el archivo
    CLOSE(unit_number)
  
    PRINT *, 'Archivo creado exitosamente:', TRIM(filename)
  END PROGRAM CrearArchivoTXT