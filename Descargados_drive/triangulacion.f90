Module Data
Implicit none
Integer, parameter :: n=4     !Dim. Matrix
Real(8) a(n,n), b(n), x(n)
End Module

Program Gauss
Use Data
call lecdat !Lectura de datos
call mat    !Metodo exacto-Eliminación Gaussiana (Pivoteo trivial o parcial)
call Det    !Determinante
End program

!-------------------------Lectura de datos-------------------------!
Subroutine lecdat
Use Data
Implicit none
!Declaración de variables
Integer :: i,j  !Variables de control
!Inicialización de variables
Open(1,File='Data.txt')
	Read(1,*) !Matrix
	Do i=1,n
	Read(1,*)(a(i,j),j=1,n)
	End do
	Read(1,*) !Ind. terms
	Read(1,*)(b(i),i=1,n)
End subroutine

!Metodo exacto-Eliminación Gaussiana (Pivoteo trivial o parcial)
Subroutine mat
Use Data
Implicit none
Real(8) m,Prod
Integer :: i,j,k !Variables de control
!Procesamiento y salida de datos
Print*,'La matriz A es'
Do i=1,n
   Print*, (a(i,j),j=1,n),'|',b(i)
End do  
Print*,'- - - - -'
!Primera etapa (Triangulación)
Do i=n,2,-1 !Columnas
    !call Trivial(i)  !Pivoteo trivial
    !call parcial(i)  !Pivoteo parcial
    Do j=i-1,1,-1 !Filas
		m=a(j,i)/a(i,i)
		a(j,i)=0.d0
		b(j)=b(j)-m*b(i)
		Do k=i-1,1,-1 !Pasa por todas las columnas
			a(j,k)=a(j,k)-m*a(i,k)
		End do
	End do
End do
!Segunda etapa (Retrosustitución)
Do i=1,n
	Prod=0.d0
	Do j=i-1,1,-1
		Prod=Prod+a(i,j)*x(j)
	End do
	x(i)=(b(i)-Prod)/a(i,i)
End do
Print*,'La matriz triangular inferior es'
Do i=1,n
Print*,(a(i,j),j=1,n),'│',b(i)
End do
Print*,'Las soluciones son'
Do i=1,n
Print*,'x',i,x(i)
End do	
End subroutine
!-------------------------Pivoteo parcial-------------------------!
Subroutine parcial(i)
Use Data
Implicit none
Real(8) auxi,aux(n)
Integer i,j,k 
!Intercambio de filas
Do j=i-1,1,-1 !Filas de la col. fijada
  If (ABS(a(i,i))<ABS(a(j,i))) then !Pivote<a(j,i)?
    Do k=n,1,-1 !Me pasea por todas las col. de la fila detectada
	  aux(k)=a(i,k) !Guardo toda la fila i
	  a(i,k)=a(j,k) !Le asigno a toda la fila i su nuevo valor de fila
	  a(j,k)=aux(k)
	End do
	auxi=b(i)
	b(i)=b(j)
	b(j)=auxi
  End if	 
End do
End subroutine
!-------------------------Pivoteo trivial-------------------------!
Subroutine trivial(i)
Use Data
Implicit none
Real(8) auxi,aux(n)
Integer i,j,k 
!Intercambio de filas
If (a(i,i)==0.d0) then !Pivote=0?
  Do j=i-1,1,-1 !Filas de la col. fijada
    If (a(j,i)/=0.d0) then
        Do k=n,1,-1 !Me pasea por todas las col. de la fila detectada
	      aux(k)=a(i,k) !Guardo toda la fila i
	      a(i,k)=a(j,k) !Le asigno a toda la fila i su nuevo valor de fila
	      a(j,k)=aux(k)
	      auxi=b(i)
	      b(i)=b(j)
	      b(j)=auxi
	      Return
	    End do
    End if
  End do
End if
End subroutine
!-------------------------Determinante-------------------------!
Subroutine Det 
Use Data
Implicit none
Real(8) D 
Integer :: i
D=1.d0
Do i=1,n
    D=D*a(i,i)
End do 
Print*,'El determinante de la matriz es',D 
End subroutine
boca
