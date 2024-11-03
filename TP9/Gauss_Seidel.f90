Module Datos

	Implicit none
	Integer, parameter :: n=2     !Dim. Matrix
	Real(8) a(n,n), b(n), x(n)
	Real(8) Error_res, Error_inc
	Real(8), parameter :: Tol_res=1.d-5 
	Real(8), parameter :: Tol_inc=1.d-4 
	
End Module

Program Gauss

	call LD 
	call GaussSeidel
	
End program

!------------------------Lectura de datos------------------------!

Subroutine LD

	Use Datos
	Implicit none
	Integer :: i,j  
	Open(1,File='Datain.txt')
		Read(1,*) !Matrix
		Do i=1,n
			Read(1,*)(a(i,j),j=1,n)
		End do
		Read(1,*) !Ind. terms
		Do i=1,n
			Read(1,*) b(i)
		End do
		
End subroutine


!--------------------Gauss-Seidel--------------------!

Subroutine GaussSeidel

	Use Datos
	Implicit none
	
	Real(8) xold(n),Suma
	Integer :: i,j,k 
	
	xold(1)=1.d0 !Inicializo todas las raíces
    xold(2)=(1/2)
	Error_res=1.d0
	Error_inc=1.d0
	k=1
	
	Do while (Error_inc>Tol_inc .or. Error_res>Tol_res)
	
		If (k<100) then
		
			Do i=1,n !Filas
			
				Suma=0.d0
				Do j=1,i-1 !Columnas hasta la raíz que estoy calculando
				    Suma=Suma+a(i,j)*x(j) !Parte inferior de la matriz a, con todas la raíces viejas
				End do 
				Do j=i+1,n !Columnas después de la raíz que estoy calculando
				    Suma=Suma+a(i,j)*xold(j) !Parte superior de la matriz a, con todas raíces viejas
				End do 
				x(i)=(b(i)-Suma)/a(i,i) !Aproximo raíz(k) con raíces viejas(k-1) 
				
			End do 
			
			Call Res 
			
			Call Inc(xold) 
			
			Call Check(k)
			
			xold=x
			k=k+1 !Contador de iteraciones
		
		Else
		
			Stop
			
		End if
		
	End do
	
End subroutine

!--------------------------Calculo de la norma euclidiana de ax-b--------------------------!

Subroutine Res

	Use Datos
	Implicit none
	Integer :: i,j,k
	Real(8) Multax(n),Entra
	Real(8) Restaxb(n)
	Real(8) norm
	
	! Mult. a*x
	  DO i=1,n !Fija las filas de a
	  		Entra=0.d0
	  		Do k=1,n !Recorre las filas de x, y las columnas de a
	  			Entra=Entra+a(i,k)*x(k)
	  		End do
	  		Multax(i)=Entra         
	  END DO
	  
	! Resta a*x-b
	  
	  DO i=1,n
	  	Restaxb(i)=Multax(i)-b(i)
	  END DO
	  
	  Norm=0.d0
	  Do i=1,n
	  	Norm=Norm+(Restaxb(i))**2
	  End do
	  
	  Error_res=sqrt(Norm)
	  
End subroutine

!--------------------------Calculo del error por incremento--------------------------! 

Subroutine Inc(xold)

	Use Datos
	Implicit none
	
	Real(8) xold(n)
	Real(8) num,den
	Integer :: i

	num=0.d0
	den=0.d0
	
	Do i=1,n
		num=num+(x(i)-xold(i))**2
		den=den+x(i)**2
	End do
	Error_inc=sqrt(num/den)
	
End subroutine

! - - - - - - - - - - - - - Impresión de los errores y raíces - - - - - - - - - - - - - !

Subroutine Check(k)

	Use Datos
	Implicit none
	
	Integer :: i,k
	
	Open(5,File="Raices&Errores_GS.out")
	
		Write(5,*) 'Aproximación nro.', k
		Write(5,*) 'Error de incremento', Error_inc
		Write(5,*) 'Error de residuo ', Error_res
		Write(5,*) 'Las raíces aproximadas'
			Do i=1,n
				Write(5,*) x(i)
			End do
	
End subroutine
