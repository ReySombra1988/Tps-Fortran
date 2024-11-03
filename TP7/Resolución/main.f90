Module Datos

	Implicit none
	Integer,Parameter :: n=3
	Real(8) a(n,n),b(n),Delta(n) 
	Real(8) X(n)	
	Real(8) x1,x2,x3   
	
	Real(8), Parameter :: pi=dacos(-1.d0)
	                                  
	Real(8), Parameter :: h=1.d-8
	Real(8), Parameter :: Tol_res=1.d-16
	Real(8), Parameter :: Tol_inc=1.d-16	
	Real(8)  Error_res
	Real(8)  Error_inc
	
	!TUBO 1
	Real(8), Parameter :: d_a=24.3d-3
	Real(8), Parameter :: s_a=(pi*d_a**2)/4.d0
	Real(8), Parameter :: Ka=4.d0
	Real(8), Parameter :: e_a=0.d0
	!TUBO 2
	Real(8), Parameter :: d_b=24.3d-3
	Real(8), Parameter :: s_b=(pi*d_b**2)/4.d0
	Real(8), Parameter :: Kb=8.d0
	Real(8), Parameter :: e_b=0.d0
	!TUBO 3
	Real(8), Parameter :: d_c=24.3d-3
	Real(8), Parameter :: s_c=(pi*d_c**2)/4.d0
	Real(8), Parameter :: Kc=12.d0
	Real(8), Parameter :: e_c=0.d0
	
	Real(8), Parameter :: l1=6.d0,l2=3.d0
	Real(8), Parameter :: vis=1.005d-6,ldc=30.d0 !Viscosidad cinemática, Pérdida de carga puntual
	
	Real(8), Parameter :: Lam2=1.d-5
	Real(8), Parameter :: Lam3=1.d-5
	
	 
End module

Program Main

	Use Datos
	Implicit none
	Integer :: K 
	
	K=0
	Error_res=1.d0
	Error_inc=1.d0
	
	Call LD
	Do while (Error_inc>Tol_inc .or. Error_res>Tol_res) 
		If (K<100) then
			K=K+1
			Call Jacobiana
			Call IndTerms
			Call PrintMatrix
			Call MatrixResolver 
			Call New(k)
		Else
			Stop
		End if
	End do  
	
End program

!- - - - - - - - - - - Lectura del X (vector) propuesto - - - - - - - - - - -!

Subroutine LD
 Use Datos
 Implicit none
 Integer :: i
 Open(7,File='DatosIniciales.in')
 	Do i=1,n
 		Read(7,*) X(i)
 	End do
 	x1=X(1)                                                         !0J0
 	x2=X(2)                                                         !0J0
 	x3=X(3)                                                         !0J0
 	
End subroutine

!- - - - - - - - - - - - DERIVADA DE LAS FUNCIONES  - - -  - - - - - - - - - !
!- - - - - - - - - - - - - - MATRIZ JACOBIANA - - - - - - - - - - - - - - - -!	
	
Subroutine Jacobiana

 Use Datos
 Implicit none
 Integer :: i,j
 Real(8) R
 Do j=1,n
 	Do i=1,n
 		Call Deriv(i,j,R)
 		a(i,j)=R
 	End do
 End do 
 
End subroutine

	Subroutine Deriv(i,j,R)

	 Use Datos
	 Implicit none
	 Integer :: i,j
	 Real(8) R
	 Real(8) f1,f2,f3                                                           !OJO
	 
	 ! - - - - - - - - - - - - - Primera columna - - - - - - - - - - - - - !
	 
	 If (j==1) then !Primera columna
	 
	 	If (i==1) then !Primera fila
	 	
	 		R=(f1(x1+h,x2,x3)-f1(x1,x2,x3))/h
	 		
	 	Else if (i==2) then !Segunda fila
	 	
	 		R=(f2(x1+h,x2,x3)-f2(x1,x2,x3))/h
	 		
	 	Else if (i==3) then !Tercer fila
	 	
	 		R=(f3(x1+h,x2,x3)-f3(x1,x2,x3))/h
	 		
	 	End if
	 	
	 End if
	 
	 ! - - - - - - - - - - - - - Segunda columna - - - - - - - - - - - - - !

	 If (j==2) then !Segunda columna 
	 
	  	If (i==1) then !Primera fila
	 	
	 		R=(f1(x1,x2+h,x3)-f1(x1,x2,x3))/h
	 		
	 	Else if (i==2) then !Segunda fila
	 	
	 		R=(f2(x1,x2+h,x3)-f2(x1,x2,x3))/h
	 		
	 	Else if (i==3) then !Tercer fila
	 	
	 		R=(f3(x1,x2+h,x3)-f3(x1,x2,x3))/h
	 		
	 	End if
	 	
	 End if

	 ! - - - - - - - - - - - - - Tercer columna - - - - - - - - - - - - - !	 
	 
	 If (j==3) then !Tercer columna 
	 
	  	If (i==1) then !Primera fila
	 	
	 		R=(f1(x1,x2,x3+h)-f1(x1,x2,x3))/h
	 		
	 	Else if (i==2) then !Segunda fila
	 	
	 		R=(f2(x1,x2,x3+h)-f2(x1,x2,x3))/h
	 		
	 	Else if (i==3) then !Tercer fila
	 	
	 		R=(f3(x1,x2,x3+h)-f3(x1,x2,x3))/h
	 		
	 	End if
	 	
	 End if
	  
	End subroutine
	
	! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
	
	! - - - - - - - - - - - - - Función 1 - - - - - - - - - - - - - !

	Function f1(Qa,Qb,Qc)                                                          !OJO
	
	 Use Datos
	 Implicit none
	 Real(8) f1,Qa,Qb,Qc                                                           !OJO
	 
	 f1=Qa+Qb+Qc-0.01                                                               
	 
	End function

	! - - - - - - - - - - - - - Función 2 - - - - - - - - - - - - - !

	Function f2(Qa,Qb,Qc)                                                          !OJO
	
	 Use Datos
	 Implicit none
	 Real(8) f2,Qa,Qb,Qc                                                                               
	 
	 f2= Lam2*( (  0.25/( log10(e_a/(3.7*d_a) + 5.74/( (Qa/s_a)*(d_a/vis)  )**0.9)  )**2   &
	     * ( (l1+2.d0*l2)/d_a + 2*ldc ) + Ka )*(Qa/s_a)**2/2.d0                            &
	     - (    0.25/( log10(e_b/(3.7*d_b) + 5.74/( (Qb/s_b)*(d_b/vis)  )**0.9)  )**2      &
	     * (l1/d_b) + Kb ) * (Qb/s_b)**2/2.d0 )                                  
	 
	End function

	! - - - - - - - - - - - - - Función 3 - - - - - - - - - - - - - !

	Function f3(Qa,Qb,Qc)                                                         !OJO
	
	 Use Datos
	 Implicit none
	 Real(8) f3,Qa,Qb,Qc  
	 Real(8) f_i                                                                  !OJO                   
	 
	 f3= Lam3*((  0.25/( log10(e_b/(3.7*d_b) + 5.74/( (Qb/s_b)*(d_b/vis)  )**0.9)  )**2    &
	     * (l1/d_b) + Kb ) * (Qb/s_b)**2/2.d0 -                                            & 
	     ( 0.25/( log10(e_c/(3.7*d_c) + 5.74/( (Qc/s_c)*(d_c/vis)  )**0.9)  )**2           &
	     * ( (l1+2.d0*l2)/d_c + 2*ldc ) + Kc ) * (Qc/s_c)**2/2.d0 )                    
	 
	End function
 
 
!- - - - - - - - - - - - TÉRMINOS INDEPENDIENTES  - - - - - - - - - - - - -!

Subroutine IndTerms                                               !n Funciones=0 
 
 Use Datos
 Implicit none
 Integer :: i
 Real(8) f1,f2,f3                                                            !OJO
 
 b(1)=f1(x1,x2,x3)                                                           !OJO
 b(2)=f2(x1,x2,x3)                                                           !OJO
 b(3)=f3(x1,x2,x3)                                                           !OJO
 
 Do i=1,n
 	b(i)=-1.d0*b(i)
 End do

End subroutine	

! - - - - - - - - - - - - Eliminación Gaussiana - - - - - - - - - - - - ! 

Subroutine MatrixResolver

 Use Datos
 Implicit none
 Real(8) m,Prod
 Integer :: i,j,k 
 !Primera etapa (Triangulación)
 Do i=1,n-1 
    Call Parcial(i)  !Pivoteo parcial
    Do j=i+1,n 
		m=a(j,i)/a(i,i)
		a(j,i)=0.d0
		b(j)=b(j)-m*b(i)
		Do k=i+1,n 
			a(j,k)=a(j,k)-m*a(i,k)
		End do
	End do
 End do
 !Segunda etapa (Retrosustitución)
 Do i=n,1,-1
	Prod=0.d0
	Do j=i+1,n
		Prod=Prod+a(i,j)*Delta(j)
	End do
	Delta(i)=(b(i)-Prod)/a(i,i)
 End do

End subroutine

		!------------------------Pivoteo parcial------------------------!
		Subroutine Parcial(i)

		 Use Datos
		 Implicit none
		 Real(8) auxi,aux(n)
		 Integer i,j,k 

		 Do j=i+1,n 
		  If (ABS(a(i,i))<ABS(a(j,i))) then 
			Do k=1,n 
			  aux(k)=a(i,k) 
			  a(i,k)=a(j,k) 
			  a(j,k)=aux(k)
			End do
			auxi=b(i)
			b(i)=b(j)
			b(j)=auxi
		  End if	 
		 End do
		 
		End subroutine

! - - - - - - - - - - - - NUEVAS APROXIMACIONES  - - - - - - - - - - - - !

Subroutine New(k)

 Use Datos
 Integer :: k,i
 Real(8) XOLD(n)
  
 XOLD=X
 
 Do i=1,n
 	X(i)=X(i)+Delta(i)
 End do
 
 x1=X(1)                                                                        !0J0
 x2=X(2)                                                                        !0J0
 x3=X(3)                                                                        !0J0
 
 Call Inc(XOLD)
 Call Res
 
 Print*, 'El error res', k, Error_res
 Print*, 'El error inc', k, Error_inc
 Open(10,File='Raices.out')
  	Write(10,*) "Aproximación nro.",k
 	Do i=1,n
 		Write(10,*) X(i)
 	End do

End subroutine

!- - - - - - - - - - - - - - - - - Error resuido - - - - - - - - - - - - - - - - - 

Subroutine Res
 
 Use Datos
 Implicit none
 Integer :: i
 Real(8) efes
 Real(8) f1,f2,f3                                                                       !OJO
 
 efes=f1(x1,x2,x3)**2+f2(x1,x2,x3)**2+f3(x1,x2,x3)**2                                   !OJO
 
 Error_res=sqrt(efes)
 
End subroutine
 
!- - - - - - - - - - - - - - - - - Error incremento - - - - - - - - - - - - - - - - - 

Subroutine Inc(XOLD)
 
 Use Datos
 Implicit none
 Integer :: i
 Real(8) XOLD(n)
 Real(8) num,den
 
 num=0.d0
 den=0.d0

 Do i=1,n
	 num=num+((XOLD(i)-X(i)))**2
	 den=den+X(i)**2
 End do

 Error_inc=sqrt(num/den)
 
End subroutine



! - - - - - - - - - - - CHECK MATRIX - - - - - - - - - - - !

Subroutine PrintMatrix
 
 Use Datos
 Integer :: i,j
 
 Open(4,File='Matrix')
 	Write(4,*) 'Matrix de coef.'
 	Do i=1,n
 			Write(4,*) (a(i,j),j=1,n)
 	End do
 	Write(4,*) 'Ind. terms'
 	Do i=1,n
 		Write(4,*) b(i)
 	End do 

End subroutine
