Module Datos

	Implicit none
	
	Real(8),parameter :: Inc=1.d-5
	Real(8),parameter :: Res=1.d-5
	Real(8),parameter :: lam=-1.d0
	Real(8) x_0

End module

PROGRAM Punto_fijo
  
  Use Datos

  x_0=1.25                            !Punto cercano a al punto fijo (aproximación)

  call Point
  
END PROGRAM


Subroutine Point
	  
	  	Use Datos
	  	Implicit none
	  	
	  	Real(8) root,error_inc,error_res
	  	Real(8) g,f   				      !Funciones
	  	Integer k
	  	
	  	Open(1,File="Data.out")
	  	
		k=0
	    error_inc=1.+inc
		error_res=1.+res
		
		Do while (error_inc>inc .or. error_res>res)	
			
		  	k=k+1					            
		  	root=g(x_0,lam)                                   !x(k+1)=g(x(k))
		  	error_res=ABS(f(root))
			error_inc=ABS((root-x_0)/root)  
			x_0=root 
			
			If (k>200) then
				Print*,'Alcanzó el máx. de las iter.', k
			Exit
			End if  
			
			Write(1,*) 'Para k igual a', k
			Write(1,*) 'La raíz es', root
			Write(1,*) 'Residuo', error_res , 'Incremento', error_inc  
			       
		End do
		
End subroutine
  
  		  Function f(x)          !f(x)=0                   
		  Implicit none
		  Real(8) f,x
		  f=exp(-1.d0*x)*(x-1)
		  End function 
		  
		  FUNCTION g(x,lam)      !g(x)=x               
		  Implicit none
		  Real(8) f,g,x,lam
		  g=lam*f(x)+x
		  End function
  
	  
