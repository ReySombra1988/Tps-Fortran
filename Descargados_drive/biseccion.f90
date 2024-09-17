  Module Datos
  
  	Implicit none
  	Real(8) a
  	Real(8) b
  	Real(8),parameter :: Inc=1.d-15
  	Real(8),parameter :: Res=1.d-15
  
  End module
  
  Program Bolzano
      
      Use Datos

      a=0.d0
      b=3.d0
      
	  Call Point
	  
  End program
  
  
  Subroutine Point
  
	  Use Datos
	  Implicit none 
	  
	  Real(8) Error_inc,Error_res,root_old,root_new
	  Real(8) f
	  Integer :: i
	  
	  Error_inc=1.d0+Inc
	  Error_res=1.d0+Res
	  i=0
	  
	  Open(10,File="Raices.out")
	  
	  Write(10,*) 'Raíz', i , (a+b)/2.d0 
	  
	  Do while (Error_inc>inc .or. Error_res>res)	
	  	
	  	root_old=(a+b)/2.d0  	                     !Aproximación de la raíz vieja
	  	
	  	Call Resi(root_old,Error_res,i)
	  	
	  	If (f(a)*f(root_old)<0) then
	  	
	  		b=root_old
	  		
	  	Else if (f(b)*f(root_old)<0)  then
	  	
	  		a=root_old
	  		
	  	Else
	  	
	  		Print*,'La raíz es', root_old
	  		exit
	  		
		End if  
		
		root_new=(a+b)/2.d0                          !Aproximación de la raíz nueva        
		
		Call Incr(root_old,root_new,Error_inc,i)
		
		i=i+1
		
		Write(10,*) 'Raíz', i , root_new
		
	  End do
	  
  End subroutine
  
!---------------------------------------------!

  Subroutine Resi(root_old,Error_res,i)
  	Implicit none
  	
  	Real(8) root_old,Error_res,f
  	Integer :: i
  	
  	Open(1,File="Residuo.out")
  		
  	Error_res=ABS(f(root_old)) 
  	
  	Write(1,*) 'Aprox', i , Error_res 
  	
  End subroutine
  
!---------------------------------------------! 
	
  Subroutine Incr(root_old,root_new,Error_inc,i)
  	Implicit none
  	
  	Real(8) root_new,root_old,Error_inc
  	Integer :: i
  	
  	Open(2,File="Incremento.out")
  		
  	Error_inc=ABS((root_new-root_old)/root_new)  
  	
  	Write(1,*) 'Aprox', i , Error_inc 
  	
  End subroutine 	
  	
  
  
	  Function f(x)
	  Implicit none
	  Real(8) f,x
          f=(5.d0/6.d0)-(5.d0/2.d0)*cos(x)+(11.d0/6.d0)-cos( (dacos(-1.d0)/3.d0) - x )                
	  End function
!En este método de bisección se plantea un intervalo que encierra la raíz de interés
!Es notable que se plantea un promedio de las raíces y se le asigna esa nueva aproximación a la raíz como una nueva
!cota para nuestro intervalo (a,b) 
