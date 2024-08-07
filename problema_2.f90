program problema_2
    implicit none
    integer i, j 
    real a, b
    i=5
    j=4
    a= i*j 
    print*, "con variables enteras:"
    print*, "i*j=", a 
    a= i/j 
    print*, "i/j=", a 
    a= j/i 
    a=5
    b=4
    
    print*, "j/i=", a 
    i=a*b 
    print*, "con variables reales:"
    print*, "a*b=", i
    end program problema_2