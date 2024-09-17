program clase_anterior_biseccion
    implicit none
    integer i
    real y, a, b, Xk, Xa, f, x
    a=0.5
    b=2.5
i=1
Xa=1
    do while (i/=11 .and. abs(Xa-Xk)>=0.01)
        
Xa=Xk
Xk=(a+b)/2

If (Xk==0) then
exit
else if (f(a)*f(Xk)<0) then
    b=Xk
else
    a=Xk

end if
print*, "La iteracion", i, "es:", Xk
i=i+1
end do

print*, "la raiz es", Xk

end program clase_anterior_biseccion

FUNCTION f(x)
    implicit none
    real f, x
    f=1-(20**2/9.81)*(3+x)/(3*x+(x**2)/2)**3
    END FUNCTION