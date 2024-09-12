program clase_anterior_biseccion
    implicit none
    integer i
    real y, a, b, Y1, Y2, Y3,  Xk, Xa
    a=0.5
    b=2.5
i=1
Xa=1
    do while (i/=11 .and. abs(Xa-Xk)>=0.01)
        
    Y1=1-(20**2/9.81)*(3+a)/(3*a+(a**2)/2)**3
    Y2=1-(20**2/9.81)*(3+b)/(3*b+(b**2)/2)**3
Xa=Xk
Xk=(a+b)/2
Y3=1-(20**2/9.81)*(3+Xk)/(3*Xk+(Xk**2)/2)**3

If (Xk==0) then
exit
else if (Y1*Y3<0) then
    b=Xk
else
    a=Xk

end if
print*, "La iteracion", i, "es:", Xk
i=i+1
end do

print*, "la raiz es", Xk

end program clase_anterior_biseccion