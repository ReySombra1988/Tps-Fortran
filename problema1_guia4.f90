program problema1_guia4
    integer, parameter::n=200
    integer k
    real*8 x(n), sumneg, sumpos, cantneg, cantpos, prompos, promneg
    cantneg=0
    cantpos=0
    sumneg=0
    sumpos=0
    k=11
 open (11, file="vector1.txt")   
 read(11,*)
 read(11,*) ndatos
 read(11,*)
 do i=1,ndatos
    read(11,*) x(i)
 end do
 close (11)
do = j, k
    if (x(i)<0) then
        cantneg=cantneg+1
        sumneg=sumneg+x(i)
    else if (x(i)>0) then
        cantpos=cantpos+1
        sumpos=sumpos+x(i) 
    end if
end do
prompos=sumpos/cantpos
promneg=sumneg/cantneg
print*, "El promedio de los positivos es:", prompos
print*, "El promedio de los negativos es:", promneg
 end program problema1_guia4