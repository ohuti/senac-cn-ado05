program bhaskara
    implicit none

    DOUBLE PRECISION :: a,b,c,delta,r1,r2
    CHARACTER (len=8) :: temp
    CHARACTER (len=64) :: temp1, temp2
    
    CALL GETARG(1, temp)
    read (temp,*) a
    CALL GETARG(2,temp)
    read (temp,*) b
    CALL GETARG(3,temp)
    read (temp,*) c
    
    delta = b**2-4*a*c
    if (delta >= 0) then
        if (delta == 0) then
            print*,'r1 =',(-b + SQRT(delta))/2*a
        else
            print*,'r1 =',(-b + SQRT(delta))/2*a
            print*,'r2 =',(-b - SQRT(delta))/2*a
        end if
    else
        delta = delta*(-1)
        r1 = (-b/2*a)
        r2 = (SQRT(delta)/2*a)
        write(temp1,*)r1
        write(temp2,*)r2
        print*,'r1 =',trim(temp1),' +',trim(temp2),' i'
        print*,'r2 =',trim(temp1),' -',trim(temp2),' i'
    end if
end program