module excesscalc
use variables
use mpi
use outputdata
use inputdata

contains

subroutine excess1() !daily WBM output, 1-hr Routing
character(120):: fname3, fname5,fmt
integer dvalread
integer i,j,k,n
real getpval

    !update date/time for next DT only used after spinup period
    if(tt.eq.1) then 
            iday=iday+1
            jday=jday+1
            if(iday.eq.29.AND.imonth.eq.2) then
                leapyrint=int(iyear/4.) !integer
                leapyrft=float(iyear)/4. !float
                leapyrdiff=leapyrft-leapyrint !on leap year diff = 0.0
                if(leapyrdiff.eq.0) then
                    iday=29 !do leap year, feb 29
                else
                    iday=1
                    imonth=imonth+1
                endif
            elseif(iday.eq.30.AND.imonth.eq.2) then !just finished Feb 29
                    iday=1
                    imonth=imonth+1
            elseif(iday.eq.31) then
                    Select Case (imonth)
                            case (1,3,5,7,8,10,12)
                                    iday=iday
                            case (4,6,9,11)
                                    iday=1
                                    imonth=imonth+1
                    End Select
            elseif(iday.eq.32) then
                    Select Case (imonth)
                            case (1,3,5,7,8,10)
                                    iday=1
                                    imonth=imonth+1
                            case (12)
                                    iday=1
                                    imonth=1
                                    iyear=iyear+1
                                    jday=1
                    End Select
            endif
    endif
    
!    fname3 = 'hrrlinkusgs.csv'
!    open(3,file=fname3)
!    read(3,*)hrrlinkusgs(j,1), hrrlinkusgs(j,2),hrrlinkusgs(j,3)
!    close(3)
    
        
    do j=1,pfafunits
    
        prate_sur(j) = 0.0
        prate_sub(j) = 0.0
    
        do n=1,usgsggnum
            !if (j==1) write(*,*) hrrlinkusgs(j,1),hrrlinkusgs(j,2),hrrlinkusgs(j,3), usgsid(n)
            if (hrrlinkusgs(j,3) .eq. usgsid(n)) then
                prate_sur(j) = usgsrunoff_sur(n,t)/12  !in/hour to ft/hour
                prate_sub(j) = usgsrunoff_sub(n,t)/12  !in/hour to ft/hour
                !write(*,*) hrrlinkusgs(j,3),usgsid(n)
            endif
        enddo
        
        
        ex_s(j)=prate_sur(j)/dtis !ft/DT to ft/s

        ex_gw(j)=prate_sub(j)/dtis
        
        !if (j==10) write(*,*) ex_s(j), ex_gw(j)

    enddo 
    
   
return
end subroutine excess1

end module excesscalc
