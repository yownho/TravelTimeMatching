module outputdata
use variables
use mpi
contains

subroutine outputdata1()
CHARACTER(LEN=80) :: FMT

!Get output locations
fname11 = 'output_calibration.txt'
open(11,file=fname11,status='OLD')
read(11,*) numout
do k=1,numout
        read(11,*) idout(k), dvalint, idgauge(k)
enddo
close(11)

fname12 = 'discharge_cfs.txt'
open(12,file=fname12,status='REPLACE')
FMT = "(000000i20)"
write(FMT(2:7),'(i6.6)') numout
!write(12,FMT) (idout(i),i=1,numout)
write(12,FMT) (idgauge(i),i=1,numout)
close(12)

!!!-----velocity on plane
!!fname210 = 'vel_pl_out.txt'
!!open(210, file=fname210, status='REPLACE')
!!close(210)
!!
!!!-----Qch for dx = 1 and 10
!!fname220 = 'dep_pl_out.txt'
!!open(220, file=fname220, status='REPLACE')
!!close(220)

!!-----Output plane and channel q for dx, 8/22/2017
!!Open results file for specified model units 


return
end subroutine outputdata1

subroutine resultsout()
CHARACTER(LEN=80) :: FMT
!Open results file for specified model units
fname12 = 'discharge_cfs.txt'
open(12,file=fname12,status='OLD',POSITION='APPEND')
FMT = "(000000F20.5)"
write(FMT(2:7),'(i6.6)') numout
write(12,FMT) (old_q_day(idout(i)),i=1,numout)
close(12)



return
end subroutine resultsout

end module outputdata
