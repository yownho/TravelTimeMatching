module hillsloperouting
use variables
use mpi
use surfacerunoff
use groundwaterrunoff
contains

subroutine hillslopes(hstart,hend)
integer hstart, hend
!Loop over all planes
!determine surface runoff and subsurface runoff to channels
!This loop can be sent to all process without waiting for results
 do j=hstart,hend
     !write(*,*) j
 
    !groundwater routing
	!check is subsurface routing is needed for plane 1
	!if(ex_sv(j).eq.0.AND.y_pl_sv(j,ndx).lt.0.0001)then
	!	q_pl_sv(j,ndx)=0.
	!else 
!		call qgroundwater(j,alp_pl_gw(j))
		call qgroundwater(j)
	!endif
   
    !surface routing
	!check is surface routing is needed for plane 1		
!	if(ex_s(j).eq.0.AND.y_pl_s(j,ndx).lt.0.00001)then
!		q_pl_s(j,ndx)=0
!	else 
	   !call qsurface(j,alp_pl_s(j),bet_pl_s(j))
	   call qsurface(j,bet_pl_s(j))
!	endif
 enddo

return
end subroutine hillslopes

end module hillsloperouting