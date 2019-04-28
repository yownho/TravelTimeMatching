module groundwaterrunoff
use variables
use mpi
contains

!subroutine qgroundwater(j,alp)
subroutine qgroundwater(j)
	real alp
	real dtx,alat,assum,qss,qlatp,qup
	integer j,k1
!
!	Hillslope Routing for Subsurface Runoff
!	j	subunit identifier
!   alp y=alp x q (linear function for relating depth and flow)
!	k1 	space step index
!	plane parameters
	dtx=dtis*ndx/length_p(j)
	qup = 0.
!   begin the kinematic wave routing calculation for the current time step.
    do k1=1,ndx   !loop over downstream distance steps
            !************************
            ! Scaling of ground water
            alp = alp_pl_gw(j,k1)
            !************************
!           average the new value of lateral inflow with that from the last time step.
            qlatp=0.5*(ex_gw(j)+qlat_gw_old(j))
            if(t.eq.1) qlatp=ex_gw(j)
        	alat=qlatp*dtis
        	
!           asum is omega in b-10.
		    assum=alat+y_pl_gw(j,k1)+dtx*qup
		    
!           The solution for qss, below, is exact
		    qss=assum/(dtx+alp)
		    y_pl_gw(j,k1)=alp*qss
		    q_pl_gw(j,k1)=qss
		    
		    !if(j.eq.10) write(*,*) alp,q_pl_gw(j,ndx)
		    
!	        Set qup for the next space step
		    qup=qss
    enddo ! loop over downstream distance steps
        
	qlat_gw_old(j)=ex_gw(j)
	
	return
end subroutine qgroundwater

end module groundwaterrunoff
