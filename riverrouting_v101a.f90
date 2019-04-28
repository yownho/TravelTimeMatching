module riverrouting
use variables
use mpi
use channel
contains

subroutine rivers(rstart,rend)
integer rstart, rend, k
character*11 fname160
character*2 cRR 
!real q_10_ch_old, q_1_ch_old
!Loop over all channels (planes must be done first, see above)
!This loop is dependent on upstream inflows, use domains to parallelize
do j=rstart,rend    

	qlat_ch=(q_pl_s(j,ndx)*2+q_pl_gw(j,ndx)*2)/sin_ch(j)
	qlat_ch_ave=0.5*(qlat_ch+qlat_ch_old(j)) !added 11/19/09

	!inflow = will be sum of upstream outflows;
	if (nup(j).eq.0)then
		q_in(j)=old_q(j,1) !first DX q
	elseif(nup(j).eq.1)then
		q_in(j)=old_q(uppfaf(j,1),ndx)
	elseif(nup(j).eq.2)then
		q_in(j)=old_q(uppfaf(j,1),ndx)+old_q(uppfaf(j,2),ndx)
	elseif(nup(j).eq.3)then
		q_in(j)=old_q(uppfaf(j,1),ndx)+old_q(uppfaf(j,2),ndx)+old_q(uppfaf(j,3),ndx)
    elseif(nup(j).eq.4)then
		q_in(j)=old_q(uppfaf(j,1),ndx)+old_q(uppfaf(j,2),ndx)+old_q(uppfaf(j,3),ndx)+old_q(uppfaf(j,4),ndx)
	endif

    !added to check mass balance
	if(t.eq.1) then
	    qlat_ch_ave=qlat_ch
        q_out(j)=q_in(j)
    endif
    
	if(qlat_ch.eq.0.AND.q_in(j).lt.0.0001.AND.q_out(j).lt.0.0001)then
	    q_out(j)=0.
	    old_q(j,ndx)=0. 
    else
	    call route_mc_ch(j)
	endif
    qlat_ch_old(j)=qlat_ch 
!    if (j.eq.100) write(*,*) qlat_ch_old(j)
    
enddo

return
end subroutine rivers

end module riverrouting
