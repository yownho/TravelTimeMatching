module hrrmpi
use mpi
use variables
contains

subroutine hrrmpi1()
    call MPI_INIT (ierr)
    call MPI_COMM_SIZE (MPI_COMM_WORLD, pssize, ierr)
    call MPI_COMM_RANK (MPI_COMM_WORLD, myrank, ierr)
    call MPI_GET_PROCESSOR_NAME (myname, namelen, ierr)
    return
end subroutine hrrmpi1

subroutine hrrmpi2()
    call MPI_BCAST(pfafunits,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(usgsggnum,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr) !total usgs gauges number
    call MPI_BCAST(usgsnd,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr) ! USGS days
    call MPI_BCAST(ndx,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(ndt,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)  
    call MPI_BCAST(dtis,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(iyear,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(imax,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(eps,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)        
    call MPI_BCAST(Khgw_all,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(k_all,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(n_ch_all,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    return
end subroutine hrrmpi2

subroutine hrrmpi3()
    call MPI_BCAST(id,pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(pfaf,pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)  
    call MPI_BCAST(downpfaf,pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(nup,pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(uppfaf(1,1),pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(uppfaf(1,2),pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(uppfaf(1,3),pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(uppfaf(1,4),pfafunits,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

    !for storage calcs
    call MPI_BCAST(A,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    !call MPI_BCAST(Aup,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(chv1,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)

    !call MPI_BCAST(qbar_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(depth_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
   
    call MPI_BCAST(sin_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(cc1,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(cc2,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(cc3,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)  
    call MPI_BCAST(cc4,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)

    call MPI_BCAST(old_q(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    call MPI_BCAST(old_q_ch(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(old_q_ch(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    !*****output qch for each dx for channel, 9/1/2016
    call MPI_BCAST(q_QA_ch(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_QA_ch(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    call MPI_BCAST(length_p,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(length_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    call MPI_BCAST(width_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(Qr_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    !call MPI_BCAST(alp_pl_s,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    !-----YZ
    call MPI_BCAST(alp_pl_s(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_s(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    !-----
    call MPI_BCAST(bet_pl_s,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)  
    !call MPI_BCAST(alp_pl_gw,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    !############################################
    call MPI_BCAST(alp_pl_gw(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(alp_pl_gw(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    !##############################################        

    call MPI_BCAST(qlat_ch_old,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr) 
    call MPI_BCAST(qlat_s_old,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr) 
    call MPI_BCAST(qlat_gw_old,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr) 
    
    call MPI_BCAST(Kh_gw,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(n_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)

    call MPI_BCAST(slope_p,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(slope_ch,pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
        
    call MPI_BCAST(y_pl_s(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_s(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)

    call MPI_BCAST(y_pl_gw(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(y_pl_gw(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    call MPI_BCAST(q_pl_s(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_s(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    call MPI_BCAST(q_pl_gw(1,1),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,2),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,3),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,4),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,5),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,6),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,7),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,8),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,9),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(q_pl_gw(1,10),pfafunits,MPI_REAL,0,MPI_COMM_WORLD,ierr)
    
    call MPI_BCAST(tt,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(ttt,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        
    return
end subroutine hrrmpi3

subroutine hrrmpi4()
    call MPI_BCAST(numout,1,MPI_INTEGER,pssize-1,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(idout,numout,MPI_INTEGER,pssize-1,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(idgauge,numout,MPI_INTEGER,pssize-1,MPI_COMM_WORLD,ierr) !--YZ
    return
end subroutine hrrmpi4

end module hrrmpi
