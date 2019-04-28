!****************************************************************************
!  PROGRAM:  Hillslope River Routing (HRR) Model Verison 2b Parallel
!  PURPOSE:  Entry point for the console application.
!****************************************************************************
program PFAF_Model
    use variables
    use inputdata
    use outputdata
    use excesscalc
    use hillsloperouting
    use riverrouting
    use channel
    use mpi !MPI library (Intel MPI 3.2)
    use hrrmpi
    
    call cpu_time(beg_cpu_time)
    
    !set MPI parameters
    call hrrmpi1()

    !get main model parmaters and build arrays
    if (myrank.eq.0) call inputdata1()
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    !write(*,*) 'Input data1 done!'
    
    !Broadcast MPI for inputdata1 data
    call hrrmpi2()
    !write(*,*) 'hrrmpi2 done!'
    
    !initialize all arrays
    call allocatenow()
    !write(*,*) 'allocate done!'        
    call setzero()
    !write(*,*) 'initialize done!'
    
    !get planes and channel data
    if (myrank.eq.0) call inputdata2()
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    !write(*,*) 'input data2 done'
    
    !Broadcast MPI for inputdata2 data
    call hrrmpi3()
    !write(*,*) 'hrrmpi3 done!'
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    
    ! get usgs data and hrrid linked to usgsid
    if (myrank.eq.(pssize-1)) call inputdata3()
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    !Prepare output files for results
    if (myrank.eq.(pssize-1)) call outputdata1()
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    
    !Broadcast MPI for outputdata1
    call hrrmpi4()
    if (myrank.eq.(pssize-1)) write(*,*) 'Starting MPI setup'
    
    !**********build linkages for MPI to sentout model units********** 
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    npp = INT(pfafunits/(pssize-1)) !number of model units per CPU/core
    if (myrank .eq. 0) write(*,*) myrank, pssize, pfafunits, npp
    if (myrank.lt.(pssize-1)) then
        do j=((myrank*npp)+1),((myrank+1)*npp)
            !Determine MPI send info
            if (downpfaf(j).gt.((myrank+1)*npp)) then
                npass(myrank+1) = npass(myrank+1)+1
                passfrom(npass(myrank+1),myrank+1) = j
               ! passtop(npass(myrank+1),myrank+1) = INT(downpfaf(j)/npp)
                passtop(npass(myrank+1),myrank+1) = INT((downpfaf(j)-1)/npp) !fixed
            endif
            !Determine MPI receive info
            if (uppfaf(j,1).lt.((myrank*npp)+1).AND.uppfaf(j,1).gt.0) then
                nget(myrank+1) = nget(myrank+1)+1
                getfrom(nget(myrank+1),myrank+1) = uppfaf(j,1)
                getfromp(nget(myrank+1),myrank+1) = INT((uppfaf(j,1)-1)/npp)
            endif
            if (uppfaf(j,2).lt.((myrank*npp)+1).AND.uppfaf(j,2).gt.0) then
                nget(myrank+1) = nget(myrank+1)+1
                getfrom(nget(myrank+1),myrank+1) = uppfaf(j,2)
                getfromp(nget(myrank+1),myrank+1) = INT((uppfaf(j,2)-1)/npp)
            endif
            if (uppfaf(j,3).lt.((myrank*npp)+1).AND.uppfaf(j,3).gt.0) then
                nget(myrank+1) = nget(myrank+1)+1
                getfrom(nget(myrank+1),myrank+1) = uppfaf(j,3)
                getfromp(nget(myrank+1),myrank+1) = INT((uppfaf(j,3)-1)/npp)
            endif
        enddo
    else !last processor (myrank = pssize-1)
        do j=((myrank*npp)+1),pfafunits
            !Determine MPI receive info
            if (uppfaf(j,1).lt.((myrank*npp)+1).AND.uppfaf(j,1).gt.0) then
                nget(myrank+1) = nget(myrank+1)+1
                getfrom(nget(myrank+1),myrank+1) = uppfaf(j,1)
                getfromp(nget(myrank+1),myrank+1) = INT((uppfaf(j,1)-1)/npp)
            endif
            if (uppfaf(j,2).lt.((myrank*npp)+1).AND.uppfaf(j,2).gt.0) then
                nget(myrank+1) = nget(myrank+1)+1
                getfrom(nget(myrank+1),myrank+1) = uppfaf(j,2)
                getfromp(nget(myrank+1),myrank+1) = INT((uppfaf(j,2)-1)/npp)
            endif
            if (uppfaf(j,3).lt.((myrank*npp)+1).AND.uppfaf(j,3).gt.0) then
                nget(myrank+1) = nget(myrank+1)+1
                getfrom(nget(myrank+1),myrank+1) = uppfaf(j,3)
                getfromp(nget(myrank+1),myrank+1) = INT((uppfaf(j,3)-1)/npp)
            endif
        enddo
    endif 
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
                            
    !##############################################################################
    !Start of Main time loop
    if (myrank.eq.(pssize-1)) write(*,*) 'Start Main Time Loop'
    daynum=1 !start run on day 1
    dayval=1
    imonth=1
    iday=1
    tt=1 !hour of day at start of the run
    ttt=1.0 !30 day counter
    jday=1
    ht = 1.0
    
    do t=1,ndt
 
 !##############################               
!        if (t.le.25) then  !after you broadcast all zeros you don't have to do this again          
!         if(tt.eq.1) then
            if (myrank.eq.(pssize-1)) call excess1() 
            !write(*,*) 'running excess...'
            
            call MPI_BARRIER(MPI_COMM_WORLD,ierr)
            
            call MPI_BCAST(ex_s,pfafunits,MPI_REAL,&
                            (pssize-1),MPI_COMM_WORLD,ierr)
                            
            call MPI_BCAST(ex_gw,pfafunits,MPI_REAL,&
                            (pssize-1),MPI_COMM_WORLD,ierr)
!        endif
!################################
        
        !Loop over all planes: surface and subsurface runoff to channels
        !This loop can be sent to all process without waiting for results
        if (myrank.lt.(pssize-1)) then
                    call hillslopes((myrank*npp)+1,((myrank+1)*npp))
                    call rivers((myrank*npp)+1,((myrank+1)*npp))
                    !pass channel discharge to downstream units
                    if (npass(myrank+1).gt.0) then
                        do m=1,npass(myrank+1)
                            call MPI_Send(old_q(passfrom(m,myrank+1),ndx),1,&
                                    MPI_REAL,passtop(m,myrank+1),&
                                            passfrom(m,myrank+1),&
                                    MPI_COMM_WORLD,ierr)     
                        enddo
                    endif
                    !Get channel discharge from upstream units
                    if (nget(myrank+1).gt.0) then
                        do m=1,nget(myrank+1)
                            getval = old_q(getfrom(m,myrank+1),ndx)
                            getp = getfromp(m,myrank+1)
                            call MPI_Recv(old_q(getfrom(m,myrank+1),ndx),1,&
                                MPI_REAL,getfromp(m,myrank+1),&
                                    getfrom(m,myrank+1),&
                            MPI_COMM_WORLD,stat,ierr)
                        enddo
                    endif
        else        !last set of model units
                    call hillslopes((myrank*npp)+1,pfafunits)
                    call rivers((myrank*npp)+1,pfafunits)

                    !Get channel discharge from upstream units
                    if (nget(myrank+1).gt.0) then
                        do m=1,nget(myrank+1)
                            getval = old_q(getfrom(m,myrank+1),ndx)
                            getp = getfromp(m,myrank+1)
                            call MPI_Recv(old_q(getfrom(m,myrank+1),ndx),1,&
                                MPI_REAL,getfromp(m,myrank+1),&
                                    getfrom(m,myrank+1),&
                                MPI_COMM_WORLD,stat,ierr)
                        enddo
                    endif
        endif
          
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        
        !send requested nodes to (myrank.eq.(pssize-1)) for output to file
        do k=1,numout
            if (myrank.lt.(pssize-1)) then
                if (idout(k).ge.(myrank*npp+1).AND.idout(k).le.((myrank+1)*npp)) then
                    call MPI_Send(old_q(idout(k),ndx),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    !-----add output of each dx, 8/22/2017
!                    call MPI_Send(vel_pl_s(idout(k),1),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),2),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),3),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),4),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),5),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),6),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),7),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),8),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),9),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
!                    call MPI_Send(vel_pl_s(idout(k),10),1,MPI_REAL,pssize-1,idout(k),MPI_COMM_WORLD,ierr)
                endif
            endif
            
            if (myrank.eq.(pssize-1)) then
                do m=1,(pssize-1)
                    if (idout(k).ge.((m-1)*npp+1).AND.idout(k).le.(m*npp)) then
                        call MPI_Recv(old_q(idout(k),ndx),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        !-----add output of each dx, 8/22/2017
!                        call MPI_Recv(vel_pl_s(idout(k),1),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),2),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),3),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),4),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),5),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),6),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),7),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),8),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),9),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
!                        call MPI_Recv(vel_pl_s(idout(k),10),1,MPI_REAL,(m-1),idout(k),MPI_COMM_WORLD,stat,ierr)
                        !----- 8/22/2017
                    endif
                enddo
            endif               
        enddo
             
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        
        !Write out hydrographs at select nodes: cfs at each 15-min   		 
        if (myrank.eq.(pssize-1)) then
                        do i=1, numout
                                old_q_day(idout(i)) = old_q(idout(i),ndx) !- hourly
                        enddo
                        call resultsout()
                
                if(tt.eq.24)then
                        if (dayval.ge.1) then                                 
                                tt = 0 !will be 1 after endif
                                dayval=dayval+1
                                daynum = daynum+1
                                if(iday.eq.1) write(*,*) iyear, imonth, iday, 'Q(cfs) ', old_q_day(idout(1))
                        endif
                endif
                tt = tt + 1 !hr counter
        endif

        call MPI_BCAST(tt,1,MPI_INTEGER,(pssize-1),MPI_COMM_WORLD,ierr)
        call MPI_BCAST(daynum,1,MPI_INTEGER,(pssize-1),MPI_COMM_WORLD,ierr)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        
        if (myrank.eq.(pssize-1).AND.ttt.eq.720) then
                        !get MPI storages
!            do m=1,(myrank*npp)
!                !need to check for more than two cores
!                call MPI_Recv(chv(m),1,MPI_REAL,INT((m-1)/npp),m,MPI_COMM_WORLD,stat,ierr)
!                call MPI_Recv(sv(m),1,MPI_REAL,INT((m-1)/npp),m,MPI_COMM_WORLD,stat,ierr)
!                call MPI_Recv(gwv(m),1,MPI_REAL,INT((m-1)/npp),m,MPI_COMM_WORLD,stat,ierr)
!            enddo
!            call resultsout1()
            ttt = 1
        elseif (myrank.eq.(pssize-1)) then
            ttt = ttt + 1
        endif
        call MPI_BCAST(ttt,1,MPI_INTEGER,(pssize-1),MPI_COMM_WORLD,ierr)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
                
    enddo
    
    call MPI_FINALIZE(ierr)
!##############################################################################
!End Main time loop
call cpu_time (end_cpu_time)
write (*, *) (end_cpu_time - beg_cpu_time)
   
end program PFAF_Model
