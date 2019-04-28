module variables
use mpi

        !misc file names
        character(120)::    fname10,fname11,fname12,fname13,fname15
        character(120)::    fname100
        character(120)::    fname20,fname21,fname22,fname23,fname24
        character(120)::    fname30,fname40
        character(120)::    fname210,fname220
        
        !output parameters
        integer,allocatable    :: idout(:)      !model unit ID for output (1 to pfafunits)
        integer,allocatable    :: idgauge(:)    !Gauge ID for output PFAF ID
                
        !Model unit information
        integer,allocatable    :: id(:)         !model unit ID (1 to pfafunits)
        !**********************
        integer,allocatable    :: usgsid(:)         !usgs ID (1 to pfafunits)
        !**********************
        integer,allocatable    :: pfaf(:)       !model unit pfaf ID (9 to 1)
        integer,allocatable    :: downpfaf(:)   !model unit downstream
        integer,allocatable    :: nup(:)        !number of upstream model units
        integer,allocatable    :: uppfaf(:,:)   !model unit ID for 1 or 2 unpstream units
        real,allocatable       :: A(:)          !model unit area (km2)
        real,allocatable       :: Aup(:)        !total upstream pfaf area (km2)
        real,allocatable       :: chv1(:)       !part of mannings equ, for getting depth from Q
        real,allocatable       :: chv(:)        !channel storage (cm)
        real,allocatable       :: sv(:)         !surface storage (cm)
        real,allocatable       :: gwv(:)        !groundwater storage (cm)
        real,allocatable       :: ssv(:)        !subsurface storage (cm)
        real,allocatable       :: soilv(:)      !soil storage (cm)
        real,allocatable       :: qbar_ch(:)    !mean channel flow (cfs)
        real,allocatable       :: depth_ch(:)   !channel depth (ft)
        
        !WBM parameters
	    real,allocatable    :: ex_s(:)          !surface excess from WBM (ft/s)
	    real,allocatable    :: ex_ss(:)         !subsurface ecess from WBM (ft/s)
	    real,allocatable    :: ex_gw(:)         !groundwater ecess from WBM (ft/s)
        real,allocatable    :: etrate(:)        !ET in from other models, units vary
        !**************************************
        real,allocatable    :: prate_sur(:)         !P in from other models, units vary 
        real,allocatable    :: prate_sub(:)         !P in from other models, units vary 
            
        real,allocatable    :: ptime(:,:)       !precip window values, units vary        
	    real,allocatable    :: cumF(:)          !cum infiltration (ft)
	    real,allocatable    :: satmc(:)         !sat moisture content (ft/ft)
	    real,allocatable    :: soilD(:)         !soil depth (ft/ft)
	    !real,allocatable    :: Kh_gw(:)          !Ksat (ft/s)
        real,allocatable    :: Kh_ss(:)         !Horizontal Condutivity, ft/s, shallow subsurface
        real,allocatable    :: Kh_gw(:)         !Horizontal Condutivity, ft/s, groundwater
        real,allocatable    :: ksurf(:)         !surface roughness
        real,allocatable    :: LCval(:)         !land cover type
        real,allocatable    :: runco(:)         !runoff coefficient for each catchment
	    
	    !runoff coeff. parameters, added 7/7/2014
	    real,allocatable    :: pFlg1F(:)
	    real,allocatable    :: pFlg1AGB(:)
	    real,allocatable    :: pFlg1WU(:)
	    real,allocatable    :: pFlg2F(:)
	    real,allocatable    :: pFlg2AGB(:)
	    real,allocatable    :: pFlg2WU(:)
	    real,allocatable    :: pFlg3F(:)
	    real,allocatable    :: pFlg3AGB(:)
	    real,allocatable    :: pFlg3WU(:)
	    
	    real,allocatable    :: pMg1F(:)
	    real,allocatable    :: pMg1AGB(:)
	    real,allocatable    :: pMg1WU(:)
        real,allocatable    :: pMg2F(:)
        real,allocatable    :: pMg2AGB(:)
        real,allocatable    :: pMg2WU(:)
        real,allocatable    :: pMg3F(:)
        real,allocatable    :: pMg3AGB(:)
        real,allocatable    :: pMg3WU(:)
        
        real,allocatable    :: pSg1F(:)
        real,allocatable    :: pSg1AGB(:)
        real,allocatable    :: pSg1WU(:)
        real,allocatable    :: pSg2F(:)
        real,allocatable    :: pSg2AGB(:)
        real,allocatable    :: pSg2WU(:)
        real,allocatable    :: pSg3F(:)
        real,allocatable    :: pSg3AGB(:)
        real,allocatable    :: pSg3WU(:)
        
        !*************************************
        real,allocatable    :: k_all_seg(:,:) !kall for each segment, Mar 2, 2015
        real,allocatable    :: khgw_seg(:,:) !kall for each segment, Mar 2, 2015
        
        !**************************************
        real,allocatable    :: usgsrunoff_sur(:,:) !usgs syn runoff
        real,allocatable    :: usgsrunoff_sub(:,:) !usgs syn runoff
        integer,allocatable    :: hrrlinkusgs(:,:) !hrr with usgs gauge number table
	    
        !channel parameters
	    real,allocatable    :: Qr_ch(:)         !reference discharge (cfs)
	    real,allocatable    :: sin_ch(:)        !sinuocity of the channel (e.g., 1.5 or 2.0)
	    real,allocatable    :: n_ch(:)          !Manning's n of the channel
	    real,allocatable    :: length_ch(:)     !length of the channel, ft
	    real,allocatable    :: slope_ch(:)      !slope of the channel, ft/ft
	    real,allocatable    :: width_ch(:)      !width of the channel, ft
            
	    real,allocatable    :: cc1(:)           !MC - Constant Parameter, ch: 1 
	    real,allocatable    :: cc2(:)           !MC - Constant Parameter, ch: 2
	    real,allocatable    :: cc3(:)           !MC - Constant Parameter, ch: 3
	    real,allocatable    :: cc4(:)           !MC - Constant Parameter, ch: 4

	    real,allocatable    :: q_out(:)         !model unit ch Q (cfs) out
	    real,allocatable    :: q_in(:)          !model unit ch Q (cfs) in
	    real,allocatable    :: q_in_old(:)      !model unit ch Q (cfs) old in
	    real,allocatable    :: q_out_old(:)     !model unit ch Q (cfs) old out
	    real,allocatable    :: old_q(:,:)       !model unit ch Q (cfs) old for each dx
	    real,allocatable    :: old_q_day(:)     !model unit ch Q (cfs) for daily average
	    real,allocatable    :: old_q_ch(:,:)    !model unit ch Q (cfs) old for each dx
	    real,allocatable    :: q_QA_ch(:,:)     !output of each dx for channel q
	    real,allocatable    :: qlat_ch_old(:)   !old lateral flow into channel (cfs/ft)
        
        !Plane parameters
	    real,allocatable    :: length_p(:)      !down slope plane length (ft)
	    real,allocatable    :: slope_p(:)       !down slope plane  ft/ft)

	    !real,allocatable    :: alp_pl_s(:)      !surface runoff alpha; y=alha*q^beta
	    real,allocatable    :: alp_pl_s(:,:)      !surface runoff alpha; y=alha*q^beta !
	    real,allocatable    :: bet_pl_s(:)      !surface runoff beat; y=alha*q^beta
	    real,allocatable    :: alp_pl_gw(:,:)     !groundwater runoff alpha; y=alha*q^1
	    real,allocatable    :: alp_pl_ss(:)     !subsurface runoff alpha; y=alha*q^1

	    real,allocatable    :: y_pl_s(:,:)      !surface runoff depth (ft)
	    real,allocatable    :: q_pl_s(:,:)      !surface runoff (cfs/ft of channel)
	    !*****vel of plane
	    real,allocatable    :: vel_pl_s(:,:)    !vel of plane
	    real,allocatable    :: vel_pl_s_max(:,:)    !vel of plane
	    real,allocatable    :: y_pl_gw(:,:)     !groundwater runoff depth (ft)
	    real,allocatable    :: q_pl_gw(:,:)     !groundwater runoff (cfs/ft of channel)
	    !real,allocatable    :: y_pl_ss(:,:)     !subsurface runoff depth (ft)
	    !real,allocatable    :: q_pl_ss(:,:)     !subsurface runoff (cfs/ft of channel)
	    real,allocatable    :: qlat_s_old(:)    !old lateral flow into surface runoff (ft/s)
	    real,allocatable    :: qlat_ss_old(:)   !old lateral flow into subsurface runoff (ft/s)
	    real,allocatable    :: qlat_gw_old(:)   !old lateral flow into groundwater runoff (ft/s)

        !Misc. parameters
        integer j, t, k, tt, ttt, ii, krec,ht     !counters
        integer l
        integer numout          !total number of model units to output data
        integer pfafunits       !total number of model units
        
        !********************************
        integer usgsnd          ! days of syn usgs runoff
        integer usgsggnum       !***usgs gauge total number
        
        integer ndx             !number of dx steps 
        integer ndt             !number of dt steps
        integer dtis            !seconds in a dt step
        integer imax            !max number of iterations in the surface_kwave routing
        integer dvalint         !blank value used to read data tables
        integer dataMethod      !1 or 2 for the data methods
        
        real dval               !blank value used to read data tables
        integer dayval, iyear, imonth, iday, daynum, jday !counters	

        !plane parameters
        real eps                !min error for accepting surface_kwave routing solution
    
        !Channel Paramters
        real a_w_ch, b_w_ch         !alpha and beta for channel W eq.
        real a_Q_ch, b_Q_ch         !alpha and beta for channel Qr eq.
        real Qr_ref                 !Q reference split for q_bank between ch and fp
        real C1, y, Ax, celert, sreach, tv, c, d, cdenom   !MC channel parameters
        real qlat_ch, qlat_ch_ave   !combined lateral inflow (qs + qss from both planes) to a channel (cfs/ft)
        real shbar, sshbar        !used to determine mean depths for surface and subsurface planes

        !Uniform adjustment factors
        real n_ch_all          ! Manning's n, uniform value for all channels
        real Khss_all          ! Horizontal Condutivity adjustment, shallow
        real Khgw_all          ! Horizontal Condutivity adjustment, groundwater
        real k_all             ! surface roughness adjustment
        real soilD_all         ! soil deoth
        real Ksat_all          ! vertical ksat
        real Kdecay_all        ! vertical decay in Ksat 
        real et_all            ! ET 
        real satmc_all         ! sat soil moisture
        real sstip             ! relative soil moisture above which turns on shallow flow
        real sspart            ! fraction of drainage sent to shallow flow
        
        !real ksat_min, ksat_max
        real Khgw_min, Khgw_max
        real k_min, k_max
        real soilD_min, soilD_max
        real satmc_min, satmc_max
        real n_ch_min, n_ch_max
        real Lch_min_slope, Lch_max_slope
        real Lp_min_slope, Lp_max_slope        
        real min_rz_sm, setfsub_rate, setfsurf_rate  
        
        real RC1m                !adjustment to runoff coefficient dry conditions
        real RC2m                !adjustment to runoff coefficient wet conditions
        real pmin                !mm of rainfall in "numpdays" prior to time t, threshold between dry/wet
        integer numpdays         !number of days to sum precip over for initial conditions

        real RCFlg1F
        real RCFlg1AGB
        real RCFlg1WU
        real RCFlg2F
        real RCFlg2AGB
        real RCFlg2WU
        real RCFlg3F
        real RCFlg3AGB
        real RCFlg3WU
        
        real RCMg1F
        real RCMg1AGB
        real RCMg1WU
        real RCMg2F
        real RCMg2AGB
        real RCMg2WU
        real RCMg3F
        real RCMg3AGB
        real RCMg3WU
        
        real RCSg1F
        real RCSg1AGB
        real RCSg1WU
        real RCSg2F
        real RCSg2AGB
        real RCSg2WU
        real RCSg3F
        real RCSg3AGB
        real RCSg3WU
        
        real scaleFac
        real sf_ss
        
        integer storageR        !used to include storage (1) or not (0)
        
        ! MPI initialize **************************************
        integer :: pssize, myrank, namelen, ierr
        character (len=MPI_MAX_PROCESSOR_NAME) :: myname
        integer :: stat(MPI_STATUS_SIZE)
   
        real passval                            !used to pass MPI real values
        integer npp                             !number of model units for each processor 
        integer,allocatable    :: npass(:)      !number of values to pass from a processor
        integer,allocatable    :: nget(:)       !number of values to get from processors
        integer,allocatable    :: passfrom(:,:) ! num to pass,  model unit ID
        integer,allocatable    :: passtop(:,:)  ! num to pass, pass to processor
        integer,allocatable    :: getfrom(:,:)  ! num to get, model unit ID
        integer,allocatable    :: getfromp(:,:) ! num to get, get from processor
        ! ******************************************************
      
contains

subroutine allocatenow
        allocate        (idout(1:pfafunits))
        allocate        (idgauge(1:pfafunits))

        allocate        (id(1:pfafunits))
        allocate        (usgsid(1:usgsggnum))
        allocate        (pfaf(1:pfafunits))
        allocate        (downpfaf(1:pfafunits))
        allocate        (nup(1:pfafunits))
        !allocate        (uppfaf(1:pfafunits,1:3))
        allocate        (uppfaf(1:pfafunits,1:4))

        allocate        (A(1:pfafunits))
        allocate        (Aup(1:pfafunits))
        allocate        (chv1(1:pfafunits))

        allocate        (chv(1:pfafunits))
        allocate        (sv(1:pfafunits))
        allocate        (ssv(1:pfafunits))
        allocate        (gwv(1:pfafunits))
        allocate        (soilv(1:pfafunits))
        allocate        (qbar_ch(1:pfafunits))
        allocate        (depth_ch(1:pfafunits))

        allocate        (ex_s(1:pfafunits))
        allocate        (ex_gw(1:pfafunits))
        allocate        (ex_ss(1:pfafunits))
        !*****************USGS syn runoff
        allocate        (prate_sur(1:pfafunits))
        allocate        (prate_sub(1:pfafunits))
        
        allocate        (etrate(1:pfafunits))

        allocate        (ptime(1:pfafunits,1:numpdays))
        
        allocate        (cumF(1:pfafunits))
        allocate        (satmc(1:pfafunits))
        allocate        (soilD(1:pfafunits))
        !allocate        (Kh_gw(1:pfafunits))
        allocate        (Kh_gw(1:pfafunits))
        allocate        (Kh_ss(1:pfafunits))
        allocate        (ksurf(1:pfafunits)) 
        allocate        (LCval(1:pfafunits))
        allocate        (runco(1:pfafunits))

        !runoff coeff. parameters, added 7/7/2014
	    allocate     (pFlg1F(1:pfafunits))
	    allocate     (pFlg1AGB(1:pfafunits))
	    allocate     (pFlg1WU(1:pfafunits))
	    allocate     (pFlg2F(1:pfafunits))
	    allocate     (pFlg2AGB(1:pfafunits))
	    allocate     (pFlg2WU(1:pfafunits))
	    allocate     (pFlg3F(1:pfafunits))
	    allocate     (pFlg3AGB(1:pfafunits))
	    allocate     (pFlg3WU(1:pfafunits))
	    
	    allocate     (pMg1F(1:pfafunits))
	    allocate     (pMg1AGB(1:pfafunits))
	    allocate     (pMg1WU(1:pfafunits))
        allocate     (pMg2F(1:pfafunits))
        allocate     (pMg2AGB(1:pfafunits))
        allocate     (pMg2WU(1:pfafunits))
        allocate     (pMg3F(1:pfafunits))
        allocate     (pMg3AGB(1:pfafunits))
        allocate     (pMg3WU(1:pfafunits))
        
        allocate     (pSg1F(1:pfafunits))
        allocate     (pSg1AGB(1:pfafunits))
        allocate     (pSg1WU(1:pfafunits))
        allocate     (pSg2F(1:pfafunits))
        allocate     (pSg2AGB(1:pfafunits))
        allocate     (pSg2WU(1:pfafunits))
        allocate     (pSg3F(1:pfafunits))
        allocate     (pSg3AGB(1:pfafunits))
        allocate     (pSg3WU(1:pfafunits))
        !runoff coeff. parameters, added 7/7/2014
       
        allocate        (sin_ch(1:pfafunits))
        allocate        (Qr_ch(1:pfafunits))
        allocate        (length_ch(1:pfafunits))
        allocate        (slope_ch(1:pfafunits))
        allocate        (width_ch(1:pfafunits))
        allocate        (n_ch(1:pfafunits))
        allocate        (cc1(1:pfafunits))
        allocate        (cc2(1:pfafunits))
        allocate        (cc3(1:pfafunits))
        allocate        (cc4(1:pfafunits))

        allocate        (q_out(1:pfafunits))
        allocate        (q_in(1:pfafunits))
        allocate        (q_in_old(1:pfafunits))
        allocate        (q_out_old(1:pfafunits))
        allocate        (old_q(1:pfafunits,1:ndx))
        allocate        (old_q_day(1:pfafunits))
        allocate        (old_q_ch(1:pfafunits,1:ndx))
        allocate        (q_QA_ch(1:pfafunits,1:ndx)) !*****channel q output, 9/1/2016
        allocate        (qlat_ch_old(1:pfafunits))
        allocate        (length_p(1:pfafunits))
        allocate        (slope_p(1:pfafunits))
        allocate        (bet_pl_s(1:pfafunits))
        !allocate        (alp_pl_s(1:pfafunits))
        allocate        (alp_pl_s(1:pfafunits,1:ndx)) !scaling, change to each section
        allocate        (k_all_seg(1:pfafunits,1:10))  !scaling, kall for each section
        allocate        (alp_pl_gw(1:pfafunits,1:ndx))
        allocate        (khgw_seg(1:pfafunits,1:10))  !scaling, kall for each section
        
        !*************
        allocate        (usgsrunoff_sur(1:usgsggnum,1:(ndt+1))) !usgs runoff table
        allocate        (usgsrunoff_sub(1:usgsggnum,1:(ndt+1))) !usgs runoff table
        allocate        (hrrlinkusgs(1:pfafunits,1:3))  !***** USGS syn runoff
        
        
        !allocate        (alp_pl_ss(1:pfafunits))

        allocate        (y_pl_s(1:pfafunits,1:ndx))
        allocate        (q_pl_s(1:pfafunits,1:ndx))
        allocate        (vel_pl_s(1:pfafunits,1:ndx))
        allocate        (vel_pl_s_max(1:pfafunits,1:ndx))
        allocate        (y_pl_gw(1:pfafunits,1:ndx))
        allocate        (q_pl_gw(1:pfafunits,1:ndx))
        !allocate        (y_pl_ss(1:pfafunits,1:ndx))
        !allocate        (q_pl_ss(1:pfafunits,1:ndx))
        allocate        (qlat_s_old(1:pfafunits))
        allocate        (qlat_ss_old(1:pfafunits))
        allocate        (qlat_gw_old(1:pfafunits)) 
        
        ! *****MPI
        allocate        (npass(1:pssize+1))
        allocate        (nget(1:pssize+1))
        allocate        (passfrom(1:20000,1:pssize+1))
        allocate        (passtop(1:20000,1:pssize+1))
        allocate        (getfrom(1:20000,1:pssize+1))
        allocate        (getfromp(1:20000,1:pssize+1))
        ! ********


        return
end subroutine allocatenow

subroutine setzero()
        tt=1.
        !!!ttt=1
        dayval = 1.
        qlat_ch = 0.
        qlat_ch_ave = 0.
        krec =1

!### needed to inititialize your arrays
        do j = 1,pssize+1
                npass(j) = 0
                nget(j) =0
                do k = 1,100
                        passfrom(k,j) = 0
                        passtop(k,j) = 0
                        getfrom(k,j) = 0
                        getfromp(k,j) = 0
                enddo
        enddo
 !###
            
        do j = 1,pfafunits
                idout(j) = 0.
                idgauge(j) = 0.

                id(j) = 0.
                pfaf(j) = 0.
                downpfaf(j) = 0.
                nup(j) = 0.
                uppfaf(j,1) = 0.
                uppfaf(j,2) = 0.
                uppfaf(j,3) = 0.
                uppfaf(j,4) = 0.

                sin_ch(j)=0.
                Qr_ch(j) = 0.
                length_ch(j) = 0.
                slope_ch(j) = 0.
                width_ch(j) = 0.
                n_ch(j) = 0.
                depth_ch(j) = 0.
!!!                !qbar_ch(j) = 0.

                chv1(j) = 0.
!!!                chv(j) = 0.
!!!                sv(j) = 0.
!!!                gwv(j) = 0.
!!!                ssv(j) = 0.
!!!                soilv(j) = 0.
            
                cc1(j) = 0.        
                cc2(j) = 0.
                cc3(j) = 0.
                cc4(j) = 0.
       
                length_p(j) = 0.
                slope_p(j) = 0.
                !alp_pl_s(j) = 0.
                bet_pl_s(j) = 0.
                !alp_pl_gw(j) = 0.
!!!                alp_pl_ss(j) = 0.     
         
                qlat_s_old(j) = 0.     
!!!                qlat_ss_old(j) = 0.     
                qlat_gw_old(j) = 0.     
                qlat_ch_old(j) = 0.

                Kh_gw(j) = 0.     
!!!                Kh_ss(j) = 0.     
                ksurf(j) = 0.   
                LCval = 0.  
                ex_s(j) = 0.     
                ex_gw(j) = 0.     
!!!                ex_ss(j) = 0.     
!!!                etrate(j) = 0.     
!                prate_sur(j) = 0. 
!                prate_sub(j) = 0. 
                !Kh_gw(j) = 0.
!!!                runco(j) = 0.
               
!!!                do k = 1,10
!!!                    ptime(j,k)=0.
!!!                enddo    
!!!                cumF(j) = 0.  
                satmc(j) = 0.
                soilD(j) = 0.

                q_in(j) = 0.
                q_out(j) = 0.
                old_q_day(j)=0.
                
                do k = 1,ndx
                        old_q(j,k)=0.
                        old_q_ch(j,k)=0.
                        q_QA_ch(j,k)=0.
                        alp_pl_s(j,k) = 0. !scaling
                        alp_pl_gw(j,k) = 0. !scaling
                        y_pl_s(j,k)=0.
                        q_pl_s(j,k)=0.
                        vel_pl_s(j,k)=0. !vel of plane
                        vel_pl_s_max(j,k)=0. !vel of plane
                        y_pl_gw(j,k)=0.0
                        q_pl_gw(j,k)=0.0
!!!                        y_pl_ss(j,k)=0.0
!!!                        q_pl_ss(j,k)=0.
                enddo
        enddo        
                
        return
end subroutine setzero

subroutine deallocatenow()
        deallocate      (idout)
        deallocate      (idgauge)

        deallocate      (id)
        deallocate      (usgsid) !****************
        
        deallocate      (pfaf)
        deallocate      (downpfaf)
        deallocate      (nup)
        deallocate      (uppfaf)

        deallocate      (A)
        deallocate      (Aup)
        deallocate      (chv1)

!!!        deallocate      (chv)
!!!        deallocate      (sv)
!!!        deallocate      (gwv)
!!!        deallocate      (ssv)
!!!        deallocate      (soilv)
!!!        deallocate      (qbar_ch)
        deallocate      (depth_ch)
        
        deallocate      (ex_s)
        deallocate      (ex_gw)
!!!        deallocate      (ex_ss)
!!!        deallocate      (etrate)
!        deallocate      (prate_sur) 
!        deallocate      (prate_sub)      
!!!
!!!        deallocate      (cumF)
        deallocate      (satmc)
        deallocate      (soilD)
        !deallocate      (Kh_gw)
        deallocate      (Kh_gw)
!!!        deallocate      (Kh_ss)
        deallocate      (runco)
        deallocate      (ksurf)
        deallocate      (LCval)

        deallocate (pFlg1F)
        deallocate (pFlg1AGB)
        deallocate (pFlg1WU)
        deallocate (pFlg2F)
        deallocate (pFlg2AGB)
        deallocate (pFlg2WU)
        deallocate (pFlg3F)
        deallocate (pFlg3AGB)
        deallocate (pFlg3WU)
        
        deallocate (pMg1F)
        deallocate (pMg1AGB)
        deallocate (pMg1WU)
        deallocate (pMg2F)
        deallocate (pMg2AGB)
        deallocate (pMg2WU)
        deallocate (pMg3F)
        deallocate (pMg3AGB)
        deallocate (pMg3WU)
        
        deallocate (pSg1F)
        deallocate (pSg1AGB)
        deallocate (pSg1WU)
        deallocate (pSg2F)
        deallocate (pSg2AGB)
        deallocate (pSg2WU)
        deallocate (pSg3F)
        deallocate (pSg3AGB)
        deallocate (pSg3WU)
        
        deallocate      (sin_ch)
        deallocate      (Qr_ch)
        deallocate      (length_ch)
        deallocate      (slope_ch)
        deallocate      (width_ch)
        deallocate      (n_ch)
        deallocate      (cc1)
        deallocate      (cc2)
        deallocate      (cc3)
        deallocate      (cc4)

        deallocate      (q_out)
        deallocate      (q_in)
        deallocate      (q_in_old)
        deallocate      (q_out_old)
        deallocate      (old_q)
        deallocate      (old_q_day)

        deallocate      (old_q_ch)
        deallocate      (q_QA_ch)
        
        deallocate      (length_p)
        deallocate      (slope_p)
        deallocate      (bet_pl_s)
        deallocate      (alp_pl_s)
        deallocate      (k_all_seg)  !scaling surface
        deallocate      (khgw_seg)  !scaling sub-surface
        !*********************************
        deallocate      (usgsrunoff_sur) ! usgs syn runoff
        deallocate      (usgsrunoff_sub) ! usgs syn runoff
        deallocate      (hrrlinkusgs) !usgs syn runoff
        
!!!        deallocate      (alp_pl_ss)
        deallocate      (alp_pl_gw)

        deallocate      (y_pl_s)
        deallocate      (q_pl_s)
        deallocate      (vel_pl_s) !velocity of plane
        deallocate      (vel_pl_s_max) !velocity of plane
        deallocate      (y_pl_gw)
        deallocate      (q_pl_gw)
!!!        deallocate      (y_pl_ss)
!!!        deallocate      (q_pl_ss)
        
        ! ***** MPI *****
        deallocate      (npass)
        deallocate      (nget)
        deallocate      (passfrom)
        deallocate      (passtop)
        deallocate      (getfrom)
        deallocate      (getfromp)
        ! ***************
                
    return
end subroutine deallocatenow

end module variables
