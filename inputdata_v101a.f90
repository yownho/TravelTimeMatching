module inputdata
use variables
use mpi

contains
!***Read the input.txt for those parameters
subroutine inputdata1()
        character(120):: fname2
        fname2 = 'input_v1.txt'
        open(2,file=fname2)
        read(2,*) pfafunits
        read(2,*) usgsggnum
        read(2,*) usgsnd
        read(2,*) ndx
        read(2,*) ndt
        read(2,*) dtis
        read(2,*) iyear
        read(2,*) imax
        read(2,*) eps
        
        read(2,*) Khgw_all !horizontal K for qgw
        read(2,*) k_all !surface roughness for qs
        read(2,*) n_ch_all !channel roughness (minimal impacts)

        read(2,*) setfsub_rate !used to set baseflow (x mm/day); set to 0.0 for routing experiments
        read(2,*) setfsurf_rate !used to set baseflow from surface runoff (x mm/day); set to 0.0 for routing experiments     
           
        read(2,*) Khgw_min !0.1
        read(2,*) Khgw_max !1.0
        read(2,*) k_min !0.1
        read(2,*) k_max !100
        read(2,*) n_ch_min !0.02
        read(2,*) n_ch_max !10

        read(2,*) Lch_min_slope !0.025661
        read(2,*) Lch_max_slope !0.025661
        read(2,*) Lp_min_slope !0.086578
        read(2,*) Lp_max_slope !0.086578
        
        read(2,*) dataMethod !for use in channel setup code
        !width_ch(j) = (a_w_ch*(Aup(j)**b_w_ch))/0.3048 !m to ft
        !Qr_ch(j) = (a_Q_ch*(Aup(j)**b_Q_ch))/(0.3048**3) !cms to cfs
        read(2,*) a_w_ch
        read(2,*) b_w_ch
        read(2,*) a_Q_ch
        read(2,*) b_Q_ch
        
        read(2,*) scaleFac
        read(2,*) sf_ss
   
        close(2)        

        return
end subroutine inputdata1


!***read planes and channels data
subroutine inputdata2()
        character(120):: fname3, fname4, fname6, fname7, fname8
        real    Lch_min, setfsub, setfsurf, qperdx, qtop

        fname3 = 'planes.txt'
        fname4 = 'channels.txt'
        fname6 = 'KallSeg_sur.csv'    
        fname7 = 'KgwSeg_cdf_sub.csv'
!        fname8 = 'hrrlinkusgs.csv'      
         
        open(3,file=fname3)    
        open(4,file=fname4)  
        open(6,file=fname6) 
        open(7, file = fname7)
!        open(8,file=fname8)
         
    !load the plane data
    do j=1,pfafunits
            read(3,*) dval,dval,length_p(j),slope_p(j),ksurf(j),Kh_gw(j),satmc(j),soilD(j),LCval(j)
            !in the above, ksat is now called Kh_gw
                               
            !Added the channel read here to get areas    
            read(4,*) id(j),pfaf(j),downpfaf(j),nup(j),uppfaf(j,1),uppfaf(j,2),uppfaf(j,3), uppfaf(j,4),&
                A(j),Aup(j),length_ch(j),slope_ch(j),n_ch(j),width_ch(j),Qr_ch(j)
            
                
            !---read k_all for each segment, YZ
            read(6,*) id(j), k_all_seg(j,10), k_all_seg(j,9),k_all_seg(j,8),k_all_seg(j,7),k_all_seg(j,6),k_all_seg(j,5),&
                k_all_seg(j,4),k_all_seg(j,3),k_all_seg(j,2),k_all_seg(j,1)
            !write(*,*) 'read k_all_seg'
            
            !################################
            !read kseg for ground water
            read(7,*) id(j), khgw_seg(j,10), khgw_seg(j,9),khgw_seg(j,8),khgw_seg(j,7),khgw_seg(j,6),khgw_seg(j,5),&
                khgw_seg(j,4),khgw_seg(j,3),khgw_seg(j,2),khgw_seg(j,1)
            !################################
            
            !*************************************
!            read(8,*) hrrlinkusgs(j,1), hrrlinkusgs(j,2),hrrlinkusgs(j,3)
!            write(*,*) hrrlinkusgs(j,1),hrrlinkusgs(j,3)

            !**********Check plane length
            length_p(j) = (A(j)/2)*(1000.**2)/length_ch(j)   
                                          
            if(Kh_gw(j).lt.Khgw_min) Kh_gw(j)=Khgw_min !cm/hr
            if(Kh_gw(j).gt.Khgw_max) Kh_gw(j)=Khgw_max !cm/hr
            
            if(ksurf(j).lt.k_min) ksurf(j)=k_min 
            if(ksurf(j).gt.k_max) ksurf(j)=k_max 
            
            if(slope_p(j).lt.Lp_min_slope) slope_p(j)=Lp_min_slope 
            if(slope_p(j).gt.Lp_max_slope) slope_p(j)=Lp_max_slope

            !adjust all values uniformly
            if(Khgw_all.ne.1.0) Kh_gw(j)=Khgw_all*Kh_gw(j) 
            if(k_all.ne.1.0) ksurf(j)=k_all*ksurf(j)

            !convert Units
            length_p(j) = length_p(j)/0.3048 !m to ft
            Kh_gw(j) = (Kh_gw(j)/3600/2.54/12.) !cm/hr to ft/s
            !ksurf(j) = ksurf(j) !no units

            !surface runoff
            bet_pl_s(j) = (3./5.)
            !write(*,*) 'before change alp_s'
            !alp_pl_s(j) =((ksurf(j)/1.49)/(slope_p(j)**0.5))**bet_pl_s(j)
            !-----loop to change velocity, scale, YZ
            scaleFac = 1.0
            do k = 1,ndx
                 k_all_seg(j,k) = 1.0
!                if(k .EQ. 10) then
!                    alp_pl_s(j,k) = (ksurf(j)/1.49*(k_all_seg(j,k)*(scaleFac/5*k))/(slope_p(j)**0.5))**bet_pl_s(j)
!                else
                    alp_pl_s(j,k) = (ksurf(j)/1.49*(k_all_seg(j,k)*scaleFac)/(slope_p(j)**0.5))**bet_pl_s(j)
                    
!                endif
            enddo
            
            !#######################################
            !groundwater runoff (groundwater flow)
            !bet_pl_gw(j) = 1; linear
            !assume porosity = 0.4
            !satmc(j) = 0.4
!            alp_pl_gw(j)=satmc(j)/((Kh_gw(j))*slope_p(j))  
            sf_ss = 1.0
            do k=1,ndx 
                khgw_seg(j,k) = 1.0
                alp_pl_gw(j,k)=satmc(j)/((Kh_gw(j)/khgw_seg(j,k)/sf_ss)*slope_p(j)) 
                !if(j.eq.10 .AND. k.eq.10) write(*,*) khgw_seg(j,k),sf_ss
            enddo
            !########################################             
            
            
            setfsub=(setfsub_rate/10./2.54/12./3600./24.)
            setfsurf=(setfsurf_rate/10./2.54/12./3600./24.)
                        
            qperdx=(A(j)*1000**2/0.3048**2)*setfsub/ndx
            qtop=(Aup(j)-A(j))*(1000**2/0.3048**2)*setfsub
            
            do k = 1,ndx                    
                    !set GW soil moisture/flow
                    y_pl_gw(j,k)=alp_pl_gw(j,k)*(length_p(j)/ndx)*setfsub*k
                    q_pl_gw(j,k)=y_pl_gw(j,k)/alp_pl_gw(j,k)

                    !set surface runoff storage/moisture
                    y_pl_s(j,k)=alp_pl_s(j,k)*(length_p(j)/ndx)*setfsurf*k
                    q_pl_s(j,k)=(y_pl_s(j,k)/alp_pl_s(j,k))**(1/bet_pl_s(j))
                                        
                    !setup initial discharge in all channels
                    old_q(j,k)=qtop+qperdx*k
                    old_q_ch(j,k)=qtop+qperdx*k !ft3/s
            enddo
                        
            !channel setting
            sin_ch(j) = 1 !assumed for all
            if(n_ch(j).lt.n_ch_min) n_ch(j)=n_ch_min
            if(n_ch(j).gt.n_ch_max) n_ch(j)=n_ch_max
                        
            if(slope_ch(j).lt.Lch_min_slope) slope_ch(j)=Lch_min_slope
            if(slope_ch(j).gt.Lch_max_slope) slope_ch(j)=Lch_max_slope

            if(n_ch_all.ne.1.0) n_ch(j)=n_ch_all*n_ch(j)
           
            length_ch(j) = length_ch(j)/0.3048 !m to ft

            if(dataMethod.eq.1) then
                width_ch(j) = (a_w_ch*(Aup(j)**b_w_ch))/0.3048 !m to ft
                Qr_ch(j) = (a_Q_ch*(Aup(j)**b_Q_ch))/(0.3048**3) !cms to cfs
!                if(width_ch(j).lt.10.) width_ch(j) = 10. !ft
!                if(Qr_ch(j).lt.1.0) Qr_ch(j) = 1.0 !cfs 
            else
                width_ch(j) = width_ch(j)/0.3048 !m to ft
                Qr_ch(j) = Qr_ch(j)/(0.3048**3) !cms to cfs
                
!                if(width_ch(j).lt.10.) width_ch(j) = 10. !ft
!                if(Qr_ch(j).lt.1.0) Qr_ch(j) = 0.1 !cfs
            endif

            !determine MC constant parameter method coefficients
            C1 = (1.486*slope_ch(j)**0.5)/n_ch(j)
            y = (Qr_ch(j)/(C1*width_ch(j)))**0.6
            Ax = width_ch(j)*y
            celert = (5./3.)*Qr_ch(j)/Ax
            sreach = (length_ch(j)*sin_ch(j)/ndx)
            chv1(j) = width_ch(j)*C1 !used to solve for depth in MC model
            tv = dtis/sreach
            c = celert*tv
            d = Qr_ch(j)/width_ch(j)/(slope_ch(j)*celert*sreach)
            cdenom = 1 + c + d
            cc1(j) = (1 + c - d)/cdenom
            cc2(j) = (-1 + c + d)/cdenom
            cc3(j) = (1 - c + d)/cdenom
            cc4(j) = 2.*celert/cdenom
            
    enddo
    close(3)
    close(4)
    close(6)
    close(7)
!    close(8)
       
        return
end subroutine inputdata2

subroutine inputdata3()
        character(120):: fname3,fname5,fname7,fmt
        integer dvalread,p,q
        
        fname3 = 'hrrlinkusgs.txt'
        open(3,file=fname3)
        do j = 1,pfafunits
            read(3,*)hrrlinkusgs(j,1), hrrlinkusgs(j,2),hrrlinkusgs(j,3)
            !write(*,*) hrrlinkusgs(j,1), hrrlinkusgs(j,2),hrrlinkusgs(j,3)
        enddo
        close(3)
        
        fname5 = 'USGS_hourdata_inphr_sur_nath.txt'
        open(5,file=fname5) 
        do k = 1, usgsggnum
            read(5,*) usgsid(k),(usgsrunoff_sur(k,p),p=1,ndt)
            !write(*,*) usgsid(k)
            !if (k .eq. 10) write(*,*)usgsid(k),(usgsrunoff_sur(k,1:10))
        enddo
        close(5)
        
        fname7 = 'USGS_hourdata_inphr_sub_nath.txt'
        open(7,file=fname7) 
        do k = 1, usgsggnum
            read(7,*) usgsid(k),(usgsrunoff_sub(k,p),p=1,ndt)
            !write(*,*) usgsid(k)
            !if (k .eq. 10) write(*,*)usgsid(k),(usgsrunoff_sub(k,1:10))
        enddo
        close(7)

        return
end subroutine inputdata3

end module inputdata
