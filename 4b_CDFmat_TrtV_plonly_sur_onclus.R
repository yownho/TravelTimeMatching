####******************
#
#
#By Yuanhao

rm(list = ls())

require(foreign)
require(Hmisc)
library(rootSolve)
library(data.table)

########## INPUT and OUTPUT ##########
# codedir <- "C:\\Users\\Yuanhao\\Google Drive\\RschGeneral\\Scale_SSS_OHRO\\Analysis\\kall_seg\\3200"

# folder of output for velocity on the plane from HRR model
# datadir <- "C:\\Research\\Scale_SSS_OHRO\\Data\\HRR_plout_sur\\3200_sur"
# f_Vp <- paste(datadir,'Vp_sur.csv',sep='\\')
f_Vp <- 'Vp_sur.csv'

# fTrtMS <- "C:\\Research\\Scale_SSS_OHRO\\Data\\GIS\\TT_Sur_match\\3200\\Tt3200_1stMS.dbf"
# fTrtMM <- "C:\\Research\\Scale_SSS_OHRO\\Data\\GIS\\TT_Sur_match\\3200\\Tt3200_1stMM.dbf"
fTrtMS <- "Tt3200_1stMS.dbf"
fTrtMM <- "Tt3200_1stMM.dbf"

# filehrr3 <- "C:\\Research\\Scale_RC_aveVel_0316\\HRRSetup\\3200\\GISworking2\\HRR_Table3_OH01.dbf" #HRR table
filehrr3 <- "HRR_Table3_OH01.dbf" #HRR table
# OUtput
kalltxtname <- paste("KallSeg_sur",".txt", sep="")

per_thr <- 0.01 #percentage of the threshold
vel_rat <- 1 #DO NOT change, vel_rat already used 
check <- 9 #99 if need check examples

countnum <- 100000

# cdf_x <- seq(10/100, 100/100, by <- 10/100) # plane and channel
cdf_x <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99) # plane and channel

#####################


alpbet <- function(x){
  c(F1 = (std^2)*x[1] + (std^2)*mean - mean^2 + mean^3,
    F2 = x[1] - mean*x[1] - mean*x[2])
}

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# inverse ecdf function
inv_ecdf <- function(f){ 
  x <- environment(f)$x 
  y <- environment(f)$y 
  approxfun(y, x) 
}

# TrtGIS <- read.dbf(fTrtGIS)
hrr <- as.data.frame(read.dbf(filehrr3))

# sort in hrr id and gridcode
hrrGC <- hrr[order(hrr$GRID_CODE),]
hrrHRR <- hrr[order(hrr$HRR_ID),]
hrrid <- hrrHRR$HRR_ID

# plane_n <- as.data.frame(read.dbf(planendbf))
# plane_slp <- as.data.frame(read.dbf(planeslpdbf))

# read mean,std,max, and min for plane 1sqkm, and rank in HRR id
TrtMS <- as.data.frame(read.dbf(fTrtMS))
TrtMSGC <- data.frame(matrix(1, nrow <- length(hrrid), ncol <- 5))
TrtMSGC[,1] <- seq(1,length(hrrid))
for (i in 1:length(hrrid)){
  if(is.element(i,TrtMS$GRIDCODE) == TRUE){
    TrtMSGC[i,] <- TrtMS[which(TrtMS$GRIDCODE == i),] 
  }
}
TrtMS <- cbind(hrrGC$HRR_ID,TrtMSGC)
TrtMS <- TrtMS[order(TrtMS$`hrrGC$HRR_ID`),]
colnames(TrtMS) <- c('hrrid','GRIDCODE','COUNT','AREA','MEAN','STD')

TrtMM <- as.data.frame(read.dbf(fTrtMM))
TrtMMGC <- data.frame(matrix(1, nrow <- length(hrrid), ncol <- 5))
TrtMMGC[,1] <- seq(1,length(hrrid))
for (i in 1:length(hrrid)){
  if(is.element(i,TrtMM$GRIDCODE) == TRUE){
    TrtMMGC[i,] <- TrtMM[which(TrtMM$GRIDCODE == i),] 
  }
}
TrtMM <- cbind(hrrGC$HRR_ID,TrtMMGC)
TrtMM <- TrtMM[order(TrtMM$`hrrGC$HRR_ID`),]
colnames(TrtMM) <- c('hrrid','GRIDCODE','COUNT','AREA','MIN','MAX')

#----- read vel on the plane, in ft/s
velplfinal <- fread(f_Vp)
colnames(velplfinal) <- c('hrrID','v1','v2','v3','v4','v5', 
                               'v6','v7','v8','v9','v10')


velpl_gc <- as.data.frame(cbind(hrrHRR$GRID_CODE, velplfinal))

# channel length
Lc_LFP <- hrrHRR$Lc_LFP_km #It's meters
Acat <- hrrHRR$A_sqkm
Lp <- Acat/(Lc_LFP)/2 #half plane, km
Lp_each <- Lp*1000/10 #meter

#Trt in Model
# print("Trt in Model")
ini_kall_seg <- data.frame(matrix(1, nrow <- length(hrrid), ncol <- 10))
kall_seg <- data.frame(matrix(1, nrow <- length(hrrid), ncol <- 10))
finkallseg <- data.frame(matrix(1, nrow <- length(hrrid), ncol <- 10))
kall_seg_opt <- data.frame(matrix(1, nrow <- length(hrrid), ncol <- 10))
df_RMSE <- data.frame(matrix(1, nrow <- length(hrrid), ncol <- 4))
gc_over500 <- numeric()

# for (p in 1:length(vel_rat)){
  p <- 1

# ---Start the loop to have the kall_seg

for (i in 1:length(hrrid)){
  # i <- 15
  if(i %% 100 == 0){
  print(i)
  }
  
  #vel of plane for select catch
  Vp_cat = velpl_gc[velpl_gc$hrrID == i,] #Velocity of plane
  Vp_ms = Vp_cat[,3:12]*0.3048 #ft/s to m/s
  
  Vp_ms_ave <- numeric()
  for (j in 1:length(Vp_ms)){
    if(j == 1){
      Vp_ms_ave[j] <- 0.5*Vp_ms[j]
    }else{
      Vp_ms_ave[j] <- 0.5*(Vp_ms[j] + Vp_ms[j-1])
    }
  }

  Vp_ms_ave <- as.numeric(Vp_ms_ave)
  
  #depth and vel of channel
  strslp <- hrrHRR$Slope_str[i]
  strslp <- 0.025661
  if(strslp<=0){
    strslp = 0.0001
  }
  # depth_ch <- (qch_sh*0.025/(qch_cat$wid_ch*strslp^0.5))^0.6
  # vel_ch <- 1/0.025*depth_ch^(5/3)*(strslp^0.5)
  
  Trt_HRR = data.frame(matrix(1, nrow = 10, ncol = 10))
  
  # vel_ch_rev <- rev(vel_ch)*vel_rat[p]
  Vp_ms_rev <- rev(Vp_ms)*vel_rat[p]
  
  
  for (m in 1:10){
    for (n in 1:10){
      if(m == 1){
        Trt_HRR[m,n] = Lp[i]*1000/10/Vp_ms_rev[m]*ini_kall_seg[i,m]*n/10
      }else{
        Trt_HRR[m,n] = Lp[i]*1000/10/Vp_ms_rev[m]*ini_kall_seg[i,m]*n/10+
          Trt_HRR[(m-1),10]
      }
    }
  }
  
  allTrtHRR <- as.numeric(unlist(Trt_HRR)) # plane only
  TrtHRR_cdf <- ecdf(allTrtHRR)
  
  #---Trt in equation
  mean <- TrtMS$MEAN[i]
  std <- TrtMS$STD[i]
  max <- TrtMM$MAX[i]
  min <- TrtMM$MIN[i]
  
  ###if max = min, just 1 pixel in the catchment
  if(max == min){
    print('small catchments, no needs to adjust roughness')
    next
  }
  
  mean <- (mean-min+0.0001)/(max-min+0.0001)
  std <- std/(max-min+0.0001)
  
  #solve the beta distribution with mean and std
  ss <- multiroot(f <- alpbet, start <- c(0.1, 0.1))
  alp <- ss$root[1]
  beta <- ss$root[2]
  if (alp <=0.05 || beta<=0.05 || std == 0){
    alp <- 2
    beta <- 5
    print(c(i,'negative value in alp and beta'))
  }
  
  #generate cdf for the alpha and beta 
  Trt_P <- rbeta(countnum, alp, beta)
  TrtP <- Trt_P*(max-min)
  TrtP_cdf <- ecdf(TrtP)
  
  # cdf_x <- seq(0.1, 1.0, by <- 0.1) #Only plane
  # cdf_x <- seq(5/100, 95/100, by <- 10/100) # plane and channel
  p_extract <- inv_ecdf(TrtP_cdf)
  HRR_extract <- inv_ecdf(TrtHRR_cdf)
  
  Trt_segP <- numeric()
  Trt_segHRR <- numeric()
  for (k in 1:length(cdf_x)){
    Trt_segP[k] <- p_extract(cdf_x[k])
    Trt_segHRR[k] <- HRR_extract(cdf_x[k])
  }
  
  if(is.na(Trt_segHRR[1] == TRUE)){
    Trt_segHRR[1] <- 0.5*Trt_segHRR[2]
  }

  dlt_trt_ini <- Trt_segHRR - Trt_segP
  
  V_star <- numeric()
  V_rat <- numeric()
  for (o in 1:10){
      if (is.na(Trt_segP[o]==T)){
        Trt_segP[o] <- 0 #Trt_segP[o+1]*0.1
      }
      V_star[o] <- Lp_each[i]/(Lp_each[i]/Vp_ms_rev[o]- (Trt_segHRR[o]-Trt_segP[o]))
      if(V_star[o]<0 | is.infinite(as.numeric(V_star[o])) == TRUE){
        V_star[o] <- Trt_segP[o]/Lp_each[i]
      }
      V_rat[o] <- Vp_ms_rev[o]/V_star[o]
  }
  Trt_HRR_loop <- Trt_HRR
  if(check==99){
    # Ecdf(TrtP, lty = 6, lwd = 2, col = 'blue', subtitles = FALSE)
    # Ecdf(allTrtHRR, lty = 6, add = TRUE, lwd = 3, col = 'green', subtitles = FALSE)
    Ecdf(allTrtHRR, lty = 6, lwd = 3, xlab='Travel time, sec', ylab='CDF', col = 'green', 
         label.curves=TRUE,subtitles = FALSE)
    Ecdf(TrtP, lty = 6, add = TRUE,lwd = 2, col = 'blue', 
         label.curves=TRUE,subtitles = FALSE)
  }
  
  Vp_loop <- as.numeric(V_star) # V-start in the loop
  Vrat_loop <- as.numeric(V_rat)
  kseg_loop <- as.numeric(ini_kall_seg[i,])*as.numeric(Vrat_loop)
  rmse_orig <- rmse(Trt_segHRR - Trt_segP)
  kseg_loop_ini <- kseg_loop
  
  #-----get the first time kall_seg
  for(l in 1:10){
    
    old_ini_kall_seg <- ini_kall_seg[i,l]
    ini_kall_seg[i,l] = kseg_loop[l]
    
    # if the initial value is negative, calculate ratio using CDF in beta
    if (ini_kall_seg[i,l] < 0){
      if(l == 1){
        ini_kall_seg[i,l] = Vp_ms_rev[l]/((Lp[i]*1000/10)/Trt_segP[l])*old_ini_kall_seg
      }else{
        ini_kall_seg[i,l] = Vp_ms_rev[l]/((Lp[i]*1000/10)/(Trt_segP[l]-Trt_segP[l-1]))*old_ini_kall_seg
      }
    }
    
  
    # travel time on plane
    for (m in 1:10){
      for (n in 1:10){
        if(m == 1){
          Trt_HRR_loop[m,n] = Lp[i]*1000/10/Vp_ms_rev[m]*ini_kall_seg[i,m]*n/10
        }else{
          Trt_HRR_loop[m,n] = Lp[i]*1000/10/Vp_ms_rev[m]*ini_kall_seg[i,m]*n/10+
            Trt_HRR_loop[(m-1),10]
        }
      }
    }
    
    allTrtHRR_loop <- unlist(Trt_HRR_loop) # plane and channel
    TrtHRR_cdf_loop <- ecdf(allTrtHRR_loop)
    
    HRR_extract_loop <- inv_ecdf(TrtHRR_cdf_loop)
    
    Trt_segHRR_loop <- numeric()
    for (j in 1:length(cdf_x)){
      Trt_segP[j] <- p_extract(cdf_x[j])
      Trt_segHRR_loop[j] <- HRR_extract_loop(cdf_x[j])
    }
    
    if(is.na(Trt_segHRR_loop[1])==TRUE){
      Trt_segHRR_loop[1] <- 0.5*Trt_segHRR_loop[2]
    }
    dlt_Trt_loop <-  Trt_segHRR_loop - Trt_segP

    for (j in l:10){
      T_HRR_out <- Lp_each[i]/Vp_ms_rev[j]
      T_diff_loop <- Trt_segHRR_loop[j]-Trt_segP[j]
      Vp_loop[j] <- Lp_each[i]/(Lp_each[i]/Vp_ms_rev[j]- (Trt_segHRR_loop[j]-Trt_segP[j]))
      if(Vp_loop[j]<0){
        if (j ==1){
          Vp_loop[j] <- Trt_segP[j]/Lp_each[i]
        }else{
          Vp_loop[j] <- (Trt_segP[j]-Trt_segP[j-1])/Lp_each[i]
        }
      }
      Vrat_loop[j] <- Vp_ms_rev[j]/Vp_loop[j]
    }
    
    kseg_loop <- ini_kall_seg[i,]*Vrat_loop
    
    checkmat1 <- cbind(Trt_segP, Trt_segHRR_loop, dlt_Trt_loop, as.numeric(Vp_ms_rev), as.numeric(Vp_loop), as.numeric(Vrat_loop),
                       as.numeric(ini_kall_seg[i,]), as.numeric(kseg_loop))
    colnames(checkmat1) <- c('Trt_segP','Trt_segHRR_loop','dlt_Trt_loop','Vp_hrrout','Vp_loop','Vrat_loop','ini_kall_seg',
                             'kseg_loop')
    # print(l)
    # print(checkmat1)
    
    if (check == 99){
      Ecdf(TrtP, lty = 6, lwd = 2, col = 'blue', xlab='Travel time, sec', ylab='CDF',
           label.curves=TRUE,subtitles = FALSE)
      Ecdf(allTrtHRR_loop, lty = 6, add = TRUE, lwd = 3, xlab='Travel time, sec', ylab='CDF',
           label.curves=TRUE,col = 'green', subtitles = FALSE)
      # print(checkmat1)
    }
  }
  
  
  ini_kall_segfirst <- ini_kall_seg[i,]
  
  # Get rmse for the first try
  rmse_1 <-rmse(Trt_segHRR_loop - Trt_segP) 
  rmse_th <- per_thr*max(TrtP)  
  
  #---run the loop to get the optimal Kall_seg
  kall_seg_opt[i,] <- ini_kall_seg[i,]
  iter <- 1
  rmse_vec <- numeric()
  Trt_HRR_iter <- Trt_HRR_loop
  Vp_iter <- Vp_loop
  Vrat_iter <- Vrat_loop
  
  repeat{
    # print(iter)
    # print(kall_seg[i,])
    rmse_vec[1] <-  rmse_1
    if (rmse_vec[1] < rmse_th){
      finkallseg[i,] = ini_kall_segfirst
      print(paste(i, 'reach threshold at beginning'))
      break
    }
    
    if(iter %% 10 == 0){
      k = 10
    }else{
      k <- iter %% 10
    }
      
    kall_seg_opt[i,k]= kseg_loop[k]
    
    if (kall_seg_opt[i,k] < 0){
      if(k == 1){
        kall_seg_opt[i,k] = Vp_ms_rev[k]/((Lp[i]*1000/10)/Trt_segP[k])*kseg_loop[k]
      }else{
        kall_seg_opt[i,k] = Vp_ms_rev[k]/((Lp[i]*1000/10)/(Trt_segP[k]-Trt_segP[k-1]))*kseg_loop[k]
      }
    }
    
    for (m in 1:10){
      for (n in 1:10){
        if(m == 1){
          Trt_HRR_iter[m,n] = Lp[i]*1000/10/Vp_ms_rev[m]*kall_seg_opt[i,m]*n/10
        }else{
          Trt_HRR_iter[m,n] = Lp[i]*1000/10/Vp_ms_rev[m]*kall_seg_opt[i,m]*n/10+
            Trt_HRR_iter[(m-1),10]
        }
      }
    }
    
    
      
    allTrtHRR_iter <- unlist(Trt_HRR_iter) # plane and channel
    TrtHRR_cdf_iter <- ecdf(allTrtHRR_iter)
    
    HRR_extract_iter <- inv_ecdf(TrtHRR_cdf_iter)
    
    Trt_segHRR_iter <- numeric()
    for (j in 1:length(cdf_x)){
      Trt_segP[j] <- p_extract(cdf_x[j])
      Trt_segHRR_iter[j] <- HRR_extract_iter(cdf_x[j])
    }
    dlt_Trt_iter <-  Trt_segHRR_iter - Trt_segP
    
    for (j in k:10){
      T_HRR_out <- Lp_each[i]/Vp_ms_rev[j]
      T_diff_iter <- Trt_segHRR_iter[j]-Trt_segP[j]
      Vp_iter[j] <- Lp_each[i]/(T_HRR_out - T_diff_iter)
      # Vrat_iter[j] <- Vp_ms_rev[j]/Vp_iter[j]
      Vrat_iter[j] <- Vp_ms_rev[j]/Vp_iter[j]
      if (Vp_iter[j] < 0){
        T_diff_iter <- Trt_segP[j]-Trt_segHRR_iter[j]
        Vp_iter[j] <- Lp_each[i]/(T_HRR_out - T_diff_iter)
        Vrat_iter[j] <- Vp_ms_rev[j]/Vp_iter[j]
      }
    }
      
    
    # finkallseg[i,] <- kall_seg[i,]
    kseg_iter <- kall_seg_opt[i,]*Vrat_iter
    # kall_seg_opt[i,] <- kall_seg[i,]*Vrat_iter
    
    checkmat1 <- cbind(Trt_segP, Trt_segHRR_iter, dlt_Trt_iter, as.numeric(Vp_iter), as.numeric(Vrat_iter),
                       as.numeric(kall_seg_opt[i,]), as.numeric(kseg_iter))
    colnames(checkmat1) <- c('Trt_segP','Trt_segHRR_iter','dlt_Trt_iter','Vp_iter','Vrat_iter','kall_seg_opt',
                             'kseg_iter')

    
    rmse_vec[iter] <- rmse(Trt_segHRR_iter - Trt_segP)
    
    # Find the threshold and break from the loop
    if (is.na(rmse_vec[iter]) == 'TRUE'){
      finkallseg[i,] = ini_kall_segfirst
      print(paste(i, 'does not converge, iteration is', iter))
      break
    }else if(iter > 0 && rmse_vec[iter] < rmse_th){
      print(paste(i,', threshold reached, ', iter))
      finkallseg[i,] = kall_seg_opt[i,]
      df_RMSE[i,] <- c(i, iter, rmse_orig, rmse_vec[iter])
      break
    }else if(iter > 500){
      finkallseg[i,] = kall_seg_opt[i,]
      df_RMSE[i,] <- c(i, iter, rmse_orig, rmse_vec[iter])
      print(paste(i,'Over 500 iterations'))
      
      label(TrtP) <- paste(i,', Travel time')
      # Ecdf(TrtP, lty = 6, lwd = 2, col = 'blue', subtitles = T)
      # Ecdf(allTrtHRR_iter, lty = 6, add = TRUE, lwd = 3, col = 'green', subtitles = T)
      print(paste(i, ', iter=', iter, ', rmse=',rmse_vec[iter]))
      
      gc_over500 <- rbind(gc_over500,i)
      
      break
    }else if(iter>50 && rmse_vec[iter]==rmse_vec[iter-1]){
      finkallseg[i,] = kall_seg_opt[i,]
      print(paste(i,'rmse not changing'))
      
      # Ecdf(TrtP, lty = 6, lwd = 2, col = 'blue', subtitles = T)
      # Ecdf(allTrtHRR_iter, lty = 6, add = TRUE, lwd = 3, col = 'green', subtitles = T)
      print(paste(i, ', iter=', iter, ', rmse=',rmse_vec[iter]))
      
      break
    } 
    
    iter = iter + 1
    # if(check == 99){
    #   print(iter)
    #   print(checkmat1)
    #   Ecdf(TrtP, lty = 6, lwd = 2, col = 'blue', subtitles = FALSE)
    #   Ecdf(allTrtHRR_iter, lty = 6, add = TRUE, lwd = 3, col = 'green', subtitles = iter)
    # }
  
  } #for repeat


} #for hrr id



# # # # #---combine the hrrID to the list, output the text file
kall_table <- cbind(hrrid, finkallseg)
outKall_table <- kall_table[order(hrrid),]
# #---output
# outputfile <- paste(codedir, kalltxtname,sep = "\\")
outputfile <- kalltxtname
write.table(outKall_table, file = outputfile, sep="\t", row.names = FALSE,col.names = FALSE )
write.table(outKall_table, file = "KallSeg_sur.csv", sep=",", row.names = FALSE,col.names = FALSE )

  