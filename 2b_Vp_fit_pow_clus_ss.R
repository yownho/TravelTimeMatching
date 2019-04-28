############################
#
#
#
############################

rm(list = ls())
library(data.table)
require(foreign)
library(stats)
library(tools)

### INPUT and OUTPUT
#Input
# wdir <-  'C:\\Research\\Scale_RC_aveVel_0316\\Analysis\\vel_topbot\\Vel_topbot_all_d_max0p5III_0316'
# areafile <- 'D:\\Research\\scale_RC\\Analysis\\QoverA\\qchANDvpl\\velplsame_slpN\\velpl_area.csv' #sqkm
# vellentable <- 'D:\\Research\\scale_RC\\Analysis\\QoverA\\qchANDvpl\\velplsame_slpN\\velpl_len.csv' #km
# hrr3table <- 'C:\\Research\\Scale_RC_aveVel_0316\\HRRSetup\\1\\HRRtxt\\HRR_Table3_OH07.dbf'
hrr3table <- 'HRR_Table3_OH07.dbf'

#Output
Vpall_csv = 'vp_d_pow_max0p5_ss.csv'
Vp_csv = 'vp_d_max0p5_ss.csv'

##### MAIN#####

# read  area for each plane, half
# area_pl <- data.frame(fread(areafile)) #area of plane, sqkm
# lenpl <- data.frame(fread(vellentable)) #plane length, km
hrr3 <- data.frame(read.dbf(hrr3table))
pl_area <- hrr3$A_sqkm

# get GC and hrrID
hrr3mod <- hrr3[order(hrr3$GRID_CODE),]
hrr3hrr <- hrr3[order(hrr3$HRR_ID),]

# list all pattern
fpattern = c("\\p1.csv$","\\p2.csv$","\\p3.csv$","\\p4.csv$","\\p5.csv$",
            "\\p6.csv$","\\p7.csv$","\\p8.csv$","\\p9.csv$","\\p10.csv$")

Vp <- data.frame()
for (i in 1:length(fpattern)){
  # flist <- list.files(wdir, pattern = fpattern[i], recursive = TRUE)
  flist <- list.files(pattern = fpattern[i], recursive = TRUE)
  # flist <- list.files(wdir, pattern = glob2rx(fpattern[i]), recursive = TRUE)
  
  Vp_units <- data.frame()
  for (j in 1:length(flist)){
    # Vp_temp <- as.data.frame(fread(paste(wdir,flist[j],sep='\\')))
    Vp_temp <- as.data.frame(fread(flist[j]))
    Vp_units <- rbind(Vp_units,Vp_temp) 
    Vp_units <- Vp_units[order(Vp_units$hrrid),]
  } # that section
  
  if (i == 1){
    Vp <- Vp_units
  }else{
    Vp <- cbind(Vp,Vp_units$velpl_ave)
  }
  
} #all csv files

# rename the column from section1 to section 10
colnames(Vp) <- c('hrrID','v1','v2','v3','v4','v5', 
                  'v6','v7','v8','v9','v10')
#output Vp
write.csv(Vp, file = Vp_csv,row.names=FALSE)

Vp_ab <- data.frame()
# loop for alp and bet in the power function of statistical fit
for (i in 1:dim(Vp)[1]){
  if(i %% 1000 ==0){
    print(i)
  }
  
  Vp_ab[i,1] <- Vp[i,]$hrrID
  
  # velocity for each section
  Vp_temp <- Vp[i,2:11]*0.3048 #fps to m/s
  
  # plane length for each section
  lenseq <- seq(0.1,1,0.1)
  lenseq_km <- hrr3hrr$Lp_km[i]*lenseq
  # len_pl <- lenpl[i,2:11] #km
  
  df_vpfit <- data.frame(cbind(as.numeric(lenseq_km),as.numeric(Vp_temp)))
  
  colnames(df_vpfit) <- c('Lp','velpl')
  
  df_vpfit_log <- log(df_vpfit) #take log
  
  fit <- lm(velpl ~ Lp, data = df_vpfit_log) #log(vel)=log(alp)+bet*log(Lp)
  aandb <- coefficients(fit)
  Vp_ab[i,2] <- exp(aandb[1])
  Vp_ab[i,3] <- aandb[2]
  Vp_ab[i,4] <- summary(fit)$r.squared
}

# Output CSV of a and b
write.csv(Vp_ab, file = Vpall_csv,row.names=FALSE)
