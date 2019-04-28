#####################
#
#
#####################

rm(list = ls())
library(data.table)
require(foreign)
library(stats)
library(tools)

### INPUT and OUTPUT
#Input
# wdir <-  'C:\\Research\\Scale_RC_aveVel_0316\\Analysis\\Q_topbot\\Q_topbot_all_d_max0p5II_0316' #cfs
# facctable <- 'C:\\Research\\Scale_RC_aveVel\\Data\\GIS\\BaseRas902\\facc_mima0225.dbf'
facctable <- 'facc_mima0225.dbf'
# hrr3table <- 'C:\\Research\\Scale_RC_aveVel_0316\\HRRSetup\\1\\HRRtxt\\HRR_Table3_OH07.dbf'
hrr3table <- 'HRR_Table3_OH07.dbf'

#Output
# Qaveall_csv = 'C:\\Research\\Scale_RC_aveVel_0316\\Analysis\\qch_d\\qch_d_ab_lin_max0p5II_0316.csv'
Qaveall_csv = 'qch_d_ab_lin_max0p5II_s.csv'

##### MAIN#####

#reading
hrr3 <- data.frame(read.dbf(hrr3table))
facc_ch <- data.frame(read.dbf(facctable))

facc <- cbind(facc_ch[,1],facc_ch[,4],facc_ch[,5])
colnames(facc) <- c('GC','faccmin','faccmax')

# get GC and hrrID
# hrr3mod <- hrr3[order(hrr3$GRID_CODE),]
hrr3mod <- hrr3[order(hrr3$GRID_CODE),]
hrr3hrr <- hrr3[order(hrr3$HRR_ID),]

#combine hrrid to facc table
facc_hrr <- data.frame(cbind(hrr3mod$HRR_ID,facc))
colnames(facc_hrr) <- c('hrr','GC','faccmin','faccmax')

# rank based on hrr id
facc_hrr <- facc_hrr[order(facc_hrr$hrr),]

# calculate facc min and max
facc_2nd <- facc_hrr$faccmin + (facc_hrr$faccmax-facc_hrr$faccmin)*2/10
facc_chHRR <- data.frame(cbind(facc_hrr$hrr,facc_2nd,facc_hrr$faccmax))
colnames(facc_chHRR) <- c('hrr', 'facc_2nd','faccmax')


# list  text files for 2nd section in working directory
# flist_2nd = list.files(wdir, pattern = "\\ch2.csv$", recursive = TRUE)
flist_2nd = list.files(pattern = "\\ch2.csv$", recursive = TRUE)

# list  text files for 10th section in working directory
# flist_10th = list.files(wdir, pattern = "\\ch10.csv$", recursive = TRUE)
flist_10th = list.files(pattern = "\\ch10.csv$", recursive = TRUE)

Qave2nd <- data.frame()
# read through the csv Qave for channel section 2
for (i in 1:length(flist_2nd)){
  # Qave_temp <- as.data.frame(fread(paste(wdir,flist_2nd[i],sep='\\')))
  Qave_temp <- as.data.frame(fread(flist_2nd[i]))
  Qave2nd <- rbind(Qave2nd,Qave_temp)
}
Qave2nd <- Qave2nd[order(Qave2nd$hrrid),]

Qave10th <- data.frame()
# read through the csv Qave for channel section 2
for (i in 1:length(flist_2nd)){
  # Qave_temp <- as.data.frame(fread(paste(wdir,flist_10th[i],sep='\\')))
  Qave_temp <- as.data.frame(fread(flist_10th[i]))
  Qave10th <- rbind(Qave10th,Qave_temp)
}
Qave10th <- Qave10th[order(Qave10th$hrrid),]

# Combine 2nd and 10th q
Qave_all <- cbind(Qave2nd,Qave10th[,2])
colnames(Qave_all) <- c('hrrid','Qave2nd','Qave10th')

Qab <- data.frame()
# loop to have a and b,
for (i in 1:nrow(Qave_all)){
  if(i %% 1000 ==0){
    print(i)
  }
  Qave_temp2 <- Qave_all[Qave_all$hrrid == i,]
  if (facc_chHRR[i,]$facc_2nd == facc_chHRR[i,]$faccmax|
      (facc_chHRR[i,]$faccmax-facc_chHRR[i,]$facc_2nd) < 0.0081){
    Qab[i,1] <- 0
    Qab[i,2] <- 0.55*(Qave_temp2$Qave2nd + Qave_temp2$Qave10th)*(0.3048^3) #cfs to cms
  }else if(Qave_temp2$Qave2nd > Qave_temp2$Qave10th){
    Qab[i,1] <- 0
    Qab[i,2] <- 0.55*(Qave_temp2$Qave2nd + Qave_temp2$Qave10th)*(0.3048^3) #cfs to cms
  }else{
    Qab[i,1] <- (Qave_temp2$Qave10th - Qave_temp2$Qave2nd)*(0.3048^3)/(facc_chHRR[i,]$faccmax - facc_chHRR[i,]$facc_2nd)
    Qab[i,2] <- Qave_temp2$Qave2nd*(0.3048^3) - facc_chHRR[i,]$facc_2nd/(facc_chHRR[i,]$faccmax - facc_chHRR[i,]$facc_2nd)*
      ((Qave_temp2$Qave10th - Qave_temp2$Qave2nd)*(0.3048^3))
  }
}

Qabout <- cbind(Qave_all$hrrid,Qab,Qave_all$Qave2nd*0.3048^3)

# Output CSV of a and b
write.csv(Qabout, file = Qaveall_csv,row.names=FALSE)
