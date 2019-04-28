#################################################################
# 1. Read the text file output from Cluster for all plane velocities
# 2. Calculate the average velocity P for each model unit, using a threshold ('per_thr')
# 3. Output the average velocity with HRR_ID
# By Yuanhao Zhao
################################################################

# rm(list = ls())
library(data.table)
require(foreign)
library(stats)
library(tools)

### INPUT and OUTPUT
#Input
# wdir <-  'C:\\Research\\Scale_SSS_OHRO\\Data\\HRR_plout_sur\\3200_sur'

unitpick <- 278588

vel_rat <- 0.5
check <- 9

# list all pattern
fpattern = c("p1.csv$","p2.csv$","p3.csv$","p4.csv$","p5.csv$",
             "p6.csv$","p7.csv$","p8.csv$","p9.csv$","p10.csv$")

#Output
# Vp_csv <- paste(wdir,'Vp_sur.csv',sep='\\')
Vp_csv <- 'Vp_sur.csv'

######################## Main #################################
# list all text files in working directory
# flist <- list.files(wdir, pattern = "plane_discharge_fps", recursive = TRUE)
flist <- list.files(pattern = "plane_discharge_fps", recursive = TRUE)
hrrtxtread <- as.data.frame(fread(flist[1]))

hrrid_top <- 1
hrrid_bot <- ncol(hrrtxtread)

# generate hrr id
hrrid = seq(hrrid_top,hrrid_bot)

pick_mu <- data.frame()
for (i in 1:length(flist)){
  print(flist[i])
  #file name of the discharge text file
  # dischargetxt <- paste(wdir,flist[i],sep = '\\')
  dischargetxt <- flist[i]
  
  #read discharge data
  hrrflow = as.data.frame(fread(dischargetxt))
  
  # average q and max q
  velpl_ave <- numeric()
  vepl_max <- numeric()
  
  # read in each column (each gauge)
  for (j in 1: dim(hrrflow)[2]){
    
    # print(j)
    
    flow_gg = hrrflow[12:dim(hrrflow)[1],j]

    
    if (check==99 && j == (unitpick-hrrid_top+1)){
      plot(velpl_set)
      print(velpl_set[length(velpl_set)])
    }
    
    vepl_max[j] <- max(flow_gg)
    velpl_ave[j] <- vel_rat*vepl_max[j]
    
  } # for each gauge
  
  # combine hrrid with average q
  velpl_ave_gg <- cbind(hrrid,velpl_ave)
  if (check == 99){
    pick_mu_temp <- velpl_ave_gg[which(velpl_ave_gg[,1] == unitpick),]
    pick_mu <- rbind(pick_mu,pick_mu_temp)
  }

  # set csv name
  outname <- file_path_sans_ext(flist[i])

  # write to csv
  outcsv <- paste(paste('max0p5',outname,sep='_'),'csv',sep='.')
  write.csv(velpl_ave_gg, file = outcsv,row.names=FALSE)
  
}# for each text file

# final average Vp table
Vp <- data.frame()

for (i in 1:length(fpattern)){
  # flist <- list.files(wdir, pattern = fpattern[i], recursive = TRUE)
  flist <- list.files( pattern = fpattern[i], recursive = TRUE)
  # flist <- list.files(wdir, pattern = glob2rx(fpattern[i]), recursive = TRUE)

  Vp_units <- data.frame()
  for (j in 1:length(flist)){
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

print('Done!')
