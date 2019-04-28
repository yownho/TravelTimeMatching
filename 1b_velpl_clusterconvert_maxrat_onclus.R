#################################################################
# 1. Read the text file output from Cluster for all plane velocities
# 2. Calculate the average velocity P for each model unit, using a threshold ('per_thr')
# 3. Output the average velocity with HRR_ID
# By Yuanhao Zhao
################################################################

rm(list = ls())
library(data.table)
require(foreign)
library(stats)
library(tools)

### INPUT and OUTPUT
#Input
# unitnumber <- c(0,40000, 80000, 120000, 160000, 200000, 240000,
#                 280000, 339635)
# 
# listnumber <- c('1-40K','40-80K','80-120K','120-160K', '160-200K',
#                 '200-240K', '240-280K', '280K')


vel_rat <- 0.5
maxrat <- 'max0p5'
dir.name <- getwd()
unitpick <- 278588

# vel_rat <- 0.1
check <- 9

#Output
# outdir <- 'C:\\Users\\Yuanhao\\Google Drive\\RschGeneral\\Scale_SSS_OHRO\\Analysis\\vel_topbot\\Vel_topbot_all_S_per20'


# for (k in 1:length(listnumber)){
# k <- 8
  # wdir <- paste(wdir_up,listnumber[k], sep='\\')

lastch <- "1-40K"
hrrid_top <- 1
hrrid_bot <- 40000
  
######################## Main #################################
  # list all text files in working directory
  flist = list.files(path = dir.name, pattern = "plane_ss_fps", recursive = TRUE)
  
  # generate hrr id
  hrrid = seq(hrrid_top,hrrid_bot)
  
  pick_mu <- data.frame()
  for (i in 1:length(flist)){
    print(flist[i])
    #file name of the discharge text file
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
    outcsv <- paste(paste(maxrat,lastch,outname,sep='_'),'csv',sep='.')
    write.csv(velpl_ave_gg, file = outcsv,row.names=FALSE)
    
  }# for each text file
  


