#################################################################
# 1. Read the text file output from Cluster for section2
# and section 10 on the plane for Q ch.
# 2. Calculate the average Q for each model unit for section 2 
# and section 10, using a threshold ('per_thr')
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
unitnumber <- c(0,40000, 80000, 120000, 160000, 200000, 240000,
                 280000, 339635)

listnumber <- c('1-40K','40-80K','80-120K','120-160K', '160-200K',
                '200-240K', '240-280K', '280K')

wdir_up <- 'D:\\Data\\Trt_vel_ave_ss_OHRO\\Qch_all'
# wdir_up <- 'D:\\Data\\Trt_vel_ave\\Qch_all_s'

vel_rat <- 0.5
txtstart <- 'max0p5'

# output
outdir <- 'C:\\Users\\Yuanhao\\Google Drive\\RschGeneral\\Scale_SSS_OHRO\\Analysis\\vel_topbot\\Q_topbot_all_SS_nat'

######################## Main #################################

for (k in 1:length(listnumber)){
  wdir <- paste(wdir_up,listnumber[k], sep='\\')
  lastch <- listnumber[k]
  hrrid_top <- unitnumber[k]+1
  hrrid_bot <- unitnumber[k+1]
  
  # list all text files in working directory
  flist = list.files(wdir, pattern = "\\.txt$", recursive = TRUE)
  
  # generate hrr id
  hrrid = seq(hrrid_top,hrrid_bot)
  
  for (i in 1:length(flist)){
    print(flist[i])
    #file name of the discharge text file
    dischargetxt <- paste(wdir,flist[i],sep = '\\')
    
    #read discharge data
    hrrflow = as.data.frame(fread(dischargetxt))
    
    # average q and max q
    Qave <- numeric()
    Qmax <- numeric()
    
    # read in each column (each gauge)
    for (j in 1: dim(hrrflow)[2]){
      
      # print(j)
      
      flow_gg = hrrflow[12:dim(hrrflow)[1],j]
      
      Qmax[j] <- max(flow_gg)
      Qave[j] <- vel_rat*Qmax[j]
      
    } # for each gauge
    
    # combine hrrid with average q
    Qave_gg <- cbind(hrrid,Qave)
    # set csv name
    outname <- file_path_sans_ext(flist[i])
    # write to csv
    outcsv <- paste(paste(outdir,paste(txtstart,lastch,outname,sep='_'),sep='\\'),'csv',sep='.')
    write.csv(Qave_gg, file = outcsv,row.names=FALSE)
    
  }# for each text file
  
}# for hrr list

