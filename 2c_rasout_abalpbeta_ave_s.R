###########################
# read a and b for channel q and area
# read alp and beta for vel on the plane
# Output 4 rasters, a, b, alp, and beta
# By Yuanhao Zhao
###########################

library(data.table)
require(foreign)
library(stats)
# library(raster)
# library(rgdal)
# library(rgeos)
# library(maptools)
require(shapefiles)
require(sp)

##### INPUT and OUTPUT
# input
hrr3table <- 'C:\\Research\\Scale_RC_aveVel_0316\\HRRSetup\\1\\GISworking\\HRR_Table3_OH07.dbf'
ohiocatch_1 <- 'C:\\Research\\Scale_RC_aveVel_0316\\HRRSetup\\1\\GISworking\\Catchments.shp'
# strRas <- 'C:\\Research\\Scale_RC_aveVel_0316\\HRRSetup\\1\\GISworking\\strgrid'
# faccRas <- 'C:\\Research\\NASA_Decomp\\Ohio_GIS_basic\\faccm_ohio'

qch_outfitfile <- 'C:\\Users\\Yuanhao\\Google Drive\\RschGeneral\\Scale_SSS_OHRO\\Analysis\\vel_topbot\\sub\\qch_d_ab_lin_max0p5_nat_ss.csv'
vel_outfitfile <- 'C:\\Users\\Yuanhao\\Google Drive\\RschGeneral\\Scale_SSS_OHRO\\Analysis\\vel_topbot\\sub\\vp_d_pow_max0p5_nat_ss.csv'

# output
ohiofitcsv <- 'C:\\Users\\Yuanhao\\Google Drive\\RschGeneral\\Scale_SSS_OHRO\\Analysis\\vel_topbot\\sub\\ohiofit_d_max0p5_nath_ss.csv'

############################
# Read 
hrr3 <- data.frame(read.dbf(hrr3table)) #HRR3 table
qchfit <- fread(qch_outfitfile) # qch fit, linear
velplfit <- fread(vel_outfitfile) # vel on the plane fit, alp and beta, vel=alp*(Lp)^beta

# hrr and gc
hrrgc <- cbind(hrr3$dbf.HRR_ID,hrr3$dbf.GRID_CODE,
               hrr3$dbf.A_sqkm,hrr3$dbf.COUNT,hrr3$dbf.A_sqkm/hrr3$dbf.COUNT,
               sqrt(hrr3$dbf.A_sqkm/hrr3$dbf.COUNT))
hrrgc <- data.frame(hrrgc[order(hrrgc[,1]),])

# specify column names
colnames(hrrgc) <- c('HRRID', 'GRIDCODE', 'Area', 'COUNT','A_pix','L_pix')
colnames(qchfit) <- c('HRRID', 'a', 'b','Q2')
colnames(velplfit) <- c('HRRID', 'alp', 'bet','r')


qchfitwithgc <- merge(hrrgc, qchfit, by = 'HRRID')
allfitwithgc <- merge(qchfitwithgc,velplfit, by = 'HRRID')

# ohiocatshape <- readShapePoly(ohiocatch_1, IDvar = "GRIDCODE") # catchment shapefile
# # add a and b of qch to shapefile
# joinohicatshape <- merge(ohiocatshape, allfitwithgc, by="GRIDCODE")
# shapefile(joinohicatshape, ohiofitshape, overwrite=TRUE)
# write.shapefile(joinohicatshape,ohiofitshape,arcgis=T)


#write to csv
write.csv(allfitwithgc,ohiofitcsv)
# write.dbf(allfitwithgc,ohiofitdbf,factor2char = TRUE)

print('Done!')
