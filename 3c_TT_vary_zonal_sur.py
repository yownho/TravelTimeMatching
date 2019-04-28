# -*- coding: utf-8 -*-
"""
Created on Mon Mar 09 15:45:17 2015

@author: Yuanhao
"""
import time
import arcpy, os, os.path, time
from arcpy import env
from arcpy.sa import *

start_time = time.time()
arcpy.CheckOutExtension("Spatial")
arcpy.env.overwriteOutput = True

def assure_path_exists(path):
    if not os.path.exists(path):
            os.makedirs(path)

#%% INPUT, OUTPUT, SETTING
##### Input
wkdir = r'C:\Research\Scale_SSS_OHRO\Data\GIS\TT_Sur_match_nath\3p2'
fdirOH = arcpy.sa.Raster(r'C:\Research\NASA_Decomp\Ohio_GIS_basic\fdirm_ohio')
slctdir = r'C:\Research\Scale_RC_aveVel\Data\GIS\TT_d_match_1019\slct10_86'
# Catchemnts
###############################################################
CatLar = r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\3p2\GISworking\Catchments.shp'###!!!
select_ana = 0
###############################################################

# LFP for large catchments
###################################################
lfp = r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\3p2\GISworking\lfpstr.shp'
###################################################

#Raster
Trt = arcpy.sa.Raster(r'C:\Research\Scale_SSS_OHRO\Data\GIS\TT_Sur_nath\3p2\tt_3p2d')
#Trtdiff = Raster(r'C:\Research\scale_RC\GIS\TravelTime\BaseRas_diff\Trt_diff')
#Nras_OH = arcpy.sa.Raster(r'C:\\Research\\Scale_RC_aveVel\\Data\\GIS\\FitRasterD1017\\N_S_OH')
#slpras_OH = Raster(r'C:\Research\NASA_Decomp\Ohio_GIS_basic\slope_wgs84')

##### Outputs
######################################################
lfprasout = 'lfp_3p2'
TableL_1MstdS = 'Tt3p2_1stMS.dbf' #mean and std
TableL_1MMS = 'Tt3p2_1stMM.dbf' #min and max
Trt_null0ras = 'Trt_null3p2'
######################################################

cellSize = 0.00083333333
arcpy.env.extent = fdirOH
arcpy.env.snapRaster = fdirOH

#%% Main

# create folder
assure_path_exists(wkdir)

arcpy.FeatureToRaster_conversion(lfp, "GRID_CODE", os.path.join(wkdir,lfprasout), cellSize)

print 'get travel time raster excluding channel'
#lfptrt = arcpy.sa.Con(IsNull(lfpras),0, lfpras)
#lfptrt.save(os.path.join(wkdir,'lfptrt'))

#lfptrt_pl = (lfptrt-1)*(-1)
#lfptrt_pl.save(os.path.join(wkdir,'lfptrt_pl'))

#lfp_nullch = arcpy.sa.SetNull(lfptrt_pl,lfptrt_pl,"VALUE = 0")
Trt_null0 = arcpy.sa.SetNull(Trt,Trt,"VALUE = 0")
Trt_null0.save((os.path.join(wkdir,Trt_null0ras)))

#pltrt = Trtsame*lfp_nullch
#pltrt.save(os.path.join(wkdir,'pltrt1K'))

print 'Runing zonal stat'
## Trt_same slope, n
#TrtsIS = Int(Trtsame) #convert float raster to interger raster
###TrtsI.save(r"C:\Research\scale\GIS-test\TranvelTime\Trt1All1_0p01\TtI")
#
arcpy.sa.ZonalStatisticsAsTable(CatLar, "GRIDCODE", Trt_null0, 
                                os.path.join(wkdir,TableL_1MstdS),"DATA","MEAN_STD")
arcpy.sa.ZonalStatisticsAsTable(CatLar, "GRIDCODE", Trt_null0, 
                                os.path.join(wkdir,TableL_1MMS),"DATA","MIN_MAX")
#
#TrtsID = Int(Trtdiff) #convert float raster to interger raster
###TrtsI.save(r"C:\Research\scale\GIS-test\TranvelTime\Trt1All1_0p01\TtI")
#
#ZonalStatisticsAsTable(CatLar, "GRIDCODE", TrtsID, TableL_1MstdD,"DATA","MEAN_STD")
#ZonalStatisticsAsTable(CatLar, "GRIDCODE", TrtsID, TableL_1MMD,"DATA","MIN_MAX")

# zonal on slope and n for plane
#ZonalStatisticsAsTable(CatLar, "GRIDCODE", slpras_OH, os.path.join(wkdir,Table_slp_pl),"DATA","MEAN_STD")
#ZonalStatisticsAsTable(CatLar, "GRIDCODE", Nras_OH, os.path.join(wkdir,Table_n_pl),"DATA","MEAN_STD")

#%% #---select one catchments and ouput the raster of that catchments
if select_ana == 1:
    SlctCat = os.path.join(slctdir,'SlctCat.shp') 
    arcpy.Select_analysis(CatLar, SlctCat, ' "GRIDCODE" = 88 ')
    SlctCatRL_s = arcpy.sa.ExtractByMask(Trt_null0, SlctCat)
    SlctCatRL_s.save(os.path.join(slctdir,'SCatRL_s'))
    SCatRsI = arcpy.sa.Int(SlctCatRL_s)
    #---Build the raster attributte table
    slctTable = arcpy.BuildRasterAttributeTable_management(SCatRsI, "Overwrite")
    path = slctdir
    arcpy.TableToTable_conversion(SCatRsI, path,"SCat.dbf")
    Tables = os.path.join(slctdir,'TrtS.dbf')
    arcpy.sa.ZonalStatisticsAsTable(SlctCat, "GRIDCODE", SCatRsI, Tables,"DATA")


#%%
print("--- %s seconds ---" % (time.time() - start_time))
print 'Done!'
