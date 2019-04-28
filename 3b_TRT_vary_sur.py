# -*- coding: utf-8 -*-
"""
Created on Wed Aug 09 18:01:54 2017
1. combine vel_ch and vel_qch raster
2. 
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

#%% INPUT and OUTPUT
#-----Input
wkdir = r'C:\Research\Scale_SSS_OHRO\Data\GIS\TT_Sur_nath\3p2'
fdirOH = arcpy.sa.Raster(r'C:\Research\NASA_Decomp\Ohio_GIS_basic\fdirm_ohio')
#Strs = arcpy.sa.Raster(r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\1\GISworking\strgrid')  ###lfp stream
L_pix = arcpy.sa.Raster(r'C:\Research\Scale_RC_aveVel\Data\GIS\BaseRas902\l_pix') #km

velch = arcpy.sa.Raster(r'C:\Research\Scale_SSS_OHRO\Data\GIS\FitRaster_nath_Sur\vel_chras')
velpl = arcpy.sa.Raster(r'C:\Research\Scale_SSS_OHRO\Data\GIS\FitRaster_nath_Sur\vel_plras')

# LFP for large catchments
###################################################
# 3200 and 1000 is GISworking2
#lfp = r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\1000\GISworking2\lfpstr.shp'
lfp = r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\3p2\GISworking\lfpstr.shp'
###################################################
#----Output
tt_s = 'tt_3p2'
vel = 'vel_3p2'
ttout = 'Trtdp_3p2'
tt_d = 'tt_3p2d'

lfpras = 'lfp_3p2'
lfp_chnull = 'lfpchna_3p2'
vel_chna = 'velchna_3p2'

cellSize = 0.00083333333
arcpy.env.extent = fdirOH
arcpy.env.snapRaster = fdirOH

#%%
# create dir
assure_path_exists(wkdir)

# convert lfp feature to raster
arcpy.FeatureToRaster_conversion(lfp, "GRID_CODE", os.path.join(wkdir,lfpras), cellSize)

# combine velocity raster
#vel_rat = velch_rat+velpl_rat
vel_s = arcpy.sa.Con(arcpy.sa.IsNull(velch),velpl, velch)
vel_s.save(os.path.join(wkdir,vel))

## for different slope,0.0003768 is the min velocity except 0
#vel = arcpy.sa.Con(vel_s == 0, 0.0003768, vel_s)
#vel.save(os.path.join(wkdir,'vel_d_1'))

# for same slope, 0.00103233 is the min velocity except 0
vel = arcpy.sa.Con(vel_s == 0, 0.000932999, vel_s)
vel.save(os.path.join(wkdir,'vel_s_1'))

# make channel velocity at large scale 'null'
lfp_ch1 = arcpy.sa.Con(arcpy.sa.IsNull(os.path.join(wkdir,lfpras)),0, os.path.join(wkdir,lfpras))
#lfp_ch1.save(os.path.join(wkdir,'lfptrt'))

lfp_pl1 = (lfp_ch1-1)*(-1)
lfp_pl1.save(os.path.join(wkdir,'lfptrt_pl'))

lfp_nullch = arcpy.sa.SetNull(lfp_pl1,lfp_pl1,"VALUE = 0")
lfp_nullch.save(os.path.join(wkdir,lfp_chnull))

vel_chnull = vel*lfp_nullch
vel_chnull.save(os.path.join(wkdir,vel_chna))

# make new fdir raster with nodata in lfp channel
fdiroh_chnull = fdirOH*lfp_nullch
fdiroh_chnull.save(os.path.join(wkdir,'fdir_noch'))

# length of each pixel times parameter of the length in direction, generate the true flow length in km
lentotime = (L_pix/0.00083333333*1000)/vel_chnull
#lentotime = (L_pix*1000)/vel_chnull
lentotime.save(os.path.join(wkdir,'lentotime'))

#%% Travel time
# travel time by flowlength
#Trt_same = arcpy.sa.FlowLength(fdirOH, "DOWNSTREAM", lentotime)
#Trt_same.save(os.path.join(wkdir,tt_s))

Trt = arcpy.sa.FlowLength(fdiroh_chnull,"DOWNSTREAM", lentotime)
Trt.save(os.path.join(wkdir,tt_d))

Trt_same_p1 = Trt + 0.11714*1000/9.523114
Trt_same_p1.save(os.path.join(wkdir,ttout))
#%%
print("--- %s seconds ---" % (time.time() - start_time))
print 'Done!'
