# -*- coding: utf-8 -*-
"""
Created on Wed Nov 02 15:08:56 2016
calculate vel of q channel and velocity on the plane
@author: Yuanhao
"""
import arcpy, os, os.path, time
from arcpy import env
from arcpy.sa import *
import dbf

arcpy.CheckOutExtension("Spatial")
arcpy.env.overwriteOutput = True

#%%INPUT
#Input
wdir = r'C:\Research\Scale_SSS_OHRO\Data\GIS\FitRaster_nath_Sur'

####### DO NOT change BELOW ######
fdirOH = arcpy.sa.Raster(r'C:\Research\NASA_Decomp\Ohio_GIS_basic\fdirm_ohio') #Fdir
facc = arcpy.sa.Raster(r'C:\Research\NASA_Decomp\Ohio_GIS_basic\faccm_ohio') #flow accumulation
projection = r"C:\Research\HRR_0326\2.1\2.1\Lambert Azimuthal Eq Area N America (Flood).prj" #meters
slpras = arcpy.sa.Raster(r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\1\GISworking\slope_m') # basic slope raster, flood projection
#slpras = arcpy.sa.Raster(r'D:\Data\Trt_vel_ave\slope_mod\slpohio_wgs84') # basic slope raster, wgs84
#Strs = arcpy.sa.Raster(r'C:\Research\Scale_RC_aveVel_II\HRRSetup\1\GISworking\strgrid')  ###lfp stream
Strfea = r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\1\GISworking\lfpstr.shp' ###lfp stream
Oh1cat_orig = r'C:\Research\Scale_RC_aveVel_0316\HRRSetup\1\GISworking\Catchments.shp'

#facc_sqkm = Raster(r'C:\Research\Scale_RC_aveVel\facc+1_sqkm')
flenCat1pix= arcpy.sa.Raster(r'C:\Research\Scale_RC_aveVel\Data\GIS\BaseRas902\flencat1pix')
faccp1sqkm = arcpy.sa.Raster(r'C:\Research\Scale_RC_aveVel\Data\GIS\BaseRas902\facc+1sqkm')
cumA_cons = arcpy.sa.Raster(r'C:\Research\Scale_RC_aveVel\Data\GIS\Basic\cumaOH_cons')
###### DO NOT change ABOVE ######

# csv file includes a,b,alp,beta
fitcsv = r'C:\Users\Yuanhao\Google Drive\RschGeneral\Scale_SSS_OHRO\Analysis\vel_topbot\Sur\ohiofit_d_max0p5_nath_s.csv'
#Output
fitdbf = r'ohiofit_SSlp.dbf'
qchfit_a = 'a_ch' # alp of power fit for qch
qchfit_b = 'b_ch' # bet of power fit for qch
velplfit_alp = 'alp_pl' # alp of power fit for vel on the plane
velplfit_bet = 'bet_pl' # bet of pwoer fit for vel on the plane
cat_area = 'cat_area' # area(sqkm) for each model unit
cat_pix = 'cat_pix' # pixel for each model unit
cat1fitshp = 'cat1fit.shp'
L_pix = 'L_pix'

strras = 'stronly'
faccstr = 'str_facc'
catras = 'catonly'
faccpl = 'pl_facc'
qstr = 'q_str-1' #q of stream, cms
Wras = 'wras'

slprasmodout = 'slprasmod'
vel_chras = 'vel_chras'
depthchras = 'depthchras'
vel_plras = 'vel_plras'

lfpras = 'lfpras'
cellSize = 0.00083333333
arcpy.env.extent = fdirOH
arcpy.env.snapRaster = fdirOH

def assure_path_exists(path):
    if not os.path.exists(path):
            os.makedirs(path)

#%% Define ohio catchment output raster of a, b, alp, bet
# create directory 
assure_path_exists(wdir)

# convert lfp feature to raster
arcpy.FeatureToRaster_conversion(Strfea, "GRID_CODE", os.path.join(wdir,lfpras), cellSize)
Strs = arcpy.sa.Raster(os.path.join(wdir,lfpras))

#copy catchment of 1sqkm to folder
OH1cat = os.path.join(wdir,'Catchments.shp')
arcpy.Copy_management(Oh1cat_orig, OH1cat)

#convert csv to dbf 
arcpy.TableToTable_conversion(fitcsv,wdir,fitdbf)
#table_fit = dbf.from_csv(fitcsv,os.path.join(wdir,'fit.dbf'))

arcpy.AddField_management(os.path.join(wdir,fitdbf),"GRIDCODE_d","DOUBLE")
arcpy.AddField_management(os.path.join(wdir,fitdbf),"a_d","DOUBLE")

# convert gridcode from string to number
with arcpy.da.UpdateCursor(os.path.join(wdir,fitdbf), ["GRIDCODE","GRIDCODE_d","a","a_d"]) as cursor:
    for row in cursor:
        row[1]=round(float(row[0]), 1)
        row[3]=float(row[2])
        cursor.updateRow(row)
        #print row[1]


#%% join fit dbf file to OH 1sqkm catchment
#arcpy.JoinField_management(OH1cat,"GRIDCODE", os.path.join(wdir,fitdbf), "GRIDCODEN",["bet_ch"])
arcpy.JoinField_management(OH1cat,"GRIDCODE", os.path.join(wdir,fitdbf), "GRIDCODE_d")

#%%
print('define prj')
#spatial_ref = arcpy.Describe(facc).spatialReference
#arcpy.DefineProjection_management (OH1cat, spatial_ref)

print('converting feasture to raster')
arcpy.FeatureToRaster_conversion (OH1cat, "Q2", os.path.join(wdir,'q2'), 0.00083333333)
arcpy.FeatureToRaster_conversion (OH1cat, "a_d", os.path.join(wdir,qchfit_a), 0.00083333333)
arcpy.FeatureToRaster_conversion (OH1cat, "b", os.path.join(wdir,qchfit_b), 0.00083333333)
arcpy.FeatureToRaster_conversion (OH1cat, "alp", os.path.join(wdir,velplfit_alp), 0.00083333333)
arcpy.FeatureToRaster_conversion (OH1cat, "bet", os.path.join(wdir,velplfit_bet), 0.00083333333)
#arcpy.FeatureToRaster_conversion (OH1cat, "A_pix", os.path.join(wdir,cat_pix), 0.00083333333)
#arcpy.FeatureToRaster_conversion (OH1cat, "L_pix", os.path.join(wdir,L_pix), 0.00083333333)

#%% Pre
print('velocity in channel')
# facc calculation
#inweightras = Raster(os.path.join(wdir,cat_pix))
#facc_sqkm = FlowAccumulation(fdirOH,inweightras,"FLOAT")
#faccp1sqkm = facc_sqkm + 0.00710581
#faccp1sqkm.save(os.path.join(wdir,'facc+1sqkm'))

#faccp1sqkm= Raster(os.path.join(wdir,'facc+1sqkm'))
#%%
# stream raster
Stronly = arcpy.sa.SetNull(Strs, Strs, "Value = 0")
#Stronly.save(os.path.join(wdir,strras))

#catchment grid
catgrid = arcpy.sa.Con(Strs==0,1,0)
#catonly = arcpy.sa.SetNull(catgrid, catgrid, "VALUE = 0")
#catonly.save(os.path.join(wdir,catras))

# a raster for stream q
stra = Stronly*(os.path.join(wdir,qchfit_a))
#stra.save(os.path.join(wdir,'a_ch_only'))
# b raster for stream q
strb = Stronly*(os.path.join(wdir,qchfit_b))
#strb.save(os.path.join(wdir,'b_ch_only'))

#facc for channel
str_facc = Stronly*faccp1sqkm
str_facc.save(os.path.join(wdir,faccstr))
#str_facc.save(r'C:\Research\scale_RC\GIS\Fit_raster\facc_strNcov')

#str_q = (stra*str_facc*525341/78061690*0.01+strb)/0.01 #calculate q for each pixel
str_q = stra*(str_facc)+strb #calculate q based on facc of cumA--sqkm
str_q.save(os.path.join(wdir,qstr))

# minimum q2 on stream
q2_str = Stronly*(os.path.join(wdir,'q2'))
q2_str.save(os.path.join(wdir,'q2_str'))

str_q_no0 = arcpy.sa.Con(str_q<q2_str.minimum, q2_str.minimum, str_q)
str_q_no0.save(os.path.join(wdir,'strqpos_m'))

# width grid from G.Allen
#Wras = Stronly*(1.1093*((str_facc)*0.00673)**0.4914) # width in meter, A in sqkm
Wras = Stronly*(1.1093*faccp1sqkm**0.4914) # width in meter, A in sqkm
#Wras.save(os.path.join(wdir,'widthOH'))

Wras_cons = 1.1093*(cumA_cons**0.4914) # width in meter, A in sqkm
#Wras_cons.save(os.path.join(wdir,'widOH_con'))

#slope of the basin
#####----- same slope
#slprascat = 0.086578*catgrid 
#slprasstr = 0.025661*Strs
#slprasmod = slprascat+slprasstr

#####----- different slope
G1 = slpras > 0.01 
#G1.save(os.path.join(wdir,'g1'))
G2 = slpras <= 0.01
#G2.save(os.path.join(wdir,'g2'))
slprasmod = G1*slpras + G2*0.01
slprasmod.save(os.path.join(wdir,slprasmodout))

# n for channel and plane
Nall = 0.8*catgrid + 0.025*Strs
#Nall.save(os.path.join(wdir,'N_S_OH'))

#depth_ch = (str_q_no0*Nall/(Wras*(slprasmod*0.01)**0.5))**0.6 #in meter
arcpy.env.cellSize = str_q_no0
depth_ch = (str_q_no0*Nall/(Wras_cons*(slprasmod*0.01)**0.5))**0.6 #in meter
depth_ch.save(os.path.join(wdir,depthchras))

# velocity in channel-
vel_ch = 1/Nall*(depth_ch**0.6667)*((slprasmod*0.01)**0.5)
#vel_ch.save(os.path.join(wdir,'vel_ch_rat'))
vel_ch.save(os.path.join(wdir,vel_chras))

#%% vel on the plane
print('Calculating vel_pl')
#facc on the plane
#pl_facc = catonly*facc_sqkm
#pl_facc.save(os.path.join(wdir,faccpl))

#cat_area = Raster(r'C:\Research\scale_RC\GIS\Fit_raster\cat_area')
#cat_pix = Raster(r'C:\Research\scale_RC\GIS\Fit_raster\cat_pix')
alp_pl = Raster(os.path.join(wdir,velplfit_alp))
bet_pl = Raster(os.path.join(wdir,velplfit_bet))
velpl = (alp_pl*(flenCat1pix**bet_pl)) # this based on plane length number
#velpl = velplfit_alp*(velpl_facc*525341/78061690)**velplfit_bet #this based on area, sqkm
velpl.save(os.path.join(wdir,vel_plras))


#%%
print('Done!')