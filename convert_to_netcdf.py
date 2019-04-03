#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: este script escreve campos selecionados em arquivos NetCDF.

carlos.bastarz@inpe.br (13/03/2019)
"""

import os, errno
import numpy as np
import xarray as xr

from py3grads import Grads
from datetime import datetime, timedelta

ga = Grads(verbose=False)
    
def create_netcdf(mfile,nreg,var_list,base_path,dateanl,f_dateanl,datefct,f_datefct,mem,fname,Vars):
    try:
        ga("open " + mfile)
        if nreg == "GL":
            ilat = -90; flat = 90
            ilon = 0  ; flon = 360
        elif nreg == "NH":
            ilat = 20 ; flat = 90
            ilon = 0  ; flon = 360
        elif nreg == "TR":
            ilat = -20; flat = 20
            ilon = 0  ; flon = 360
        elif nreg == "SH":
            ilat = -90; flat = -20
            ilon = 0  ; flon = 360
        elif nreg == "AS":
            ilat = -60; flat = 15
            ilon = 270; flon = 330
        
        ga(f"set lat {ilat} {flat}")
        ga(f"set lon {ilon} {flon}")
        
        v_list = []
        for var in var_list:
            nvar = Vars[var][1]
            data = ga.exp(nvar)
            menv = ga.env()
            ipts = menv.nx
            jpts = menv.ny
            lons = np.linspace(ilon,flon,ipts)
            lats = np.linspace(ilat,flat,jpts)
            da_moddata = xr.DataArray(data,                       \
                                      name=Vars[var][0],          \
                                      coords={"dateanl": dateanl, \
                                              "datefct": datefct, \
                                              "lat": lats,        \
                                              "lon": lons},       \
                                      dims=["lat", "lon"])
            da_moddata.attrs["Units"] = Vars[var][2]
            da_moddata.attrs["Long Name"] = Vars[var][3]
            v_list.append(da_moddata)    
    
        da_lstdata = xr.merge(v_list)
    
        ga("close 1")
        
        memdir = base_path + "_nc/" + f_dateanl + "/" + mem + "/"
        
        try:
            os.makedirs(memdir)
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise
        
        da_lstdata.to_netcdf(memdir + fname + ".nc")
        ierr = 0
    except:
        print("Error reading " + mfile)       
#    finally:
#        ga("close 1")

def main():
    datei = datetime.strptime("2015020500", "%Y%m%d%H")
    datef = datetime.strptime("2015022812", "%Y%m%d%H")
    
    Fincs = np.arange(00,384,24)
        
    ctrname = "NMC"
    Mems = ["01N", "01P", "02N", "02P", \
            "03N", "03P", "04N", "04P", \
            "05N", "05P", "06N", "06P", \
            "07N", "07P", ctrname]
    
    var = "t"
    Vars = {
            "z": ["z500","zgeo(lev=500)","gpm","Geopotential Height at 500 hPa"], \
            "p": ["psml","psnm","hPa","Sea Level Pressure"],           \
            "t": ["t850","temp(lev=850)","K","Absolute Temperature at 850 hPa"],  \
            "q": ["q925","umes(lev=925)","kg/kg","Specific Humidity at 925 hPa"]
           }
    var_list = ["t", "z", "p", "q"]
    
    reg = "GL"
    
    resol = "TQ0126L028"
    
    base_path = "/home/carlos/Documents/INPE2019/ENSEMBLE/ens_tq0126l028_tupa"
    
    date = datei
    while (date <= datef):
    
        for mem in Mems:
    
            dateanl = date
            f_dateanl = dateanl.strftime("%Y%m%d%H")
            
            for finc in Fincs:
    
                datefct = date + timedelta(hours=int(finc))
                f_datefct = datefct.strftime("%Y%m%d%H")
        
                print(f_dateanl,f_datefct,mem,finc)
    
                yyyy = str(f_dateanl[0:4])
                mm = str(f_dateanl[4:6])
                dd = str(f_dateanl[6:8])
                hh = str(f_dateanl[8:10])
    
#                modpath = "grb/" + mem + "/" + yyyy + "/" + mm + "/" + dd + "/" + hh + "/"
                modpath = "../ens_tq0126l028_tupa/" + f_dateanl + "/" + mem + "/"
    
                if finc == 00:
                    fname = "GPOS" + mem + f_dateanl + f_dateanl + "P.icn." + resol
                    modfnam = fname + ".ctl"
                else:
                    fname = "GPOS" + mem + f_dateanl + f_datefct + "P.fct." + resol
                    modfnam = fname + ".ctl"
    
                modfile = modpath + modfnam
                
                create_netcdf(modfile,reg,var_list,base_path,dateanl,f_dateanl,datefct,f_datefct,mem,fname,Vars)
    
        date = date + timedelta(hours=12)

if __name__ == "__main__":
    main()
