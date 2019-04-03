#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: criar um Dataset com os membros do conjunto,
          para uma ou mais datas de referência e tempos 
          de previsão.

Observações: esta versão cria um datset da análise e da
             climatologia com as mesmas dimensões do 
             datset do ensemble (i.e., 15 membros e 15 
             tempos de previsão).

carlos.bastarz@inpe.br (28/02/2019)
"""

import numpy as np
import xarray as xr
import xesmf as xe
import dask
import distributed

from datetime import datetime, timedelta
from f_subdomain import subdomain
from f_options import options
from f_regrid import regrid

def load_dataset():
    opts      = options()
    rdatei    = opts[0] 
    rdatef    = opts[1]
    f_rdatei  = opts[2]
    f_rdatef  = opts[3]
    Fincs     = opts[4]
    ctrname   = opts[5]
    Mems      = opts[6]
    var       = opts[7]
    Vars      = opts[8]
    reg       = opts[9]
    resol     = opts[10]
    grdres    = opts[11]
    do_regrid = opts[12]
    ctrname   = opts[13]
    
    rdate = rdatei
        
    cref_data = []  
    mref_data = []
    while (rdate <= rdatef):
    
        f_rdate = rdate.strftime("%Y%m%d%H")
    
        mm = str(f_rdate[4:6])
        dd = str(f_rdate[6:8])
        hh = str(f_rdate[8:10])
   
        if mm == "11":
            nmm = "nov/"
        elif mm == "12":
            nmm = "dez/"
        elif mm == "01":
            nmm = "jan/"
        elif mm == "02":
            nmm = "fev/"
    
        if resol == "TQ0126L028":
            dpath = "../ens_tq0126l028_tupa_nc/"
        elif resol == "TQ0213L042":
            dpath = "../ens_tq0213l042/" + nmm
    
        print(">> Reference Date:",f_rdate)
        print("mm, dd, hh, dpath, var:",mm,dd,hh,dpath,Vars[var][0])

        clmpath = "../ERAinterim1.5/nc/"
        clmfnam = "ERA40.DailyTimeSeries.1979-2001." + str(mm) + str(dd) + str(hh) + ".nc"
        clmfile = clmpath + clmfnam
        
        clmgrid = subdomain(clmfile,str(reg),Vars[str(var)][0],rdate,"CLM",int(0))
        cref_data.append(clmgrid)
        
        mfct_data = []
        for finc in Fincs:
    
            datefct = rdate
            f_datefct = datefct.strftime("%Y%m%d%H")
    
            dateanl = rdate - timedelta(hours=int(finc))
            f_dateanl = dateanl.strftime("%Y%m%d%H")

            mem_data = []
            for mem in Mems:

                print("f_rdate, finc, mem:",f_rdate,finc,mem)
    
                mempath = dpath + f_dateanl + "/" + mem + "/"
                if finc == 00:
                    memfnam = "GPOS" + mem + f_dateanl + f_dateanl + "P.icn." + resol + ".nc"
                else:
                    memfnam = "GPOS" + mem + f_dateanl + f_datefct + "P.fct." + resol + ".nc"
                memfile = mempath + memfnam
   
                memgrid = subdomain(memfile,str(reg),Vars[str(var)][0],rdate,mem,finc)
                mem_data.append(memgrid)

            print("\n")

            m_data = xr.concat(mem_data, dim="ens")
            mfct_data.append(m_data)

        mf_data = xr.concat(mfct_data, dim="fct")
        mref_data.append(mf_data)
   
        rdate = rdate + timedelta(hours=24)
    
    cr_data = xr.concat(cref_data, dim="time")
    ma_data = xr.concat(mref_data, dim="time")

    ds_clmdata = cr_data.to_dataset(name = Vars[var][0].upper())
    print(ds_clmdata)
    
    ds_ensdata = ma_data.to_dataset(name = Vars[var][0].upper())
    print(ds_ensdata)

    ds_clmdata.chunk().to_netcdf("ERAINT40" + f_rdatei + "-" + f_rdatef + "-" + Vars[var][0].upper() + "-" + reg + "-" + resol + "-dask.nc")
    ds_ensdata.chunk().to_netcdf("CPTECENS" + f_rdatei + "-" + f_rdatef + "-" + Vars[var][0].upper() + "-" + reg + "-" + resol + "-dask.nc")

    if do_regrid:
        ds_ensdata_interp = regrid(ds_ensdata,clmgrid.lat.values,clmgrid.lon.values,grdres,Vars[var][0].upper())
        ds_ensdata_interp.to_netcdf("CPTECENS" + f_rdatei + "-" + f_rdatef + "-" + Vars[var][0].upper() + "-" + reg + "-" + resol + "-interp.nc")
        ds_clmdata_interp = regrid(ds_clmdata,clmgrid.lat.values,clmgrid.lon.values,grdres,Vars[var][0].upper())
        ds_clmdata_interp.to_netcdf("ERAINT40" + f_rdatei + "-" + f_rdatef + "-" + Vars[var][0].upper() + "-" + reg + "-" + resol + "-interp.nc")

        ds_ensdata_interp.close()
        ds_clmdata_interp.close()

    ds_ensdata.close()
    ds_clmdata.close()
