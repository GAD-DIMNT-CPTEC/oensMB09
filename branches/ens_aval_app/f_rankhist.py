#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: calcular Rank Histogram do SPCON.

Observações: adaptado da versão original 
             https://github.com/oliverangelil/rankhistogram/blob/master/ranky.py

carlos.bastarz@inpe.br (28/03/2019)
"""

import sys
import numpy as np
import xarray as xr
import dask
import dask.array as da
import matplotlib.pyplot as plt

from dask import compute, delayed
from scipy.stats import rankdata
from datetime import datetime
from f_options import options

@delayed
def comp_histogram(obs_in,ensemble_in,mask_in):
    mask = np.bool_(mask_in.data)

    obs = obs_in.data[mask.data]
    ensemble = ensemble_in.data[:,mask]
    
    combined = np.vstack((obs[np.newaxis],ensemble))

    ranks = np.apply_along_axis(lambda x: rankdata(x,method="min"),0,combined)

    ties=np.sum(ranks[0]==ranks[1:], axis=0)
    ranks=ranks[0]
    tie=np.unique(ties)

    for i in range(1,len(tie)):
        index=ranks[ties==tie[i]]
        ranks[ties==tie[i]]=[np.random.randint(index[j],index[j]+tie[i]+1,tie[i])[0] for j in range(len(index))]

    return np.histogram(ranks, bins=np.linspace(0.5, combined.shape[0]+0.5, combined.shape[0]+1))

def rankhist():
    opts     = options()
    rdatei   = opts[0]
    f_rdatei = opts[2]
    f_rdatef = opts[3]
    Fincs    = opts[4]
    ctrname  = opts[5]
    Mems     = opts[6]
    var      = opts[7]
    Vars     = opts[8]
    reg      = opts[9]
    resol    = opts[10]
    
    ds_memdata = xr.open_mfdataset("CPTECENS" + f_rdatei + "-" + f_rdatef + "-" + Vars[str(var)][0].upper() + "-" + str(reg) + "-" + resol + "-interp.nc")
    
    da_ensemble = ds_memdata[Vars[str(var)][0].upper()].isel(variable=0).drop("time")
    da_obs = ds_memdata[Vars[str(var)][0].upper()].isel(variable=0,ens=-1).drop("time")
    
    ds_memdata.close()
    
    da_ensemble = da_ensemble.transpose("ens","fct","time","y","x")
    da_obs = da_obs.transpose("fct","time","y","x")
    
    nens = da_ensemble.shape[0]
    nfct = da_ensemble.shape[1]
    ntim = da_ensemble.shape[2]
    nlat = da_ensemble.shape[3]
    nlon = da_ensemble.shape[4]
    
    nlatmax = da_ensemble.lat.max()
    nlatmin = da_ensemble.lat.min()
    nlonmax = da_ensemble.lon.max()
    nlonmin = da_ensemble.lon.min()
    
    mask = da.random.randint(0, 2, (nfct, ntim, nlat, nlon))
    da_mask = xr.DataArray(mask, name="mask", coords={"fct": range(nfct), "ts": range(ntim), "lat": np.linspace(nlatmin,nlatmax,nlat), "lon": np.linspace(nlonmin,nlonmax,nlon)}, dims=("fct", "ts", "lat", "lon"))
    
    delayed_results = []
    for ifct in range(nfct):
    
        da_obs_isel = da_obs.isel(fct=ifct)
        da_ensemble_isel = da_ensemble.isel(fct=ifct)
        da_mask_isel = da_mask.isel(fct=ifct)
    
        histogram = comp_histogram(da_obs_isel,da_ensemble_isel,da_mask_isel)
    
        delayed_results.append(histogram)
    
    # Consome menos memória, mas é mais devagar
    results = dask.compute(*delayed_results)
    # Consome mais memória, mas é mais rápido
    #results = dask.compute(*delayed_results, scheduler="processes")
    
    for i in range(len(results)):
        relfreq = results[i][0] / np.sum(results[i][0])
        mean_relfreq = np.mean(relfreq)   
    
        plt.figure()
        plt.tight_layout()
        plt.bar(range(1,da_ensemble.shape[0]+2),relfreq,facecolor="grey",edgecolor="black",alpha=0.75,align="center",width=1)
        plt.axhline(y=mean_relfreq,color="red",linewidth=1,label='$\mu$')
        plt.xticks(np.arange(1,da_ensemble.shape[0]+2,step=1))
        axes = plt.gca()
        axes.set_ylim([0,1.0])
        plt.xlabel("Bin Number")
        plt.ylabel("Relative Frequency")
        plt.title("Rank Histogram of Ensemble Members\n" + Vars[str(var)][0].upper() + \
                  " over " + reg + " grids for " + f_rdatei + " - " + f_rdatef + " (" + str(i) +"h)")
        plt.legend()
        plt.savefig("./rankhist_" + Vars[str(var)][0] + "-" + reg + "-" + f_rdatei + \
                    "-" + f_rdatef + "-" + resol + "-" + str(i) + "h.png")
        plt.close()
