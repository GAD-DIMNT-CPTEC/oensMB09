#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: calcular o CRPS do SPCON.

carlos.bastarz@inpe.br (28/02/2019)
"""

import xarray as xr
import numpy as np
import properscoring as ps
import matplotlib.pyplot as plt
import pandas as pd

from datetime import datetime
from f_options import options

def crps():
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

    ds_clmdata = xr.open_dataset("ERAINT40" + f_rdatei + "-" + f_rdatef + "-" + Vars[str(var)][0].upper() + "-" + str(reg) + "-" + resol + "-interp.nc")
    ds_memdata = xr.open_dataset("CPTECENS" + f_rdatei + "-" + f_rdatef + "-" + Vars[str(var)][0].upper() + "-" + str(reg) + "-" + resol + "-interp.nc")
    
    clm = ds_clmdata[Vars[str(var)][0].upper()].isel(variable=0)
    ens = ds_memdata[Vars[str(var)][0].upper()].isel(variable=0)
    obs = ds_memdata[Vars[str(var)][0].upper()].isel(variable=0,fct=0)

    ds_clmdata.close()
    ds_memdata.close()
    
    print("clm:",clm.shape)
    print("ens:",ens.shape)
    print("obs:",obs.shape)
    
    # CRPSF: obs X ens
    crpsf_fct_list = []
    crpsf_ctr_list = []
    for f in range(len(ens.fct)):
        crpsf = ps.crps_ensemble(obs,ens.isel(fct=f))
        dacrpsf = xr.DataArray(crpsf, name="CRPSF",                                         \
                                      coords={"ens": ens.ens.values,                        \
                                              "lat": np.linspace(-20,20,len(ens.y.values)), \
                                              "lon": np.linspace(0,360,len(ens.x.values))}, \
                                      dims=["ref_time", "ens", "lat", "lon"])
        dacrpsf_ctr = dacrpsf.isel(ens=-1)
        dacrpsf_ctr.attrs["Units"] = Vars[var][2]
        dacrpsf_ctr.attrs["Long Name"] = "CRPS for FORECAST [THE LOWER THE BETTER]"
        crpsf_ctr_list.append(dacrpsf_ctr)
        dacrpsf_mean = dacrpsf.mean(dim=["ens"])
        dacrpsf_mean.attrs["Units"] = Vars[var][2]
        dacrpsf_mean.attrs["Long Name"] = "CRPS for FORECAST [THE LOWER THE BETTER]"
        crpsf_fct_list.append(dacrpsf_mean)
    
    da_crpsf_ctr = xr.concat(crpsf_ctr_list, dim="fct_time")
    da_crpsf_ctr.to_netcdf("CRPSCTR" + f_rdatei + "-" + f_rdatef + "-" + Vars[str(var)][0].upper() + "-" + reg + "-" + resol + ".nc")
    print("da_crpsf_ctr:",da_crpsf_ctr.shape)
    da_crpsf = xr.concat(crpsf_fct_list, dim="fct_time")
    da_crpsf.to_netcdf("CRPSF" + f_rdatei + "-" + f_rdatef + "-" + Vars[str(var)][0].upper() + "-" + reg + "-" + resol + ".nc")
    print("da_crpsf:",da_crpsf.shape)
    
    # CRPSC: obs X clm
    crpsc_ens_list = []
    for e in range(len(ens.ens)):
        crpsc = ps.crps_ensemble(obs.isel(ens=e), clm)
        
        dacrpsc = xr.DataArray(crpsc, name="CRPSC",                                         \
                                      coords={"lat": np.linspace(-20,20,len(ens.y.values)), \
                                              "lon": np.linspace(0,360,len(ens.x.values))}, \
                                      dims=["ref_time", "lat", "lon"])
        print(dacrpsc.shape)
        dacrpsc.attrs["Units"] = Vars[var][2]
        dacrpsc.attrs["Long Name"] = "CRPS for CLIMATOLOGY [THE LOWER THE BETTER]"
        crpsc_ens_list.append(dacrpsc)
    
    da_crpsc = xr.concat(crpsc_ens_list, dim="ens")
    da_crpsc.to_netcdf("CRPSC" + f_rdatei + "-" + f_rdatef + "-" + Vars[str(var)][0].upper() + "-" + reg + "-" + resol + ".nc")
    print("da_crpsc:",da_crpsc.shape)

    plotcrps(da_crpsf,da_crpsf_ctr,da_crpsc)

def plotcrps(da_crpsf,da_crpsf_ctr,da_crpsc):
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

    # CURVAS
    # O CRPSC é utilizado apenas para o cálculo do Skill (CRPSS). 
    # A curva do CRPS "Control (deterministic) será referente às 
    # análises e previsões do membro controle do ensemble.
    plt.tight_layout()
    plt.title("Deterministic and Ensemble of CPTEC\n" + Vars[str(var)][0].upper() + \
              " over " + reg + " grids for " + f_rdatei + " - " + f_rdatef)
    plt.xlabel("Forecast day")
    if Vars[str(var)][0] == "t850":
        unit = "(degreeC)"
    elif Vars[str(var)][0] == "psml":
        unit = "(hPa)"
    elif Vars[str(var)][0] == "z500":
        unit = "(m)"
    plt.ylabel("CRPS " + unit)
    
    # Curva CRPSF (EPS distribution)
    # 1. média espacial:
    da_crpsf_eps_aave = da_crpsf.mean(dim=["lat","lon"])
    print(da_crpsf_eps_aave)
    # 2. cria uma tabela fct_time X ref_time
    da_crpsf_eps_aave_frame = pd.DataFrame(np.column_stack((da_crpsf_eps_aave[i]) for i in range(len(da_crpsf_eps_aave.fct_time))))
    da_crpsf_eps_aave_frame_transp = da_crpsf_eps_aave_frame.transpose()
    print(da_crpsf_eps_aave_frame_transp)
    #for col in range(len(da_crpsf_eps_aave_frame_transp.columns)):
    #    plt.plot(da_crpsf_eps_aave_frame_transp[col],linestyle="--")
    # 3. média dos tempos de previsão (fct_time) entre os dias de referência (ref_time)
    da_crpsf_eps_aave_frame_transp_ave = da_crpsf_eps_aave_frame_transp.mean(axis=1)
    print(da_crpsf_eps_aave_frame_transp_ave)
    # 4. plota a curva final do crpsf
    plt.plot(da_crpsf_eps_aave_frame_transp_ave,color="red",linewidth="2",marker="P",label="EPS Distribution")

    # Curva CRPSF (Control deterministic)
    # 1. média espacial:
    da_crpsf_ctr_aave = da_crpsf_ctr.mean(dim=["lat","lon"])
    print(da_crpsf_ctr_aave)
    # 2. cria uma tabela fct_time X ref_time
    da_crpsf_ctr_aave_frame = pd.DataFrame(np.column_stack((da_crpsf_ctr_aave[i]) for i in range(len(da_crpsf_ctr_aave.fct_time))))
    da_crpsf_ctr_aave_frame_transp = da_crpsf_ctr_aave_frame.transpose()
    print(da_crpsf_ctr_aave_frame_transp)
    #for col in range(len(da_crpsf_ctr_aave_frame_transp.columns)):
    #    plt.plot(da_crpsf_ctr_aave_frame_transp[col],linestyle="--")
    # 3. média dos tempos de previsão (fct_time) entre os dias de referência (ref_time)
    da_crpsf_ctr_aave_frame_transp_ave = da_crpsf_ctr_aave_frame_transp.mean(axis=1)
    print(da_crpsf_ctr_aave_frame_transp_ave)
    # 4. plota a curva final do crpsf
    plt.plot(da_crpsf_ctr_aave_frame_transp_ave,color="blue",linewidth="2",marker="o",label="Deterministic (Control)")
    
    # Curva CRPSS (Skill CRPS)
    # 1. média espacial:
    da_crpsc_aave = da_crpsc.mean(dim=["lat","lon"])
    print(da_crpsc_aave)
    # 2. cria uma tabela fct_time X ref_time
    da_crpsc_aave_frame = pd.DataFrame(np.column_stack((da_crpsc_aave[i]) for i in range(len(da_crpsc_aave.ens))))
    da_crpsc_aave_frame_transp = da_crpsc_aave_frame.transpose()
    print(da_crpsc_aave_frame_transp)
    #for col in range(len(da_crpsc_aave_frame_transp.columns)):
    #    plt.plot(da_crpsc_aave_frame_transp[col],linestyle="--")
    # 3. média dos tempos de previsão (fct_time) entre os dias de referência (ref_time)
    da_crpsc_aave_frame_transp_ave = da_crpsc_aave_frame_transp.mean(axis=1)
    print(da_crpsc_aave_frame_transp_ave)
    # 4. plota a curva final do crpsc
    #plt.plot(da_crpsc_aave_frame_transp_ave,color="green",linewidth="2",marker="o",label="Climatotlogy")
    
    plt.plot(((da_crpsc_aave_frame_transp_ave - da_crpsf_ctr_aave_frame_transp_ave) / da_crpsc_aave_frame_transp_ave),color="green",linewidth="2",marker="o", label="CRPS Skill")
    
    plt.xticks(da_crpsf_eps_aave_frame_transp.index.values)
    #plt.xticks(ds_memdata.fct.values)
    plt.legend()
    plt.axhline(y=0,color="black",linewidth=1)
    plt.grid(True)
    plt.savefig("crps_" + Vars[str(var)][0] + "-" + reg + "-" + f_rdatei + "-" + f_rdatef + "-" + resol + ".png")
    plt.show()
