#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: retornar para o programa principal um dicionário
          com os dados lidos sobre uma região e as coordenadas 
          de latitude e longitude. As coordenadas estão sendo
          calculadas de acordo com os limites da região de interesse
          e resolução da grade.

carlos.bastarz@inpe.br (28/02/2019)
"""

import numpy as np
import xarray as xr

def subdomain(mfile,nreg,nvar,fdate,nmem,ffct):
    ds = xr.open_mfdataset(mfile)

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

    if (nmem == "ANL") or (nmem == "CLM"):
        ds_sel = ds[nvar].sel(lat=slice(ilat,flat),lon=slice(ilon,flon))
        ds_sel = ds_sel.expand_dims(["time"], axis=[0])
        ds_sel["time"] = [fdate]
    else:
        ds_sel = ds[nvar].sel(lat=slice(ilat,flat),lon=slice(ilon,flon))
        ds_sel = ds_sel.expand_dims(["time", "ens", "fct"], axis=[0, 1, 2])
        ds_sel["time"] = [fdate]
        ds_sel["ens"] = [nmem]
        ds_sel["fct"] = [ffct]

    ds.close()

    return ds_sel
