#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: realizar uma interpolação bilinear entre dois
          DataArrays.

carlos.bastarz@inpe.br (28/02/2019)
"""

import xarray as xr
import xesmf as xe

def regrid(ds_ensdata,memlats,memlons,grdres,nvar):
    grd = xe.util.grid_2d(memlons[0],memlons[-1],grdres,memlats[0],memlats[-1],grdres)
    grd_newcoord = grd.assign_coords(ref="")
    grd_expanded = grd_newcoord.expand_dims("ref")
    grd_newcoord = grd_expanded.assign_coords(ens="")
    grd_expanded = grd_newcoord.expand_dims("ens")
    grd_newcoord = grd_expanded.assign_coords(fct="")
    grd_expanded = grd_newcoord.expand_dims("fct")
    
    gridout = grd_expanded
    
    da_ensdata = ds_ensdata.to_array(name = nvar)
    
    regridder = xe.Regridder(da_ensdata, gridout, "bilinear", reuse_weights=True)
    da_ensdata_interp = regridder(da_ensdata)

    regridder.clean_weight_file()

    return da_ensdata_interp
