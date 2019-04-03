#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: concatenar os membros de cada horário em um
          único arquivo NetCDF.

carlos.bastarz@inpe.br (18/03/2019)
"""

import dask
import xarray as xr

from datetime import datetime, timedelta

datei = datetime.strptime("2014110100", "%Y%m%d%H")
datef = datetime.strptime("2015022812", "%Y%m%d%H")

ctrname = "NMC"
Mems = ["01N", "01P", "02N", "02P", \
        "03N", "03P", "04N", "04P", \
        "05N", "05P", "06N", "06P", \
        "07N", "07P", ctrname]

resol = "TQ0126L028"

base_path = "/home/carlos/Documents/INPE2019/ENSEMBLE/ens_tq0126l028_tupa"

date = datei
while (date <= datef):

    dateanl = date
    f_dateanl = dateanl.strftime("%Y%m%d%H")

    datefct = date + timedelta(hours=360)
    f_datefct = datefct.strftime("%Y%m%d%H")

    for mem in Mems:

        print(f_dateanl,mem)

        ds = xr.open_mfdataset(f_dateanl + "/" + mem + "/*.nc", chunks = {"lat": 194, "lon": 385}, concat_dim = "datefct")

        fname = f_dateanl + "/" + mem + "/GPOS" + mem + f_dateanl + f_datefct + "P.ens." + resol + ".nc"
        ds.to_netcdf(fname)

    ds.close()

    date = date + timedelta(hours=12)
