#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivos: definir datas, tempos de previsão, variáveis,
           regiões e resolução para as avaliações.

carlos.bastarz@inpe.br (28/02/2019)
"""

import numpy as np
from datetime import datetime, timedelta

def options():
    rdatei = datetime.strptime("2015010112", "%Y%m%d%H")
    rdatef = datetime.strptime("2015013112", "%Y%m%d%H")
    
    f_rdatei = rdatei.strftime("%Y%m%d%H")
    f_rdatef = rdatef.strftime("%Y%m%d%H")
    
    Fincs = np.arange(00,384,24)
    
    ctrname = "NMC"
    Mems = ["01N", "01P", "02N", "02P", \
            "03N", "03P", "04N", "04P", \
            "05N", "05P", "06N", "06P", \
            "07N", "07P", ctrname]
    
    var = "t"
    Vars = {
            "z": ["z500","zgeo(lev=500)","gpm","Geopotential Height"], \
            "p": ["psml","psnm","hPa","Sea Level Pressure"],           \
            "t": ["t850","temp(lev=850)","K","Absolute Temperature"],  \
            "q": ["q925","umes(lev=925)","kg/kg","Specific Humidity"]
           }
    
    reg = "SH"
    
    resol = "TQ0126L028"

    grdres = 1.5 

    do_regrid = True

    return rdatei, rdatef, f_rdatei, f_rdatef, Fincs, ctrname, Mems, var, Vars, reg, resol, grdres, do_regrid, ctrname
