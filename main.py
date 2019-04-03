#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objetivo: programa principal do sistema de avaliação
          do SPCON.

carlos.bastarz@inpe.br (28/02/2019)
"""

from f_subdomain import subdomain
from f_options import options
from f_regrid import regrid
from f_load_dataset import load_dataset
from f_crps import crps
from f_roc import roc
from f_rankhist import rankhist
from f_pdfcdf import pdfcdf

load_dataset()
crps()
roc()
rankhist()
pdfcdf()
