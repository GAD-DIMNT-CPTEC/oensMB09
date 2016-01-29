#! /usr/bin/env python

"""
Objetivo:
Este script le diretamente os arquivos
CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads
para plotar a curva do CRPSS.

Estrutura do arquivo CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads:
TODO

Uso:
./plota_crpss.py

carlos.frederico@cptec.inpe.br, 26/01/2015
"""

# -*- coding: latin-1 -*-

"""
Modulos utilizados:
datetime: permite a manipulacao de datas e formatos;
timedelta: permite operacoes com datas;
struct: permite ler dados binarios;
numpy: permite calculos matematicos;
matplotlib: permite plotar arrays 1D/2D
seaborn: permite customizar os plots (opcional)
sys: permite utilizar funcoes do SO, incluindo argumentos da linha de comando
pickle: permite salvar/ler listas
"""

from datetime import datetime, timedelta
import struct
import numpy as np
import matplotlib.pyplot as plt

"""
Altere as opcoes abaixo conforme o caso:
"""

# Tipo de figura:
f_type = 'pdf'

# Datas:
yyyymmddB = '20141201'
yyyymmddE = '20150227'

# Variavel:
#n_var = ['psnm', 't850']
n_var = 't850'

# Regiao:
#n_reg = ['hn', 'tr', hs']
n_reg = 'hs'

# Horario sinotico:
#h_sin = ['00', '12']
h_sin = '12'

# Nome do experimento:
#n_exp = ['oensMB09_mcgav4.0', 'oensMB09', 'oensMCGA']
n_exp = 'oensMB09_mcgav4.0'

# Titulo da figura:
title = 'CRPSS DEC/JAN/FEB - 2014/2015\n' + n_var.upper() + ' ' + h_sin  + 'Z' + ' ' + n_reg.upper() + ' Exp. ' + n_exp

# Diretorio onde estao os arquivos com o nome CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads:
d_name = '/home/carlos/Documents/INPE2016/GDAD/SPCON/CRPS/python/crps_' + n_var + '_' + n_reg + '_' + h_sin  + '/scripts/python/scripts/crps_dataout/' + n_exp + '/crps_' + n_var + '_' + n_reg + '_' + h_sin + '/dataout/'

"""
Nao alterar nada a partir desta linha!
"""

def assembly_crps_lists():
  dateb = datetime.strptime(str(yyyymmddB) + str(h_sin), '%Y%m%d%H')
  datee = datetime.strptime(str(yyyymmddE) + str(h_sin), '%Y%m%d%H')

  delta = 24

  h_prev = np.arange(24, 384, 24)

  cont1 = 0

  global crps

  crps_f = []
  crps_c = []
  crps = [[] for i in range(2)]

  for hprev in h_prev:

    cont2 = 0

    date = dateb

    somaff = 0
    somacc = 0

    somaf = 'somaf' + str(hprev)
    a_somaf = 'a_somaf' + str(hprev)
    vars()[somaf] = 0
    vars()[a_somaf] = []

    somac = 'somac' + str(hprev)
    a_somac = 'a_somac' + str(hprev)
    vars()[somac] = 0
    vars()[a_somac] = []

    mediaf = 'mediaf' + str(hprev)
    vars()[mediaf] = 0

    mediac = 'mediac' + str(hprev)
    vars()[mediac] = 0

    contf = 'contf' + str(hprev)
    vars()[contf] = 0

    contff = contf

    while (date <= datee):

      contf = str(cont2)

      f_date = date.strftime('%Y%m%d%H')

      f_name = 'CRPS4CPTECEPS.' + str(hprev) + 'hForecastFor' + str(f_date) + '.aave.grads'

      file = d_name + f_name

      with open (file, 'rb') as f:
    
        r_data = f.read()

        u_data = struct.unpack('6f', r_data)

        crpsf = u_data[1]
        crpsc = u_data[4]

        global crpsc1

        if cont1 == 0:
          crpsc1 = u_data[4]

        somaf = 'somaf' + str(hprev)
        somaff = somaff + crpsf
        somaf = str(somaff)

        a_somaf = 'a_somaf' + str(hprev)
        eval(a_somaf).append(somaf)

        somac = 'somac' + str(hprev)
        somacc = somacc + crpsc
        somac = str(somacc)

        a_somac = 'a_somac' + str(hprev)
        eval(a_somac).append(somac)

      date = date + timedelta(hours=delta)

      cont1 += 1
      cont2 += 1

    mediaff = somaff / int(contf)
    crps_f.append(mediaff)

    mediacc = somacc / int(contf)
    crps_c.append(mediacc)

    crps[0].append(mediaff)
    crps[1].append(mediacc)

  print('')
  print('crps_f = ', crps_f)
  print('')
  print('crps_c = ', crps_c)
  print('')
  print('crps = ', crps)

"""
Calculo CRPSS:
"""

def calc_crpss(crps, crpsc1):
  global crpss
  crpss = []

#  for val_f in crps[0]:
  for val_f in crps:
    crpss_val = 1 - (val_f / crpsc1)
    crpss.append(crpss_val)

  print('')
  print('crpss = ', crpss)

"""
Figura CRPSS:
"""

def plot_crpss(crpss):
  fig, ax = plt.subplots()

  plt.plot(crpss, 'o-', color='#2C99C6', linewidth='3', markersize='10', label=str(n_exp))
  plt.legend(loc='best')

  plt.title(title)

  plt.ylabel('Score')
  plt.yticks([-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
  plt.ylim([-0.2,1])

  plt.xlabel('Forecast Days')
  xlabels = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  ax.set_xticklabels(xlabels)
  plt.xticks(range(len(crpss)))

  plt.grid(True)
  plt.axhline(0, color='#999999')

  plt.show()

  filename = 'crpss_decjanfev_20142015_' + n_var + '_' + n_reg +'_' + h_sin  + 'Z_' + n_exp + '.' + f_type
  fig.savefig(filename, bbox_inches='tight')

  plt.close()

"""
Chamada das Funcoes:
"""

def main():
  assembly_crps_lists()
  calc_crpss(crps[0], crpsc1)
  plot_crpss(crpss)

main()
