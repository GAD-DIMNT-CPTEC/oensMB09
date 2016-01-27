#!/usr/bin/env python

# Objetivo:
# Este script le diretamente os arquivos
# CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads
# para plotar a curva do CRPSS.

# Estrutura do arquivo CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads:
# TODO

# carlos.frederico@cptec.inpe.br, 26/01/2015

# -*- coding: latin-1 -*-

# Modulos utilizados:
# datetime: permite a manipulacao de datas e formatos;
# timedelta: permite operacoes com datas;
# struct: permite ler dados binarios;
# numpy: permite manipulacoes matematicas em geral;
# matplotlib: permite plotar arrays 1D/2D
from datetime import datetime, timedelta
import struct
import numpy as np
import matplotlib.pyplot as plt

# Variavel:
n_var = 't850'

# Regiao:
n_reg = 'hs'

# Horario Sinotico:
h_sin = '12'

# Caminho dos dados a serem lidos:
d_name = '/home/carlos/Documents/INPE2016/GDAD/SPCON/CRPS/python/crps_' + n_var + '_' + n_reg + '_' + h_sin  + '/dataout/'

# Data inicial:
dateb = datetime.strptime('2014120112', '%Y%m%d%H')

# Data final:
datee = datetime.strptime('2015022712', '%Y%m%d%H')

# Incremento da data (24 horas):
delta = 24

# date e a data dentro do loop:
date = dateb

# Vetor com os horarios das previsoes (para cada data, serao abertos 15 arquivos):
#h_prev = [24, 48, 72, 96, 120, 144, 168, 192, 216, 240, 264, 288, 312, 336, 360]
h_prev = np.arange(24, 384, 24)

# Inicializa o contador soma para cada um dos horarios de previsao:
for hprev in h_prev:
  somaf = 'somaf' + str(hprev)
  a_somaf = 'a_somaf' + str(hprev)
  vars()[somaf] = 0
  vars()[a_somaf] = []

# Inicializa o array somaf que ira conter todos os somaf* dos horarios de previsao:
#somaf = []

# Inicializa o contador:
cont = 0

# Loop sobre as datas de inicio e fim:
while (date <= datee):

  # Formata a data (YYYYMMDDHH):
  f_date = date.strftime('%Y%m%d%H')

  # Loop sobre os horarios sinoticos:
  for hprev in h_prev:
    
    # Nome do arquivo a ser aberto:
    f_name = 'CRPS4CPTECEPS.' + str(hprev) + 'hForecastFor' + str(f_date) + '.aave.grads'

    # Caminho + nome do arquivo:
    file = d_name + f_name

    # Imprime o nome do arquivo:
    print('open', f_name)

    # Abre o arquivo referente a cada uma das datas:
    with open (file, 'rb') as f:

      # Le todo o conteudo do arquivo:
      r_data = f.read()

      # Transforma os bytes (ascii) lidos para um type do tipo "float";
      # Sao 6 floats, sendo que apenas dois deles sao necessarios:
      u_data = struct.unpack('6f', r_data)
 
      # A informacao do crpsf esta na posicao 2 (no python, inicia-se em 0):
      crpsf = u_data[1]

      # A informacao do crpsc esta na posicao 5 (no python, e a posicao 4):
      crpsc = u_data[4]

      # Armazena o crpsc do primeiro arquivo:
      if cont == 0:
        crpsc1 = u_data[4]

      if hprev == 24:
        somaf24 = somaf24 + crpsf
        a_somaf24.append(somaf24)
      elif hprev == 48:
        somaf48 = somaf48 + crpsf
        a_somaf48.append(somaf48)
      elif hprev == 72:
        somaf72 = somaf72 + crpsf
        a_somaf72.append(somaf72)
      elif hprev == 96:
        somaf96 = somaf96 + crpsf
        a_somaf96.append(somaf96)
      elif hprev == 120:
        somaf120 = somaf120 + crpsf
        a_somaf120.append(somaf120)
      elif hprev == 144:
        somaf144 = somaf144 + crpsf
        a_somaf144.append(somaf144)
      elif hprev == 168:
        somaf168 = somaf168 + crpsf
        a_somaf168.append(somaf168)
      elif hprev == 192:
        somaf192 = somaf192 + crpsf
        a_somaf192.append(somaf192)
      elif hprev == 216:
        somaf216 = somaf216 + crpsf
        a_somaf216.append(somaf216)
      elif hprev == 240:
        somaf240 = somaf240 + crpsf
        a_somaf240.append(somaf240)
      elif hprev == 264:
        somaf264 = somaf264 + crpsf
        a_somaf264.append(somaf264)
      elif hprev == 288:
        somaf288 = somaf288 + crpsf
        a_somaf288.append(somaf288)
      elif hprev == 312:
        somaf312 = somaf312 + crpsf
        a_somaf312.append(somaf312)
      elif hprev == 336:
        somaf336 = somaf336 + crpsf
        a_somaf336.append(somaf336)
      elif hprev == 360:
        somaf360 = somaf360 + crpsf
        a_somaf360.append(somaf360)
      else:
        print('Erro soma')

      # Imprime na tela as informacoes lidas:
      print('crpsf =',crpsf)
      print('crpsc =',crpsc)

      # Atualiza o contador:
      cont = cont + 1

    # Pula uma linha:
    print('')

  # Atualiza a data do loop:
  date = date + timedelta(hours=delta)

# Imprime a quantidade total dos arquivos lidos:
print('Total:', cont)

# Medias referentes as somas a_somaf* calculadas:
mediaf24 = somaf24 / 90
mediaf48 = somaf48 / 90
mediaf72 = somaf72 / 90
mediaf96 = somaf96 / 90
mediaf120 = somaf120 / 90
mediaf144 = somaf144 / 90
mediaf168 = somaf168 / 90
mediaf192 = somaf192 / 90
mediaf216 = somaf216 / 90
mediaf240 = somaf240 / 90
mediaf264 = somaf264 / 90
mediaf288 = somaf288 / 90
mediaf312 = somaf312 / 90
mediaf336 = somaf336 / 90
mediaf360 = somaf360 / 90

crps = [mediaf24, mediaf48, mediaf72, mediaf96, mediaf120, mediaf144, mediaf168, mediaf192, mediaf216, mediaf240, mediaf264, mediaf288, mediaf312, mediaf336, mediaf360]

# Calcula o crpss (skill score) para cada um dos valores armazenados nos arrays a_soamf*:
crpss = []
for val in crps:
  crpss_val = 1 - (val / crpsc1)
  crpss.append(crpss_val)

fig, ax = plt.subplots()

plt.plot(crpss, 'ro-', linewidth='2')

plt.title('CRPSS DEC/JAN/FEB - 2014/2015\n' + n_var.upper() + ' ' + h_sin  + 'Z' + ' ' + n_reg.upper())

plt.ylabel('CRPSS (CRPS Score)')
plt.yticks([-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
plt.ylim([-0.2,1])

plt.xlabel('Forecast Days')
xlabels = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
ax.set_xticklabels(xlabels)
plt.xticks(range(len(crpss)))

plt.grid(True)
plt.axhline(0, color='black')

plt.show()

filename = 'crpss_decjanfev_20142015_' + n_var + '_' + n_reg +'_' + h_sin  + 'Z.pdf'
fig.savefig(filename, bbox_inches='tight')
plt.close()




