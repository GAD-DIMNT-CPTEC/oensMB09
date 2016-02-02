#! /usr/bin/env python
# -*- coding: latin-1 -*-

"""
Objetivo:
- Este script le diretamente os arquivos
  CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads
  para plotar a curva do CRPSS.

Estrutura do arquivo CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads:
- Binario sequencial padrao fortran;
- Possui dois records (crpsf e crpsc).

Uso:
./plota_crpss.py

carlos.frederico@cptec.inpe.br, 26/01/2015
"""

"""
Modulos utilizados:
datetime: permite a manipulacao de datas e formatos;
timedelta: permite operacoes com datas;
struct: permite ler dados binarios;
numpy: permite calculos matematicos;
matplotlib: permite plotar arrays 1D/2D (ver arquivo matplotlibrc para opcoes)
"""

from datetime import datetime, timedelta
import struct
import numpy as np
import matplotlib.pyplot as plt

"""
Altere as opcoes abaixo conforme o caso:
"""

# Tipo de figura:
f_type = 'png'

# Salvar figura?
save_fig = 'True'

# Datas:
yyyymmddB = '20141201'
yyyymmddE = '20150227'

# Variavel:
#n_var = ['psnm', 't850']
n_var = 'psnm'

# Regiao:
#n_reg = ['hn', 'tr', hs']
n_reg = 'hn'

# Horario sinotico:
#h_sin = ['00', '12']
h_sin = '12'

# Nome do experimento:
n_exp = ['oensMB09_mcgav4.0', 'oensMB09', 'oensMCGA']
#n_exp = ['oensMB09_mcgav4.0', 'oensMB09']
#n_exp = ['oensMB09_mcgav4.0']

# Titulo da figura:
title = 'CRPSS ' + n_var.upper() + ' ' + h_sin  + 'Z' + ' ' + n_reg.upper() + '\n (Valid for: DEC/JAN/FEB - 2014/2015)'

# Raiz do diretorio onde estao os arquivos com o nome CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads:
d_root = '/home/carlos/Documents/INPE2016/GDAD/SPCON/CRPS/tupa_dataout/'

"""
Nao alterar nada a partir desta linha!
"""

def assembly_crps_lists(nexp):
  n_exp = nexp

  d_name = d_root + n_exp  + '/crps_' + n_var + '_' + n_reg + '_' + h_sin + '/dataout/' 

  dateb = datetime.strptime(str(yyyymmddB) + str(h_sin), '%Y%m%d%H')
  datee = datetime.strptime(str(yyyymmddE) + str(h_sin), '%Y%m%d%H')

  delta = 24 # Incremento de 24 horas dentro do loop das datas

  h_prev = np.arange(24, 384, 24) # Monta um array de 24 a 360 com incremento de 24

  cont1 = 0

  # Variaveis globais utilizadas em outros escopos
  global crps
  global crps_nexp, crps_nexp_vals

  crps_nexp = 'crps-' + str(n_exp) # Monta o nome do array (eg., crps-oensMCGA)

  # Arrays locais
  crps_f = [] # Array 1D
  crps_c = [] # Array 1D
  crps = [[] for i in range(2)] # Array 2D

  # Loop sobre o array com os horarios das previsoes
  for hprev in h_prev:

    cont2 = 0

    date = dateb # date e a data dentro do loop das previsoes

    # Variaveis em que sera acumulados os crps das previsoes para cada horario
    somaff = 0
    somacc = 0

    somaf = 'somaf' + str(hprev) # Monta o nome da soma (eg., somaf24)
    a_somaf = 'a_somaf' + str(hprev) # a_somaf e um array com as somas dos horarios das previsoes (a_soma_f24)
    vars()[somaf] = 0
    vars()[a_somaf] = []

    somac = 'somac' + str(hprev)
    a_somac = 'a_somac' + str(hprev)
    vars()[somac] = 0
    vars()[a_somac] = []

    mediaf = 'mediaf' + str(hprev) # Media das somas dos horarios de cada previsao (eg., mediaf24)
    vars()[mediaf] = 0

    mediac = 'mediac' + str(hprev)
    vars()[mediac] = 0

    contf = 'contf' + str(hprev)
    vars()[contf] = 0

    contff = contf

    # Loop sobre as datas das previsoes
    while (date <= datee):

      contf = str(cont2)

      f_date = date.strftime('%Y%m%d%H') # Formata a data (YYYYMMDHH)

      f_name = 'CRPS4CPTECEPS.' + str(hprev) + 'hForecastFor' + str(f_date) + '.aave.grads' # Nome do arquivo

      file = d_name + f_name # Diretorio e nome do arquivo

      # Abre o arquivo
      with open (file, 'rb') as f:
    
        # Le tudo
        r_data = f.read()

        # Converte o byte lido de ascii para float
        u_data = struct.unpack('6f', r_data)

        # Como e um array lido com 6 posicoes, a posicao 2 (no python, 1)
        # contem o crps da previsao (crpsf) e a posicao 5 (no python, 4) contem
        # o crps da climarologia (crpsc)
        crpsf = u_data[1]
        crpsc = u_data[4]

        global crpsc1 # crpsc1 e o crps da climatologia do primeiro arquivo lido, e utilizado em outros escopos

        if cont1 == 0:
          crpsc1 = u_data[4]

        somaf = 'somaf' + str(hprev) # somaf24 e apenas o nome do array
        somaff = somaff + crpsf # somaff contem os valores lidos
        somaf = str(somaff)

        a_somaf = 'a_somaf' + str(hprev)
        eval(a_somaf).append(somaf) # Os valores lidos em somaf sao incluidos no array a_somaf24

        somac = 'somac' + str(hprev)
        somacc = somacc + crpsc
        somac = str(somacc)

        a_somac = 'a_somac' + str(hprev)
        eval(a_somac).append(somac)

      date = date + timedelta(hours=delta) # Incremento da data do loop das previsoes

      cont1 += 1
      cont2 += 1

    mediaff = somaff / int(contf) # Calcula a media com as somas (o contados representa o numero de arquivos abertos)
    crps_f.append(mediaff) # Inclui as medias lidas (mediaff) no array crps_f

    mediacc = somacc / int(contf)
    crps_c.append(mediacc)

    crps[0].append(mediaff) # Na primeira posicao do array crps, temos crpsf
    crps[1].append(mediacc) # Na segunda posicao do array crps, temos crpsc

    crps_nexp_vals = crps

# Descomente as linhas abaixo para "debugar"
#  print('')
#  print('crps_f = ', crps_f)
#  print('')
#  print('crps_c = ', crps_c)
#  print('')
#  print('crps = ', crps)

"""
Calculo CRPSS:
"""

def calc_crpss(crps, crpsc1, crps_nexp, crps_nexp_vals, nexp, cont_nexps):
  global crpss, crpss_exp, n_crpss_exp

  crpss = []
  crpss_exp = []

  n_crpss_exp = 'crpss-' + str(nexp)

  for val_f in crps:
    crpss_val = 1 - (val_f / crpsc1)
    crpss.append(crpss_val)

  crpss_exp = crpss

#  print('')
#  print('crpss = ', crpss)

#  print('')
#  print(crps_nexp, ' = ', crps_nexp_vals)

  print('')
  print(n_crpss_exp, ' = ', crpss_exp)

  crpss_crpss_exps[cont_nexps].append(crpss_exp) # Adiciona o array com o crpss do experimento em um array unico nD (n dimensoes, vai depender do numero de experimentos avaliados)

#  print('')
#  print('crpss_crpss_exps = ', crpss_crpss_exps)

"""
Figura CRPSS:
"""

def plot_crpss(crps_nexp, crps_nexp_vals, crpss_crpss_exps):

  fig = plt.figure() # Define o objeto fig (figura)
  ax = fig.add_subplot(111) # Define o objeto ax (eixo)

#  ax.set_autoscale_on(False)

  # Tipo de fonte e cor (titulo, legenda e eixos)
  font = {'fontname':'Serif', 'color':'#303030'}

  # Cor do fundo
  ax.set_axis_bgcolor('#F0F0F0')

  # Cores das bordas
  ax.spines['top'].set_color('#545454')
  ax.spines['right'].set_color('#545454')
  ax.spines['bottom'].set_color('#545454')
  ax.spines['left'].set_color('#545454')

  # Adicione mais cores/paletas, caso necessario
  colors = ['#348ABD', '#7A68A6', '#A60628', '#467821', '#CF4457', '#188487', '#E24A33']
#  colors = ['#8C4792', '#2980DA', '#A6B372']
#  colors = ['#588C75', '#B0C47F', '#F3E395', '#F3AE73', '#DA645A', '#AB5351', '#8D4548']
#  colors = ['#EB6C1B', '#EA4B36', '#A11252', '#683F6B', '#0C8A7C']
#  colors = ['#0C8A7C', '#683F6B', '#A11252', '#EA4B36', '#EB6C1B']
#  colors = ['#2B8C7F', '#BFC42D', '#C4712D', '#C42D44', '#363636']
#  colors = ['#363636', '#C42D44', '#C4712D', '#BFC42D', '#2B8C7F']
#  colors = ['#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#D8A75E', '#A0A0A0']
#  colors = ['#556270', '#4ECDC4', '#C7F464', '#FF6B6B', '#C44D58']
#  colors = ['#2C2C2C', '#C62828', '#558B2E', '#FF8F01', '#1565C1', '#6A1E9A', '#00838F']

  # Determina o valor minimo entre todos os arrays
  minvals = []

  print('')
  
  for i in range(len(n_exp)):
    minv = min(crpss_crpss_exps[i][0])
    minvals.append(minv)
    print(minv)

  print('')

  min_val_o = min(minvals)
  minval = round(min_val_o,1)
  print('Min:',minval)

  print('')

  # O valor abaixo sera usado caso o valor minimo encontrado seja maior do que zero
  minval_f = round(-0.1,1)

  # Verifica se o valor minimo determinado e maior/menor do que minval_f
  if (minval < minval_f):
    minval3 = minval
  else:
    minval3 = minval_f

  # Plota uma curva para cada item dentro do array n_exp
  for i in range(len(n_exp)):
    exp_n = n_exp[i]
    plt.plot(crpss_crpss_exps[i][0], 'o-', color=colors[i], linewidth='4', markersize='8', label=str(exp_n), zorder=i+2)

  # Legenda
  leg = plt.legend(loc='best', fancybox=True, framealpha=1)
  leg.get_frame().set_edgecolor('#545454')
  leg.get_frame().set_facecolor('#F0F0F0')
  ltxt  = leg.get_texts()
  plt.setp(ltxt, fontsize='11', **font) 

  # Titulo
  plt.title(title, fontsize='16', **font)

  incr = round(0.1,1)

  maxval = round(1.0,1)

  # Eixo y
  plt.ylabel('Score', fontsize='14', **font)
  y = np.arange(minval, maxval, 0.1)
  plt.yticks(np.arange(min(y)-0.1, 1.1, 0.1)) # 0.1 esta sendo subtraido para sobrar um pouco de espaco no grafico
  ax.set_ylim([min(y)-0.1,1.0]) # Forca os limites maximo e minimo do eixo

  # Eixo x
  plt.xlabel('Forecast Days', fontsize='14', **font)
  x = np.arange(0, 15, 1)
  xlabels = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15']
  plt.xticks(x, xlabels, fontsize='12', **font)

  # Grade
  plt.grid(True)
  ax.grid(which='major', linestyle=':', color='#8C8C8C', zorder=0)

  # Curva na origem
  plt.axhline(0, color='#000000', zorder=1)

  # Mostra a figura
  plt.show()

  # Salva, caso tenha sido escolhido
  if (save_fig == 'True'):  
    n_exps = '-'.join(n_exp)
    filename = 'crpss_' + n_exps + '_' + n_var + '_' + n_reg +'_' + h_sin  + 'Z' + '.' + f_type
    fig.savefig(filename)

  # Fecha a figura
  plt.close()

"""
Chamada das Funcoes:
"""

def main():
  global cont_nexps, crpss_crpss_exps

  cont_nexps = 0 # Conta o numero de experimentos avaliados
  crpss_crpss_exps = [[] for i in range(len(n_exp))] # Array com os valores do crpss dos experimentos

  # Loop sobre os experimentos
  for nexp in n_exp:
    assembly_crps_lists(nexp)
    calc_crpss(crps[0], crpsc1, crps_nexp, crps_nexp_vals, nexp, cont_nexps)
    cont_nexps += 1

  # Faz a figura
  plot_crpss(crps_nexp, crps_nexp_vals, crpss_crpss_exps)

# Chama a funcao principal
# O script comeca aqui
main()
