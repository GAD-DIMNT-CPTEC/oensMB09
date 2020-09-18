#! /usr/bin/env python3
# -*- coding: latin-1 -*-

"""
Objetivo:
- Calcular o Skill do CRPS e plotar a curva do CRPSS;
- Este script lê diretamente os arquivos
  CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads
  para plotar a curva do CRPSS;
- Dispensa o uso do GrADS.

Estrutura do arquivo CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads:
- Binário sequencial padrão fortran;
- Possui dois records, crpsf e crpsc.

Uso:
./plota_crpss.py

Dependencias:
- Distribuicão python (v>=3) com matplotlib e numpy;
- Recomenda-se o uso da distribuicão Anaconda.

Revisões:
- 26/01/2016: primeira versão (cfbastarz)
- 05/04/2016: melhorias nos comentários (cfbastarz)
- 18/09/2020: revisão e outros ajustes (cfbastarz)

Todo:
- Criar função para plotar as CDFs dos membros do conjunto;
- Melhorar a forma de se atribuir os rótulos do eixo x na figura do CRPSS (lista xlabels);

carlos.bastarz@inpe.br (26/01/2015)
"""

from datetime import datetime, timedelta
import struct
import numpy as np
import matplotlib.pyplot as plt

# Altere as opções abaixo conforme o caso:

# Tipo de figura:
#f_type = 'jpg'
#f_type = 'png'
#f_type = 'eps'
f_type = 'pdf'

# Salvar figura?
save_fig = 'True'

# Datas:
yyyymmddB = '20141201'
yyyymmddE = '20150227'

# Variável:
#n_var = ['psnm', 't850']
n_var = 't850'

# Região:
#n_reg = ['hn', 'tr', hs']
n_reg = 'hs'

# Horário sinótico:
#h_sin = ['00', '12']
h_sin = '00'

# Nome(s) do(s) experimento(s):
n_exp = ['oensMB09_mcgav4.0', 'oensMB09', 'oensMCGA']
#n_exp = ['oensMB09_mcgav4.0', 'oensMB09']
#n_exp = ['oensMB09_mcgav4.0']

# Título da figura:
title = 'CRPSS ' + n_var.upper() + ' ' + h_sin  + 'Z' + ' ' + n_reg.upper() + '\n (Valid for: DEC/JAN/FEB - 2014/2015)'

# Raiz do diretório onde estão os arquivos com o nome CRPS4CPTECEPS.%f2hForecastFor2015022712.aave.grads:
d_root = '/home/carlos/Documents/INPE2016/GDAD/SPCON/CRPS/tupa_dataout/'

# Não alterar nada a partir desta linha!

# A seguir estão as definicoes de várias funções utilizadas no programa;
# A ordem das chamadas e feita pela função main, ao final do programa.

# A função a seguir monta as listas com os valores de crpsf e crpsc lidos
# dos arquivos binários associados a cada experimento.
def assembly_crps_lists(nexp):
  n_exp = nexp

  d_name = d_root + n_exp  + '/crps_' + n_var + '_' + n_reg + '_' + h_sin + '/dataout/' 

  # Formata as datas
  dateb = datetime.strptime(str(yyyymmddB) + str(h_sin), '%Y%m%d%H')
  datee = datetime.strptime(str(yyyymmddE) + str(h_sin), '%Y%m%d%H')

  delta = 24 # Incremento de 24 horas dentro do loop das datas

  h_prev = np.arange(24, 384, 24) # Monta um array de 24 a 360 com incremento de 24

  cont1 = 0

  # Variáveis globais utilizadas em outros escopos
  global crps
  global crps_nexp, crps_nexp_vals

  crps_nexp = 'crps-' + str(n_exp) # Monta o nome do array (eg., crps-oensMCGA)

  # Arrays locais
  crps_f = [] # Array 1D
  crps_c = [] # Array 1D
  crps = [[] for i in range(2)] # Array 2D

  # Loop sobre o array com os horários das previsões
  for hprev in h_prev:

    cont2 = 0

    date = dateb # date e a data dentro do loop das previsões

    # Variáveis em que serão acumulados os crps das previsões para cada horário
    somaff = 0
    somacc = 0

    somaf = 'somaf' + str(hprev) # Monta o nome da soma (eg., somaf24)
    a_somaf = 'a_somaf' + str(hprev) # a_somaf e um array com as somas dos horários das previsões (a_soma_f24)
    vars()[somaf] = 0
    vars()[a_somaf] = []

    somac = 'somac' + str(hprev)
    a_somac = 'a_somac' + str(hprev)
    vars()[somac] = 0
    vars()[a_somac] = []

    mediaf = 'mediaf' + str(hprev) # Média das somas dos horários de cada previsão (eg., mediaf24)
    vars()[mediaf] = 0

    mediac = 'mediac' + str(hprev)
    vars()[mediac] = 0

    contf = 'contf' + str(hprev)
    vars()[contf] = 0

    contff = contf

    # Loop sobre as datas das previsões
    while (date <= datee):

      contf = str(cont2)

      f_date = date.strftime('%Y%m%d%H') # Formata a data (YYYYMMDHH)

      f_name = 'CRPS4CPTECEPS.' + str(hprev) + 'hForecastFor' + str(f_date) + '.aave.grads' # Nome do arquivo

      file = d_name + f_name # Diretório e nome do arquivo

      # Abre o arquivo
      with open (file, 'rb') as f:
    
        # Lê tudo
        r_data = f.read()

        # Converte o byte lido de ascii para float
        u_data = struct.unpack('6f', r_data)

        # Como e um array lido com 6 posições, a posição 2 (no python, 1)
        # contém o crps da previsão (crpsf) e a posição 5 (no python, 4) contém
        # o crps da climatologia (crpsc)
        crpsf = u_data[1]
        crpsc = u_data[4]

        global crpsc1 # crpsc1 é o crps da climatologia do primeiro arquivo lido, e utilizado em outros escopos

        if cont1 == 0:
          crpsc1 = u_data[4]

        somaf = 'somaf' + str(hprev) # somaf24 é apenas o nome do array
        somaff = somaff + crpsf # somaff contém os valores lidos
        somaf = str(somaff)

        a_somaf = 'a_somaf' + str(hprev)
        eval(a_somaf).append(somaf) # Os valores lidos em somaf são incluídos no array a_somaf24

        somac = 'somac' + str(hprev)
        somacc = somacc + crpsc
        somac = str(somacc)

        a_somac = 'a_somac' + str(hprev)
        eval(a_somac).append(somac)

      date = date + timedelta(hours=delta) # Incremento da data do loop das previsões

      cont1 += 1
      cont2 += 1

    mediaff = somaff / int(contf) # Calcula a média com as somas (o contador representa o numero de arquivos abertos)
    crps_f.append(mediaff) # Inclui as médias lidas (mediaff) no array crps_f

    mediacc = somacc / int(contf)
    crps_c.append(mediacc)

    crps[0].append(mediaff) # Na primeira posição do array crps, temos crpsf
    crps[1].append(mediacc) # Na segunda posição do array crps, temos crpsc

    crps_nexp_vals = crps

# Descomente as linhas abaixo para "debugar"
#  print('')
#  print('crps_f = ', crps_f)
#  print('')
#  print('crps_c = ', crps_c)
#  print('')
#  print('crps = ', crps)


# Calculo CRPSS:

# A função a seguir calcula o skill do CRPS com base nos valores do crpsf e crpsc lidos
def calc_crpss(crps, crpsc1, crps_nexp, crps_nexp_vals, nexp, cont_nexps):
  global crpss, crpss_exp, n_crpss_exp

  # Cria as listas crpss e crpss_exp
  crpss = [] # contém todos os valores de crpss calculados
  crpss_exp = [] # representa o nome da lista com os crpss do experimento

  n_crpss_exp = 'crpss-' + str(nexp) # acrescenta o prefixo crpss à variavel nexp (nome do experimento)

  # Calcula o crpss (crpss = 1 - crpsf / crpsc) e adiciona o valor na lista crpss
  for val_f in crps:
    crpss_val = 1 - (val_f / crpsc1) # calcula o crpss
    crpss.append(crpss_val) # adiciona o valor

  crpss_exp = crpss

#  print('')
#  print('crpss = ', crpss)

#  print('')
#  print(crps_nexp, ' = ', crps_nexp_vals)

  print('')
  print(n_crpss_exp, ' = ', crpss_exp)

  crpss_crpss_exps[cont_nexps].append(crpss_exp) # Adiciona o array com o crpss do experimento em um array unico nD (n dimensões, vai depender do número de experimentos avaliados)

#  print('')
#  print('crpss_crpss_exps = ', crpss_crpss_exps)

# Figura CRPSS:

# A função a seguir plota a curva do CRPSS a partir das listas montadas
def plot_crpss(crps_nexp, crps_nexp_vals, crpss_crpss_exps):

  fig = plt.figure() # Define o objeto fig (figura)
  ax = fig.add_subplot(111) # Define o objeto ax (eixo)

#  ax.set_autoscale_on(False)

  # Tipo de fonte e cor (título, legenda e eixos)
  font = {'fontname':'Serif', 'color':'#303030'}

  # Cor do fundo
  ax.set_axis_bgcolor('#FFFFFF')

  # Cores das bordas
  ax.spines['top'].set_color('#545454')
  ax.spines['right'].set_color('#545454')
  ax.spines['bottom'].set_color('#545454')
  ax.spines['left'].set_color('#545454')

  # Adicione mais cores/paletas, caso necessário
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

  # Determina o valor mínimo entre todos os arrays
  minvals = []

  print('')
 
  # Define o limite inferior do exio y (valores do crpss) entre os experimentos envolvidos 
  for i in range(len(n_exp)):
    minv = min(crpss_crpss_exps[i][0])
    minvals.append(minv) # adiciona os valores mínimos de todos os experimentos em uma lista (minv)
    print(minv)

  print('')

  min_val_o = min(minvals) # decide qual o valor minimo dentro da lista montada
  minval = round(min_val_o,1) # arredonda o valor (cuidado aqui!)
  print('Min:',minval)

  print('')

  # O valor abaixo será usado caso o valor minimo encontrado seja maior do que zero
  minval_f = round(-0.1,1)

  # Verifica se o valor minimo determinado e maior/menor do que minval_f
  if (minval < minval_f):
    minval3 = minval
  else:
    minval3 = minval_f

  # Plota uma curva para cada item dentro do array n_exp
  for i in range(len(n_exp)):
    exp_n = n_exp[i]
    plt.plot(crpss_crpss_exps[i][0], 'o-', color=colors[i], linewidth='4', markersize='8', label=str(exp_n), zorder=i+4)

  # Legenda
  leg = plt.legend(loc='best', fancybox=True, framealpha=1)
  leg.get_frame().set_edgecolor('#545454')
  leg.get_frame().set_facecolor('#FFFFFF')
  ltxt  = leg.get_texts()
  plt.setp(ltxt, fontsize='12', **font) 

  # Título
  plt.title(title, fontsize='18', **font)

  incr = round(0.1,1)

  maxval = round(1.0,1)

  # Eixo y
  y = np.arange(minval, maxval, 0.1)
  ax.set_ylim([min(y)-0.1,maxval]) # Força os limites máximo e mínimo do eixo
  plt.yticks(np.arange(min(y)-0.1, maxval+0.1, 0.1), fontsize='14', **font) # 0.1 está sendo subtraido para sobrar um pouco de espaço no gráfico
  plt.ylabel('Score', fontsize='16', **font)

  # Eixo x
  x = np.arange(0, 15, 1)
  # Alterar aqui caso o número de dias de previsões seja diferente de 15
  xlabels = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15']
  plt.xticks(x, xlabels, fontsize='14', **font)
  plt.xlabel('Forecast Days', fontsize='16', **font)

  # Grade
  plt.grid(True)
  ax.grid(which='major', linestyle=':', color='#8C8C8C', zorder=1)

  # Curva na origem
  plt.axhline(0, color='#000000', zorder=2)

  # Mostra a figura
  plt.show()

  # Salva, caso tenha sido escolhido
  if (save_fig == 'True'):  
    n_exps = '-'.join(n_exp)
    filename = 'crpss_' + n_exps + '_' + n_var + '_' + n_reg +'_' + h_sin  + 'Z' + '.' + f_type
    fig.savefig(filename)

  # Fecha a figura
  plt.close()

# Chamada das Funções:

# A função a seguir chama as demais funções em sequência
def main():
  global cont_nexps, crpss_crpss_exps

  cont_nexps = 0 # Conta o número de experimentos avaliados
  crpss_crpss_exps = [[] for i in range(len(n_exp))] # Array com os valores do crpss dos experimentos

  # Loop sobre os experimentos
  for nexp in n_exp:
    assembly_crps_lists(nexp)
    calc_crpss(crps[0], crpsc1, crps_nexp, crps_nexp_vals, nexp, cont_nexps)
    cont_nexps += 1

  # Faz a figura
  plot_crpss(crps_nexp, crps_nexp_vals, crpss_crpss_exps)

# Chama a função principal
# O script comeca aqui
main()
