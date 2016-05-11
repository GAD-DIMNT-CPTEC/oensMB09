#! /usr/bin/env python

"""
Objetivo:
- Este script le as tabelas de nome:
  RANKHIST4CPTECEPS.%fhForecastFor%y4%m2%d2%h2.txt
  para plotar o rank histogram (ou Talagrand histogram).

Estrutura do arquivo RANKHIST4CPTECEPS.%fhForecastFor2014111712.txt:
- ASCII;
- Matriz retangular com 0 e 1.

Uso:
./plot_rank_hist.py <nome_do_arquivo>

Exemplo:
./plot_rank_hist.py RANKHIST4CPTECEPS.24hForecastFor2014111712.txt

carlos.frederico@cptec.inpe.br, 29/04/2016
"""

"""
Modulos utilizados:
sys: permite utilizar comandos do shell
numpy: permite calculos matematicos
matplotlib: permite plotar arrays 1D/2D
"""

import sys
import numpy as np
import matplotlib.pyplot as plt

"""
Altere as opcoes abaixo conforme o caso:
"""

for arg in sys.argv:
  print(arg)

arg_size = len(arg)

if arg_size == 47: # 47: tamanho do nome do arquivo (eg., RANKHIST4CPTECEPS.12hForecastFor2014111712.txt) 
  date = arg[33:43]
  lag = arg[18:22]
else:
  date = arg[32:42]
  lag = arg[18:21]

fig_out = 'RANKHIST4CPTECEPS.' + lag  + 'ForecastFor' + date + '.png'

"""
Nao alterar nada a partir desta linha!
"""

# Funcao para a leitura das tabelas (arquivos):
def read_table():

  # r_data e uma variavel global (utilizada dentro e fora deste escopo)
  # e contem toda a matriz lida
  global r_data

  r_data = np.loadtxt(arg) # arg e o nome do arquivo, vem da linha de comando

# Funcao para calculo da frequencia relativa:
def calc_rel_freq():

  # Variaveis globais:
  # bins: e o numero de membros do conjunto +1
  # f_rel: e a frequencia relativa (soma todos os elementos da
  # coluna e divide pelo numero de linhas)
  # m_data: e a "media" do histograma (vai ser a linha preta que aparece sobre o histograma)
  global bins, f_rel, m_data

  l_data = len(r_data) # numero de linhas da tabela

  c_data = len(r_data[0,:]) # numero de colunas da tabela

  sum_rows = r_data.sum(axis=0) # soma todas as linhas

  f_rel = sum_rows / l_data # calcula a frequencia relativa de cada coluna (bin)
 
  bins = np.arange(1, c_data+1) # calcula a quantidade de bins (bins = numero de membros +1)

  m_data = f_rel.sum(axis=0) / c_data # calcula a "media" do histograma

"""
Plota o Rank Histogram:
"""

def plot_rank_hist():

  # Define a fonte e a cor do titulo, legendas, eixos etc)
  font = {'fontname':'Serif', 'color':'#303030'}

  # Largura de cada bin (valores menores deixarao um espaco entre os bins)
  width=1

  # Titulo da figura
  plt.title('Rank Histogram for CPTEC EPS, valid for '+date+'\nTQ0126L028 - 15 Members, lag = '+lag, fontsize='18', y='1.0', **font)

  # Marcacoes e rotulo do eixo "x" (para um numero de bins diferente, alterar aqui)
  xlabels = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  plt.xticks(bins + (width / 2.0), xlabels, fontsize='14', **font)
  plt.xlabel('Rank', fontsize='16', **font)

  # Marcacoes e rotulo do eixo "y"
  plt.yticks(fontsize='14', **font)
  plt.ylabel('Relative Frequency', fontsize='16', **font)

  # Plota o histograma (na realidade, plota-se apenas os valores em forma de barras)
  plt.bar(bins, f_rel, width, color='#CCCCCC')

  # Define o dominio do eixo "y" (alterar caso necessario)
  axes = plt.gca()
  #axes.set_xlim([1,16])
  axes.set_ylim([0,0.5])

  # Plota a linha da "media" do histograma 
  plt.axhline(m_data, xmin=0, xmax=1, linewidth=2, color='black')

  # Salva a figura
  plt.savefig(fig_out)

#  plt.show()

"""
Chamada das Funcoes:
"""

def main():

  # Le o arquivo de input (tabela)
  read_table()

  # Calcula a frequencia relativa
  calc_rel_freq()

  # Faz a figura
  plot_rank_hist()

# Chama a funcao principal
# O script comeca aqui
main()
