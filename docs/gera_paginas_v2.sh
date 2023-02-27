#! /bin/bash 

# Script para gerar páginas em Markdown para o oensMB09
# Nota: este script gera páginas mais organizadas, com abas para variáveis diferentes e menus pul-down para as imagens
# @cfbastarz (08/02/2023)

if [ $# -ne 3 ]
then
  echo "Uso: ./gera_paginas_v2.sh YYYYiMMiDDiHHi YYYYfMMfDDfHHf exp"
  echo "Exemplo: ./gera_paginas_v2.sh 2020120100 2020121500 gnu_egeon_m128p_p64p"
  exit 1
else
  export date=${1}
  export datef=${2}
  export exp=${3}
fi  

inctime=/usr/local/bin/inctime

mkdir ${3}

rm /tmp/list_*.txt
rm /tmp/options_*.txt

# Se hide_sections=true, esconde o toc, navigation e path das páginas
hide_sections=false
if [ ${hide_sections} == true ]
then
#  header="---
#hide:
#  - navigation
#  - toc
#  - path
#---"  
  header="---
hide:
  - toc
---"  
else
  header=""
fi

Prods=(spread spaguetti cluster probability probagr grh plumes perturbations chievol)

export burl=http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/${exp}/prod

date_diff() {
  data_anl=${1}
  data_fct=${2}
  
  yyyy_anl=$(echo ${data_anl} | cut -c 1-4)
    mm_anl=$(echo ${data_anl} | cut -c 5-6)
    dd_anl=$(echo ${data_anl} | cut -c 7-8)
    hh_anl=$(echo ${data_anl} | cut -c 9-10)
  
  yyyy_fct=$(echo ${data_fct} | cut -c 1-4)
    mm_fct=$(echo ${data_fct} | cut -c 5-6)
    dd_fct=$(echo ${data_fct} | cut -c 7-8)
    hh_fct=$(echo ${data_fct} | cut -c 9-10)
  
  data1=$(date -d "${yyyy_anl}-${mm_anl}-${dd_anl} ${hh_anl}:00:00 UTC" "+%s")
  data2=$(date -d "${yyyy_fct}-${mm_fct}-${dd_fct} ${hh_fct}:00:00 UTC" "+%s")
  
  if [ ${data2} -gt ${data1} ]
  then
    if [ ${hh_fct} -eq "0" -a ${hh_fct} -ne ${hh_anl} ]
    then 
      hh_diff=$(( 24-${hh_anl} ))
    else    
      hh_diff=$(( ${hh_fct}-${hh_anl} ))
    fi
  else
    if [ ${hh_anl} -eq "0" -a ${hh_fct} -ne ${hh_anl} ]
    then 
      hh_diff=$(( 24-${hh_fct} ))
    else    
      hh_diff=$(( ${hh_anl}-${hh_fct} ))
    fi
  fi  
    
  dd_diff=$(( (${data2}-${data1}) / 86400 ))
  
  hh_tot=$(( ${dd_diff}*24 + ${hh_diff} ))
}        

do_spread() {
cat << EOT1 > ${exp}/spread_${date}.md        
${header}

=== "geop"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_spread_geop.txt)    
    </SELECT>

    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe1" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

=== "psnm"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_spread_psnm.txt)    
    </SELECT>

    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe2" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>
    
=== "temp"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_spread_temp.txt)    
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe3" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

!!! note "Sobre este produto"

    Média e espalhamento são informações produzidas a partir de um conjunto de previsões. A média representa o somatório dos membros do conjunto para uma determinada variável e nível, dividido pelo número de membros. Espalhamento é desvio-padrão dos membros do conjunto em relação à média.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT1
}        

do_spaguetti() {
cat << EOT2 > ${exp}/spaguetti_${date}.md
${header}

=== "sptas"

    === "geop"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_spaguetti_sptas_geop.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe2" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "temp"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_spaguetti_sptas_temp.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe2" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
=== "sptgl"

    === "geop"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_spaguetti_sptgl_geop.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe3" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "temp"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe4').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_spaguetti_sptgl_temp.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe4" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

!!! note "Sobre este produto"

    Diagrama de espaguete é uma representação gráfica dos membros do conjunto. Os membros do conjunto são plotados na figura para algumas variáveis, níveis e quantidades com o objetivo de mostrar as diferentes situações simuladas pelo conjunto de previsões.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT2
}        

do_cluster() {
cat << EOT3 > ${exp}/cluster_${date}.md
${header}

=== "precpsnm"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_cluster_precpsnm.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe1" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>
    
=== "precvento"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_cluster_precvento.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe2" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>
    
=== "temp"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_cluster_temp.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe3" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>
    
=== "tems"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe4').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_cluster_tems.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe4" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>
    
=== "zgeo"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe5').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_cluster_zgeo.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe5" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

!!! note "Sobre este produto"

    Clusters ou agrupamentos são representações gráficas de um conjunto de previsões. Neste tipo de gráfico, os membros que apresentam previsões semelhantes entre si, considerando um determinado critério, são agrupados de forma que possam indicar as situações previstas mais prováveis.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT3
}        

do_probability() {
cat << EOT4 > ${exp}/probability_${date}.md
${header}

=== "probability"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_probability_prec.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>


!!! note "Sobre este produto"

    A previsão de probabilidade é obtida com base nos membros de previsão de uma determinada variável. Considerando-se a precipitação, pode-se calcular a probabilidade de ocorrência de precipitação acima de 1, 5, 10 e 20 mm (ou outros limiares), para cada ponto de grade do modelo.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT4
}        

do_probagr() {
cat << EOT5 > ${exp}/probagr_${date}.md
${header}

=== "probagr"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_probagr_prec_agric.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

!!! note "Sobre este produto"

    As previsões de probabilidade podem ser apresentadas de diferentes formas. Nesta forma, considera-se a pêntada da previsão de precipitação acima de 10 mm. Neste caso, isto é feito para três semanas consecutivas.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT5
}        

do_plumes() {
cat << EOT6 > ${exp}/plumes_${date}.md
${header}

=== "Norte"

    === "AC"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_ac.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe1" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "AM"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_am.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe3" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "AP"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe4').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_ap.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe4" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RO"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe21').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_ro.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe21" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RR"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe22').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_rr.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe22" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "PA"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe14').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_pa.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe14" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
=== "Nordeste"

    === "AL"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_al.txt)
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe2" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>


    === "BA"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe5').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_ba.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe5" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "CE"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe6').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_ce.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe6" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "MA"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe10').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_ma.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe10" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "TO"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe27').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_to.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe27" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "PB"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe15').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_pb.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe15" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "PE"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe16').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_pe.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe16" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "PI"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe17').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_pi.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe17" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RN"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe20').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_rn.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe20" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "SE"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe25').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_se.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe25" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Centro-Oeste"

    === "DF"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe7').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_df.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe7" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "GO"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe9').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_go.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe9" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "MS"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe12').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_ms.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe12" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "MT"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe13').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_mt.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe13" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Sudeste"

    === "ES"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe8').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_es.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe8" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "MG"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe11').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_mg.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe11" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RJ"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe19').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_rj.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe19" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "SP"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe26').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_sp.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe26" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Sul"

    === "PR"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe18').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_pr.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe18" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RS"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe23').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_rs.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe23" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "SC"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe24').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_plumes_sc.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe24" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Mundo"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe28').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_plumes_ww.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe28" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

!!! note "Sobre este produto"

    As plumas de probabilidade são meteogramas que apresentam informações sobre as previsões probabilísticas de diferentes variáveis meteorológicas. Diferentemente de um meteograma comum, os gráficos apresentados nesta página mostram também a incerteza associada à previsão ao longo do tempo para diferentes localidades.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT6
}        

do_perturbations() {
cat << EOT7 > ${exp}/perturbations_${date}.md
${header}

=== "temp"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_perturbations_temp.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe1" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>
    
=== "uvel"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_perturbations_uvel.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe2" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>
    
=== "vvel"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_perturbations_vvel.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe3" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

!!! notes "Sobre este produto"

    As perturbações iniciais são representações gráficas espaciais dos valores utilizados para a geração do conjunto iniciais de perturbações utilizadas pelo método MB09.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT7
}        

do_chievol() {
cat << EOT8 > ${exp}/chievol_${date}.md
${header}

=== "chievol"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_chievol.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

!!! note "Sobre este produto"

    O potencial de velocidade apresentado nesta página apresenta a evolução temporal da variável $\chi$ (velocidade potencial) no nível de 200 hPa. Esta informação permite verificar as áreas que indicam a convergência dos ventos em 200 hPa (subsidência abaixo, inibe a convecção - tons de verde) e as áreas que indicam a divergência dos ventos em 200 hPa (convergência abaixo, indica a convecção - tons de marrom).

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT8
}        

do_grh() {
cat << EOT9 > ${exp}/grh_${date}.md
${header}

=== "Norte"

    === "AC"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_ac.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe1" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "AM"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_am.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe3" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "AP"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe4').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_ap.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe4" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RO"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe21').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_ro.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe21" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RR"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe22').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_rr.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe22" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "PA"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe14').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_pa.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe14" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
=== "Nordeste"

    === "AL"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_al.txt)
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe2" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>


    === "BA"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe5').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_ba.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe5" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "CE"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe6').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_ce.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe6" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "MA"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe10').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_ma.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe10" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "TO"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe27').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_to.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe27" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "PB"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe15').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_pb.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe15" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "PE"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe16').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_pe.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe16" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "PI"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe17').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_pi.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe17" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RN"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe20').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_rn.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe20" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "SE"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe25').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_se.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe25" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Centro-Oeste"

    === "DF"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe7').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_df.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe7" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "GO"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe9').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_go.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe9" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "MS"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe12').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_ms.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe12" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "MT"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe13').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_mt.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe13" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Sudeste"

    === "ES"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe8').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_es.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe8" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "MG"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe11').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_mg.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe11" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RJ"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe19').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_rj.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe19" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "SP"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe26').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_sp.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe26" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Sul"

    === "PR"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe18').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_pr.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe18" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "RS"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe23').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_rs.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe23" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "SC"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe24').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_grh_sc.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe24" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "Mundo"

    <form name="change">
    
    <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe28').src = this.options[this.selectedIndex].value">
    <option>Selecionar imagem...</option>
    $(cat /tmp/options_grh_ww.txt)
    </SELECT>
    
    <div class="wrapper">
      <div class="h_iframe">
        <iframe name="iframe" id="youriframe28" src="http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
      </div>
    </div>

!!! note "Sobre este produto"

    Os grid history são meteogramas que apresentam informações sobre as previsões determinísticasde diferentes variáveis meteorológicas ao longo do tempo para diferentes localidades.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT9
}        

##################################################

while [ ${date} -le ${datef} ]
do        

for prod in ${Prods[@]}
do        

  echo ""
  echo "${date} ${prod}"
  echo ""

  # Para cada imagem encontrada no servidor, formata e adiciona a linha ao arquivo

  ## Obtém uma lista das imagens do produto

  wget -c ${burl}/${prod}/${date}/list.txt
  mv list.txt /tmp/list_${prod}.txt

  for i in $(cat /tmp/list_${prod}.txt)
  do

    fig_url=${burl}/${prod}/${date}/${i}

    # Determina o nome do produto de acordo com o tipo

    if [ ${prod} == spread ]
    then       
      vname=$(echo $i | cut -c 1-4)
      if [ ${vname} == psnm ]
      then
        level="1000"
        datai=$(echo $i | cut -c 5-14)
        dataf=$(echo $i | cut -c 15-24)
        date_diff ${datai} ${dataf}
        prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF1 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF1
      elif [ ${vname} == geop ]
      then
        level=$(echo $i | cut -c 5-7)
        datai=$(echo $i | cut -c 8-17)
        dataf=$(echo $i | cut -c 18-27)
        date_diff ${datai} ${dataf}
        prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF2 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF2
      elif [ ${vname} == temp ]
      then
        level=$(echo $i | cut -c 5-7)
        datai=$(echo $i | cut -c 8-17)
        dataf=$(echo $i | cut -c 18-27)
        date_diff ${datai} ${dataf}
        prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF3 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF3
      fi
    fi

    if [ ${prod} == spaguetti ] 
    then
      tipo=$(echo $i | cut -c 1-5)
      if [ ${tipo} == sptgl ]
      then        
        vname=$(echo $i | cut -c 6-9)
        level_tmp=$(echo $i | cut -c 10-12)
        if [ ${vname} == temp ] && [ ${level_tmp} == 100 ]
        then
          level=$(echo $i | cut -c 10-13)
          datai=$(echo $i | cut -c 14-23)
          dataf=$(echo $i | cut -c 24-33)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${tipo} ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF4 >> /tmp/options_${prod}_${tipo}_${vname}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF4
        elif [ ${vname} == temp ] && [ ${level_tmp} != 100 ]
        then        
          level=$(echo $i | cut -c 10-12)        
          datai=$(echo $i | cut -c 13-22)
          dataf=$(echo $i | cut -c 23-32)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${tipo} ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF5 >> /tmp/options_${prod}_${tipo}_${vname}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF5
        elif [ ${vname} == geop ]
        then        
          level=$(echo $i | cut -c 10-12)        
          datai=$(echo $i | cut -c 13-22)
          dataf=$(echo $i | cut -c 23-32)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${tipo} ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF6 >> /tmp/options_${prod}_${tipo}_${vname}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF6
        fi  
      elif [ ${tipo} == sptas ]
      then        
        vname=$(echo $i | cut -c 6-9)
        level_tmp=$(echo $i | cut -c 10-12)
        if [ ${vname} == temp ] && [ ${level_tmp} == 100 ]
        then
          level=$(echo $i | cut -c 10-13)
          datai=$(echo $i | cut -c 14-23)
          dataf=$(echo $i | cut -c 24-33)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${tipo} ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF7 >> /tmp/options_${prod}_${tipo}_${vname}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF7
        elif [ ${vname} == temp ] && [ ${level_tmp} != 100 ]
        then        
          level=$(echo $i | cut -c 10-12)        
          datai=$(echo $i | cut -c 13-22)
          dataf=$(echo $i | cut -c 23-32)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${tipo} ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF8 >> /tmp/options_${prod}_${tipo}_${vname}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF8
        elif [ ${vname} == geop ]
        then        
          level=$(echo $i | cut -c 10-12)        
          datai=$(echo $i | cut -c 13-22)
          dataf=$(echo $i | cut -c 23-32)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${tipo} ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF9 >> /tmp/options_${prod}_${tipo}_${vname}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF9
        fi  
      fi  
    fi

    if [ ${prod} == cluster ] 
    then
      vname=$(echo $i | cut -c 8-11)
      if [ ${vname} == prec ]
      then
        vname=$(echo $i | cut -c 8-15)
        datai=$(echo $i | cut -c 20-29)
        dataf=$(echo $i | cut -c 30-39)
        if [ ${vname} == precpsnm ]
        then
          vname=$(echo $i | cut -c 8-15)
          level=$(echo $i | cut -c 16-19)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF10 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF10
        elif [ ${vname} == precvent ]
        then       
          vname=$(echo $i | cut -c 8-16)
          level=$(echo $i | cut -c 17-19)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF11 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF11
        fi  
      elif [ ${vname} == temp ]
      then
        level=$(echo $i | cut -c 12-14)
        if [ ${level} == 100 ]
        then
          level=$(echo $i | cut -c 12-15)
          datai=$(echo $i | cut -c 16-25)
          dataf=$(echo $i | cut -c 26-35)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF12 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF12
        else        
          datai=$(echo $i | cut -c 15-24)
          dataf=$(echo $i | cut -c 25-34)
          date_diff ${datai} ${dataf}
          prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF13 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF13
        fi
      elif [ ${vname} == zgeo ]
      then
        level=$(echo $i | cut -c 12-14)
        datai=$(echo $i | cut -c 15-24)
        dataf=$(echo $i | cut -c 25-34)
        date_diff ${datai} ${dataf}
        prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF14 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF14
      elif [ ${vname} == tems ] 
      then
        level=$(echo $i | cut -c 12-15)
        datai=$(echo $i | cut -c 16-25)
        dataf=$(echo $i | cut -c 26-35)
        date_diff ${datai} ${dataf}
        prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF15 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF15
      fi              
    fi

    if [ ${prod} == probability ] 
    then
      vname=$(echo $i | cut -c 1-4)
      datai=$(echo $i | cut -c 5-14)
      dataf=$(echo $i | cut -c 15-24)
      date_diff ${datai} ${dataf}
      prod_name="${prod} - ${vname}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF16 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF16
    fi  

    if [ ${prod} == probagr ] 
    then
      vname=$(echo $i | cut -c 1-10)
      week=$(echo $i | cut -c 12-17)
      if [[ ${week} == large5 ]]
      then
        wname="semana 1"
        prod_name="${prod} - ${vname} (${wname})"
cat << EOF17 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF17
      elif [[ ${week} == large1 ]]
      then
        week=$(echo $i | cut -c 12-18)
        if [ ${week} == large10 ]
        then
          wname="semana 2"
          prod_name="${prod} - ${vname} (${wname})"
cat << EOF18 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF18
        elif [ ${week} == large15 ]
        then         
          wname="semana 3"
          prod_name="${prod} - ${vname} (${wname})"
cat << EOF19 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF19
        else
          continue
        fi
      else
        continue
      fi  
    fi

    if [ ${prod} == plumes ] 
    then
      estado=$(echo $i | awk -F "/" '{print $2}')
      local_coords=$(echo $i | awk -F "/" '{print $3}' | awk -F "." '{print $1}')
      lc_line_number=$(cat -n plumes_local_coords.txt | grep ${local_coords} | awk -F " " '{print $1}')
      local_name=$(sed -n ${lc_line_number}p plumes_local_name.txt)
      prod_name="${prod} - ${estado}: ${local_name} - ${local_coords} (${datai} - ${dataf})"
cat << EOF20 >> /tmp/options_${prod}_${estado,,}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF20
    fi

    if [ ${prod} == grh ] 
    then
      estado=$(echo $i | awk -F "/" '{print $2}')
      local_coords=$(echo $i | awk -F "/" '{print $3}' | awk -F "." '{print $1}')
      lc_line_number=$(cat -n grh_local_coords.txt | grep ${local_coords} | awk -F " " '{print $1}')
      local_name=$(sed -n ${lc_line_number}p grh_local_name.txt)
      prod_name="${prod} - ${estado}: ${local_name} - ${local_coords} (${datai} - ${dataf})"
cat << EOF20 >> /tmp/options_${prod}_${estado,,}.txt
        <option value="${fig_url}">${prod_name}</option>
EOF20
    fi

    if [ ${prod} == perturbations ] 
    then
      vname=$(echo $i | cut -c 14-17)
      level=$(echo $i | cut -c 18-20)
      datai=$(echo $i | cut -c 22-31)
      dataf=$(echo $i | cut -c 32-41)
      date_diff ${datai} ${dataf}
      prod_name="${prod} - ${vname} @ ${level}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF21 >> /tmp/options_${prod}_${vname}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF21
    fi

    if [ ${prod} == chievol ] 
    then
      datai=$(echo $i | cut -c 9-18)
      dataf=$(echo $i | cut -c 19-28)
      date_diff ${datai} ${dataf}
      prod_name="${prod}, fct ${hh_tot} h (${datai}-${dataf})"
cat << EOF22 >> /tmp/options_${prod}.txt
    <option value="${fig_url}">${prod_name}</option>
EOF22
    fi

  done

done

do_spread
do_spaguetti
do_cluster
do_probability
do_probagr
do_grh
do_plumes
do_perturbations
do_chievol

rm /tmp/list_*.txt
rm /tmp/options_*.txt

  date=$(${inctime} ${date} +1d %y4%m2%d2%h2)

done

exit 0
