#! /bin/bash -x 

# Script para gerar páginas em Markdown para o oensMB09
# Nota: este script gera páginas mais organizadas, com abas para variáveis diferentes e menus pul-down para as imagens
# @cfbastarz (20/03/2023)

Regs=(gl hn tr hs as)

mkdir ${1}

rm /tmp/options_*.txt

# Servidor proxy para mostrar as imagens via HTTPS
proxy=https://images.weserv.nl/?url=

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

Prods=(vies_rmse_acor scorecard)

export burl=http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/aval/scantec

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

do_vies_rmse_acor() {
cat << EOT1 > vies_rmse_acor.md        
${header}

=== "vies"

    === "agpl"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_vies_agpl.txt)    
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe1" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "psnm"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_vies_psnm.txt)    
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe2" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
        
    === "temp"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_vies_temp.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe3" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "umes"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe4').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_vies_umes.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe4" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "uvel"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe5').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_vies_uvel.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe5" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "vvel"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe6').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_vies_vvel.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe6" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "zgeo"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe7').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_vies_zgeo.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe7" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "rmse"

    === "agpl"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe8').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_rmse_agpl.txt)    
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe8" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "psnm"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe9').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_rmse_psnm.txt)    
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe9" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
        
    === "temp"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe10').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_rmse_temp.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe10" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "umes"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe11').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_rmse_umes.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe11" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "uvel"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe12').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_rmse_uvel.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe12" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "vvel"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe13').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_rmse_vvel.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe13" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "zgeo"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe14').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_rmse_zgeo.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe14" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

=== "acor"

    === "agpl"

        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe15').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_acor_agpl.txt)    
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe15" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "psnm"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe16').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_acor_psnm.txt)    
        </SELECT>
    
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe16" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
        
    === "temp"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe17').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_acor_temp.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe17" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "umes"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe18').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_acor_umes.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe18" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "uvel"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe19').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_acor_uvel.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe19" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "vvel"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe20').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_acor_vvel.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe20" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "zgeo"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe21').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_acor_zgeo.txt)    
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe21" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

!!! note "Sobre esta verificação"

    VIES, RMSE e ACOR representam valores que medem a habilidade das previsões obtidas a partir da análise controle e média das análises dos membros do conjunto. Nesse sentido, VIES é o desvio (ou bias) entre as previsões e a análise; RMSE é a raiz do erro quadrático médio entre as previsões e a análise e ACOR é a correlação entre as anomalias das previsões e análises em relação à climatologia do CFSR.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT1
}        

do_scorecard() {
cat << EOT2 > scorecard.md
${header}

=== "fc"

    === "acor"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe1').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_scorecard_fc_acor.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe1" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "rmse"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe2').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_scorecard_fc_rmse.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe2" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "vies"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe3').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_scorecard_fc_vies.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe3" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
=== "ganho"

    === "acor"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe4').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_scorecard_ganho_acor.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe4" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>
    
    === "rmse"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe5').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_scorecard_ganho_rmse.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe5" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

    === "vies"
    
        <form name="change">
        
        <SELECT NAME="options" ONCHANGE="document.getElementById('youriframe6').src = this.options[this.selectedIndex].value">
        <option>Selecionar imagem...</option>
        $(cat /tmp/options_scorecard_ganho_vies.txt)
        </SELECT>
        
        <div class="wrapper">
          <div class="h_iframe">
            <iframe name="iframe" id="youriframe6" src="${proxy}http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/assets/white_bkg_big.png" width="2" height="2" frameborder="0" allowfullscreen></iframe>
          </div>
        </div>

!!! note "Sobre esta verificação"

    Scorecards apresentam informações sobre a habilidade de previsão do modelo em relação à diferentes variáveis e níveis ao longo do tempo. No título de cada figura, constam os nomes de dois experimentos e os scorecards representam os ganhos ou perdas do segundo experimento em relação ao primeiro.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
EOT2
}        

##################################################

for reg in ${Regs[@]}
do        

for prod in ${Prods[@]}
do        

  echo ""
  echo "${prod} ${reg}"
  echo ""

  # Para cada imagem encontrada no servidor, formata e adiciona a linha ao arquivo

  ## Obtém uma lista das imagens do produto

  if [ ${prod} == vies_rmse_acor ]
  then
    wget -c ${burl}/${reg}/list_acor_agpl.txt
    wget -c ${burl}/${reg}/list_acor_psnm.txt
    wget -c ${burl}/${reg}/list_acor_temp.txt
    wget -c ${burl}/${reg}/list_acor_umes.txt
    wget -c ${burl}/${reg}/list_acor_uvel.txt
    wget -c ${burl}/${reg}/list_acor_vvel.txt
    wget -c ${burl}/${reg}/list_acor_zgeo.txt

    wget -c ${burl}/${reg}/list_rmse_agpl.txt
    wget -c ${burl}/${reg}/list_rmse_psnm.txt
    wget -c ${burl}/${reg}/list_rmse_temp.txt
    wget -c ${burl}/${reg}/list_rmse_umes.txt
    wget -c ${burl}/${reg}/list_rmse_uvel.txt
    wget -c ${burl}/${reg}/list_rmse_vvel.txt
    wget -c ${burl}/${reg}/list_rmse_zgeo.txt

    wget -c ${burl}/${reg}/list_vies_agpl.txt
    wget -c ${burl}/${reg}/list_vies_psnm.txt
    wget -c ${burl}/${reg}/list_vies_temp.txt
    wget -c ${burl}/${reg}/list_vies_umes.txt
    wget -c ${burl}/${reg}/list_vies_uvel.txt
    wget -c ${burl}/${reg}/list_vies_vvel.txt
    wget -c ${burl}/${reg}/list_vies_zgeo.txt
  else
    wget -c ${burl}/${reg}/list_scorecard_fc_acor.txt
    wget -c ${burl}/${reg}/list_scorecard_fc_rmse.txt
    wget -c ${burl}/${reg}/list_scorecard_fc_vies.txt

    wget -c ${burl}/${reg}/list_scorecard_ganho_acor.txt
    wget -c ${burl}/${reg}/list_scorecard_ganho_rmse.txt
    wget -c ${burl}/${reg}/list_scorecard_ganho_vies.txt
  fi

  mv list_*.txt /tmp/

  if [ ${prod} == vies_rmse_acor ]
  then
    ls /tmp/list_acor*.txt > /tmp/prod_list.txt
    ls /tmp/list_rmse*.txt >> /tmp/prod_list.txt
    ls /tmp/list_vies*.txt >> /tmp/prod_list.txt
  else
    ls /tmp/list_${prod}*.txt > /tmp/prod_list.txt
  fi

  for j in $(cat /tmp/prod_list.txt)
  do

    for i in $(cat ${j})
    do
  
      fig_url=${burl}/${reg}/${i}
  
      # Determina o nome do produto de acordo com o tipo
  
      if [ ${prod} == vies_rmse_acor ]
      then       
        prodn=$(echo $i | cut -c 1-4)
        datai=$(echo $i | cut -c 10-19)
        dataf=$(echo $i | cut -c 20-29)
        vname=$(echo $i | cut -c 30-33)
        if [ ${vname} == psnm ]
        then
          level="1000"
        else
          level=$(echo $i | cut -c 34-36)
        fi
          prod_name="${prodn} - ${vname} @ ${level} hPa (${datai}-${dataf})"
cat << EOF1 >> /tmp/options_${prodn,,}_${vname}.txt
        <option value="${proxy}${fig_url}">${prod_name}</option>
EOF1
      elif [ ${prod} == scorecard ]
      then
        prodn=$(echo $i | cut -c 1-9)
        tipo=$(echo $i | cut -c 11-12)
        if [ ${tipo} == FC ]
        then
          statn=$(echo $i | cut -c 14-17)
          exp1=$(echo $i | cut -c 19-24)
          exp2=$(echo $i | cut -c 26-31)
          datai=$(echo $i | cut -c 35-44)
          dataf=$(echo $i | cut -c 45-52)
        else
          tipo=$(echo $i | cut -c 11-15)
          statn=$(echo $i | cut -c 17-20)
          exp1=$(echo $i | cut -c 22-27)
          exp2=$(echo $i | cut -c 29-34)
          datai=$(echo $i | cut -c 38-47)
          dataf=$(echo $i | cut -c 48-55)
        fi
          prod_name="${prodn} - ${tipo}, ${statn}: ${exp1} X ${exp2} (${datai}-${dataf}00)"
cat << EOF1 >> /tmp/options_${prodn,,}_${tipo,,}_${statn,,}.txt
        <option value="${proxy}${fig_url}&w=800&h=800">${prod_name}</option>
EOF1
      fi
  
    done

  done

done

do_vies_rmse_acor
do_scorecard

rm /tmp/options_*.txt
rm /tmp/prod_list.txt

done

exit 0
