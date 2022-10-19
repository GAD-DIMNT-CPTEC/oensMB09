# Métricas de Verificação da Previsão Sub-sazonal

Este conjunto de scripts calculam as métricas de verificação para as previsões sub-sazonais do CPTEC/INPE geradas com o modelo BAM V1.2.0. 

## Verificação das Previsões Determinísticas de Precipitação

O script `cal_verification_prec.gs` está preparado para calcular as seguintes quantidades estatísticas para o campo de precipitação:

* Média do ensemble;
* Desvio-padrão;
* Correlação de Pearson;
* Raiz do erro quadrático médio;
* Erro quadrático médio;
* Erro de fase;
* Erro de amplitude;
* Viés condicional;
* Viés não-condicional;
* Skill Score do erro quadrático médio.

Além dessas métricas, os scripts calculam também os termos das equações 11 e 12 do trabalho de [Murphy (1988)](https://journals.ametsoc.org/view/journals/mwre/116/12/1520-0493_1988_116_2417_ssbotm_2_0_co_2.xml)[^1].

## Scripts

Neste repositório, estão disponíveis dois scripts principais:

* `probabilistic_verification.r`: verificações probabilísticas, depende dos scripts:
  - `roc.area.eurobrisa.r`: cálculo do ROC (Receiver Operating Characteristic);
  - `attributediagrboot_b.r`: cálculo do diagrama de confiabilidade;
* `deterministic_verification.gs`: verificações determinísticas (e.g., média e desvio-padrão do ensemble, correlação de Pearson, raiz do erro quadrático médio, erro quadrático médio, erros de fase e amplitude, viés condicional e não-condicional, skill score do erro quadrtático médio);
* `graf_esp.gs`: gráfico de espaguete do ensemble.

### Requisitos

1. Instalação do GrADS:
  - No Ubuntu e derivados, instale o pacote `grads`:
    ```
    $ sudo apt install grads
    ```
  - Em seguida, instale baixe os scripts adicionais do Grads, e adicione a variável `GASCRP` ao seu `~/.bashrc` ou `~/.profile`:
    ```
    $ cd ~/Downloads
    $ mkdir grads_scripts
    $ cd grads_scripts
    $ ncftp ftp://cola.gmu.edu/grads/scripts
    $ ncftp /grads> get *
    $ ncftp /grads> quit
    $ echo "export GASCRP=$HOME/Downloads/grads_scripts" >> ~/.bashrc
2. Instalação do R:
  - No Ubuntu e derivados, instale o pacote `r-base`:
    ```
    $ sudo apt install r-base
    ```
  - Em seguida, abra o `R` e instale o pacote `verification`:
    ```
    $ R
    > install.packages('verification')
    ```

### Dados

Os dados do modelo BAM, utilizados para testar os scripts deste repositótio, representam as anomalias de 30 dias geradas para as quatro primeiras quartas-feiras do mês de junho de 1999 a 2018. Desse modo, os dados apresentam 80 anomalias mensais (20 anos x 4 previsões em cada mês). O script `cal_verification_prec.gs` está preparado para plotar apenas a região global e não depende de outros scripts para plotar as figuras.

### Uso

Antes de utilizar os scripts, baixe o pacote com os dados de teste no endereço ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/EPS/verification_s2s_bam12.tar.gz e, em seguida, descompacte. 

Para a utilização dos scripts deste repositório, os dados e os scripts deverão estar organizados da seguinte forma:

```
$ tree
.
├── BAM12_JUN_MNTH01.ctl
├── GPCP_JUN_MNTH01.ctl
├── attributediagrboot_b.r
├── deterministic_verification.gs
├── graf_esp.gs
├── JUN
│   ├── BAM12_ANOMALY_01N_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_01P_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_02N_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_02P_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_03N_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_03P_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_04N_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_04P_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_05N_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_05P_JUN_MNTH01.bin
│   ├── BAM12_ANOMALY_EIT_JUN_MNTH01.bin
│   ├── GPCP_ANOMALY_JUN_MNTH01.bin
├── probabilistic_verification.r
├── README.md
├── roc.area.eurobrisa.r
└── verification_s2s_bam12.tar.gz
```

Para utilizar o script `deterministic_verification.gs`, basta executar o comando:

```
$ grads -blc "run deterministic_verification.gs"
```

Com a finalização do script, serão geradas as seguintes figuras:

Para utilizar o script `probabilistic_verification.r`, basta executar o comando:

**Atenção:** Assumindo os valores padrão para o bootstrap (utilizado para o cálculo do intervalo de confiança utilizado pelo diagrama de ROC e outras estatísticas), o script `probabilistic_verification.r` levará muito tempo para ser finalizado. Considere alterar o valor da variável `n.boot` de 1000 para 1.

```
$ Rscript probabilistic_verification.r
```

Com a finalização do script, serão geradas as seguintes figuras:


## Créditos

Os scripts são de autoria dos pesquisadores Caio Augusto dos Santos Coelho ([webpage](https://www.cptec.inpe.br/pesquisadores/caio.coelho/) | [lattes](http://lattes.cnpq.br/4978912302419377)) e Bruno dos Santos Guimarães ([lattes](http://lattes.cnpq.br/0307886068495121)) e estão aqui organizados para uso e referência. 

[^1]: MURPHY, Allan H.: Skill scores based on the mean square error and their relationships to the correlation coefficient. Monthly weather review, v. 116, n. 12, p. 2417-2424, 1988.
