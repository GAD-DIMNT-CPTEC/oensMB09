# Método de Perturbação baseado em EOFs para o Brazilian global Atmospheric Model - BAM

* Versão: V2.2.0 

## Notas da Versão:

* Resolução: TQ0126L028 (~ 100Km de resolução espacial horizontal, com 28 níveis sigma)
* Níveis de pressão pós-processados (hPa): 1000, 925, 850, 700, 500, 300, 250, 200, 50
* Condição inicial atmosférica: NCEP (TQ0154L064), suavização de topografia (SmoothTopo=T, SmthPerCut=0.18, Iter=100)
* 15 membros TQ0126L028 (7 perturbações EOF somadas + 7 perturbações EOF subtraídas + 1 controle)
* Transição para o XC50 e atualização para a versão operacional do modelo BAM (V1.2.2), inclusive com o modelo de superfície IBIS

Nesta versão (V2.2.0), a componente do modelo (pre/model/pos) foi atualizada para a versão do modelo BAM operacional determinístico em coordenada sigma, em uso no XC50. Na etapa de previsão (model), os arquivos de ozônio e traçadores para cada membro, devem ser lincados para o arquivo do membro controle. Por exemplo:

```
model/datain:
TRAC[0..7,R,P,N]YYYYMMDDHHS.grd.G00192L028 -> TRACSMTYYYYMMDDHHS.grd.G00192L028
OZON[0..7,R,P,N]YYYYMMDDHHS.grd.G00192L028 -> OZONSMTYYYYMMDDHHS.grd.G00192L028
```

## Ambiente de compilação de referência no XC50

**Apenas para os módulos do método de perturbação e produtos:**

* Compilador GNU (gcc/7.2.0)

```
$ module swap PrgEnv-cray/6.0.4 PrgEnv-gnu
$ module unload craype-x86-skylake
```

```
$ module list
Currently Loaded Modulefiles:
  1) modules/3.2.10.6                                  14) Base-opts/2.4.123-6.0.5.0_11.2__g6460790.ari
  2) alps/6.5.28-6.0.5.0_18.6__g13a91b6.ari            15) gcc/7.2.0
  3) nodestat/2.3.78-6.0.5.0_8.13__gbe57af8.ari        16) craype-network-aries
  4) sdb/3.3.751-6.0.5.0_20.25__gb936019.ari           17) craype/2.5.13
  5) udreg/2.3.2-6.0.5.0_13.12__ga14955a.ari           18) cray-mpich/7.7.0
  6) ugni/6.0.14-6.0.5.0_16.9__g19583bb.ari            19) pbs/default
  7) gni-headers/5.0.12-6.0.5.0_2.15__g2ef1ebc.ari     20) cray-python/3.6.1.1
  8) dmapp/7.1.1-6.0.5.0_49.8__g1125556.ari            21) cray-libsci/17.12.1
  9) xpmem/2.2.4-6.0.5.0_4.8__g35d5e73.ari             22) pmi/5.0.13
 10) llm/21.3.467-6.0.5.0_25.1__g4c6f300.ari           23) atp/2.1.1
 11) nodehealth/5.5.7-6.0.5.0_9.8__g7732908.ari        24) rca/2.2.16-6.0.5.0_15.34__g5e09e6d.ari
 12) system-config/3.5.2545-6.0.5.0_5.1__g8a8b20f.ari  25) perftools-base/7.0.0
 13) sysadm/2.4.125-6.0.5.0_16.13__g98e00a9.ari        26) PrgEnv-gnu/6.0.4
```

## Compilação

Módulos do método de perturbação (considerando o ambiente de compilação configurado):

```
cd oensMB09
./config_spcon.sh configure TQ0126L028 gnu
make comp=gnu
```

## Produtos

```
cd oensMB09/produtos
make
```

## Uso

Vide o arquivo `oensMB09/run/README`.

**Observação:** esta distribuição não inclui o modelo BAM (pre/model/pos/grh), sendo este necessário para a completa utilização da suíte.

## Contato

Solicitações sobre alterações e correções no método de perturbação, devem ser feitas por meio do Redmine no projeto https://projetos.cptec.inpe.br/projects/spconcptec/issues marcando a versão atual V2.2.0. Outras dúvidas e informações podem ser solicitadas através do email carlos.bastarz@inpe.br.