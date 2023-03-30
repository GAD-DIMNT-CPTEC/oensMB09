# oensMB09

## Método de Perturbação baseado em EOFs para o Brazilian global Atmospheric Model - BAM

O Sistema de Previsão por Conjunto (SPCON) Global do CPTEC é um sistema composto pelo modelo de circulação geral da atmosfera desenvolvido pelo CPTEC e um módulo de perturbação da condição inicial baseado em Funções Ortogonais Empíricas (EOF, em inglês). 

## Notas da Versão

* Versão atual: V2.2.1 
* Resolução: TQ0126L028 (~ 100Km de resolução espacial horizontal, com 28 níveis sigma)
* Níveis de pressão pós-processados (hPa): 1000, 925, 850, 700, 500, 300, 250, 200, 50
* Condição inicial atmosférica: NCEP (TQ0154L064), suavização de topografia (SmoothTopo=T, SmthPerCut=0.18, Iter=100)
* 15 membros TQ0126L028 (7 perturbações EOF somadas + 7 perturbações EOF subtraídas + 1 controle)
* Transição para o Egeon, testado com o modelo BAM (V1.2.1), inclusive com o modelo de superfície IBIS

Nesta versão (V2.2.1), a componente do modelo (pre/model/pos) foi atualizada para a versão do modelo BAM determinístico em coordenada sigma, compilado para uso na Egeon. Na etapa de previsão (model), os arquivos de ozônio e traçadores para cada membro, devem ser lincados para o arquivo do membro controle. Por exemplo:

```
model/datain:
TRAC[0..7,R,P,N]YYYYMMDDHHS.grd.G00192L028 -> TRACSMTYYYYMMDDHHS.grd.G00192L028
OZON[0..7,R,P,N]YYYYMMDDHHS.grd.G00192L028 -> OZONSMTYYYYMMDDHHS.grd.G00192L028
```

## Ambiente de compilação e uso

Para instruções de compilação e uso, vide informações no site [https://gad-dimnt-cptec.github.io/oensMB09](https://gad-dimnt-cptec.github.io/oensMB09).
