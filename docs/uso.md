# Uso

Os procedimentos para o uso do método de perturbação oensMB09 em conjunto com o modelo BAM bem como os procedimentos para a geração dos produtos é apresentado nas seções a seguir. Os procedimentos apresentados são os mesmos para qualquer versão dos códigos, mas há diferenças quanto à especificação do número de processadores a serem utilizados nos processos que demandam processamento paralelo para as máquina XC50 e Egeon. Os procedimentos a seguir, consideram os requisitos necessários para a realização de toda a suíte para a resolução TQ0126L028 e um data de análise arbitrária 2020062000 (20 de Junho de 2020).

!!! warning "Atenção"

    Na versão V2.2.1, há opções específicas no arquivo [`EnvironmentVariablesMCGA`](https://github.com/GAD-DIMNT-CPTEC/oensMB09/blob/master/run/EnvironmentalVariablesMCGA) que devem ser ajustadas antes de se iniciar as simulações. Estas opções devem ser consideradas por meio das seguintes variáveis:

    * `SEND_TO_FTP`: valor booleano que indica se as figuras dos produtos devem ser enviadas para algum servidor de imagem. Valor padrão `SEND_TO_FTP=false`;
    * `FTP_ADDRESS`: endereço do host para onde devem ser enviadas as imagens, caso `SEND_TO_FTP=true`. Veja o arquivo [`EnvironmentVariablesMCGA`](https://github.com/GAD-DIMNT-CPTEC/oensMB09/blob/master/run/EnvironmentalVariablesMCGA) para mais informações;
    * `convert`: indica o local do script `convert` a ser utilizado para realizar o crop das imagens dos produtos. Valor padrão `convert=/opt/spack/opt/spack/linux-rhel8-zen2/gcc-11.2.0/imagemagick-7.0.8-7-46pk2gowitva2xtvogtfg2ubug2d6v2w/bin/convert`;
    * `USE_INTEL`: valor booleano que indica se o compilador Intel foi utilizado para criar os executáveis do método de perturbação, modelo e produtos. Valor padrão `USE_INTEL=true`;
    * `USE_SINGULARITY`: valor booleano que indica se os executáveis a serem utilizados (método de perturbação, modelo e produtos) serão fornecidos por um container. Nesse caso, o MPI também será fornecido pelo container. Valor padrão `USE_SINGULARITY=false`;
    * `HOMEBIND`: string que indica o caminho do `$HOME` do usuário a ser passado para dentro do container. Útil caso arquivos que estejam no `$HOME` do usuário precisem ser utilizados dentro do container;
    * `WORKBIND`: string que indica o caminho do `$WORK_HOME` do usuário a ser passado para dentro do container. Útil caso arquivos que estejam no `$WORK_HOME` do usuário precisem ser utilizados dentro do container;
    * `SIFIMAGE`: nome do arquivo _Singularity Image File_ (SIF) a ser utilizado caso `USE_SINGULARITY=true`;
    * `SIFOENSMB09BIN`: string com o caminho dentro do container onde se encontram os executáveis do método de perturbação e produtos do oensMB09.

## Pré-processamento

Nesta parte inicial, são realizadas todas as etapas relacionadas com o pré-processamento do modelo BAM. É nesta etapa em que são preparadas as condições inicial (controle) e de contorno do modelo.

```markdown title="Realização do pré-processamento da primeira análise controle"
./runPre 126 28 2020062000 NMC 1 T F 574 64
```

## Realização do membro controle (48 horas)

Com as condições iniciais e de contorno prontas, a próxima etapa envolve a realização do membro controle por um período de 48 horas. As previsões obtidas nesta etapa serão utilizadas posteriormente pelo método de perturbação apenas.

```markdown title="Realização do membro controle a partir da primeira análise (nesta primeira integração, são apenas 48 horas 3/3h - deverão haver também previsões para até 15 dias a partir do membro controle)"
./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020062200 CTR 2 1
```

!!! nota "Nota"

    O monitoramento do job durante a realização desta etapa é feita com o seguinte comando:

    === "XC50"

        ```
        qstat -u $USER
        ```

    === "Egeon"

        ```
        squeue --me
        ```

## Método de perturbação

Nesta parte, são realizadas as várias etapas do método de perturbação do oensMB09.

### recanl

```markdown title="Recomposição dos coeficientes espectrais da análise para ponto de grade"
./run_recanl.sh TQ0126L028 SMT ANLSMT 2020062000
```

### rdpert

```markdown title="Gera e soma as perturbações randômicas à análise controle"
./run_rdpert.sh TQ0126L028 SMT YES 2020062000 7
```

### decanl

```markdown title="Decomposição das análises perturbadas em ponto de grade para coeficientes espectrais"
./run_decanl.sh TQ0126L028 SMT YES 2020062000 7
```

### model

```markdown title="Realização das previsões a partir das análises perturbadas para uso na análise de EOF"
./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020062200 RDP 2 7
```

!!! nota "Nota"

    O monitoramento do job durante a realização dos membros é feita com o seguinte comando:

    === "XC50"

        ```
        qstat -J -t -u $USER @eslogin13
        ```

    === "Egeon"

        ```
        squeue --me
        ```

### recfct

```markdown title="Recomposição para ponto de grade das previsões realizadas a partir da análise controle"
./run_recfct.sh TQ0126L028 CTR 2020062000
```

```markdow title="Recomposição para ponto de grade das previsões realizadas a partir das análises perturbadas randomicamente"
./run_recfct.sh TQ0126L028 7 2020062000
```

!!! nota "Nota"

    O monitoramento do job durante a realização dos membros é feita com o seguinte comando:

    === "XC50"

        ```
        qstat -J -t -u $USER @eslogin13
        ```

    === "Egeon"

        ```
        squeue --me
        ```

### eof

```markdown title="Realização da análise de EOF para gerar as perturbações ótimas a serem utilizadas na composição final dos membros do conjunto"
./run_eof.sh TQ0126L028 7 YES 2020062000 SMT
```

### deceof

```markdown title="Composição do arquivo de análise a partir das perturbações EOF"
./run_deceof.sh TQ0126L028 EOF YES 2020062000 7 SMT
```

## Realização dos membros do conjunto (360 horas)

```markdown title="Realização do membro controle a partir da primeira análise (nesta primeira integração, são apenas 48 horas 3/3h - deverão haver também previsões para até 15 dias a partir do membro controle)"
./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020070500 NMC 2 1
```

```markdown title="Realização das previsões a partir do conjunto de análises com perturbações ótimas (análises prefixo N - 15 dias 6/6h)"
./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020070500 NPT 2 7
```

```markdown title="Realização das previsões a partir do conjunto de análises com perturbações ótimas (análises prefixo P - 15 dias 6/6h)"
./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020070500 PPT 2 7
```

## Pós-processamento

Na etapa de pós-processamento, as previsões espectrais do modelo são recompostas para a grade no formato Grib. É a partir destes arquivos que os produtos do oensMB09 são gerados.

### Realização dos membros do conjunto (360 horas)

```markdown title="Realização do pós-processamento das previsões produzidas a partir da análise controle"
./run_pos.sh 48 4 6 TQ0126L028 2020062000 2020070500 NMC
```

```markdown title="Realização do pós-processamento do conjunto de previsões realizado a partir do conjunto de análises com perturbações ótimas (análises prefixo N)"
./run_pos.sh 48 4 6 TQ0126L028 2020062000 2020070500 NPT 7
```

```markdown title="Realização do pós-processamento do conjunto de previsões realizado a partir do conjunto de análises com perturbações ótimas (análises prefixo P)"
./run_pos.sh 48 4 6 TQ0126L028 2020062000 2020070500 PPT 7
```

```markdown title="Execução do programa gribmap a partir dos arquivos pós-processados do BAM para os prefixos NMC, NPT e PPT"
./run_gribmap.sh TQ0126L028 2020062000 1 NMC
./run_gribmap.sh TQ0126L028 2020062000 7 NPT
./run_gribmap.sh TQ0126L028 2020062000 7 PPT
```

### Média do Conjunto

A média representa o somatório dos membros do conjunto para uma determinada variável e nível, dividido pelo número de membros.

```markdown title="Cálculo da média do conjunto (é necessário lincar todos os arquivos dos membros (ctl,grb) para fora dos diretórios dos membros)"
./run_ensmed.sh TQ0126L028 2020062000 15 NMC 7
```

!!! note "Nota"

    Para lincar os referidos arquivos, utilizar os scripts (revisar os scripts e copiá-los para o diretório `oensMB09/pos/dataout/TQ0126L028`):
    
    * `oensMB09/run/utils_pos/cria_link.sh`: faz os links simbólicos dos arquivos de análise como nomes dos arquivos de previsão de cada membro para uma data específica;
    * `oensMB09/run/utils_pos/exp_links.sh`: faz os links simbólicos de todos os arquivos (ctl, grb e idx) para fora dos diretórios dos membros.

### Espalhamento

Espalhamento é desvio-padrão dos membros do conjunto em relação à média.

!!! note "Nota"

    Nesta etapa são geradas as figuras do produto "Média e Espalhamento".

```markdown title="Cálculo do espalhamento do conjunto e plotagem das figuras com a média e o espalhamento do conjunto"
./run_spread.sh TQ0126L028 2020062000 15 NMC 7
```

Ao final do processo, deverão ser obtidas 244 figuras do tipo:

<figure markdown>
![Exemplo figura com a média e o espalhamento](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/spread/2020120100/temp85020201201002020120118.png){width=600}
Figura - Exemplo de figura com a média e o espalhamento do oensMB09.
</figure>

### Diagrama de Espaguete

Diagrama de espaguete é uma representação gráfica dos membros do conjunto. Os membros do conjunto são plotados na figura para algumas variáveis, níveis e quantidades com o objetivo de mostrar as diferentes situações simuladas pelo conjunto de previsões.

```markdown title="Espaguetes da temperatura absoluta em 1000 e 850hPa e da altura geopotencial em 500hPa, para a América do Sul e hemisférios norte e sul"
./run_spaguetti.sh TQ0126L028 2020062000 15 NMC 7
```

Ao final do processo, deverão ser obtidas 112 figuras do tipo:

<figure markdown>
![Exemplo figura com o diagrama de espaguete](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/spaguetti/2020120100/sptastemp100020201201002020120100.png){width=550}
Figura - Exemplo de figura com o diagrama de espaguete.
</figure>

### Análise de Agrupamento

Clusters ou agrupamentos são representações gráficas de um conjunto de previsões. Neste tipo de gráfico, os membros que apresentam previsões semelhantes entre si, considerando um determinado critério, são agrupados de forma que possam indicar as situações previstas mais prováveis.

```markdown title="Clusters (é necessário que os arquivos idx sejam gerados fora dos diretórios dos membros)"
./run_cluster.sh TQ0126L028 2020062000 15 NMC 7
```

Ao final do processo deverão ser obtidas 610 figuras do tipo:

<figure markdown>
![Exemplo figura com a análise de agrupamento](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/cluster/2020120100/clustertemp100020201201002020120106.png){width=500}
Figura - Exemplo de figura com a análise de agrupamento.
</figure>

### Previsão de Probabilidade de Precipitação

A previsão de probabilidade é obtida com base nos membros de previsão de uma determinada variável. Considerando-se a precipitação, pode-se calcular a probabilidade de ocorrência de precipitação acima de 1, 5, 10 e 20 mm (ou outros limiares), para cada ponto de grade do modelo.

```markdown title="Probabilidades de precipitação para 1, 5, 10 e 20mm"
./run_probability.sh TQ0126L028 2020062000 15 NMC 7
```

Ao final do processo deverão ser obtidas 57 figuras do tipo:

<figure markdown>
![Exemplo figura com a previsão de probabilidade de precipitação](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/probability/2020120100/prec20201201002020120200.png){width=600}
Figura - Exemplo de figura com a previsão de probabilidade de precipitação.
</figure>

### Probabilidade de Precipitação Acumulada

As previsões de probabilidade podem ser apresentadas de diferentes formas. Nesta forma, considera-se a pêntada da previsão de precipitação acima de 10 mm. Neste caso, isto é feito para três semanas consecutivas.

```markdown title="Probabilidades de acumulo precipitação acima de 10mm"
./run_probagr.sh TQ0126L028 2020062000 15 NMC 7
```

Ao final do processo deverão ser obtidas 3 figuras do tipo:

<figure markdown>
![Exemplo figura com a probabilidade de precipitação acumulada](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/probagr/2020120100/prec_agric_large10.png){width=600}
Figura - Exemplo de figura com a probabilidade de precipitação acumulada.
</figure>

### Grid History

Os grid history são meteogramas que apresentam informações sobre as previsões determinísticasde diferentes variáveis meteorológicas ao longo do tempo para diferentes localidades.

=== "XC50"

    ```markdown title="Grid history (necessário para o cálculo das plumas), executar para NMC, NPT e PPT (pode-se plotar os meteogramas nesta etapa, mas apenas para o membro controle)"
    ./run_grh.sh 1 TQ0126L028 2020062000 2020070500 NMC 1
    ./run_grh.sh 1 TQ0126L028 2020062000 2020070500 NPT 7
    ./run_grh.sh 1 TQ0126L028 2020062000 2020070500 PPT 7
    ```

=== "Egeon"

    ```markdown title="Grid history (necessário para o cálculo das plumas), executar para NMC, NPT e PPT (pode-se plotar os meteogramas nesta etapa, mas apenas para o membro controle)"
    ./run_grh.sh 4 TQ0126L028 2020060100 2020061600 NMC 1
    ./run_grh.sh 4 TQ0126L028 2020060100 2020061600 NPT 7
    ./run_grh.sh 4 TQ0126L028 2020060100 2020061600 PPT 7
    ```

Ao final do processo deverão ser obtidas 971 figuras do tipo:

<figure markdown>
![Exemplo figura com o grid history](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/grh/2020120100/./SP/04845W2351S.png){width=500}
Figura - Exemplo de figura com o grid history.
</figure>

### Plumas de Probabilidades

As plumas de probabilidade são meteogramas que apresentam informações sobre as previsões probabilísticas de diferentes variáveis meteorológicas. Diferentemente de um meteograma comum, os gráficos apresentados nesta página mostram também a incerteza associada à previsão ao longo do tempo para diferentes localidades.

```markdown title="Plumas de probabilidade (GridHistory do conjunto de previsões - É NECESSÁRIO executar o script `run_grh.sh` antes para os prefixos NMC, NPT e PPT)"
./run_plumes.sh TQ0126L028 2020062000 15 NMC 7
```

Ao final do processo deverão ser obtidas 971 figuras do tipo:

<figure markdown>
![Exemplo figura com as plumas de probabilidade](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/plumes/2020120100/./SP/04845W2351S.png){width=500}
Figura - Exemplo de figura com as plumas de probabilidade.
</figure>

### Perturbações Iniciais

As perturbações iniciais são representações gráficas espaciais dos valores utilizados para a geração do conjunto iniciais de perturbações utilizadas pelo método MB09.

```markdown title="Perturbações iniciais da temperatura e componentes do vento em 250, 500 e 850hPa"
./run_perturbations.sh TQ0126L028 2020062000 15 NMC 7
```
Ao final do processo deverão ser obtidas 9 figuras do tipo:

<figure markdown>
![Exemplo figura com as perturbações iniciais](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/perturbations/2020120100/perturbationstemp250_20201201002020120100.png){width=600}
Figura - Exemplo de figura com as perturbações iniciais.
</figure>

### Potencial de Velocidade em Altos Níveis

O potencial de velocidade apresentado nesta página apresenta a evolução temporal da variável $\chi$ (velocidade potencial) no nível de 200 hPa. Esta informação permite verificar as áreas que indicam a convergência dos ventos em 200 hPa (subsidência abaixo, inibe a convecção - tons de verde) e as áreas que indicam a divergência dos ventos em 200 hPa (convergência abaixo, indica a convecção - tons de marrom).

```markdown title="Potencial de velocidade em 200hPa"
./run_chievol.sh TQ0126L028 2020062000 15 NMC 7
```

Ao final do processo deverá ser obtida 1 figura do tipo:

<figure markdown>
![Exemplo figura com o potencial de velocidade em altos níveis](https://corsproxy.io/?http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/gnu_singularity_m128p_p64p/prod/chievol/2020120100/chi_evol20201201002020121600.png){width=600}
Figura - Exemplo de figura com o potencial de velocidade em altos níveis.
</figure>

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.


