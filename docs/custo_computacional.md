# Custo Computacional

!!! note "Nota"

    Todos os códigos utilizados nos experimentos na máquina Egeon foram compilados sem otimizações (tudo compilado com `-O0`). Nesta seção, porém, são considerados apenas os tempos de processamento com o compilador Intel a partir de uma única execução do código. Todos os códigos compilados na máquina XC50 (i.e., método de perturbação e produtos), foram compilados sem indicação explícita da nível de otimização. Todos os valores referem-se às simulações do oensMB09 na resolução TQ0126L028 (~200 km sobre a linha do Equador e 28 níveis verticais em coordenada sigma).

| Componente                                               | XC50 (GNU)                               | Egeon (GNU)                             | Egeon (Intel)                           | XC50/Egeon                                                  |
|----------------------------------------------------------|------------------------------------------|-----------------------------------------|-----------------------------------------|-------------------------------------------------------------|
| **Modelo BAM**                                           |                                          |                                         |                                         |                                                             |
| Pré-processamento	                                       | 10min (Chopping) + 5min demais processos |	2min (Chopping) + 5min demais processos	| 5min (Chopping) + 5min demais processos | 18GB (anl NCEP + TSM + SOILM + outros arquivos)             |
| Previsões controle (48 horas) – prefixos RDP e CTR       | 1h20min/membro[^1]                       | 5min/membro[^1]                         | 5min/membro[^1]                         | 5,6GB (7membros, 48 horas de previsão)                      |
| Previsões conjunto (360 horas) – prefixos NMC, PPT e NPT | 4h/membro[^1]                            |	35min/membro[^1]                        |	45min/membro[^1]                        |	45GB (15 membros, 360 horas de previsão)                    |
| Pós-processamento (360 horas) – prefixos NMC, PPT e NPT  | 5min/membro[^1]                          |	3min/membro[^1]                         |	3min/membro[^1]                         |	17GB (15 membros, 15 dias prevs., 9 níveis)                 |
| **Parcial**	                                             | **5h40min**[^2]                          | **50min**[^2]                           | **1h03min**[^2]                         |	**85,6GB**                                                  |
| **Método de Perturbação**                                |                                          |                                         |                                         |                                                             |
| recanl                                                   | 1min	                                    | 1min	                                  | 1min	                                  | 32MB (1 anl)                                                |
| rdpert                                                   | 2min	                                    | 2min	                                  | 2min	                                  | 224MB (7 anl)                                               |
| decanl                                                   | 2min	                                    | 2min	                                  | 2min	                                  | 50MB (7 anl)[^3]                                            |
| recfct                                                   | 2min	                                    | 2min	                                  | 2min	                                  | 4GB (PREVS 48 h CTR, 1-7 R)                                 |
| eof	                                                     | 2min	                                    | 2min	                                  | 2min	                                  | 3,3GB (logs e arquivos temporários de perturbação das anls) |
| deceof	                                                 | 2min	                                    | 2min	                                  | 2min	                                  | 138MB (14 anl)                                              |
| **Parcial**	                                             | **11min**[^2]                            | **11min**[^2]                           |	**11min**[^2]                           |	**7,8GB**                                                   |
| **Produtos**                                             |                                          |                                         |                                         |                                                             |
| ensmed	                                                 | 2min	                                    | 2min	                                  | 2min	                                  | 1,2GB                                                       |
| spread	                                                 | 3min	                                    | 3min	                                  | 3min	                                  | 332M (binários) + 66M (imagens)                             |
| perturbations	                                           | 1min	                                    | 1min	                                  | 1min	                                  | 2.6MB (imagens)                                             |
| chievol	                                                 | 1min                                     | 1min	                                  | 1min                                    |	200K (imagem)                                               |
| spaguetti                                                | 3min	                                    | 3min                                    | 3min                                    |	30M (imagens)                                               |
| cluster                                                  | 2min                                     | 2min	                                  | 2min                                    |	32K (binários) + 221M (imagens)                             |
| probability	                                             | 1min                                     | 1min                                    | 1min                                    |	65M (binários) + 15M (imagens)                              |
| probagr	                                                 | 1min                                     | 1min	                                  | 1min	                                  | 875K (binários) + 714K (imagens)                            |
| grh    	                                                 | 6min                                     | 6min                                    | 6min                                    | 173M (imagens)                                              |
| plumes	                                                 | 20min                                    | 20min                                   | 20min                                   |	753M (binários) + 215M (imagens)                            |
| **Parcial**	                                             | **40min**[^2]                            | **40min**[^2]                           | **40min**[^2]                           |	**1,5GB**                                                   |
| **Total**                                                | **6h31min**[^2]                          | **1h41min**[^2]                         | **1h54min**[^2]                         |	**95GB**                                                    |

[^1]: Se todos os membros entrarem juntos, o tempo de processamento será o mesmo.
[^2]: Estimativa considerando que todos os processos sejam executados sem espera.
[^3]: Análises do modelo BAM (diretório `model/datain`).

## Outras informações

**Egeon:**

* Processos seriais (diretivas SLUMR: `nodes=1`, `tasks-per-node=1`): todo o pré-processamento com o excessão do Chopping, todo o método de perturbação, todos os produtos com excessão do ensmed;
* Processos paralelos:
    * Chopping: `nodes=1`, `tasks-per-node=128`;
    * Modelo: `nodes=1`, `tasks-per-node=128`;
    * Pós-processamento: `nodes=1`, `tasks-per-node=64`;
    * ensmed: `nodes=1`, `tasks-per-node=62`.

**XC50:**

* Processos seriais (diretivas PBS: `select=1:ncpus=1`): todo o pré-processamento com o excessão do Chopping, todo o método de perturbação, todos os produtos com excessão do ensmed;
* Processos paralelos:
    * Chopping: `nodes=3:ppn=4`;
    * Modelo: `nodes=1:ppn=40`;
    * Pós-processamento: `mppwidth=48`, `mppnppn=4`, `mppdepth=6`;
    * ensmed: `mppwidth=20`, `mppnppn=1`.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
