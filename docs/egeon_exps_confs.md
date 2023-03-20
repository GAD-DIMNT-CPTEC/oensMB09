# Experimentos (Egeon)

Experimentos realizados para os testes iniciais do oensMB09 na máquina Egeon:

1. `gnu_egeon_m16p_p16p`: Compilação com o ambiente GNU, execução do pré-processamento (Chopping), modelo e pós-processamento com 16 núcleos em 1 nó;
2. `gnu_egeon_m128p_p64p`: Compilação com o ambiente GNU, execução do pré-processamento (Chopping) e modelo com 128 núcleos, pós-processamento com 64 núcleos em 1 nó;
3. `gnu_singularity_m128p_p64p`: Compilação com o ambiente GNU por meio de container singularity, execução do pré-processamento (Chopping) modelo com 128 núcleos, pós-processamento com 64 núcleos em 1 nó;
4. `intel_egeon_m16p_p16p`: Compilação com o ambiente Intel, execução do pré-processamento (Chopping),modelo e pós-processamento com 16 núcleos em 1 nó;
5. `intel_egeon_m128p_p64p`: Compilação com o ambiente GNU, execução do pré-processamento (Chopping) e modelo com 128 núcleos, pós-processamento com 64 núcleos em 1 nó;

Para a avaliação preliminar dos resultados, foram considerados efetivamente apenas os experimentos 2, 3 e 5, para os quais, mediu-se o tempo de processamento total de uma realização de todo o processo (i.e., pré-processamento, método de perturbação e pós-processamento - com excessão da geração dos produtos):

* `gnu_egeon_m128p_p64p`: 1h;
* `gnu_singularity_m128p_p64p`: 1h45min;
* `intel_egeon_m128p_p64p`: 1h20min.

## Ambiente de compilação GNU

Módulos carregados para a compilação dos códigos do modelo, método de perturbação e produtos:

```
1. gnu9/9.4.0
2. ucx/1.11.2
3. openmpi4/4.1.1
4. netcdf/4.7.4
5. netcdf-fortran/4.5.3
6. phdf5/1.10.8
7. hwloc/2.5.0   
8. libfabric/1.13.0
9. cmake/3.21.3
```

## Ambiente de compilação Intel

Módulos carregados para a compilação dos códigos do modelo, método de perturbação e produtos:

```
1. autotools 
2. prun/2.2  
3. ohpc     
4. hwloc/2.5.0 
5. tbb/2021.4.0           
6. compiler-rt/2021.4.0
7. debugger/2021.6.0
8. dpl/2021.4.0 
9. oclfpga/2021.4.0
10. compiler/2022.1.0  
11. mkl/2021.4.0
12. intel/2022.1.0
13. mpi/2021.4.0
14. impi/2021.4.0  
15. phdf5/1.10.8
16. netcdf/4.7.4 
17. netcdf-fortran/4.5.3
18. cmake/3.21.3
```

## Ambiente de compilação do container

 Versões das ferramentas de compilação utilizadas dentro do [container](https://github.com/GAD-DIMNT-CPTEC/Containers/blob/main/oensMB09_BAM_V1.2.1_Env_XC50-Ubuntu-18.04_gcc-4.8.5_mpich-3.3_v0.2.def):

```
1. GCC 4.8
2. MPICH 3.3
3. CMAKE 3.25.2
```

## Custo Computacional

### Espaço em disco

* Modelo BAM:
    * Pré-processamento (diretórios `pre/[datain/dataout]`): ~18GB (anl NCEP + TSM + SOILM + outros arquivos);
    * Modelo (diretório `model/datain`): ~2GB (15 anls + 15 OZON + 15 TRAC + condições de contorno);
    * Modelo (diretório `model/dataout`): ~24GB (1 CTR, 7 P, 7 N, 7 R);
    * Pós-processamento (diretório `pos/dataout`): ~17GB (15 membros, 15 dias prevs., 9 níveis).


* Método de perturbação:
    * `recanl`: ~32M (1 ANL);
    * `rdpert`: ~224M (7 ANL);
    * `decanl` (já considerado no diretório `model/datain`): 50M (7 ANL);
    * `recfct`: ~4GB (PREVS 48 h CTR, 1-7 R);
    * `eof`: ~3,3GB (logs e arquivos temporários de perturbação das análises);
    * `deceof` (já considerado no `model/datain`): ~138M (14 ANL).


* Produtos:
    * ensmed: ~1.2GB;
    * spread: ~332M (binários) + 66M (imagens);
    * perturbations: 2.6M (imagens);
    * chievol: ~200K (imagem);
    * spaguetti: ~30M (imagens);
    * cluster: 32K (binários) + 221M (imagens);
    * probability: ~65M (binários) + 15M (imagens);
    * probagr: 875K (binários) + 714K (imagens);
    * grh: 173M (imagens);
    * plumes: 753M (binários) + 215M (imagens).

### Tempo de processamento

!!! note "Nota"

    Todos os códigos utilizados nos experimentos foram compilados sem otimizações (tudo compilado com `-O0`). Nesta seção, porém, são considerados apenas os tempos de processamento com o compilador Intel a partir de uma única execução do código. 

* Processos seriais (diretivas SLURM: `nodes=1` e `tasks-per-node=1`): todo o pré-processamento com o excessão do Chopping, todo o método de perturbação, todos os produtos com excessão do ensmed;
* Processos paralelos:
    * Chopping: `nodes=1`, `tasks-per-node=128`;
    * Modelo: `nodes=1`, `tasks-per-node=128`;
    * Pós-processamento: `nodes=1`, `tasks-per-node=64`;
    * ensmed: `nodes=1`, `tasks-per-node=62`.


* Modelo BAM:
    * Pré-processamento: ~5min (Chopping) + ~5min demais processos;
    * Modelo:
        * Previsões com os prefixos `RDP` e `CTR` (48 horas): ~8min/membro (se todos os membros entrarem juntos, demora o mesmo tempo);
        * Previsões com os prefixos `NMC`, `PPT` e `NPT` (360 horas): ~50min/membro (se todos os membros entrarem juntos, demora o mesmo tempo);
    * Pós-processamento:
        * Previsões com os prefixos `NMC`, `PPT` e `NPT` (360 horas): ~5min/membro (se todos os membros entrarem juntos, demora o mesmo tempo).


* Método de perturbação:
    * `recanl`: ~1min;
    * `rdpert`: ~2min;
    * `decanl`: ~2min;
    * `recfct`: ~2min;
    * `eof`: ~2min;
    * `deceof`: ~2min;


* Produtos:
    * `ensmed`: ~2min;
    * `spread`: ~3min;
    * `perturbations`: ~1min;
    * `chievol`: ~1min;
    * `spaguetti`: ~3min;
    * `cluster`: ~2min;
    * `probability`: ~1min;
    * `probagr`: ~1min;
    * `grh`: ~6min;
    * `plumes`: ~20min;

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
