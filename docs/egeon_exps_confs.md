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

## Dashboard

Um "dashboard" para a visualização dos produtos pode ser encontrado em [produtos oensMB09](https://gad-dimnt-cptec.github.io/oensMB09/produtos_oensMB09.html).

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
