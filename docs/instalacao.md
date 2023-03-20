# Instalação

As instruções apresentadas a seguir referem-se apenas para os módulos do método de perturbação e produtos do oensMB09.

!!! warning "Atenção"

    Nesta página são apresentadas instruções de compilação para as máquinas XC50 e Egeon. Observe que há diferenças nos ajustes dos ambientes nas duas máquinas e que na máquina XC50 é considerado apenas o compilador GNU enquanto que na máquina Egeon, são considerados os compiladores GNU e Intel.

## Ambiente de compilação de referência no XC50

O compilador de referência utilizado no XC50 é:

* Compilador GNU `gcc/7.2.0`.

Para carregar o ambiente de referência para o compilador GNU `gcc/7.2.0`, utilize os módulos a seguir:

```
module swap PrgEnv-cray/6.0.4 PrgEnv-gnu
module unload craype-x86-skylake
```

Em seguida, verifique se os módulos carregados correspondem aos seguintes:

```
module list
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

## Ambiente de compilação de referência na EGEON

Para a máquina Egeon, foram testados os compiladores GNU e Intel. Para o compilador GNU, foram testados o ambiente de compilação local, fornecido pela máquina e um ambiente de compilação fornecido a partir de um container do Singularity (mais informações a seguir). Para o ambiente de compilação GNU local, o compilador de referência é:

O compilador de referência utilizado no XC50 é:

* Compilador GNU `gcc/9.4.0`.

Para carregar o ambiente de referência para o compilador GNU `gcc/7.2.0`, utilize os módulos a seguir:

```
module purge
module load gnu9/9.4.0
module load ucx/1.11.2
module load openmpi4/4.1.1
module load netcdf/4.7.4
module load netcdf-fortran/4.5.3
module load phdf5/1.10.8
module load hwloc
module load libfabric/1.13.0
module load cmake/3.21.3
```

Em seguida, verifique se os módulos carregados correspondem aos seguintes:

```
module list
Currently Loaded Modules:
  1) gnu9/9.4.0   3) openmpi4/4.1.1   5) netcdf-fortran/4.5.3   7) hwloc/2.5.0
  2) ucx/1.11.2   4) netcdf/4.7.4     6) phdf5/1.10.8           8) libfabric/1.13.0
  9) cmake/3.21.3
```

Para o ambiente de compilação Intel local, o compilador de referência é:

* Compilador Intel `intel/2022.1.0`.

Para carregar o ambiente de compilação de referência para o compilador Intel, carregue os seguintes módulos:

```
module purge
module load ohpc
module swap gnu9 intel
module swap openmpi4 impi
module load hwloc
module load phdf5
module load netcdf
module load netcdf-fortran
module swap intel intel/2022.1.0
module load cmake/3.21.3
```

Em seguida, verifique se a lista de módulos carregados é a seguinte:

```
module list
Currently Loaded Modules:
1) autotools              10) compiler/2022.1.0 
2) prun/2.2               11) mkl/2021.4.0
3) ohpc                   12) intel/2022.1.0
4) hwloc/2.5.0            13) mpi/2021.4.0
5) tbb/2021.4.0           14) impi/2021.4.0
6) compiler-rt/2021.4.0   15) phdf5/1.10.8
7) debugger/2021.6.0      16) netcdf/4.7.4
8) dpl/2021.4.0           17) netcdf-fortran/4.5.3
9) oclfpga/2021.4.0       18) cmake/3.21.3
```

## Compilação

Com os ambientes desvidamentes configurados, independente da release e da máquina, a compilação dos módulos de perturbação e dos códigos dos produtos do oensMB09, é feita da mesma forma.

Para compilar o método de perturbação:

```
cd oensMB09
./config_spcon.sh configure TQ0126L028 gnu
make comp=gnu
```

Para compilar os produtos:

```
cd oensMB09/produtos
make comp=gnu
```

!!! note "Nota"

    A partir da release V2.2.1 (para uso exclusivo na Egeon), as bibliotecas `bacio`, `w3lib` e `nemsio` foram atualizadas para versões mais recentes lançadas pelo NCEP.

!!! warning "Aviso"

    Este site não se destina à disseminação de produtos operacionais ou que possam ser utilizados para tomada de decisão. As informações aqui apresentadas, na forma de texto ou imagem, possuem caráter científico com a finalidade de testes ou validação. Os autores desta página não se responsabilizam pelo uso não autorizado deste material.
