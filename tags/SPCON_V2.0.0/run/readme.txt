1 passo
Compile o Pre Pos e o Modelo

2 passo
Compile runfftp.bash
cd /stornext/home/paulo.kubota/oens-AGCM/run
./runfftp.bash clean 126 28

3 passo
cd /stornext/home/paulo.kubota/oens-AGCM/cluster/source
make -f Makefile.pgi

4 passo
Compile rundeco.bash
cd /stornext/home/paulo.kubota/oens-AGCM/run

 ./rundeco.bash clean 126 28 7 2004032600 15 NO 7

5 passo
Compile ensmed.bash
cd /stornext/home/paulo.kubota/oens-AGCM/ensmed/source
make -f Makefile.pgi

ou

./runensmed.bash clean 126 28 2004032600 15 7 NMC

6 passo
Compile runeofs.bash
cd /stornext/home/paulo.kubota/oens-AGCM/run
/runeofs.bash clean 126 28 NO 7 2004032600 15 7

7 passo
Compile plumes
cd /stornext/home/paulo.kubota/oens-AGCM/oens-AGCM/plumes/source
make -f Makefile.pgi

ou

./runplumes.bash clean 126 28 2004032600 15 7 NMC

8 passo
Compile runprcmed
cd /stornext/home/paulo.kubota/oens-AGCM/oens-AGCM//wgne/source
make -f Makefile.pgi

ou

./runprcmed.bash clean 126 28 2004032600 15      7       AVN


9 passo
Compile runprobability
cd /stornext/home/paulo.kubota/oens-AGCM/oens-AGCM/probability/source
make -f Makefile.pgi

ou
./runprobability.bash clean 126 28 2004032600 15 7 NMC

10 passo
Compile probagr
cd /stornext/home/paulo.kubota/oens-AGCM/oens-AGCM/probagr/source
make -f Makefile.pgi

ou
./runprobagr.bash clean 126 28 2004032600 15 7 NMC


11 passo
Compile /runrdpt.bash
cd /stornext/home/paulo.kubota/oens-AGCM/oens-AGCM/rdpert/source
./runrdpt.bash clean 7 126 28 2004032600 NO NMC


