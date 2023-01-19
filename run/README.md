# Realização da suíte SPCON

## Cray XE6, XC50 e Egeon

1. Realização do pré-processamento da primeira análise controle:
```
$ ./runPre 126 28 2020062000 SMT 1 T F 574 64
```
ou
```
$ ./runPre 213 42 2020062000 SMT 1 T F 574 64
```
ou
```
$ ./runPre 299 64 2020062000 SMT 1 T F 574 64
```

2. Realização do membro controle a partir da primeira análise (nesta primeira integração, são apenas 48 horas 3/3h - deverão haver também previsões para até 15 dias a partir do membro controle):
```
$ ./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020062200 CTR 2 1
```
ou
```
$ ./run_model.sh 192 4 6 TQ0213L042 SMT 2020062000 2020062200 CTR 2 1
```
ou
```
$ ./run_model.sh 384 4 6 TQ0299L064 SMT 2020062000 2020062200 CTR 2 1
```

**Obs.:** monitorar a realização dos membros com o seguinte comando:

```
$ qstat -u $USER @eslogin13
```

3. Recomposição dos coeficientes espectrais da análise para ponto de grade:
```
$ ./run_recanl.sh TQ0126L028 SMT ANLSMT 2020062000
```
ou
```
$ ./run_recanl.sh TQ0213L042 SMT ANLSMT 2020062000
```
ou
```
$ ./run_recanl.sh TQ0299L064 SMT ANLSMT 2020062000
```

4. Gera e soma as perturbações randômicas à análise controle:
```
$ ./run_rdpert.sh TQ0126L028 SMT YES 2020062000 7
```
ou
```
$ ./run_rdpert.sh TQ0213L042 SMT YES 2020062000 7
```
ou
```
$ ./run_rdpert.sh TQ0299L064 SMT YES 2020062000 7
```

5. Decomposição das análises perturbadas em ponto de grade para coeficientes espectrais:
```
$ ./run_decanl.sh TQ0126L028 SMT YES 2020062000 7
```
ou
```
$ ./run_decanl.sh TQ0213L042 SMT YES 2020062000 7
```
ou
```
$ ./run_decanl.sh TQ0299L064 SMT YES 2020062000 7
```

6. Realização das previsões a partir das análises perturbadas para uso na análise de EOF:
```
$ ./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020062200 RDP 2 7
```
ou
```
$ ./run_model.sh 192 4 6 TQ0213L042 SMT 2020062000 2020062200 RDP 2 7
```
ou
```
$ ./run_model.sh 384 4 6 TQ0299L064 SMT 2020062000 2020062200 RDP 2 7
```

**Obs.:** monitorar a realização dos membros com o seguinte comando:

```
$ qstat -J -t -u $USER @eslogin13
```

7. Recomposição para ponto de grade das previsões realizadas a partir da análise controle:
```
$ ./run_recfct.sh TQ0126L028 CTR 2020062000
```
ou
```
$ ./run_recfct.sh TQ0213L042 CTR 2020062000
```
ou
```
$ ./run_recfct.sh TQ0299L064 CTR 2020062000
```

8. Recomposição para ponto de grade das previsões realizadas a partir das análises perturbadas randomicamente:
```
$ ./run_recfct.sh TQ0126L028 7 2020062000
```
ou
```
$ ./run_recfct.sh TQ0213L042 7 2020062000
```
ou
```
$ ./run_recfct.sh TQ0299L064 7 2020062000
```

**Obs.:** monitorar a realização dos membros com o seguinte comando:

```
$ qstat -J -t -u $USER @aux20-eth4
```

9. Realização da análise de EOF para gerar as perturbações ótimas a serem utilizadas na composição final dos membros do conjunto:
```
$ ./run_eof.sh TQ0126L028 7 YES 2020062000 SMT
```
ou
```
$ ./run_eof.sh TQ0213L042 7 YES 2020062000 SMT
```
ou
```
$ ./run_eof.sh TQ0299L064 7 YES 2020062000 SMT
```

10. Composição do arquivo de análise a partir das perturbações EOF:
```
$ ./run_deceof.sh TQ0126L028 EOF YES 2020062000 7 SMT
```
ou
```
$ ./run_deceof.sh TQ0213L042 EOF YES 2020062000 7 SMT
```
ou
```
$ ./run_deceof.sh TQ0299L064 EOF YES 2020062000 7 SMT
```

11. Realização do membro controle a partir da primeira análise (nesta primeira integração, são apenas 48 horas 3/3h - deverão haver também previsões para até 15 dias a partir do membro controle):
```
$ ./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020070500 NMC 2 1
```
ou
```
$ ./run_model.sh 192 4 6 TQ0213L042 SMT 2020062000 2020070500 NMC 2 1
```
ou
```
$ ./run_model.sh 384 4 6 TQ0299L064 SMT 2020062000 2020070500 NMC 2 1
```

**Obs.:** monitorar a realização dos membros com o seguinte comando:

```
$ qstat -u $USER @eslogin13
```

12. Realização das previsões a partir do conjunto de análises com perturbações ótimas (análises prefixo N - 15 dias 6/6h):
```
$ ./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020070500 NPT 2 7
```
ou
```
$ ./run_model.sh 192 4 6 TQ0213L042 SMT 2020062000 2020070500 NPT 2 7
```
ou
```
$ ./run_model.sh 384 4 6 TQ0299L064 SMT 2020062000 2020070500 NPT 2 7
```

**Obs.:** monitorar a realização dos membros com o seguinte comando:

```
$ qstat -J -t -u $USER @eslogin13
```

13. Realização das previsões a partir do conjunto de análises com perturbações ótimas (análises prefixo P - 15 dias 6/6h):
```
$ ./run_model.sh 48 4 6 TQ0126L028 SMT 2020062000 2020070500 PPT 2 7
```
ou
```
$ ./run_model.sh 192 4 6 TQ0213L042 SMT 2020062000 2020070500 PPT 2 7
```
ou
```
$ ./run_model.sh 384 4 6 TQ0299L064 SMT 2020062000 2020070500 PPT 2 7
```

**Obs.:** monitorar a realização dos membros com o seguinte comando:

```
$ qstat -J -t -u $USER @eslogin13
```

14. Realização do pós-processamento das previsões produzidas a partir da análise controle:
```
$ ./run_pos.sh 48 4 6 TQ0126L028 2020062000 2020070500 NMC
```
ou
```
$ ./run_pos.sh 192 4 6 TQ0213L042 2020062000 2020070500 NMC
```
ou
```
$ ./run_pos.sh 192 4 6 TQ0299L064 2020062000 2020070500 NMC
```

15. Realização do pós-processamento do conjunto de previsões realizado a partir do conjunto de análises com perturbações ótimas (análises prefixo N):
```
$ ./run_pos.sh 48 4 6 TQ0126L028 2020062000 2020070500 NPT 7
```
ou
```
$ ./run_pos.sh 192 4 6 TQ0213L042 2020062000 2020070500 NPT 7
```
ou
```
$ ./run_pos.sh 192 4 6 TQ0299L064 2020062000 2020070500 NPT 7
```

16. Realização do pós-processamento do conjunto de previsões realizado a partir do conjunto de análises com perturbações ótimas (análises prefixo P):
```
$ ./run_pos.sh 48 4 6 TQ0126L028 2020062000 2020070500 PPT 7
```
ou
```
$ ./run_pos.sh 192 4 6 TQ0213L042 2020062000 2020070500 PPT 7
```
ou
```
$ ./run_pos.sh 192 4 6 TQ0299L064 2020062000 2020070500 PPT 7
```

17. Execução do programa gribmap a partir dos arquivos pós-processados do BAM para os prefixos NMC, NPT e PPT:
```
$ ./run_gribmap.sh TQ0126L028 2020062000 1 NMC
$ ./run_gribmap.sh TQ0126L028 2020062000 7 NPT
$ ./run_gribmap.sh TQ0126L028 2020062000 7 PPT
```
ou
```
$ ./run_gribmap.sh TQ0213L042 2020062000 1 NMC
$ ./run_gribmap.sh TQ0213L042 2020062000 7 NPT
$ ./run_gribmap.sh TQ0213L042 2020062000 7 PPT
```
ou
```
$ ./run_gribmap.sh TQ0299L064 2020062000 1 NMC
$ ./run_gribmap.sh TQ0299L064 2020062000 7 NPT
$ ./run_gribmap.sh TQ0299L064 2020062000 7 PPT
```

18. GridHistory (necessário para o cálculo das plumas), executar para NMC, NPT e PPT (pode-se plotar os meteogramas nesta etapa, mas apenas para o membro controle):
```
$ ./run_grh.sh 4 TQ0126L028 2020060100 2020061600 NMC 1
$ ./run_grh.sh 4 TQ0126L028 2020060100 2020061600 NPT 7
$ ./run_grh.sh 4 TQ0126L028 2020060100 2020061600 PPT 7
```
ou
```
$ ./run_grh.sh 4 TQ0213L042 2020060100 2020061600 NMC 1
$ ./run_grh.sh 4 TQ0213L042 2020060100 2020061600 NPT 7
$ ./run_grh.sh 4 TQ0213L042 2020060100 2020061600 PPT 7
```
ou
```
$ ./run_grh.sh 4 TQ0299L064 2020060100 2020061600 NMC 1
$ ./run_grh.sh 4 TQ0299L064 2020060100 2020061600 NPT 7
$ ./run_grh.sh 4 TQ0299L064 2020060100 2020061600 PPT 7
```

19. Cálculo da média do conjunto (é necessário lincar[^1] todos os arquivos dos membros (ctl,grb) para fora dos diretórios dos membros):
```
$ ./run_ensmed.sh TQ0126L028 2020062000 15 NMC 7
```
ou
```
$ ./run_ensmed.sh TQ0213L042 2020062000 15 NMC 7
```
ou
```
$ ./run_ensmed.sh TQ0299L064 2020062000 15 NMC 7
```

[^1]: Para lincar os referidos arquivos, utilizar os scripts (revisar os scripts e copiá-los para o diretório oensMB09/pos/dataout/TQ0126L028):

* `oensMB09/run/utils_pos/cria_link.sh`: faz os links simbólicos dos arquivos de análise como nomes dos arquivos de previsão de cada membro para uma data específica;
* `oensMB09/run/utils_pos/exp_links.sh`: faz os links simbólicos de todos os arquivos (ctl, grb e idx) para fora dos diretórios dos membros.

20. Cálculo do espalhamento do conjunto e plotagem das figuras com a média e o espalhamento do conjunto:
```
$ ./run_spread.sh TQ0126L028 2020062000 15 NMC 7
```
ou
```
$ ./run_spread.sh TQ0213L042 2020062000 15 NMC 7
```
ou
```
$ ./run_spread.sh TQ0299L064 2020062000 15 NMC 7
```

## Produtos

1. Clusters (é necessário que os arquivos `*.idx` sejam gerados fora dos diretórios dos membros):
```
$ ./run_cluster.sh TQ0126L028 2020062000 15 NMC 7
```

2. Probabilidades de precipitação para 1, 5, 10 e 20mm:
```
$ ./run_probability.sh TQ0126L028 2020062000 15 NMC 7
```

3. Probabilidades de acumulo precipitação acima de 10mm:
```
$ ./run_probagr.sh TQ0126L028 2020062000 15 NMC 7
```

4. Plumas de probabilidade (GridHistory do conjunto de previsões - É NECESSÁRIO executar o script `run_grh.sh` antes para os prefixos NMC, NPT e PPT):
```
$ ./run_plumes.sh TQ0126L028 2020062000 15 NMC 7
```

5. Potencial de velocidade em 200hPa:
```
$ ./run_chievol.sh TQ0126L028 2020062000 15 NMC 7
```

6. Perturbações iniciais da temperatura e componentes do vento em 250, 500 e 850hPa:
```
$ ./run_perturbations.sh TQ0126L028 2020062000 15 NMC 7
```

7. Espaguetes da temperatura absoluta em 1000 e 850hPa e da altura geopotencial em 500hPa, para a América do Sul e hemisférios norte e sul:
```
$ ./run_spaguetti.sh TQ0126L028 2020062000 15 NMC 7
```
