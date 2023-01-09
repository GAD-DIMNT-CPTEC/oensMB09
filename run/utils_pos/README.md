# Scripts Auxiliares do Pós-Processamento

Estes scripts foram feitos para facilitar a organização dos arquivos de previsões dos membros para a execução dos produtos do ensemble. 

* `cria_link.sh`: faz os links simbólicos dos arquivos de análise como nomes dos arquivos de previsão de cada membro para uma data específica;
* `exp_links.sh`: faz os links simbólicos de todos os arquivos (`.ctl`, `.grb` e `.idx`) para fora dos diretórios dos membros;
* `check_files.sh`: faz a contagem dos arquivos `.ctl`, `.idx` e `.grb` para todos os membros dentro de um intervalo de tempo.

Os scripts `fix_ctl.sh` e `dcheck_ctl_tdef.sh` devem ser utilizados se, e somente se, houver arquivos ctl vazios (isso pode ocorrer quando a tabela com as variáveis do pós-processamento em uso não for a mesma que a versão da operação).

Para executar os scripts `cria_link.sh` e `exp_links.sh` para todas as datas a partir do diretório do pós-processamento, pode-se fazer o seguinte:

```
for i in $(find . -maxdepth 1 -type d -name "2020*"); do j=$(basename $i); ./cria_link.sh $j; done
```

e 

```
for i in $(find . -maxdepth 1 -type d -name "2020*"); do j=$(basename $i); ./exp_links.sh $j; done
```

carlos.bastarz@inpe.br (09/09/2020)
