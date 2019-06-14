#!/bin/sh 

	if [[ "" = $1 ]]; then
		clear
		echo "Sintaxe errada!!!!"
		echo
		echo "./script device"
		echo
		exit
	fi

	device=$1


	        echo
        echo "Escolha o NUMERO do tipo da fita"
        echo "1. EXA"
        echo "2. DAT"
        echo "3. DLT"
        echo "4. CD"
        echo "5. LTO"
        echo

        read tipo

        if [[ $tipo -gt 5 && $tipo -lt 1 ]];then
                echo "Tipo nao existente"
                exit
        fi

        echo "Digite o numero da fita" 
        read numero


        if [[ "" = $numero ]]; then
                clear   
                echo "Sintaxe errada!!!!"
                exit
        fi

	arquivo=$numero$tipo

	clear

	( mt -f $device rewind ) | echo "Voltando a Fita, Aguarde...  ";

	touch .errodrive$arquivo;

        while [ ! -s .errodrive$arquivo  ]; do

        	let blocos=$blocos+1;

               (tar tvf $device >>fitaDescricao$arquivo  2>.ERRO$arquivo ) | echo "Listando blocos ("$blocos") para atualizar no Portal, Aguarde...";

                cat .ERRO$arquivo | grep "left on device" > .errodrive$arquivo;
                cat .ERRO$arquivo | grep "read: Erro de entrada" >> .errodrive$arquivo;
		cat .ERRO$arquivo | grep "exiting now" >> .errodrive$arquivo;
			
        rm -fr .ERRO$arquivo;

	done

	( mt -f $device rewoff ) | echo "Ejetando a Fita, Aguarde...  ";

	

        rm -fr .errodrive$arquivo;
	
	
	echo "Fita tipo -  "$tipo" Numero - "$numero" - "$(cat fitaDescricao$arquivo | wc -l)" arquivos"
	
	    echo "Pressione Enter para continuar" 

        read


	echo "Entre com senha da operacao"

	scp fitaDescricao$arquivo operacao@macunaima:/home/operacao/scriptsNovos/atualizaFita/
	
	rm -fr fitaDescricao$arquivo
	
	ssh operacao@macunaima /home/operacao/scriptsNovos/atualizaFita/atualizaFitaArquivo.sh $tipo $numero fitaDescricao$arquivo;

