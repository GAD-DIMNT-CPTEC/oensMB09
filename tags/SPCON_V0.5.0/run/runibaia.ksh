#!/bin/ksh
LABELI=$1

#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m01n_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m02n_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m03n_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m04n_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m05n_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m06n_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m07n_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m01p_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m02p_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m03p_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m04p_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m05p_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m06p_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl m07p_cptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl mcontrolcptec $LABELI"
#rsh ibaia -l grads "/home/grads/gol/disparaModel.pl EnsembleM $LABELI"

#rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl T299L64 $AAAA$MM$DD$HH"

rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m01n_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m02n_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m03n_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m04n_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m05n_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m06n_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m07n_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m01p_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m02p_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m03p_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m04p_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m05p_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m06p_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl m07p_cptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl mcontrolcptec $LABELI"
rsh intranet -l intranet "/home2/intranet/gol/disparaModel.pl EnsembleM $LABELI"

exit 0