#!/bin/bash

d=${1}

rsync -av /scratchout/oper/io/oensMB09/produtos/wgne/dataout/TQ0126L028/ /stornext/online7/pnt/preoper/oensMB09/MJO/
rsync -av /stornext/online7/pnt/preoper/tempo/oensMB09/ /stornext/oper/pre_tempo/XE/oensMB09/

cd /scratchout/oper/io/oensMB09/tigge/dataout/TQ0126L028/
find . -name "*gz" -print | cpio -pdmv /stornext/online7/pnt/preoper/oensMB09/TIGGE/

exit 0
