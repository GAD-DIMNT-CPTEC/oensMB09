#! /bin/csh
setenv PATH .:~/bin:/usr/local/nec/tools:/usr/psuite:/SX/usr/bin:$PATH
cd /gfs/home3/io_dop/tempo/global/oenspro/produtos/SKILL/source
efc -Vaxlib -static -tpp2 -o ./aki.x dayinty_gau.f

exit 0
