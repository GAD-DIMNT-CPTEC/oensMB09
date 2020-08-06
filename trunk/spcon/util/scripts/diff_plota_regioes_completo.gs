'reinit'

#prs, hum, tem, win
var='tem'

#anl_det_tupa
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pehn0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pnhn0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pehs0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pnhs0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'petr0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pntr0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'penas0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pnnas0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pesas0112020031300.ctl'
'open SPCON_OPER/scratchout/oper/tempo/oensMB09/eof/dataout/TQ0126L028/'var'pnsas0112020031300.ctl'

#oper
'open lustre/'var'pehn0112020031300.ctl'
'open lustre/'var'pnhn0112020031300.ctl'
'open lustre/'var'pehs0112020031300.ctl'
'open lustre/'var'pnhs0112020031300.ctl'
'open lustre/'var'petr0112020031300.ctl'
'open lustre/'var'pntr0112020031300.ctl'
'open lustre/'var'pesan0112020031300.ctl'
'open lustre/'var'pnsan0112020031300.ctl'
'open lustre/'var'pesas0112020031300.ctl'
'open lustre/'var'pnsas0112020031300.ctl'

'set gxout contour'

if(var='win'); var='uwnp'; endif

# HN
'set clevs -5 -4 -3 -2 -1 0 1 2 3 4 5'
'd ('var'.1-'var'.2)/2 - ('var'.11-'var'.12)/2'

# HS
'set clevs -5 -4 -3 -2 -1 0 1 2 3 4 5'
'd ('var'.3-'var'.4)/2 - ('var'.13-'var'.14)/2'

# TR
'set clevs -5 -4 -3 -2 -1 0 1 2 3 4 5'
'd ('var'.5-'var'.6)/2 - ('var'.15-'var'.16)/2'

# NAS 
'set clevs -5 -4 -3 -2 -1 0 1 2 3 4 5'
'd ('var'.7-'var'.8)/2 - ('var'.17-'var'.18)/2'

# SAS 
'set clevs -5 -4 -3 -2 -1 0 1 2 3 4 5'
'd ('var'.9-'var'.10)/2 - ('var'.19-'var'.20)/2'
