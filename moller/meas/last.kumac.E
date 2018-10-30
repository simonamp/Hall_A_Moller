*** Begin of history file: Thu Sep 17 17:52:43 2009
exec run run=13560
nt/pri //lun21/1
shell
exec run_scal run=13560
nt/pri //lun51/1
shell
nt/pl //lun51/1.a_signal.f(3,0,0,0,0,0,0) ifzug=0
zone 2 2
nt/pl //lun51/1.a_signal.f(3,0,0,0,0,0,0) ifzug=0
nt/pl //lun51/1.a_signal.f(1,0,0,0,0,0,0) ifzug=0
nt/pl //lun51/1.a_signal.f(1,0,0,0,0,0,0) ifzug=1
shell head -20 a_asymm.f
nt/pl //lun51/1.a_asymm.f(3,0,1,0,0,0,0) ifzug=0
1dh 101 'asym' 1000 -0.1 0.1
nt/pl //lun51/1.a_asymm.f(3,0,1,0,0,0,0) ifzug=0 -101
h/fit 101 g
nt/pl //lun51/1.a_asymm.f(1,0,1,0,0,0,0) ifzug=0 -101
h/fit 101 g
ld //
exec lg_spectra run=13560
shell head -20 lg_spectra.kumac
exec lg_spectra run=13560 ped=0
exec lg_spectra run=13560 ped=0 icut=50
exec run run=13537
exec lg_spectra run=13537 ped=0 icut=50
exec lg_spectra run=13537 ped=0 icut=0
nt/pl //lun22/1.iadc(1) nadc>0
zone 2 2
nt/pl //lun22/1.iadc(1) nadc>0
nt/pl //lun22/1.iadc(9) nadc>0
nt/pl //lun22/1.iadc(10) nadc>0
exit
*** End   of history file: Thu Sep 17 18:37:14 2009
