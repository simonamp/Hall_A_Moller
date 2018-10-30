ld // 
//LUN27           /adaql1/data1/moller/moller_data_led_14099.nt

 nt/scan //lun27/1 nsca>0 ! ! ! itrig(1) itrig(6) itrig(8) isca(10) isca(12)
 nt/scan //lun27/1 nsca>-1 ! ! ! itrig(1) itrig(6) itrig(8)   
 nt/pl //LUN21/1.itrig(8) itrig(6)=0
 nt/pl //LUN21/1.iadc(1) nadc=12
 nt/pl //LUN21/1.iadc(12) nadc=12 
 nt/pl //lun21/1.iadc(12)%itrig(6) nadc=12
 nt/pl //lun21/1.itrig(6)%isca(10) nsca>0
 nt/pl //lun21/1.itrig(8)%isca(12) nsca>0
