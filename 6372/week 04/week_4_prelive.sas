data Melanoma;                                                                                   
infile 'Y:\Databases\DataScience@SMU.dtBase2\Files.noindex\csv\e\Melanomatimeseries.csv' dlm=',' firstobs=2;                                                    
input Year Melanoma Sunspot;                                                                            
run;  

*Plot Sunspot versus Years;
proc sgscatter data = Melanoma;
plot Sunspot * Year;
run;
quit; 
                                                                                          
*Modeling sunspot with simple regression, intercept only;                                                                                                                                                                                                               
proc autoreg data=Melanoma all plots(unpack);
model Sunspot= ;                                                                               
run; 
quit;                                                                                                                                                                             

*Modeling sunspot with AR(1);
proc autoreg data=Melanoma all plots(unpack);                             
model Sunspot= / nlag=1;               
run;
quit;

*Modeling sunspot with AR(2);
proc autoreg data=Melanoma all plots(unpack);                             
model Sunspot= / nlag=2;               
run;
quit;

*Modeling sunspot with AR(3);
proc autoreg data=Melanoma all plots(unpack);                             
model Sunspot= / nlag=3;               
run;
quit;

*Modeling sunspot with AR(4);
proc autoreg data=Melanoma all plots(unpack);                             
model Sunspot= / nlag=4;               
run;
quit;

*Forecasting data;
data Melanoma2;
input Year Melanoma Sunspot;
datalines;
1973 . 50
1974 . 35
1975 . 20
1976 . 30
;
run;

*Append Melanoma2 to Melanoma;
proc append base=Melanoma data=Melanoma2;
run;
quit;

*Check data;
proc print data = Melanoma;
run;
quit;

*Proc autoreg;
proc autoreg data=Melanoma all plots(unpack);
model Sunspot= / nlag=3;
run;
quit;
