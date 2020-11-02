proc format;                                                              
value ExpFmt 	1 = 'Crane C.'
				0 = 'Eagle C.';                                                     
value RspFmt	1 = 'Pass'
				0 = 'Fail';                                                    
run;

data CH19_9;
  input Exposure Response Count;
  label Response='Exam Status';
  datalines;
0 0 1900
0 1 2100
1 0 2000
1 1 2000
;
                                                                    
proc sort data=CH19_9;
  by descending Exposure descending Response;
run;

proc freq data=CH19_9 order=data;
  format Exposure ExpFmt. Response RspFmt.;
  tables Exposure*Response / chisq riskdiff(equal var=null) relrisk;
  exact pchi or fisher;
  weight Count;
  title ' Prospective Study';    
run;

data CH19_92;
  input Exposure Response Count Subject $;
  label Response='Exam Status';
  datalines;
0 0 1600	Math
0 1 2000 	Math
1 0 600 	Math
1 1 1200 	Math
0 0 300 	Physics
0 1 100 	Physics
1 0 1400 	Physics
1 1 800 	Physics
;

proc sort data=CH19_92;
  by descending Exposure descending Response;
run;

proc freq data=CH19_92 order=data;
  format Exposure ExpFmt. Response RspFmt.;
  tables Subject*Exposure*Response / CMH chisq riskdiff(equal var=null) relrisk;
  exact pchi or fisher;
  weight Count;
  title ' Prospective Study';
run;
