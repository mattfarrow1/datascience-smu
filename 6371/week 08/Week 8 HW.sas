/* Load Data */
PROC IMPORT OUT= WORK.baseball 
            DATAFILE= "\\Mac\Home\Documents\DataScience@SMU\MSDS-6371-St
at-Foundations\Unit 8\Baseball_Data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* View Data */
proc print data = baseball;
run;

/* Initial Scatterplot */
proc sgscatter data = baseball;
plot Wins*Payroll;
run;

/* Calculate Critical Value */
data baseball;
critval = quantile('t', .975, 30-2);
run;

/* Print Critical Value */
proc print data = baseball;
run;

/* Run Correlation */
proc corr data = baseball;
run;





