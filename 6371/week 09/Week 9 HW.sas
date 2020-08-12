PROC IMPORT OUT= WORK.baseball 
            DATAFILE= "\\Mac\Home\Documents\DataScience@SMU\MSDS-6371-St
at-Foundations\Unit 9\Baseball_Data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print data = baseball;
run;

proc glm data = baseball;
model wins = payroll;
run;

proc reg data = baseball;
model wins = payroll;
run;

PROC IMPORT OUT= WORK.test 
            DATAFILE= "\\Mac\Home\Documents\DataScience@SMU\MSDS-6371-St
at-Foundations\Unit 9\TEST DATA.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc glm data = test;
run;
