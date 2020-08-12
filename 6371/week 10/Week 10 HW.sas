PROC IMPORT OUT= WORK.bird 
            DATAFILE= "\\Mac\Home\Documents\DataScience@SMU\MSDS-6371-St
at-Foundations\Unit 10\Male Display Data Set.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print data = bird;
run;

proc glm data = bird;
model Tcell = Mass / solution clm cli alpha = 0.01;
run;

data Xvalues;
input Mass Tcell;
cards;
4.5 .
;

data bird;
set Xvalues bird;
;

proc reg data = bird;
model Tcell = Mass / clb cli clm alpha = 0.01;
run;

data bird;
run;

proc sgplot data = bird;
yaxis grid;
refline 0 / axis=y;
scatter y = Tcell x = Mass;
run;
