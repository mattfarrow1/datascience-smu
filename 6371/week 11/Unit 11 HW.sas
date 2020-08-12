/* Load data */
PROC IMPORT OUT= WORK.metabolism 
            DATAFILE= "\\Mac\Home\Documents\DataScience@SMU\MSDS-6371-St
at-Foundations\Unit 11\Metabolism Data Prob 26.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* Check data */
proc print data = metabolism;
run;

/* Raise the mass to 3/4 power */
data metabolism;
set metabolism;
mass_34 = mass ** 0.75;
run;

/* Double-check to make sure I did that correctly */
proc print data = metabolism;
run;

/* Check assumptions with original data */
proc glm data = metabolism plots = all;
model Metab = mass_34;
run;

/* Log-transform the data */
data metabolism;
set metabolism;
log_mass_34 = log(mass_34);
log_metab = log(metab);
run;

/* Check assumptions of log-transformed data */
proc glm data = metabolism plots = all;
model log_metab = log_mass_34 /clparm;
run;
