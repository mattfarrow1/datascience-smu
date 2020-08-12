/* Inport the data */
PROC IMPORT OUT= WORK.crab 
            DATAFILE= "\\Mac\Home\Documents\DataScience@SMU\MSDS-6371-St
at-Foundations\Unit 12\Crab17.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* Verify the original data */
proc print data = crab;
run;

/* Look at the original data */
proc gplot data = crab;
plot Force*Height=Species;
run;

/* Import and prep the data for analysis */
data crabs; /* change the name from the original */
set crab;   /* original data to start with */
/* Assign numeric label to species */
    if Species = "Cancer productus" then number = 1;
	    else if species = "Hemigrapsus nudus" then number = 2;
		else if species = "Lophopanopeus bellus" then number = 3;
/* Coding indicator variables for species */
    if Species = "Cancer productus" then d1 = 1; else d1 = 0;
	if Species = "Hemigrapsus nudus" then d2 = 1; else d2 = 0;
	int1 = d1*Height; int2 = d2*Height;
run;

/* Verify the new data */
proc print data = crabs;
run;

/* Step 3: Build a model using the indicator variables */
proc reg data = crabs;
model Force = d1 d2 /R CLB;
title "Regression of Force on Indicator Variable Coefficients";
run;




