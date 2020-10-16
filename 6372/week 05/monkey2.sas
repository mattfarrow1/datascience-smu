* Load data;
data monkey;
infile 'Y:\R\datascience-smu\6372\week 05\monkey.csv' dlm=',' firstobs=2;
input Monkey $ Treatment $ Week  Memory $ NewPercent;
run;

* Examine the data;
proc print data = monkey;
run;

*Lets first run a basic two way anova;
proc glm data=monkey;
class Treatment Memory Week;
model NewPercent=Treatment Week Treatment*Week;
estimate 'What is this contrast?' Treatment -1 1 Treatment*Week -.5 -.5 0 0 0 .5 .5 0 0 0;
lsmeans Treatment*Week / pdiff tdiff adjust=Tukey;
run;

*Note the combination order to help you with the contrasts is
  Treatment "Control" "Treatment"
  Week 2 4 8 12 16;

*Now, we know that we have repeated measures on the same monkey and thus correlated data.
Let's see what happens if we try to handle that in proc mixed using a repeated statement;
*The type=CS option is the compound symmetry assumption Dr.  McGee's video mentions.  
We will discuss this in more detail later;

proc mixed data=monkey;
class Treatment Memory Monkey Week;
model NewPercent=Treatment Week Treatment*Week;
repeated Week/ type=CS subject=Monkey;
lsmeans Treatment*Week / pdiff tdiff adjust=Tukey;
estimate 'What is this contrast?' Treatment -1 1 Treatment*Week -.5 -.5 0 0 0 .5 .5 0 0 0;
run;



/* Pre-Live Code  */
proc glm data = monkey;
class Treatment Memory Week;
model NewPercent = Treatment Week Treatment*Week;
estimate 'What is this contrast? (GLM)' 
	Treatment -1 1 
	Treatment*Week -.5 -.5 0 0 0 .5 .5 0 0 0;
estimate 'Week 2: Treatment vs. Control (GLM)' 
	Treatment -1 1
	Treatment*Week -1 0 0 0 0 1 0 0 0 0;
estimate 'Week 2 vs. Week 12: Treated (GLM)'
	Week -1 0 0 1 0
	Treatment*Week 0 0 0 0 0 -1 0 0 1 0;
lsmeans Treatment*Week / pdiff tdiff adjust=Tukey;
run;

proc mixed data = monkey;
class Treatment Memory Monkey Week;
model NewPercent = Treatment Week Treatment*Week;
repeated Week/ type=CS subject=Monkey;
lsmeans Treatment*Week / pdiff tdiff adjust=Tukey;
estimate 'What is this contrast? (Mixed)' 
	Treatment -1 1 
	Treatment*Week -.5 -.5 0 0 0 .5 .5 0 0 0;
estimate 'Week 2: Treatment vs. Control (Mixed)' 
	Treatment -1 1 
	Treatment*Week -1 0 0 0 0 1 0 0 0 0;
estimate 'Week 2 vs. Week 12: Treated (Mixed)'
	Week -1 0 0 1 0
	Treatment*Week 0 0 0 0 0 -1 0 0 1 0;
run;
