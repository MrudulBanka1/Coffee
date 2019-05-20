/* In Print Setup, set .5" margins */
options ls=85;

filename dyedat 'C:\Users\mpb7722\Documents\Coffee_2.txt';
data dye;
  infile dyedat;
  input plevel tocm toce;
  label plevel = 'PH Level';
  label tocm = 'Type of Coffee Machine';
  label toce = 'Type of Coffee';
  if plevel = . then delete; 	/* delete first line of file (recorded as missing) */
proc print;

proc plot;
  plot plevel*tocm=toce / vpos=19 hpos=50;
  plot plevel*tc / vpos=19 hpos=50;

/* Interaction Plot */
proc sort data=dye;
  by tocm toce;
proc means data=dye noprint mean var;
  var plevel;
  by tocm toce;
  output out=dye2 mean=avgY var=varY;
proc print;
  var tocm toce avgY varY;
proc plot data=dye2;
  plot avgY*tocm=toce / vpos=19 hpos=50;


axis1 label=(angle = 90);
axis2 offset=(5,5) minor=none;
symbol1 v=square c=black;
symbol2 v=plus c=black;
symbol3 v=circle c=black;
proc gplot data=dye;
  plot plevel*tocm=toce / vaxis=axis1 haxis=axis2;
run;

symbol1 v=plus c=black;
proc gplot data=dye;
  plot plevel*tc / vaxis=axis1 haxis=axis2;
run;

symbol1 v=square i=join c=black;
symbol2 v=plus i=join c=black;
symbol3 v=circle i=join c=black;
proc gplot data=dye2;
  plot avgY*tocm=toce / vaxis=axis1 haxis=axis2;
run;

proc glm data=dye;
  classes tocm toce;
  model plevel = tocm | toce;
  ESTIMATE 'French Press vs. Mr.Cofeemaker'
          tocm 1 -1;
  ESTIMATE 'Coffee<3 vs. Coffee = 3'   
          toce  1  1 -2 / divisor = 2;
  CONTRAST 'French Press vs. Mr.Cofeemaker'
          tocm 1 -1;
  means tocm | toce;
  lsmeans tocm | toce;
  output out=dyeout p=yhat r=e rstudent=tres;
run;

proc glm data=dye;
  classes tocm toce;
  model plevel = tocm toce;
run;

data outlier;
   tinvtres = tinv(0.999583333,5);
proc print;

/* create normal scores for residuals */
proc rank normal=blom out=enrm data=dyeout;
  var e;
  ranks enrm;
run;


/* NPP for complete set of orthogonal contrasts */


/* NPP for trt effects */
proc sort data=dye2;
  by avgY;

proc rank normal=blom out=avgYnrm data=dye2;
  var avgY;
  ranks avgYnrm;

data dye2new; set dye2; set avgYnrm;
  label avgYnrm='Normal Scores';
  expYnrm = 6.533333 + avgYnrm*sqrt(.008333/2);
  label expYnrm='Expected value under H0';
proc print;

goptions reset = all;
symbol1 v=dot c=black;		/* plot 1 has symbol=dot, color=black */
symbol2 v=none i=join c=black;	/* plot 2 has no symbol, line, color=black */
axis1 label=(angle = 90);
proc gplot data = dye2new;
  plot avgY*avgYnrm expYnrm*avgYnrm/ overlay vaxis=axis1;
run;



/* Plots */
data dyenew; set dyeout; set enrm;
  label e = 'Residuals';
  label tres = 'Studentized Deleted Residuals';
  label yhat = 'Estimated Mean PH level';
  label enrm = 'Normal Scores';
proc print;

proc corr data=dyenew;
  var e enrm;

goptions reset = all;
axis1 label=(angle = 90);
axis2 offset=(5,5) minor=none;
symbol1 v=dot c=black;
proc gplot data = dyenew;
  plot tres*yhat /vref = 0 vaxis = axis1 haxis = axis2;
  plot e*yhat /vref = 0 vaxis = axis1 haxis = axis2;
  plot e*enrm /vaxis = axis1;
run;

goptions reset = all;
axis1 label=(angle = 90 "Residuals (Type of Coffee Machine 1)");
axis2 label=(angle = 90 "Residuals (Type of Coffee Machine 2)");
axis3 minor=none;
symbol1 v=dot i=join c=black;
proc gplot data = dyenew;
  where tocm=1;
  plot e*toce /vaxis = axis1 haxis=axis3;
run;

proc gplot data = dyenew;
  where tocm=2;
  plot e*toce /vaxis = axis2 haxis=axis3;
run;

goptions reset = all;
axis1 label=(angle = 90 "Residuals (Type of Coffee 1)");
axis2 label=(angle = 90 "Residuals (Type of Coffee 2)");
axis3 label=(angle = 90 "Residuals (Type of Coffee 3)");
axis4 minor=none;
symbol1 v=dot i=join c=black;
proc gplot data = dyenew;
  where toce=1;
  plot e*tocm /vaxis = axis1 haxis=axis4;
run;

proc gplot data = dyenew;
  where toce=2;
  plot e*tocm /vaxis = axis2 haxis=axis4;
run;
proc gplot data = dyenew;
  where toce=3;
  plot e*tocm /vaxis = axis2 haxis=axis4;
run;

/*Modified Levene*/
data dyemod; set dyeout;
  group = 1;
  if yhat > 6.3 then group = 2;
proc sort data = dyemod;
  by group;
proc univariate data = dyemod noprint;
  by group;
  var e;
  output out=mout median=mede;
proc print data = mout;
  var group mede;
data mtemp; merge dyemod mout;
  by group;
  d = abs(e - mede);
proc sort data = mtemp;
  by group;
proc means data = mtemp noprint;
by group;
var d;
output out= mout1 mean= meand;
proc print data = mout1;
var group meand;
data mtemp1; merge mtemp mout1;
by group;
ddif = (d - meand)**2;
proc sort data = mtemp1;
by group yhat;
proc ttest data= mtemp;
class group;
var d;
run;

/*TRANSFORMATIONS*/
data dye2; set dye;
/*  logtenY=log10(plevel);
pow = 2; */

/*logtenY = (plevel**pow)
logtenY = arsin(plevel);
proc print;
  var logtenY tocm toce;

 proc glm data=dye2;
  classes tocm toce;
  model logtenY = tocm | toce;
  means tocm | toce;
  output out=dyeout2 p=yhat2 r=e2;
run;

proc rank normal=blom out=enrm2 data=dyeout2;
  var e2;
  ranks enrm2;
run;

data dyenew2; set dyeout2; set enrm2;
  label e2 = 'Residuals';
  label yhat2 = 'Estimated Mean PH level';
  label enrm2 = 'Normal Scores';
proc print;

proc corr data=dyenew2;
  var e2 enrm2;

goptions reset = all;
axis1 label=(angle = 90);
axis2 offset=(5,5) minor=none;
symbol1 v=dot c=black;
proc gplot data = dyenew2;
  plot e2*yhat2 /vref = 0 vaxis = axis1 haxis = axis2;
  plot e2*enrm2 /vaxis = axis1;
run;

*/


/* Weighted Least Square*/


/*TRANSFORMATIONS for treatment combination*/
data dye3; set dye;
logtenY = plevel;
/*  logtenY=log10(plevel);
pow = 2;
tc = 3*(tocm - 1)+toce;*/
/*logtenY = (plevel**pow);
logtenY = sin(plevel);*/
proc print;
  var logtenY tc;

 proc glm data=dye3;
  classes tc;
  model logtenY = tc;
  means tc;
  output out=dyeout3 p=yhat3 r=e3;
run;

proc rank normal=blom out=enrm3 data=dyeout3;
  var e3;
  ranks enrm3;
run;

data dyenew3; set dyeout3; set enrm3;
  label e3 = 'Residuals';
  label yhat3 = 'Estimated Mean PH level';
  label enrm3 = 'Normal Scores';
proc print;

proc corr data=dyenew3;
  var e3 enrm3;

goptions reset = all;
axis1 label=(angle = 90);
axis2 offset=(5,5) minor=none;
symbol1 v=dot c=black;
proc gplot data = dyenew3;
  plot e3*yhat3 /vref = 0 vaxis = axis1 haxis = axis2;
  plot e3*enrm3 /vaxis = axis1;
run;
/* Calculate weights */
data tempw;
  set dye3;
  tcm1 = 0;
  if tc=1 then tcm1 = 1;
  tcm2 = 0;
  if tc=2 then tcm2 = 1;
  tcm3 = 0;
  if tc=3 then tcm3 = 1;
  tcm4 = 0;
  if tc=4 then tcm4 = 1;
  tcm5 = 0;
  if tc=5 then tcm5 = 1;
  tcm6 = 0;
  if tc=6 then tcm6 = 1;
run;
proc sql;
  create table abtwts as
  select *, 1/( var( plevel) ) as w
  from tempw
  group by tc;

/* ANOVA w/ weights */
proc glm data=abtwts;
  classes tc;
  model plevel = tc;
  weight w;
  means tc;
  output out=dyeout4 p=yhat4 r=e4;
run;
/*proc reg data=abtwts;*/
/*  weight w;*/
/*  model plevel = tc;*/
/*run;*/
proc rank normal=blom out=enrm4 data=dyeout4;
  var e4;
  ranks enrm4;
run;

data dyenew4; set dyeout4; set enrm4;
  label e4 = 'Residuals';
  label yhat4 = 'Estimated Mean PH level';
  label enrm4 = 'Normal Scores';
proc print;

proc corr data=dyenew4;
  var e4 enrm4;

goptions reset = all;
axis1 label=(angle = 90);
axis2 offset=(5,5) minor=none;
symbol1 v=dot c=black;
proc gplot data = dyenew4;
  plot e4*yhat4 /vref = 0 vaxis = axis1 haxis = axis2;
  plot e4*enrm4 /vaxis = axis1;
run;

/*Modified Levene*/
data dyemod4; set dyeout4;
  group = 1;
  if yhat4 > 6.3 then group = 2;
proc sort data = dyemod4;
  by group;
proc univariate data = dyemod4 noprint;
  by group;
  var e4;
  output out=mout4 median=mede4;
proc print data = mout4;
  var group mede4;
data mtemp4; merge dyemod4 mout4;
  by group;
  d4 = abs(e4 - mede4);
proc sort data = mtemp4;
  by group;
proc means data = mtemp4 noprint;
by group;
var d4;
output out= mout14 mean= meand4;
proc print data = mout14;
var group meand4;
data mtemp14; merge mtemp4 mout14;
by group;
ddif4 = (d4 - meand4)**2;
proc sort data = mtemp14;
by group yhat4;
proc ttest data= mtemp4;
class group;
var d4;
run;`
