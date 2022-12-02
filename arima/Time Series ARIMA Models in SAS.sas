* Time Series ARIMA Models in SAS;
* Copyright 2013 by Ani Katchova;

proc import out= work.data
datafile= "C:\Econometrics\Data\timeseries_ppi.csv" 
dbms=csv replace; getnames=yes; datarow=2; 
run;

* Creating a differenced variable;
data data; 
set data; 
dppi=dif(ppi); 
lppi=lag(ppi);
ldppi=lag(dppi);
run; 

%let ylist = ppi;
%let dylist = dppi;
%let time = t;
%let lylist = lppi;
%let trend = trend;
%let xlist = cpi gdp;

proc means data=data;
var &ylist &dylist &time;
run;

* Plotting the data;
proc gplot data=data;
plot &ylist*&time;
plot &dylist*&time;
run;

* ARIMA identification;
proc arima data=data;
identify var=&ylist stationarity=(adf);
run;

* Dickey-Fuller test regressions;
proc reg data=data;
model &dylist = &lylist;
model &dylist = &lylist &trend;
run; 

* ARIMA for differenced variable;
proc arima data=data;
identify var=&ylist(1) stationarity=(adf);
run;


* ARIMA(1,0,0) or AR(1);
proc arima data=data;
identify var=&ylist;
estimate p=1 method=ml; 
run;

* ARIMA(2,0,0) or AR(2);
proc arima data=data;
identify var=&ylist;
estimate p=2;
run;

* ARIMA(0,0,1) or MA(1);
proc arima data=data;
identify var=&ylist;
estimate q=1;
run;

* ARIMA(1,0,1) or ARMA(1,1);
proc arima data=data;
identify var=&ylist;
estimate p=1 q=1;
run;

* ARIMA(1,1,0);
proc arima data=data;
identify var=&dylist;
estimate p=1;
run;

* ARIMA(0,1,1);
proc arima data=data;
identify var=&dylist;
estimate q=1;
run;

* ARIMA(1,1,1);
proc arima data=data;
identify var=&dylist;
estimate p=1 q=1;
run;

* ARIMA(1,1,3);
proc arima data=data;
identify var=&dylist;
estimate p=1 q=3;
run;

* ARIMA(2,1,3);
proc arima data=data;
identify var=&dylist;
estimate p=2 q=3;
run;

* ARIMA(2,0,1) with independent variables;
proc arima data=data;
identify var=&ylist crosscorr=(&xlist);
estimate input=(&xlist) p=2 q=1 plot;
run;

* ARIMA (1,0,1) forecasting;
proc arima data=data;
identify var=&ylist;
estimate p=1 q=1;
forecast lead=12;
run;

* ARIMA (1,1,1) forecasting;
proc arima data=data;
identify var=&dylist;
estimate p=1 q=1;
forecast lead=12;
run;
