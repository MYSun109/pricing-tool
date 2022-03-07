# pricing-tool
library(shiny)
library(shinythemes)
library(ggplot2)


Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)
Lx = Lxs[-(1:3)]

Wholelife_annuity = function(age,ir){
  lx = Lx[-(1:(age-20))]; if (age==20) lx = Lx
  V = 1/(1+ir)
  epv = sum( (V^(0:(120-age)))*lx )/Lx[age-20+1]
  return(epv)} ##ax

Wholelife_annuity_inadvance = function(age,ir){
  epv =Wholelife_annuity (age,ir)+1
  return(epv)} #ax..

Wholelife_assurance = function(age,ir){
  DD = ir/(1+ir)
  epv = 1 - DD*Wholelife_annuity(age,ir)
  return(epv)}  ##Ax

pureendowment = function(age,ir,BenefitTerm){
  discountrate = 1/(1 + ir)
  survivalprobabilities = (Lx[age-20 + 1+BenefitTerm]/Lx[age-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm)
  Output = alldiscountrates * survivalprobabilities
  return(Output)} ##


termassurance = function(age,ir,BenefitTerm){
  Output= Wholelife_assurance(age,ir)-pureendowment(age,ir,BenefitTerm)*Wholelife_assurance(age+BenefitTerm,ir)
  return(Output)} ##
termassurance(30,0.04,10)
  
  
endowmentassurance= function(age,ir,BenefitTerm){
  Output= pureendowment(age,ir,BenefitTerm)+termassurance(age,ir,BenefitTerm)
  return(Output)} ##

Wholelifeannuity = function(age,interestrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lx[-(1:(age-20 + 1))]/Lx[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  Output = sum(c(1, (alldiscountrates * survivalprobabilities)))
  return(Output)} ## in advance


Termannuitydue_advance = function(age,interestrate,BenefitTerm){
  discountfactor = (Lx[age-20+1+BenefitTerm]/Lx[age-20+1]) * ((1/(1 + interestrate))^BenefitTerm)
  Output = Wholelifeannuity(age,interestrate) - discountfactor*Wholelifeannuity(age+BenefitTerm,interestrate)
  return(Output)} ## a..xn

Wholelifeannuity_arrear = function(age,interestrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lx[-(1:(age-20 + 1))]/Lx[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  Output = sum(c((alldiscountrates * survivalprobabilities)))
  return(Output)} ##


Termannuity= function(age,interestrate,BenefitTerm){
  discountfactor = (Lx[age-20+1+BenefitTerm]/Lx[age-20+1]) * ((1/(1 + interestrate))^BenefitTerm)
  Output = Wholelifeannuity_arrear(age,interestrate) - discountfactor*Wholelifeannuity_arrear(age+BenefitTerm,interestrate)
  return(Output)} ## a xn in arrear



pureendowment_y = function(age,ir,BenefitTerm){
  discountrate = 1/(1 + ir)
  survivalprobabilities = (Lxs[age-20 + 1+BenefitTerm]/Lxs[age-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm)
  Output = alldiscountrates * survivalprobabilities
  return(Output)} ## for group y

Wholelife_annuity_y = function(age,ir){
  lxs = Lxs[-(1:(age-20))]; if (age==20) lxs = Lxs
  V = 1/(1+ir)
  epv = sum( (V^(0:(120-age)))*lxs )/Lxs[age-20+1]
  return(epv)} ##ax for group y

Wholelife_assurance_y = function(age,ir){
  DD = ir/(1+ir)
  epv = 1 - DD*Wholelife_annuity_y(age,ir)
  return(epv)}  ##Ax for group y

termassurance_y = function(age,ir,BenefitTerm){
  Output= Wholelife_assurance_y(age,ir)-pureendowment_y(age,ir,BenefitTerm)*Wholelife_assurance_y(age+BenefitTerm,ir)
  return(Output)} ## for group y

endowmentassurance_y= function(age,ir,BenefitTerm){
  Output= pureendowment_y(age,ir,BenefitTerm)+termassurance_y(age,ir,BenefitTerm)
  return(Output)} ## for group y

Wholelifeannuity_y = function(age,interestrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lxs[-(1:(age-20 + 1))]/Lxs[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  Output = sum(c(1, (alldiscountrates * survivalprobabilities)))
  return(Output)} ## for group y

Termannuitydue_advance_y = function(age,interestrate,BenefitTerm){
  discountfactor = (Lxs[age-20+1+BenefitTerm]/Lxs[age-20+1]) * ((1/(1 + interestrate))^BenefitTerm)
  Output = Wholelifeannuity_y(age,interestrate) - discountfactor*Wholelifeannuity_y(age+BenefitTerm,interestrate)
  return(Output)} ## a..xn for group y

Wholelifeannuity_arrear_y = function(age,interestrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lxs[-(1:(age-20 + 1))]/Lxs[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  Output = sum(c((alldiscountrates * survivalprobabilities)))
  return(Output)} ## for y


Termannuity_y= function(age,interestrate,BenefitTerm){
  discountfactor = (Lxs[age-20+1+BenefitTerm]/Lxs[age-20+1]) * ((1/(1 + interestrate))^BenefitTerm)
  Output = Wholelifeannuity_arrear(age,interestrate) - discountfactor*Wholelifeannuity_arrear(age+BenefitTerm,interestrate)
  return(Output)} ## a xn in arrear for y
 



Wholelife_assurance_2 = function(age,ir){
  DD = ir/(1+ir)
  epv = 1 - DD*Wholelifeannuity(age,ir)
  return(epv)}  ##Ax ???



Termannuitydue_advance_reserve = function(age,interestrate,BenefitTerm){
  year=c(1:BenefitTerm)
  discountfactor = (Lx[age+year-20+1+BenefitTerm-year]/Lx[age+year-20+1]) * ((1/(1 + interestrate))^(BenefitTerm-year))
  Output = Wholelifeannuity(age+year,interestrate) - discountfactor*Wholelifeannuity(age+BenefitTerm,interestrate)
  return(Output)} ## a..xn ???

Wholelifeass=function(age,ir){
  lx1=Lx[-(1:1)]
  l=Lx-lx1
  lmin=l[1:100]
  ll = lmin[-(1:(age-20))]; if (age==20) ll = lmin
  V = 1/(1+ir)
  epv = sum( (V^(1:length(ll)))*(ll) )/Lx[age-20+1]
  return(epv)} ##AX
Wholelifeass(30,0.04)


Wholelifeass_Increase = function(age,ir){
  lx1=Lx[-(1:1)]
  l=Lx-lx1
  lmin=l[1:100]
  ll = lmin[-(1:(age-20))]; if (age==20) ll = lmin
  V = 1/(1+ir)
  epv = sum( (V^(1:length(ll)))*(ll)*(1:length(ll)))/Lx[age-20+1]
  return(epv)} ##IAX
Wholelifeass_Increase(30,0.04)

Wholelifeass_Increase_y = function(age,ir){
  lx1=Lxs[-(1:1)]
  l=Lxs-lx1
  lmin=l[1:100]
  ll = lmin[-(1:(age-20))]; if (age==20) ll = lmin
  V = 1/(1+ir)
  epv = sum( (V^(1:length(ll)))*(ll)*(1:length(ll)))/Lxs[age-20+1]
  return(epv)} ##IAX for y
Wholelifeass_Increase_y(30,0.04)

termass_Increase = function(age,ir,BenefitTerm){
  discountrate = 1/(1 + ir)
  survivalprobabilities = (Lx[age-20 + 1+BenefitTerm]/Lx[age-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm)
  epv = Wholelifeass_Increase(age,ir)-survivalprobabilities*alldiscountrates*(Wholelifeass_Increase(age+BenefitTerm,ir)+BenefitTerm*Wholelifeass(age+BenefitTerm,ir))
  return(epv)} ##IAX:n
termass_Increase(30,0.04,10)

Wholelifeass_y = function(age,ir){
  lx1=Lxs[-(1:1)]
  l=Lxs-lx1
  lmin=l[1:100]
  ll = lmin[-(1:(age-20))]; if (age==20) ll = lmin
  V = 1/(1+ir)
  epv = sum( (V^(1:length(ll)))*(ll) )/Lxs[age-20+1]
  return(epv)} ##AX for y
Wholelifeass_y(30,0.04)


termass_Increase_y = function(age,ir,BenefitTerm){
  discountrate = 1/(1 + ir)
  survivalprobabilities = (Lxs[age-20 + 1+BenefitTerm]/Lxs[age-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm)
  epv = Wholelifeass_Increase_y(age,ir)-survivalprobabilities*alldiscountrates*(Wholelifeass_Increase_y(age+BenefitTerm,ir)+BenefitTerm*Wholelifeass_y(age+BenefitTerm,ir))
  return(epv)} ##IAX:n for y
termass_Increase_y(30,0.04,10)


Wholelifeannuity_inflation = function(age,interestrate,inflationrate){
  discountrate = 1/(1 + interestrate)
  inflationrt=(1+inflationrate)
  survivalprobabilities = (Lx[-(1:(age-20 + 1))]/Lx[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  allinflationrt = inflationrt^(0:(length(survivalprobabilities)-1))
  Output = sum(c((alldiscountrates * allinflationrt*survivalprobabilities)))
  return(Output)} ##
Wholelifeannuity_inflation(20,0.04,0) # 22.08  22.08121



Wholelifeannuity_inflation_y = function(age,interestrate,inflationrate){
  discountrate = 1/(1 + interestrate)
  inflationrt=(1+inflationrate)
  survivalprobabilities = (Lxs[-(1:(age-20 + 1))]/Lxs[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  allinflationrt = inflationrt^(0:(length(survivalprobabilities)-1))
  Output = sum(c((alldiscountrates * allinflationrt*survivalprobabilities)))
  return(Output)} ## for y




termannuity_inflation = function(age,interestrate,BenefitTerm,inflationrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lx[age-20 + 1+BenefitTerm]/Lx[age-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm)
  allinflationrt = (1+inflationrate)^(BenefitTerm)
  Output = Wholelifeannuity_inflation(age,interestrate,inflationrate) - allinflationrt*alldiscountrates*survivalprobabilities*Wholelifeannuity_inflation(age+BenefitTerm,interestrate,inflationrate)
  return(Output)} ## ???



termannuity_inflation_y = function(age,interestrate,BenefitTerm,inflationrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lxs[age-20 + 1+BenefitTerm]/Lxs[age-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm)
  allinflationrt = (1+inflationrate)^(BenefitTerm)
  Output = Wholelifeannuity_inflation_y(age,interestrate,inflationrate) - allinflationrt*alldiscountrates*survivalprobabilities*Wholelifeannuity_inflation_y(age+BenefitTerm,interestrate,inflationrate)
  return(Output)} ## ???




Wholelifeass_Increase_y = function(age,ir){
  lx1=Lxs[-(1:1)]
  l=Lxs-lx1
  lmin=l[1:100]
  ll = lmin[-(1:(age-20))]; if (age==20) ll = lmin
  V = 1/(1+ir)
  epv = sum( (V^(1:length(ll)))*(ll)*(1:length(ll)))/Lxs[age-20+1]
  return(epv)} ##IAX for y
Wholelifeass_Increase_y(30,0.04)


########-RESERVE-#####

### for pure endowment
pureendowment_reserve = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  discountrate = 1/(1 + ir)
  survivalprobabilities = (Lx[age+year-20 + 1+BenefitTerm-year]/Lx[age+year-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm-year)
  Output = ((alldiscountrates * survivalprobabilities)*(1+BenefitTerm*BonusRate))*(1+ClaimExpRT);
  return(Output)} ## for reserve single 5th 1.801269 
pureendowment_reserve(30,0.04,10,0.1,0.1)


pureendowment_reserve_y = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  discountrate = 1/(1 + ir)
  survivalprobabilities = (Lxs[age+year-20 + 1+BenefitTerm-year]/Lxs[age+year-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm-year)
  Output = ((alldiscountrates * survivalprobabilities)*(1+BenefitTerm*BonusRate))*(1+ClaimExpRT);
  return(Output)} ## for reserve single  for y 
pureendowment_reserve_y(30,0.04,10,0.1,0.1)


pureendowment_reserve_l = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT,InitialExpRT,PremiumExpRT){
  year=c(1:BenefitTerm)
  discountrate = 1/(1 + ir)
  survivalprobabilities = (Lx[age+year-20 + 1+BenefitTerm-year]/Lx[age+year-20 + 1])
  alldiscountrates = discountrate^(BenefitTerm-year)
  B=alldiscountrates*survivalprobabilities*(1+BonusRate*(BenefitTerm-year))
  p=(B*(1+ClaimExpRT))/(Termannuitydue_advance(age+year,ir,BenefitTerm-year)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance(age+year,ir,BenefitTerm-year)-1))
  Output = B*(1+ClaimExpRT)+(PremiumExpRT-1)*p*(Termannuitydue_advance(age+year,ir,BenefitTerm-year));
  return(Output)} ## for reserve level ???
pureendowment_reserve_l(30,0.04,10,0,0,0,0)


### for termass

termassurance_reserve = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*((Wholelife_assurance(age+year[i],ir)-pureendowment(age+year[i],ir,BenefitTerm-year[i])*Wholelifeass (age+year[i]+BenefitTerm-year[i] ,ir))*(1+BonusRate*BenefitTerm)+BonusRate*termass_Increase(age+year[i],ir,BenefitTerm-year[i]))}
  return(vector)} ## for reserve  single 
termassurance_reserve(30,0.04,10,0,0)

termassurance_reserve_y = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*((Wholelifeass_y(age+year[i],ir)-pureendowment_y(age+year[i],ir,BenefitTerm-year[i])*Wholelifeass_y(age+year[i]+BenefitTerm-year[i] ,ir))*(1+BonusRate*BenefitTerm)+BonusRate*termass_Increase_y(age+year[i],ir,BenefitTerm-year[i]))}
  return(vector)} ## for reserve  single  for y 
termassurance_reserve_y(30,0.04,10,0,0)

### for endowment assurance

endowass_reserve = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*(endowmentassurance(age+year[i],ir,BenefitTerm-year[i])+BonusRate*(year[i]*termassurance(age+year[i],ir,BenefitTerm-year[i])+termass_Increase(age+year[i],ir,BenefitTerm-year[i])+BenefitTerm*pureendowment(age+year[i],ir,BenefitTerm-year[i])))}
  return(vector)} ## for reserve  single 


endowass_reserve_y = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*(endowmentassurance_y(age+year[i],ir,BenefitTerm-year[i])+BonusRate*(year[i]*termassurance_y(age+year[i],ir,BenefitTerm-year[i])+termass_Increase_y(age+year[i],ir,BenefitTerm-year[i])+BenefitTerm*pureendowment_y(age+year[i],ir,BenefitTerm-year[i])))}
  return(vector)} ## for reserve  single  for y
endowass_reserve_y(30,0.04,10,0,0)

endowass_reserve_con = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]= (1+ClaimExpRT)*(((1+ir)^0.5)*termassurance(age+year[i],ir,BenefitTerm-year[i])+pureendowment(age+year[i],ir,BenefitTerm-year[i])+BonusRate*((1+ir)^0.5)*termass_Increase(age+year[i],ir,BenefitTerm-year[i])+BonusRate*(BenefitTerm-year[i])*pureendowment(age+year[i],ir,BenefitTerm-year[i])) }
  return(vector)} ## for reserve  single  con
endowass_reserve_con(30,0.04,10,0,0)

endowass_reserve_con_y = function(age,ir,BenefitTerm,BonusRate,ClaimExpRT){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]= (1+ClaimExpRT)*(((1+ir)^0.5)*termassurance_y(age+year[i],ir,BenefitTerm-year[i])+pureendowment_y(age+year[i],ir,BenefitTerm-year[i])+BonusRate*((1+ir)^0.5)*termass_Increase_y(age+year[i],ir,BenefitTerm-year[i])+BonusRate*(BenefitTerm-year[i])*pureendowment_y(age+year[i],ir,BenefitTerm-year[i])) }
  return(vector)} ## for reserve  single for y con
endowass_reserve_con_y(30,0.04,10,0,0)


### for whole life ass

wholeass_reserve = function(age,ir,BonusRate,ClaimExpRT){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*((1+BonusRate*year[i])*Wholelife_assurance(age+year[i],ir)+BonusRate*Wholelifeass_Increase(age+year[i],ir))}
  return(vector)} ## for reserve  single 
wholeass_reserve(30,0.04,0,0)

wholeass_reserve_y = function(age,ir,BonusRate,ClaimExpRT){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*((1+BonusRate*year[i])*Wholelife_assurance_y(age+year[i],ir)+BonusRate*Wholelifeass_Increase_y(age+year[i],ir))}
  return(vector)} ## for reserve  single for y
wholeass_reserve_y(30,0.04,0,0)

wholeass_reserve_c = function(age,ir,BonusRate,ClaimExpRT){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=((1+ir)^0.5)*(1+ClaimExpRT)*((1+BonusRate*year[i])*Wholelife_assurance(age+year[i],ir)+BonusRate*Wholelifeass_Increase(age+year[i],ir))}
  return(vector)} ## for reserve  single 
wholeass_reserve_c(30,0.04,0,0)

wholeass_reserve_c_y = function(age,ir,BonusRate,ClaimExpRT){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=((1+ir)^0.5)*(1+ClaimExpRT)*((1+BonusRate*year[i])*Wholelife_assurance(age+year[i],ir)+BonusRate*Wholelifeass_Increase(age+year[i],ir))}
  return(vector)} ## for reserve  single for y
wholeass_reserve_c_y(30,0.04,0,0)

### for whole life annuity

wholeann_reserve = function(age,ir,ClaimExpRT,inflationrate){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*(Wholelifeannuity_inflation(age+year[i],ir,inflationrate)*((1+inflationrate)^year[i]))}
  return(vector)} ## for reserve  single 
wholeann_reserve(30,0.04,0,0)


wholeann_reserve_y = function(age,ir,ClaimExpRT,inflationrate){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*(Wholelifeannuity_inflation_y(age+year[i],ir,inflationrate)*((1+inflationrate)^year[i]))}
  return(vector)} ## for reserve  single  for y
wholeann_reserve_y(30,0.04,0,0)

### for Term Annuity

termann_reserve = function(age,ir,BenefitTerm,ClaimExpRT,inflationrate){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(termannuity_inflation(age+year[i],ir,BenefitTerm-year[i],inflationrate))*(ClaimExpRT+1)}
  return(vector)} ## for reserve  single 
termann_reserve(30,0.04,10,0,0)

termann_reserve_y = function(age,ir,BenefitTerm,ClaimExpRT,inflationrate){
  year=c(1:BenefitTerm)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(termannuity_inflation_y(age+year[i],ir,BenefitTerm-year[i],inflationrate))*(ClaimExpRT+1)}
  return(vector)} ## for reserve  single for y
termann_reserve_y(30,0.04,10,0,0)

######-Joint product-#####

### for Joint whole life annuity

Wholelifeannuity_joint = function(ageA,ageB,interestrate){
  discountrate = 1/(1 + interestrate)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[-(1:(ageA-20 + 1))]/Lx[ageA-20 + 1])
  
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[-(1:(ageB-20 + 1))]/Lxs[ageB-20 + 1])
  
  alldiscountrates = discountrate^(1:length(survivalprobabilitiesA))
  
  Output = sum(c(alldiscountrates * (survivalprobabilitiesA*survivalprobabilitiesB)))
  return(Output)}

Wholelifeannuity_joint(30,28,0.04) #20.07288



Wholelifeannuity_inflation_joint = function(ageA,ageB,interestrate,inflationrate){
  discountrate = 1/(1 + interestrate)
  inflationrt=(1+inflationrate)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[-(1:(ageA-20 + 1))]/Lx[ageA-20 + 1])
  
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[-(1:(ageB-20 + 1))]/Lxs[ageB-20 + 1])
  
  alldiscountrates = discountrate^(1:length(survivalprobabilitiesA))
  allinflationrt = inflationrt^(0:(length(survivalprobabilitiesA)-1))
  Output = sum(c((alldiscountrates * allinflationrt*survivalprobabilitiesA*survivalprobabilitiesB)))
  return(Output)} ##

Wholelifeannuity_inflation_joint(30,28,0.04,0) #20.07288 20.0728829227562

#reserve 

wholeann_reserve_joint = function(ageA,ageB,ir,inflationrate,ClaimExpRT){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*(Wholelifeannuity_inflation_joint(ageA+year[i],ageB+year[i],ir,inflationrate)*(1+inflationrate)^year[i])}
  return(vector)} ## for reserve  single 
wholeann_reserve_joint(30,28,0.04,0,0) 



### for Joint term life annuity

termannuity_joint = function(ageA,ageB,interestrate,Term){
  discountrate = 1/(1 + interestrate)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[ageA+Term-20 + 1]/Lx[ageA-20 + 1])
  
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[ageB+Term-20 + 1]/Lxs[ageB-20 + 1])
  
  alldiscountrates = discountrate^(Term)
  
  Output = Wholelifeannuity_joint(ageA,ageB,interestrate)-alldiscountrates*survivalprobabilitiesA*survivalprobabilitiesB*Wholelifeannuity_joint(ageA+Term,ageB+Term,interestrate)
  return(Output)}
termannuity_joint(30,28,0.04,10) #8.059535



termannuity_joint_inflation = function(ageA,ageB,interestrate,Term,inflationrate){
  discountrate = 1/(1 + interestrate)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[ageA+Term-20 + 1]/Lx[ageA-20 + 1])
  
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[ageB+Term-20 + 1]/Lxs[ageB-20 + 1])
  
  alldiscountrates = discountrate^(Term)
  
  Output = Wholelifeannuity_inflation_joint(ageA,ageB,interestrate,inflationrate)-alldiscountrates*survivalprobabilitiesA*survivalprobabilitiesB*Wholelifeannuity_inflation_joint(ageA+Term,ageB+Term,interestrate,inflationrate)
  return(Output)}
termannuity_joint_inflation(30,30,0.04,0,0) #?? 

#reserve
 termann_reserve_joint = function(ageA,ageB,ir,Term,inflationrate,ClaimExpRT){
  discountrate = 1/(1 + ir)
  alldiscountrates = discountrate^(Term)
  year=c(1:Term)
  vector=c()
  for (i in 1:length(year)){
    # for the FIRST person(in group X)
    survivalprobabilitiesA = (Lx[ageA+Term-20 + 1]/Lx[ageA-20 + 1+year[i]])
    
    # for the SECOND person(in group Y)
    survivalprobabilitiesB = (Lxs[ageB+Term-20 + 1]/Lxs[ageB-20 + 1+year[i]])
    
    vector[i]=(1+ClaimExpRT)*(Wholelifeannuity_inflation_joint(ageA+year[i],ageB+year[i],ir,inflationrate)*(1+inflationrate)^year[i]-discountrate^(Term-year[i])*survivalprobabilitiesA*survivalprobabilitiesB*Wholelifeannuity_inflation_joint(ageA+Term,ageB+Term,ir,inflationrate)*(1+inflationrate)^Term)}
  return(vector)} ## for reserve  single  
termann_reserve_joint(30,28,0.04,10,0,0) 


### for Joint whole life assurance
Wholelifeass_joint = function(ageA,ageB,ir){
  DD = ir/(1+ir)
  Output = 1 - DD*(Wholelifeannuity_joint(ageA,ageB,ir)+1)
  return(Output)}
Wholelifeass_joint(30,28,0.04) #0.1895045 $ 0.1895045


Wholelifeass_Increase_joint = function(ageA,ageB,ir){
  discountrate = 1/(1 + ir)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[-(1:(ageA-20 + 1))]/Lx[ageA-20 + 1])
  survivalprobabilitiesA1 = (Lx[-(1:(ageA-20 + 1+1))]/Lx[ageA-20 + 1])
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[-(1:(ageB-20 + 1))]/Lxs[ageB-20 + 1])
  survivalprobabilitiesB1 = (Lxs[-(1:(ageB-20 + 1+1))]/Lxs[ageB-20 + 1])
  deathproxy=survivalprobabilitiesA*survivalprobabilitiesB-survivalprobabilitiesA1*survivalprobabilitiesB1
  
  alldiscountrates = discountrate^(1:length(deathproxy))
  
  epv = sum(c((alldiscountrates *deathproxy*(1:length(deathproxy)))))
  return(epv)} ##IAXy



#
Wholelifeass_joint_bonus= function(ageA,ageB,ir,BonusRate){
  
  Output = Wholelifeass_joint(ageA,ageB,ir)+BonusRate*Wholelifeass_Increase_joint(ageA,ageB,ir)
  return(Output)}


#level pre
Wholelifeass_joint_level = function(ageA,ageB,ir,ClaRate,IniRate,PreRate,BonusRate){
  
  Output = Wholelifeass_joint_bonus(ageA,ageB,ir,BonusRate)*(1+ClaRate)/((Wholelifeannuity_joint(ageA,ageB,ir)+1)-IniRate-PreRate*Wholelifeannuity_joint(ageA,ageB,ir))
  return(Output)}



#reserve
wholeass_reserve_joint = function(ageA,ageB,ir,BonusRate,ClaimExpRT){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*(Wholelifeass_joint(ageA+year[i],ageB+year[i],ir)*(1+BonusRate)^year[i]+BonusRate*Wholelifeass_Increase_joint(ageA+year[i],ageB+year[i],ir))}
  return(vector)} ## for reserve  single  
wholeass_reserve_joint(30,30,0.04,0,0) 

wholeass_reserve_joint_c = function(ageA,ageB,ir,BonusRate,ClaimExpRT){
  year=c(1:90)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=(1+ClaimExpRT)*((Wholelifeass_joint(ageA+year[i],ageB+year[i],ir)*(1+BonusRate)^year[i]+BonusRate*Wholelifeass_Increase_joint(ageA+year[i],ageB+year[i],ir)))*(1+ir)^0.5}
  return(vector)} ## for reserve  single  
wholeass_reserve_joint_c(30,30,0.04,0,0) 


### for Joint Term life assurance

termass_Increase_joint = function(ageA,ageB,ir,Term){
  discountrate = 1/(1 + ir)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[ageA+Term-20+1]/Lx[ageA-20 + 1])
  
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[ageB+Term-20+1]/Lxs[ageB-20 + 1])
  
  alldiscountrates = discountrate^(Term)
  
  epv = Wholelifeass_Increase_joint(ageA,ageB,ir)-alldiscountrates*survivalprobabilitiesA*survivalprobabilitiesB*(Wholelifeass_Increase_joint(ageA+Term,ageB+Term,ir)+Term*Wholelifeass_joint(ageA,ageB,ir))
  return(epv)}


termass_joint = function(ageA,ageB,interestrate,Term,Bonusrate,ClaRate,InRate){
  discountrate = 1/(1 + interestrate)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[ageA+Term-20+1]/Lx[ageA-20 + 1])
  
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[ageB+Term-20+1]/Lxs[ageB-20 + 1])
  
  alldiscountrates = discountrate^(Term)
  
  Output =(1+ClaRate)*((Wholelifeass_joint(ageA,ageB,interestrate)-alldiscountrates*survivalprobabilitiesA*survivalprobabilitiesB*Wholelifeass_joint(ageA+Term,ageB+Term,interestrate))+Bonusrate*termass_Increase_joint(ageA,ageB,interestrate,Term))/(1-InRate)
  return(Output)}


termass_joint_level = function(ageA,ageB,interestrate,Term,Bonusrate,ClaRate,InRate,prerate){
  discountrate = 1/(1 + interestrate)
  # for the FIRST person(in group X)
  survivalprobabilitiesA = (Lx[ageA+Term-20+1]/Lx[ageA-20 + 1])
  
  # for the SECOND person(in group Y)
  survivalprobabilitiesB = (Lxs[ageB+Term-20+1]/Lxs[ageB-20 + 1])
  
  alldiscountrates = discountrate^(Term)
  
  Output =(1+ClaRate)*((Wholelifeass_joint(ageA,ageB,interestrate)-alldiscountrates*survivalprobabilitiesA*survivalprobabilitiesB*Wholelifeass_joint(ageA+Term,ageB+Term,interestrate))+Bonusrate*termass_Increase_joint(ageA,ageB,interestrate,Term))/((termannuity_joint(ageA,ageB,interestrate,Term-1)+1)-prerate*(termannuity_joint(ageA,ageB,interestrate,Term-1))-InRate)
  return(Output)}


#reserve
termass_joint_reserve = function(ageA,ageB,interestrate,Term,Bonusrate,ClaRate,InRate){
  year=c(1:Term)
  vector=c()
  for (i in 1:length(year)){
    vector[i]=termass_joint(ageA+year[i],ageB+year[i],interestrate,Term-year[i],Bonusrate,ClaRate,InRate)}
  return(vector)}


################

ui = fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(h1("Insurance Benefit Valuation Tool")),
  tabsetPanel(
    tabPanel(
      title = h3("single policy"),
      sidebarPanel(
        width=4,
        h2("Plesae choose your personal information"),
        radioButtons("LifeCategory",label="Life Category", choices = list("Group x","Group y"), selected = "Group x"),
        sliderInput(inputId="Age",label="Age", value = 30,min=20,max=100),
        h2("Plesae select the product type"),
        
        selectInput("InsuranceBenefit",label="Choose Product", choices = list("pure endowment","term assurance","endowment assurance", "whole life assurance","Whole Life Annuity","Term Annuity"), selected = "pure endowment"),
        h2("Please choose the product information"),
        radioButtons("BenefitPayment",label="Insurance Benefit payment(only for assurance product)", choices = list("End of year of death","Immediately on death"), selected = "End of year of death"),
        
        radioButtons("PremiumPayment",label="Insurance Premium Payment(only for assurance product)", choices = list("single premium","level annual premium"), selected = "single premium"),
        numericInput(inputId="AssuredSum",label="Assured Sum",value=1,min=0,max=100000000),
        numericInput(inputId="BenefitTerm",label="Benefit Term (not for whole life product)",value=10,min=0,max=100000000)
      ),
 
      sidebarPanel(
        h2("Please select the Rate"),
        sliderInput("IR",label="Interest Rate (in %) (0%-50%)",value=4,min=0,max =50),
        sliderInput("BR",label="Bonus Rate (in %) (0%-50%)(Only for assurance product)",value=0,min=0,max =50),
        sliderInput("inflationrate",label="Inflation Rate (in %) (0%-50%) (Only for annuity product)",value=0,min=0,max =50)
      ),
      sidebarPanel(
        h2("Please select the Expenses Rate"),
        sliderInput("in_expenserate",label="Initial expenses (in %) (0%-50%)",value=0,min=0,max =50),
        sliderInput("premium_exp",label="Premium expenses (in %) (0%-50%)(Only for level premiums)",value=0,min=0,max =50),
        sliderInput("claim_exp",label="Claim expenses (in %) (0%-50%)",value=0,min=0,max =50)
      ),
      mainPanel(  h1("Product Output:"),
                  tags$h3("1. PREMIUM"),
                  textOutput("OutputText"),
                  
                  tags$h3("2. RESERVE PLOT"),
                  plotOutput("reserve_plot")
      )
    ),
    tabPanel(
      title = h3("couple policy"),
      
      sidebarPanel(
        h2("Please select your age"),
      sliderInput("ageA",label="First Policyholder Age(similar to group x)",value=30,min=20,max =100),
      sliderInput("ageB",label="Second Policyholder Age(similar to group y)",value=30,min=20,max =100),
      h2("Please select the joint product type"),
      selectInput("product",label="Choose joint Product", choices = list("Joint whole life assurance","Joint term life assurance","Joint whole life annuity", "Joint term life annuity"), selected = "Joint whole life assurance"),
      h2("Please select the product information"),
      radioButtons("PaymentPattern",label="Payment Pattern (Only for assurance product)", choices = list("End of year","Immediately"), selected = "End of year"),
      radioButtons("PremiumPaymentMethod",label="Premium Payment Method (Only for assurance product)", choices = list("single","level"), selected = "single"),
      numericInput(inputId="SumAssured",label="Sum Assured ($)",value=1,min=0,max=100000000),
      numericInput(inputId="Term",label="Term (not for whole life product)",value=10,min=0,max=100000000)
      ),
     
      sidebarPanel(
        h2("Please select the Rate"),
        sliderInput("InR",label="Interest Rate(0%-50%)",value=4,min=0,max =50),
        sliderInput("BoR",label="Bonus Rate(0%-50%)(Only for assurance product)",value=0,min=0,max =50),
        sliderInput("compoundrate",label="Compound Rate (0%-50%) (Only for annuity product)",value=0,min=0,max =50)
      ),
      sidebarPanel(
        h2("Please select the Expense Rate"),
        sliderInput("in_exp_rate",label="Initial expenses (0%-50%)",value=0,min=0,max =50),
        sliderInput("premium_exp_rate",label="Premium expenses (0%-50%)(Only for level premium)",value=0,min=0,max =50),
        sliderInput("claim_exp_rate",label="Claim expenses (0%-50%)",value=0,min=0,max =50)
      ),
      mainPanel(  h1("Product Output:"),
                  tags$h3("1. PREMIUM"),
                  textOutput("OutputText_Joint"),
                  
                  tags$h3("2. RESERVE PLOT"),
                  plotOutput("reserve_plot_Joint")
      )
      
    )
  ))
         



server = function(input,output) {
  output$OutputText = renderText( {
   
    age = input$Age
    InterestRate = input$IR/100
    BenefitTerm=input$BenefitTerm
    BonusRate = input$BR/100
    inflationrate=input$inflationrate/100
    InitialExpRT=input$in_expenserate/100
    PremiumExpRT=input$premium_exp/100
    ClaimExpRT=input$claim_exp/100
    
    #####for group x
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = (ClaimExpRT+1)*((1+BenefitTerm*BonusRate)*pureendowment(age,InterestRate,BenefitTerm))/(1-InitialExpRT)}
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = ((ClaimExpRT+1)*(1+BenefitTerm*BonusRate)*pureendowment(age,InterestRate,BenefitTerm))/(1-InitialExpRT) }
  
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = ((termassurance(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase(age,InterestRate,BenefitTerm))*(ClaimExpRT+1))/(1-InitialExpRT) }
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = (((1+InterestRate)^0.5)*(termassurance(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase(age,InterestRate,BenefitTerm)))*(ClaimExpRT+1)/(1-InitialExpRT)}
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = (endowmentassurance(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment(age,InterestRate,BenefitTerm))*(ClaimExpRT+1)/(1-InitialExpRT)}#??
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = (((1+InterestRate)^0.5)*termassurance(age,InterestRate,BenefitTerm)+pureendowment(age,InterestRate,BenefitTerm)+BonusRate*((1+InterestRate)^0.5)*termass_Increase(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment(age,InterestRate,BenefitTerm))*(ClaimExpRT+1)/(1-InitialExpRT)}  # not sure
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB = (Wholelifeass(age,InterestRate)+BonusRate*Wholelifeass_Increase(age,InterestRate))*(ClaimExpRT+1)/(1-InitialExpRT)}
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB = ((((1+InterestRate)^0.5)*(Wholelifeass(age,InterestRate)+BonusRate*Wholelifeass_Increase(age,InterestRate)))*(ClaimExpRT+1))/(1-InitialExpRT)}
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = (1+ClaimExpRT)*(pureendowment(age,InterestRate,BenefitTerm))/(Termannuitydue_advance(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance(age,InterestRate,BenefitTerm)-1)) }
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = (pureendowment(age,InterestRate,BenefitTerm))*(1+ClaimExpRT)/(Termannuitydue_advance(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance(age,InterestRate,BenefitTerm)-1)) }
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = ((termassurance(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance(age,InterestRate,BenefitTerm)-1)) }
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = (((1+InterestRate)^0.5)*(termassurance(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance(age,InterestRate,BenefitTerm)-1))}
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = ((endowmentassurance(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance(age,InterestRate,BenefitTerm)-1))} #?
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = ((((1+InterestRate)^0.5)*termassurance(age,InterestRate,BenefitTerm)+pureendowment(age,InterestRate,BenefitTerm)+BonusRate*((1+InterestRate)^0.5)*termass_Increase(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance(age,InterestRate,BenefitTerm)-1))}
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB =((Wholelifeass(age,InterestRate)+BonusRate*Wholelifeass_Increase(age,InterestRate)))*(1+ClaimExpRT)/((Wholelife_annuity(age,InterestRate)-InitialExpRT-PremiumExpRT*(Wholelife_annuity(age,InterestRate)-1)))}
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB = ((((1+InterestRate)^0.5)*(Wholelifeass(age,InterestRate)+BonusRate*Wholelifeass_Increase(age,InterestRate))))*(1+ClaimExpRT)/((Wholelife_annuity(age,InterestRate)-InitialExpRT-PremiumExpRT*(Wholelife_annuity(age,InterestRate)-1)))}
    
    
    if ((input$LifeCategory=="Group x")&(input$InsuranceBenefit=="Whole Life Annuity" )){EPVB=(Wholelifeannuity_inflation(age,InterestRate,inflationrate))*(ClaimExpRT+1)/(1-InitialExpRT)} #?
    
    if ((input$LifeCategory=="Group x")&(input$InsuranceBenefit=="Term Annuity" )){EPVB=(termannuity_inflation(age,InterestRate,BenefitTerm,inflationrate))*(ClaimExpRT+1)/(1-InitialExpRT)}
    
    
    
    
    #####for group y
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = (pureendowment_y(age,InterestRate,BenefitTerm))*(1+BenefitTerm*BonusRate)*(ClaimExpRT+1)/(1-InitialExpRT) }
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = (pureendowment_y(age,InterestRate,BenefitTerm))*(1+BenefitTerm*BonusRate)*(ClaimExpRT+1)/(1-InitialExpRT)}
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = (termassurance_y(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase_y(age,InterestRate,BenefitTerm))*(ClaimExpRT+1)/(1-InitialExpRT) }
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = (((1+InterestRate)^0.5)*(termassurance_y(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase_y(age,InterestRate,BenefitTerm)))*(ClaimExpRT+1)/(1-InitialExpRT) }
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = (endowmentassurance_y(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase_y(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment_y(age,InterestRate,BenefitTerm))*(ClaimExpRT+1)/(1-InitialExpRT)}
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB =  (((1+InterestRate)^0.5)*termassurance_y(age,InterestRate,BenefitTerm)+pureendowment_y(age,InterestRate,BenefitTerm)+BonusRate*((1+InterestRate)^0.5)*termass_Increase_y(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment_y(age,InterestRate,BenefitTerm))*(ClaimExpRT+1)/(1-InitialExpRT)}
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB = (Wholelifeass_y(age,InterestRate)+BonusRate*Wholelifeass_Increase_y(age,InterestRate))*(ClaimExpRT+1)/(1-InitialExpRT)}
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB = (((1+InterestRate)^0.5)*(Wholelifeass_y(age,InterestRate)+BonusRate*Wholelifeass_Increase_y(age,InterestRate)))*(ClaimExpRT+1)/(1-InitialExpRT)}
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB =  (1+ClaimExpRT)*(pureendowment_y(age,InterestRate,BenefitTerm))/(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-1)) }
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB =  (pureendowment_y(age,InterestRate,BenefitTerm))*(1+ClaimExpRT)/(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-1)) }
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = ((termassurance_y(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase_y(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-1)) }
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = (((1+InterestRate)^0.5)*(termassurance_y(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase_y(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-1))}
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB =((endowmentassurance_y(age,InterestRate,BenefitTerm)+BonusRate*termass_Increase_y(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment_y(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-1))}
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB =((((1+InterestRate)^0.5)*termassurance_y(age,InterestRate,BenefitTerm)+pureendowment_y(age,InterestRate,BenefitTerm)+BonusRate*((1+InterestRate)^0.5)*termass_Increase_y(age,InterestRate,BenefitTerm)+BonusRate*BenefitTerm*pureendowment_y(age,InterestRate,BenefitTerm)))*(1+ClaimExpRT)/(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-InitialExpRT-PremiumExpRT*(Termannuitydue_advance_y(age,InterestRate,BenefitTerm)-1))}
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB =((Wholelifeass_y(age,InterestRate)+BonusRate*Wholelifeass_Increase_y(age,InterestRate)))*(1+ClaimExpRT)/((Wholelife_annuity_y(age,InterestRate)-InitialExpRT-PremiumExpRT*(Wholelife_annuity_y(age,InterestRate)-1)))}
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB = ((((1+InterestRate)^0.5)*(Wholelifeass_y(age,InterestRate)+BonusRate*Wholelifeass_Increase_y(age,InterestRate))))*(1+ClaimExpRT)/((Wholelife_annuity_y(age,InterestRate)-InitialExpRT-PremiumExpRT*(Wholelife_annuity_y(age,InterestRate)-1)))}
    
    if ((input$LifeCategory=="Group y")&(input$InsuranceBenefit=="Whole Life Annuity" )){EPVB=(Wholelifeannuity_inflation_y(age,InterestRate,inflationrate))*(ClaimExpRT+1) /(1-InitialExpRT)} #single pre
    
    if ((input$LifeCategory=="Group y")&(input$InsuranceBenefit=="Term Annuity" )){EPVB=(termannuity_inflation_y(age,InterestRate,BenefitTerm,inflationrate))*(ClaimExpRT+1)/(1-InitialExpRT)} #single pre
    
    
    Prem = input$AssuredSum*EPVB
    print(Prem)
  } )
 
  
  
  
   output$reserve_plot <- renderPlot({
    
    age = input$Age
    InterestRate = input$IR/100
    BenefitTerm=input$BenefitTerm
    BonusRate = input$BR/100
    inflationrate=input$inflationrate/100
    InitialExpRT=input$in_expenserate/100
    PremiumExpRT=input$premium_exp/100
    ClaimExpRT=input$claim_exp/100
   
   ##for group x
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {reserve = pureendowment_reserve(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT) } #
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {reserve = pureendowment_reserve(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {reserve = termassurance_reserve(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT) } #?
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {reserve = ((1+InterestRate)^0.5)*termassurance_reserve(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #?
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {reserve = endowass_reserve(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {reserve = endowass_reserve_con(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {reserve = pureendowment_reserve_l(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT,InitialExpRT,PremiumExpRT) } #???
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = pureendowment(age,InterestRate,BenefitTerm)/Termannuitydue_advance(age,InterestRate,BenefitTerm) }
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = termassurance(age,InterestRate,BenefitTerm)/Termannuitydue_advance(age,InterestRate,BenefitTerm) }
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = ((1+InterestRate)^0.5)*termassurance(age,InterestRate,BenefitTerm)/Termannuitydue_advance(age,InterestRate,BenefitTerm)}
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = endowmentassurance(age,InterestRate,BenefitTerm)/Termannuitydue_advance(age,InterestRate,BenefitTerm)}
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = (((1+InterestRate)^0.5)*termassurance(age,InterestRate,BenefitTerm)+ pureendowment(age,InterestRate,BenefitTerm))/Termannuitydue_advance(age,InterestRate,BenefitTerm)}
    
   
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {reserve =wholeass_reserve(age,InterestRate,BonusRate,ClaimExpRT)} #
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {reserve =wholeass_reserve_c(age,InterestRate,BonusRate,ClaimExpRT)}#
    
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB =((Wholelifeass(age,InterestRate)+BonusRate*Wholelifeass_Increase(age,InterestRate)))*(1+ClaimExpRT)/((Wholelife_annuity(age,InterestRate)-InitialExpRT-PremiumExpRT*(Wholelife_annuity(age,InterestRate)-1)))}
    if ((input$LifeCategory=="Group x")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="whole life assurance" )) {EPVB = ((((1+InterestRate)^0.5)*(Wholelifeass(age,InterestRate)+BonusRate*Wholelifeass_Increase(age,InterestRate))))*(1+ClaimExpRT)/((Wholelife_annuity(age,InterestRate)-InitialExpRT-PremiumExpRT*(Wholelife_annuity(age,InterestRate)-1)))}
    
    
    if ((input$LifeCategory=="Group x")&(input$InsuranceBenefit=="Whole Life Annuity" )){reserve = wholeann_reserve(age,InterestRate,ClaimExpRT,inflationrate)} # single pre
    
    if ((input$LifeCategory=="Group x")&(input$InsuranceBenefit=="Term Annuity" )){reserve=termann_reserve(age,InterestRate,BenefitTerm,ClaimExpRT,inflationrate)} # single pre
    
    
    
    ### for group y
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {reserve = pureendowment_reserve_y(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="pure endowment" )) {reserve = pureendowment_reserve_y(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {reserve = termassurance_reserve_y(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT) } #?
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="term assurance" )) {reserve = ((1+InterestRate)^0.5)*termassurance_reserve_y(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT) } #?
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {reserve =endowass_reserve_y(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="endowment assurance" )) {reserve = endowass_reserve_con_y(age,InterestRate,BenefitTerm,BonusRate,ClaimExpRT)} #
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = pureendowment_y(age,InterestRate,BenefitTerm)/Termannuitydue_advance_y(age,InterestRate,BenefitTerm) }
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="pure endowment" )) {EPVB = pureendowment_y(age,InterestRate,BenefitTerm)/Termannuitydue_advance_y(age,InterestRate,BenefitTerm) }
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = termassurance_y(age,InterestRate,BenefitTerm)/Termannuitydue_advance_y(age,InterestRate,BenefitTerm) }
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="term assurance" )) {EPVB = ((1+InterestRate)^0.5)*termassurance_y(age,InterestRate,BenefitTerm)/Termannuitydue_advance_y(age,InterestRate,BenefitTerm)}
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = endowmentassurance_y(age,InterestRate,BenefitTerm)/Termannuitydue_advance_y(age,InterestRate,BenefitTerm)}
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="level annual premium")&(input$InsuranceBenefit=="endowment assurance" )) {EPVB = (((1+InterestRate)^0.5)*termassurance_y(age,InterestRate,BenefitTerm)+ pureendowment_y(age,InterestRate,BenefitTerm))/Termannuitydue_advance_y(age,InterestRate,BenefitTerm)}
    
    
    
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="End of year of death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {reserve =wholeass_reserve_y(age,InterestRate,BonusRate,ClaimExpRT)} #
    if ((input$LifeCategory=="Group y")&(input$BenefitPayment=="Immediately on death")&(input$PremiumPayment=="single premium")&(input$InsuranceBenefit=="whole life assurance" )) {reserve =wholeass_reserve_c_y(age,InterestRate,BonusRate,ClaimExpRT)}#
    
    if ((input$LifeCategory=="Group y")&(input$InsuranceBenefit=="Whole Life Annuity" )){reserve = wholeann_reserve_y(age,InterestRate,ClaimExpRT,inflationrate)} #
    
    if ((input$LifeCategory=="Group y")&(input$InsuranceBenefit=="Term Annuity" )){reserve=termann_reserve_y(age,InterestRate,BenefitTerm,ClaimExpRT,inflationrate)} # single pre
    
    
    reservedata=input$AssuredSum*reserve
   
    reservedata=c(0,reservedata)
    year=c(0:BenefitTerm)

    reserveplot<-plot(reservedata, ylab=("Reserve ($) "),xlab=("Policy seniority"),type = "o",main=("reserve evolution"))
               
    print(reserveplot)
  })


# for joint

output$OutputText_Joint = renderText( {
  
  ageA = input$ageA
  ageB=input$ageB
  IR = input$InR/100
  Term=input$Term
  BonusRate = input$BoR/100
  comprate=input$compoundrate/100
  IniExpRT=input$in_exp_rate/100
  PremExpRT=input$premium_exp_rate/100
  ClaExpRT=input$claim_exp_rate/100
  
  
  # for joint 
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="single")) {EB=Wholelifeass_joint_bonus(ageA,ageB,IR,BonusRate)*(1+ClaExpRT)/(1-IniExpRT) }
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="single")) {EB =Wholelifeass_joint_bonus(ageA,ageB,IR,BonusRate)*(1+IR)^0.5*(1+ClaExpRT)/(1-IniExpRT)}
  
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="single")) {EB = termass_joint(ageA,ageB,IR,Term,BonusRate,ClaExpRT,IniExpRT)}
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="single")) {EB = (1+IR)^0.5*termass_joint(ageA,ageB,IR,Term,BonusRate,ClaExpRT,IniExpRT)}
  
  if ((input$product=="Joint whole life annuity")) {EB = Wholelifeannuity_inflation_joint(ageA,ageB,IR,comprate)*(1+ClaExpRT)/(1-IniExpRT)}#
  
  if ((input$product=="Joint term life annuity")) {EB = (termannuity_joint_inflation(ageA,ageB,IR,Term,comprate))*(1+ClaExpRT)/(1-IniExpRT)}
  
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="level")) {EB =Wholelifeass_joint_level(ageA,ageB,IR,ClaExpRT,IniExpRT,PremExpRT,BonusRate)}
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="level")) {EB = (1+IR)^0.5*Wholelifeass_joint_level(ageA,ageB,IR,ClaExpRT,IniExpRT,PremExpRT,BonusRate)}
  
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="level")) {EB = termass_joint_level(ageA,ageB,IR,Term,BonusRate,ClaExpRT,IniExpRT,PremExpRT)}
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="level")) {EB = (1+IR)^0.5*termass_joint_level(ageA,ageB,IR,Term,BonusRate,ClaExpRT,IniExpRT,PremExpRT)}
  
  
  Prem = input$AssuredSum*EB
  print(Prem)
} )


#for joint

output$reserve_plot_Joint <- renderPlot({
  
  ageA = input$ageA
  ageB=input$ageB
  IR = input$InR/100
  Term=input$Term
  BonusRate = input$BoR/100
  comprate=input$compoundrate/100
  IniExpRT=input$in_exp_rate/100
  PremExpRT=input$premium_exp_rate/100
  ClaExpRT=input$claim_exp_rate/100

  
  # for joint 
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="single")) {reserve=wholeass_reserve_joint(ageA,ageB,IR,BonusRate,ClaExpRT)}
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="single")) {reserve=wholeass_reserve_joint_c(ageA,ageB,IR,BonusRate,ClaExpRT)}
  
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="single")) {reserve=termass_joint_reserve(ageA,ageB,IR,Term,BonusRate,ClaExpRT,IniExpRT)}#?
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="single")) {reserve =(1+IR)^0.5*termass_joint_reserve(ageA,ageB,IR,Term,BonusRate,ClaExpRT,IniExpRT)}#?
  
  if ((input$product=="Joint whole life annuity")) {reserve=wholeann_reserve_joint(ageA,ageB,IR,comprate,ClaExpRT)}
  
  if ((input$product=="Joint term life annuity")) {reserve =termann_reserve_joint(ageA,ageB,IR,Term,comprate,ClaExpRT)}
  
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="level")) {EPVB = ass}
  if ((input$product=="Joint whole life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="level")) {EPVB = ass}
  
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="End of year")&(input$PremiumPaymentMethod=="level")) {EPVB = ass}
  if ((input$product=="Joint term life assurance")&(input$PaymentPattern=="Immediately")&(input$PremiumPaymentMethod=="level")) {EPVB = ass}
  
  
  
  reservedata=input$SumAssured*reserve
  
  reservedata=c(0,reservedata)
  year=c(0:Term)
  
  reserve_plot_Joint<-plot(reservedata, ylab=("Reserve ($) "),xlab=("Policy seniority"),type = "o",main=("reserve evolution"))
  
  print(reserve_plot_Joint)
})


}


shinyApp(ui = ui , server = server)
