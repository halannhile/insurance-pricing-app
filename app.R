############################################### PACKAGES ############################################### 

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(shinyAce)
library(sendmailR)
library(tidyr)


############################################### DATA ############################################### 

# starting age of Lxs is 17 
Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)


Lx = Lxs[-(1:3)]

Ly = Lxs[1:(length(Lxs)-3)]


############################################### FUNCTIONS ############################################### 

############################################### SIMPLE PRODUCTS ###############################################

## whole life annuity - in advance: 

wholelifeannuity_advance = function(L, age, IR){
  IR = IR / 100 
  v = 1/(1+IR)
  lx = L[-(1:(age-20))]; if (age==20) lx = L
  epv = sum( (v^(0:(120-age)))*lx)/ L[age-20+1]
  return(epv)
}

## term annuity - in advance: 

termannuity_advance = function(L, age, term, IR){
  n = term 
  interest = IR
  IR = IR / 100 
  v = 1/(1+IR)
  x = age-20+1
  all_tpx = L[x+n] / L[x]
  epv = wholelifeannuity_advance(L, age, interest) - 
    (v^n * all_tpx * wholelifeannuity_advance(L, age+n, interest))
  return(epv)
}

## whole life assurance - eoyod: 

wholelifeassurance_eoyod = function(L, age, IR){
  interest = IR
  IR = IR / 100 
  d = IR/(1+IR)
  
  epv = 1 - (d * wholelifeannuity_advance(L, age, interest))
  return(epv)
}

## whole life assurance - iod: 

wholelifeassurance_iod = function(L, age, IR){
  interest = IR
  IR = IR / 100 
  delta = log(1+IR)
  annuity_arrears = wholelifeannuity_advance(L, age, interest) - 0.5
  epv = 1 - (delta * annuity_arrears)
  return(epv)
}

## term assurance - eoyod: 

termassurance_eoyod = function(L, age, term, IR){
  n = term
  interest = IR
  IR = IR / 100 
  v = 1/(1+IR)
  x = age-20+1
  all_tpx = L[x+n]/L[x]
  deferred = v^n * all_tpx * wholelifeassurance_eoyod(L, age+n, interest)
  epv = wholelifeassurance_eoyod(L, age, interest) - deferred 
  return(epv)
}

## term assurance - iod:

termassurance_iod = function(L, age, term, IR){
  interest = IR
  IR = IR / 100 
  epv = (1+IR)^(0.5) * termassurance_eoyod(L, age, term, interest)
  return(epv)
}

## pure endowment - eoyod / iod:

pureendowment = function(L, age, term, IR){
  IR = IR / 100 
  v = 1/(1+IR)
  x = age-20+1
  all_tpx = L[x+term]/L[x]
  epv = v^term * all_tpx 
  return(epv)
}

## endowment assurance - eoyod: 

endowmentassurance_eoyod = function(L, age, term, IR){
  interest = IR

  epv = termassurance_eoyod(L, age, term, interest) + pureendowment(L, age, term, interest)
  return(epv)
}

## endowment assurance - iod: 
endowmentassurance_iod = function(L, age, term, IR){
  epv = termassurance_iod(L, age, term, IR) + pureendowment(L, age, term, IR)
  return(epv)
}

############################################### INCREASING PRODUCTS ###############################################

## increasing whole life assurance - eoyod: 

i_wholelifeassurance_eoyod = function(L, age, IR){
  IR = IR / 100 
  v = 1/(1+IR)
  x = age-20+1
  epv = 0 
  for (k in 0:(100-age)){
    all_tpx = L[x+k] / L[x]
    all_tqx = 1 - (L[x+k+1]/L[x+k])
    epv = epv + ((k+1) * v^(k+1) * all_tpx * all_tqx)
  }
  return(epv)
}

## increasing whole life assurance - iod: 

i_wholelifeassurance_iod = function(L, age, IR){
  interest = IR
  IR = IR / 100 
  epv = (1+IR)^(0.5) * i_wholelifeassurance_eoyod(L, age, interest)
  return(epv)
}

## increasing term assurance - eoyod: 
i_termassurance_eoyod = function(L, age, term, IR){
  interest = IR
  IR = IR / 100 
  v = 1/(1+IR)
  x = age-20+1 
  all_tpx = L[x+term]/L[x]
  
  epv = i_wholelifeassurance_eoyod(L, age, interest) - ( v^term * all_tpx * (i_wholelifeassurance_eoyod(L, age+term, interest) + term * wholelifeassurance_eoyod(L, age+term, interest)) )
  return(epv)
}

## increasing term assurance - iod: 
i_termassurance_iod = function(L, age, term, IR){
  interest = IR
  IR = IR / 100 
  epv = (1+IR)^(0.5) * i_termassurance_eoyod(L, age, term, interest)
  return(epv)
}

## increasing endowment assurance - eoyod: 

i_endowmentassurance_eoyod = function(L, age, term, IR){
  interest = IR
  n = term
  epv = i_termassurance_eoyod(L, age, term, interest) + n * pureendowment(L, age, term, interest)
  return(epv)
}

## increasing endowment assurance - iod 
i_endowmentassurance_iod = function(L, age, term, IR){
  interest = IR
  IR = IR / 100 
  epv = (1+IR)^(0.5) * i_endowmentassurance_eoyod(L, age, term, interest)
  return(epv)
}

############################################### PREMIUM ###############################################

## whole life annuity - single premium - eoyod: 

prem_wholelifeannuity_single_eoyod = function(L, age, IR, S, b, percent_init, percent_fee){
  interest = IR
  b = b / 100 
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  IR = IR / 100 
  j = (((1+IR)/(1+b))-1)*100 # in 100% 
  benefit = 1/(1+b) * (wholelifeannuity_advance(L, age, j) - 1)
  prem = (S * benefit * (1 + percent_fee)) / (1 - percent_init)
  return(prem)
}

## whole life annuity - single prem - iod: 

prem_wholelifeannuity_single_iod = function(L, age, IR, S, b, percent_init, percent_fee){
  interest = IR
  b = b / 100 
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  IR = IR / 100 
  j = (((1+IR)/(1+b))-1)*100 # in 100% 
  benefit = 1/(1+b) * (wholelifeannuity_advance(L, age, j) - 0.5)
  prem = (S * benefit * (1 + percent_fee)) / (1 - percent_init)
  return(prem)
}

## term annuity - single prem - eoyod: 

prem_termannuity_single_eoyod = function(L, age, term, IR, S, b, percent_init, percent_fee){
  interest = IR
  b = b / 100 
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  IR = IR / 100 
  v = 1/(1+IR)
  j = (((1+IR)/(1+b)) - 1)*100
  x = age-20+1
  
  all_tpx = L[x+1] / L[x]
  benefit = 1/(1+b) * (v * all_tpx * termannuity_advance(L, age+1, term, j))
  prem = (S * benefit * (1 + percent_fee)) / (1 - percent_init)
  return(prem)
}

## term annuity - single prem - iod: 

prem_termannuity_single_iod = function(L, age, term, IR, S, b, percent_init, percent_fee){
  interest = IR
  b = b / 100 
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  IR = IR / 100 
  v = 1/(1+IR)
  j = (((1+IR)/(1+b)) - 1)*100
  x = age-20+1
  
  all_tpx = L[x + term] / L[x]
  benefit = 1/(1+b) * (termannuity_advance(L, age + 1, term, j) - 0.5 * (1 - v^term * all_tpx))
  
  prem = (S * benefit * (1 + percent_fee)) / (1 - percent_init)
  return(prem)
}

## whole life assurance - single prem - eoyod: 

prem_wholelifeassurance_single_eoyod = function(L, age, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  bonus = (b/100) * S 
  
  prem = ( (S * wholelifeassurance_eoyod(L, age, interest) + bonus * i_wholelifeassurance_eoyod(L, age, interest) ) * (1 + percent_claim) )/ (1 - percent_init)
  return(prem)
}
  
## whole life assurance - level prem - eoyod: 

prem_wholelifeassurance_level_eoyod = function(L, age, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100 
  bonus = b/100 * S 
  
  term = 100 - age 
  termannuity = termannuity_advance(L, age, term, interest)
  
  prem = ( (S * wholelifeassurance_eoyod(L, age, interest) + bonus * i_wholelifeassurance_eoyod(L, age, interest)) * (1 + percent_claim) ) / (termannuity - percent_init - percent_renew * (termannuity - 1))
  return(prem)
}

## whole life assurance - single prem - iod: 

prem_wholelifeassurance_single_iod = function(L, age, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  bonus = b/100 * S 
  
  prem = ( (S * wholelifeassurance_iod(L, age, interest) + bonus * i_wholelifeassurance_iod(L, age, interest) ) * (1 + percent_claim) )/ 
    (1 - percent_init)
  
  return(prem)
}

## whole life assurance - level prem - iod: 

prem_wholelifeassurance_level_iod = function(L, age, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100 
  bonus = b/100 * S 
  
  term = 100 - age 
  termannuity = termannuity_advance(L, age, term, interest)
  
  prem = ( (S * wholelifeassurance_iod(L, age, interest) + bonus * i_wholelifeassurance_iod(L, age, interest)) * (1 + percent_claim) ) / (termannuity - percent_init - percent_claim*(termannuity - 1))
  return(prem)
}

## term assurance - single prem - eoyod: 

prem_termassurance_single_eoyod = function(L, age, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  bonus = b/100 * S 
  
  prem = ((S * termassurance_eoyod(L, age, term, interest) + bonus * i_termassurance_eoyod(L, age, term, interest)) * (1+percent_claim)) / (1 - percent_init)
  return(prem)
}

## term assurance - level prem - eoyod: 

prem_termassurance_level_eoyod = function(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100 
  bonus = b/100 * S 
  
  termannuity = termannuity_advance(L, age, term, interest)
  
  prem = ( (S * termassurance_eoyod(L, age, term, interest) + bonus * i_termassurance_eoyod(L, age, term, interest)) * (1+percent_claim) ) / (termannuity - percent_init - percent_renew*(termannuity-1))
  return(prem)
}


## term assurance - single prem - iod: 

prem_termassurance_single_iod = function(L, age, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  bonus = b/100 * S
  
  prem = ((S * termassurance_iod(L, age, term, interest) + bonus * i_termassurance_iod(L, age, term, interest)) * (1+percent_claim))/(1-percent_init)
  return(prem)
}

## term assurance - level prem - iod: 

prem_termassurance_level_iod = function(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100 
  bonus = b/100 * S
  
  termannuity = termannuity_advance(L, age, term, interest)
    
  prem = ( (S * termassurance_iod(L, age, term, interest) + bonus * i_termassurance_iod(L, age, term, interest)) * (1+percent_claim) ) / (termannuity - percent_init - percent_renew * (termannuity - 1))
  return(prem)
}

## pure endowment - single prem: 

prem_pureendowment_single = function(L, age, term, IR, S, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100
  
  prem = S * ((pureendowment(L, age, term, interest)*(1+percent_claim)) / (1 - percent_init))
  return(prem)
}

## pure endowment - level prem: 

prem_pureendowment_level = function(L, age, term, IR, S, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100
  termannuity = termannuity_advance(L, age, term, interest)
  
  prem = S * ((pureendowment(L, age, term, interest) * (1 + percent_claim)) / (termannuity - percent_init - percent_renew * (termannuity - 1)))
  return(prem)
}

## endowment assurance - single prem - eoyod: 

prem_endowmentassurance_single_eoyod = function(L, age, term, IR, S, b, percent_init, percent_claim){
  
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  bonus = b/100 * S 
  
  prem = ( (S * endowmentassurance_eoyod(L, age, term, interest) + bonus * i_endowmentassurance_eoyod(L, age, term, interest)) * (1+percent_claim) ) / (1-percent_init)
  return(prem)  
}

## endowment assurance - level prem - eoyod: 

prem_endowmentassurance_level_eoyod = function(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100 
  bonus = b/100 * S 
  
  termannuity = termannuity_advance(L, age, term, interest)
  
  prem = ( (S * endowmentassurance_eoyod(L, age, term, interest) + bonus * i_endowmentassurance_eoyod(L, age, term, interest)) * (1+percent_claim) ) / (termannuity - percent_init - percent_renew * (termannuity - 1))
  return(prem)
}

## endowment assurance - single prem - iod: 

prem_endowmentassurance_single_iod = function(L, age, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  bonus = b/100 * S 
  
  prem = ( (S * endowmentassurance_iod(L, age, term, interest) + bonus * i_endowmentassurance_iod(L, age, term, interest)) * (1+percent_claim) ) / (1-percent_init)
  return(prem)
}

## endowment assurance - level prem - iod: 

prem_endowmentassurance_level_iod = function(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100 
  bonus = b/100 * S 
  
  termannuity = termannuity_advance(L, age, term, interest)
  prem = ( (S * endowmentassurance_iod(L, age, term, interest) + bonus * i_endowmentassurance_iod(L, age, term, interest)) * (1+percent_claim) ) / (termannuity - percent_init - percent_renew * (termannuity - 1))
  return(prem)
}

############################################### SINGLE RESERVES ###############################################

## whole life annuity - eoyod: 

res_wholelifeannuity_eoyod = function(prem, L, age, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  b = b/100
  IR = IR / 100 
  j = (((1+IR)/(1+b))-1)*100
  
  reserves = c()
  
  for (t in 0:(100-age)){
    wl = 1/(1+b) * (wholelifeannuity_advance(L, age + t, j) - 1)
    
    if (t == 0)
      reserves[1] = ( S * wl * (1+percent_fee) + prem*percent_init ) - prem
    else
      reserves[t+1] = S * wl * (1+percent_fee)
    
  }
  return(reserves)
}

## whole life annuity - iod: 

res_wholelifeannuity_iod = function(prem, L, age, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  b = b/100
  IR = IR / 100 
  j = (((1+IR)/(1+b))-1)*100
  
  reserves = c()
  
  for (t in c(0:(100-age))){
    benefit = 1/(1+b) * (wholelifeannuity_advance(L, age+t, j) - 0.5)
    
    if (t == 0){
      reserves[1] = (S * benefit * (1 + percent_fee) + prem * percent_init) - prem}
    else{
      reserves[t+1] = S * benefit * (1+percent_fee)}
  }
  return(reserves)
}

## term annuity - eoyod: 

res_termannuity_eoyod = function(prem, L, age, term, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  b = b/100
  IR = IR / 100 
  v = 1/(1+IR)
  j = (((1+IR)/(1+b))-1)*100
  x = age-20+1
  
  reserves = c()
  
  for (t in c(0:term)){
    all_tpx = L[x+1+t] / L[x+t]
    termannuity = 1/(1+b) * (v * all_tpx * termannuity_advance(L, age+1+t, term-t,j))
    
    if (t==0){reserves[1] = (S * termannuity * (1+percent_fee) + prem * percent_init) - prem}
    else {reserves[t+1] = S * termannuity * (1+percent_fee)}
  }
  return(reserves)
}

## term annuity - iod: 

res_termannuity_iod = function(prem, L, age, term, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_init = percent_init / 100 
  percent_fee = percent_fee / 100 
  b = b/100
  IR = IR / 100 
  v = 1/(1+IR)
  j = (((1+IR)/(1+b))-1)*100
  x = age-20+1
  
  reserves = c()
  
  for (t in c(0:term)){
    all_tpx = L[x+term] / L[x+t]
    termannuity = termannuity_advance(L, age+t, term-t, j) - 0.5*(1-v^(term-t)*all_tpx)
    
    if (t==0){reserves[1] = (S * termannuity * (1+percent_fee) + prem * percent_init) - prem}
    else {reserves[t+1] = S * termannuity * (1+percent_fee)}
  }
  return(reserves)
}

## whole life assurance - single prem - eoyod: 

res_wholelifeassurance_single_eoyod = function(prem, L, age, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  reserves = c()
  for (t in c(0:(100-age))){
    wla = wholelifeassurance_eoyod(L, age+t, interest)
    ia = i_wholelifeassurance_eoyod(L, age+t, interest)
    if (t == 0){reserves[1] = ((S * wla + bonus * ia) * (1+percent_claim) + prem * percent_init) - prem}
    else {reserves[t+1] = (S * wla + bonus * ia) * (1 + percent_claim)}
  }
  return(reserves)
}

## whole life assurance - level prem - eoyod: 
res_wholelifeassurance_level_eoyod = function(prem, L, age, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in c(0:(100-age))){
    wla = wholelifeassurance_eoyod(L, age+t, interest)
    ia = i_wholelifeassurance_eoyod(L, age+t, interest)
    term = 100 - age
    termannuity = termannuity_advance(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = ((S * wla + bonus * ia) * (1+percent_claim) + prem * (percent_init + percent_renew * (termannuity - 1))) - (prem * termannuity)}
    else {reserves[t+1] = ((S * wla + bonus * ia) * (1+percent_claim) + prem * percent_renew * termannuity) - prem * termannuity}
  }
  return(reserves)
}

## whole life assurance - single prem - iod: 

res_wholelifeassurance_single_iod = function(prem, L, age, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in c(0:(100-age))){
    wla = wholelifeassurance_iod(L, age+t, interest)
    ia = i_wholelifeassurance_iod(L, age+t, interest)
    if (t == 0){reserves[1] = ((S * wla + bonus * ia) * (1+ percent_claim) + prem * percent_init) - prem}
    else {reserves[t+1] = (S * wla + bonus * ia) * (1 + percent_claim)}
  }
  return(reserves)
}

## whole life assurance - level prem - iod: 

res_wholelifeassurance_level_iod = function(prem, L, age, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in c(0:(100-age))){
    wla = wholelifeassurance_iod(L, age+t, interest)
    ia = i_wholelifeassurance_iod(L, age+t, interest)
    term = 100 - age 
    termannuity = termannuity_advance(L, age+t, term-t, interest)
    if (t==0){reserves[1] = ((S * wla + bonus * ia) * (1+percent_claim) + prem * (percent_init + percent_renew * (termannuity - 1))) - (prem * termannuity)}
    else {reserves[t+1] = ((S * wla + bonus * ia) * (1+percent_claim) + prem * percent_renew * termannuity) - (prem*termannuity)}
  }
  return(reserves)
}

## term assurance - single prem - eoyod: 

res_termassurance_single_eoyod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in c(0:term)){
    ta = termassurance_eoyod(L, age+t, term-t, interest)
    ia = i_termassurance_eoyod(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = (((S*ta + bonus*ia) * (1 + percent_claim)) + prem*percent_init)-prem}
    else {reserves[t+1] = (S*ta + bonus*ia) * (1+percent_claim)}
  }
  return(reserves)
}

## term assurance - level prem - eoyod 

res_termassurance_level_eoyod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in c(0:term)){
    ta = termassurance_eoyod(L, age+t, term-t, interest)
    ia = i_termassurance_eoyod(L, age+t, term-t, interest)
    termannuity = termannuity_advance(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = (((S*ta + bonus*ia) * (1+percent_claim)) + prem * (percent_init + percent_renew * (termannuity-1))) - prem*termannuity}
    else {reserves[t+1] = (((S*ta + bonus*ia) * (1+percent_claim)) + prem * percent_renew * termannuity) - prem*termannuity}
  }
  return(reserves)
}

## term assurance - single prem - iod: 
res_termassurance_single_iod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in c(0:term)){
    ta = termassurance_iod(L, age+t, term-t, interest)
    ia = i_termassurance_iod(L, age+t, term-t, interest)
    
    if (t==0){reserves[1] = (((S*ta + bonus*ia) * (1+percent_claim)) + prem*percent_init) - prem}
    else {reserves[t+1] = (S*ta + bonus*ia) * (1+percent_claim)}
  }
  return(reserves)
}

## term assurance - level prem - iod: 

res_termassurance_level_iod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in c(0:term)){
    ta = termassurance_iod(L, age+t, term-t, interest)
    ia = i_termassurance_iod(L, age+t, term-t, interest)
    termannuity = termannuity_advance(L, age+t, term-t, interest)
    
    if (t==0){reserves[1] = (((S*ta + bonus*ia)*(1+percent_claim)) + prem*(percent_init + percent_renew*(termannuity-1)))-prem*termannuity}
    else {reserves[t+1] = (((S*ta + bonus*ia)*(1+percent_claim)) + prem*percent_renew*termannuity) - prem*termannuity}
  }
  return(reserves)
}

## pure endowment - single prem: 

res_pureendowment_single = function(prem, L, age, term, IR, S, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  reserves = c()
  
  for (t in 0:term){
    pe = pureendowment(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = (S*pe*(1+percent_claim)+prem*percent_init)-prem}
    else {reserves[t+1] = S*pe*(1+percent_claim)}
  }
  return(reserves)
}

## pure endowment - level prem: 

res_pureendowment_level = function(prem, L, age, term, IR, S, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100 
  percent_renew = percent_renew / 100
  reserves = c()
  
  for (t in 0:term){
    pe = pureendowment(L, age+t, term-t, interest)
    termannuity = termannuity_advance(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = (S*pe*(1+percent_claim) + prem*(percent_init + (percent_renew*(termannuity-1))))-prem*termannuity}
    else {reserves[t+1] = (S*pe*(1+percent_claim) + prem*percent_renew*termannuity) - prem*termannuity}
  }
  return(reserves)
}

## endowment assurance - single prem - eoyod: 

res_endowmentassurance_single_eoyod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in 0:term){
    ea = endowmentassurance_eoyod(L, age+t, term-t, interest)
    ia = i_endowmentassurance_eoyod(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = (((S*ea + bonus*ia) * (1+percent_claim)) + prem*percent_init) - prem}
    else {reserves[t+1] = (S*ea + bonus*ia) * (1+percent_claim)}
  }
  return(reserves)
}

## endowment assurance - level prem - eoyod: 
res_endowmentassurance_level_eoyod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in 0:term){
    ea = endowmentassurance_eoyod(L, age+t, term-t, interest)
    ia = i_endowmentassurance_eoyod(L, age+t, term-t, interest)
    termannuity = termannuity_advance(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = (((S*ea + bonus*ia) * (1+percent_claim)) + prem*(percent_init + percent_renew*(termannuity-1)))-prem*termannuity}
    else {reserves[t+1] = (((S*ea + bonus*ia) * (1+percent_claim)) + prem*percent_renew*termannuity) - prem*termannuity}
  }
  return(reserves)
}

## endowment assurance - single prem - iod: 

res_endowmentassurance_single_iod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in 0:term){
    ea = endowmentassurance_iod(L, age+t, term-t, interest)
    ia = i_endowmentassurance_iod(L, age+t, term-t, interest)
    
    if (t == 0){reserves[1] = (((S*ea + bonus*ia) * (1+percent_claim)) + prem*percent_init) - prem}
    else {reserves[t+1] = (S*ea + bonus*ia) *(1+percent_claim)}
  }
  return(reserves)
}

## endowment assurance - level prem - iod: 

res_endowmentassurance_level_iod = function(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100 
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  reserves = c()
  
  for (t in 0:term){
    ea = endowmentassurance_iod(L, age+t, term-t, interest)
    ia = i_endowmentassurance_iod(L, age+t, term-t, interest)
    termannuity = termannuity_advance(L, age+t, term-t, interest)
    
    if (t==0){reserves[1] = (((S*ea + bonus*ia) * (1+percent_claim)) + prem*(percent_init + percent_renew*(termannuity-1))) - prem*termannuity}
    else {reserves[t+1] = (((S*ea + bonus*ia) * (1+percent_claim)) + prem*percent_renew*termannuity) - prem*termannuity}
  }
  return(reserves)
}
############################################### JOINT PRODUCTS ###############################################

age_x = seq(from = 20, to = 120)
age_y = seq(from = 20, to = 120)

# joint whole life annuity - due
j_wholelifeannuity_advance = function(age_x, age_y, IR){
  IR = IR/100
  v = 1/(1+IR)
  x = age_x - 20 + 1
  y = age_y - 20 + 1
  epv = 0 
  for ( k in 0:(100 - max(age_x, age_y)) ){
    tpx = Lx[x+k]/Lx[x]
    tpy = Ly[y+k]/Ly[y]
    epv = epv + (v^k * tpx * tpy)
  }
  return(epv)
}

# joint term annuity - due: 

j_termannuity_advance = function(age_x, age_y, term, IR){
  n = term 
  interest = IR
  IR = IR/100
  v = 1/(1+IR)
  x = age_x-20+1
  y = age_y-20+1
  tpx = Lx[x+n]/Lx[x]
  tpy = Ly[y+n]/Ly[y]
  epv = j_wholelifeannuity_advance(age_x, age_y, interest) - v^n * tpx * tpy * j_wholelifeannuity_advance(age_x + n, age_y + n, interest)
  return(epv)
}

# joint whole life assur - eoyod: 
j_wholelifeassurance_eoyod = function(age_x, age_y, IR){
  interest = IR
  IR = IR/100
  d = IR/(1+IR)
  epv = 1 - d*j_wholelifeannuity_advance(age_x,age_y,interest)
  return(epv)
}

# joint whole life assur - ido: 
j_wholelifeassurance_iod = function(age_x, age_y, IR){
  interest = IR
  IR = IR/100
  epv = (1+IR)^0.5 * j_wholelifeassurance_eoyod(age_x, age_y, interest)
  return(epv)
}

# joint term assur - eoyod:

j_termassurance_eoyod = function(age_x, age_y, term, IR){
  n = term 
  interest = IR
  IR = IR/100
  v = 1/(1+IR)
  x = age_x - 20 + 1
  y = age_y - 20 + 1
  tpx = Lx[x+n]/Lx[x]
  tpy = Ly[y+n]/Ly[y]
  epv = j_wholelifeassurance_eoyod(age_x, age_y, interest) - v^term * tpx * tpy * j_wholelifeassurance_eoyod(age_x + n, age_y + n, interest)
  return(epv)
}

# joint term assur - iod: 

j_termassurance_iod <- function(age_x, age_y, term, IR){
  interest = IR
  IR = IR/100
  epv = (1+IR)^0.5 * j_termassurance_eoyod(age_x, age_y, term, interest)
  return(epv)
}

# increasing joint whole life assur - eoyod 

ij_wholelifeassurance_eoyod = function(age_x, age_y, IR){
  interest = IR
  IR = IR/100
  v = 1/(1 + IR)
  x = age_x - 20 + 1
  y = age_y - 20 + 1
  epv = 0
  for (k in 0:(100 - max(age_x, age_y))){
    tpx = Lx[x + k] / Lx[x]
    tpy = Ly[y + k] / Ly[y]
    tkpx = Lx[x + k + 1] / Lx[x + k]
    tkpy = Ly[y + k + 1] / Ly[y + k]
    epv = epv + ( (k+1) * v^(k+1) * tpx * tpy * ( 1 - tkpx * tkpy) )
  }
  return(epv)
}

# increasing joint whole life assur - iod: 

ij_wholelifeassurance_iod = function(age_x, age_y, IR){
  interest = IR
  IR = IR/100
  epv = (1 + IR)^0.5 * ij_wholelifeassurance_eoyod(age_x, age_y, interest)
  return(epv)
}

# increasing joint term assurance - eoyod: 

ij_termassurance_eoyod = function(age_x, age_y, term, IR){
  interest = IR
  IR = IR/100
  v = 1/(1 + IR)
  x = age_x - 20 + 1
  y = age_y - 20 + 1
  tpx = Lx[x + term] / Lx[x]
  tpy = Ly[y + term] / Ly[y]
  
  epv = ij_wholelifeassurance_eoyod(age_x, age_y, interest) - v^term * tpx * tpy * (ij_wholelifeassurance_eoyod(age_x + term, age_y + term, interest) + term * j_wholelifeassurance_eoyod(age_x + term, age_y + term, interest))
  
  return(epv)
}

# increasing joint term assur - iod: 

ij_termassurance_iod = function(age_x, age_y, term, IR){
  interest = IR
  IR = IR/100
  
  epv = (1+IR)^0.5 * ij_termassurance_eoyod(age_x, age_y, term, interest)
  return(epv)
}
  
############################################### JOINT PREMIUM ###############################################

# joint whole life annuity - single prem - eoyod: 

jprem_wholelifeannuity_eoyod = function(age_x, age_y, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_fee = percent_fee / 100
  percent_init = percent_init / 100
  b = b / 100
  IR = IR / 100
  j = (((1+IR) / (1+b)) - 1)*100
  
  benefit = 1/(1+b) * (j_wholelifeannuity_advance(age_x, age_y, j) - 1)
  
  prem = (S * benefit * (1 + percent_fee)) / (1- percent_init)
  return(prem)
}

# joint term annnuity - single prem - eoyod: 

jprem_termannuity_eoyod = function(age_x, age_y, term, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_fee = percent_fee / 100
  percent_init = percent_init / 100
  b = b / 100
  IR = IR / 100
  v = 1/(1+IR)
  j = (((1 + IR)/(1 + b)) - 1)*100
  x = age_x - 20 + 1
  y = age_y - 20 + 1
  tpx = Lx[x + 1] / Lx[x]
  tpy = Ly[y + 1] / Ly[y]
  
  benefit = 1/(1+b) * (v * tpx * tpy * j_termannuity_advance(age_x + 1, age_y + 1, term, j))
  
  prem = (S * benefit *(1 + percent_fee)) / (1 - percent_init)
  
  return(prem)}


# joint whole life assur - single prem - eoyod: 

jprem_wholelifeassurance_single_eoyod = function(age_x, age_y, IR, S, b, percent_init, percent_claim){
  interest = IR
  percent_init = percent_init/100
  percent_claim =percent_claim/100
  bonus = b/100 * S
  
  prem = ((S * j_wholelifeassurance_eoyod(age_x , age_y, interest) + bonus * ij_wholelifeassurance_eoyod(age_x, age_y, interest)) * (1 + percent_claim) ) / (1 - percent_init)
  return(prem)
}

# joint whole life assur - level prem - eoyod: 

jprem_wholelifeassurance_level_eoyod = function(age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  
  term = 100 - max(age_x, age_y)
  termannuity = j_termannuity_advance(age_x, age_y, term , interest)
  
  prem = ((S * j_wholelifeassurance_eoyod(age_x , age_y, interest) + bonus * ij_wholelifeassurance_eoyod(age_x , age_y, interest)) * (1 + percent_claim) ) /(termannuity - percent_init - percent_renew*(termannuity - 1))
  return(prem)
}


# joint whole life assur - single prem - iod:

jprem_wholelifeassurance_single_iod = function(age_x, age_y, IR, S, b, percent_init, percent_claim){
  interest = IR
  
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  
  prem = ((S * j_wholelifeassurance_iod(age_x , age_y, interest) + bonus * ij_wholelifeassurance_iod(age_x, age_y, interest)) * (1 + percent_claim) ) / (1 - percent_init)
  return(prem)
}

# joint whole life assur - level prem - iod: 


jprem_wholelifeassurance_level_iod <- function(age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100 
  bonus = b/100 * S
  
  term = 100 - max(age_x, age_y)
  termannuity = j_termannuity_advance(age_x, age_y, term, interest)
  
  prem = ((S * j_wholelifeassurance_iod(age_x , age_y, interest) + bonus * ij_wholelifeassurance_iod(age_x, age_y, interest)) * (1 + percent_claim) ) /(termannuity - percent_init - percent_renew * (termannuity-1))
  return(prem)
}

# joint term assurance - single prem - eoyod: 

jprem_termassurance_single_eoyod = function(age_x, age_y, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  bonus = b/100 * S
  
  prem = ((S * j_termassurance_eoyod(age_x, age_y, term, interest) + bonus * ij_termassurance_eoyod(age_x, age_y, term, interest)) * (1 + percent_claim) ) / (1 - percent_init)
  return(prem)
}

# joint term assur - level prem - eoyod: 

jprem_termassurance_level_eoyod = function(age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  percent_renew = percent_renew/100
  bonus = b/100 * S
  
  termannuity = j_termannuity_advance(age_x, age_y, term, interest)
  
  prem = ((S * j_termassurance_eoyod(age_x, age_y, term, interest) + bonus * ij_termassurance_eoyod(age_x, age_y, term, interest))  * (1 + percent_claim) ) / (termannuity - percent_init - percent_renew*(termannuity - 1))
  return(prem)
}

# joint term assur - single prem - iod: 

jprem_termassurance_single_iod = function(age_x, age_y, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  bonus = b/100 * S
  
  prem = ((S * j_termassurance_iod(age_x, age_y, term, interest) + bonus * ij_termassurance_iod(age_x, age_y, term, interest))  * (1 + percent_claim) ) / (1 - percent_init)
  return(prem)
}

# joint term assur - level prem - iod: 

jprem_termassurance_level_iod = function(age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  percent_renew = percent_renew/100
  bonus = b/100 * S
  
  termannuity = j_termannuity_advance(age_x, age_y, term, interest)
  
  prem = ((S * j_termassurance_iod(age_x, age_y, term, interest) + bonus * ij_termassurance_iod(age_x, age_y, term, interest)) * (1 + percent_claim) ) / (termannuity - percent_init - percent_renew*(termannuity - 1))
  return(prem)
}

############################################### JOINT RESERVES ###############################################

# whole life annuity - eoyod:
jres_wholelifeannuity_eoyod = function(prem, age_x, age_y, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_fee = percent_fee / 100
  percent_init = percent_init / 100
  b = b / 100
  IR = IR/100
  j = (((1+IR)/(1+b)) - 1)*100
  
  reserves = c()
  
  for (t in 0:(100 - max(age_x, age_y))){
    wla = 1/(1+b) * (j_wholelifeannuity_advance(age_x + t, age_y + t, j) - 1)
    
    if (t == 0)
      reserves[1] = ( S * wla * (1 + percent_fee) + prem * percent_init ) - prem
    else
      reserves[t + 1] = S * wla * (1 + percent_fee)
  }
  return(reserves)}

# term annuity - eoyod 

jres_termannuity_eoyod = function(prem, age_x, age_y, term, IR, S, b, percent_init, percent_fee){
  interest = IR
  percent_fee = percent_fee / 100
  percent_init = percent_init / 100
  b = b / 100
  IR = IR/100
  v = 1/(1+IR)
  j = (((1+IR)/(1+b)) - 1)*100
  x = age_x - 20 + 1
  y = age_y - 20 + 1
  
  reserves = c()
  
  for (t in 0:term){
    tpx = Lx[x+1 + t]/Lx[x + t]
    tpy = Ly[y+1 + t]/Ly[y + t]
    
    termannuity = 1/(1+b) * ( v * tpx * tpy * j_termannuity_advance(age_x + 1 + t, age_y + 1 + t, term - t, j))
    
    if (t == 0)
      reserves[1] = ( S * termannuity * (1 + percent_fee) + prem * percent_init ) - prem
    else
      reserves[t + 1] = S * termannuity * (1 + percent_fee)
  }
  return(reserves)
}

# whole life assur - single - eoyod: 

jres_wholelifeassurance_single_eoyod = function(prem, age_x, age_y, IR, S, b, percent_init, percent_claim){
  interest = IR
  
  percent_init = percent_init / 100
  percent_claim = percent_claim/100
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:(100 - max(age_x, age_y))){
    wla = j_wholelifeassurance_eoyod(age_x + t, age_y + t, interest)
    ia = ij_wholelifeassurance_eoyod(age_x + t, age_y + t, interest)
    
    if (t == 0)
      reserves[1] = ( (S * wla + bonus * ia) * (1 + percent_claim) +  prem * percent_init ) - prem
    else
      reserves[t + 1] = (S * wla + bonus * ia) * (1 + percent_claim)
  }
  return(reserves)
}

# whole life assur - level - eoyod: 

jres_wholelifeassurance_level_eoyod = function(prem, age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  
  percent_init = percent_init / 100
  percent_claim = percent_claim/100
  percent_renew = percent_renew / 100
  
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:(100 - max(age_x, age_y))){
    wla = j_wholelifeassurance_eoyod(age_x + t, age_y + t, interest)
    ia = ij_wholelifeassurance_eoyod(age_x + t, age_y + t, interest)
    term = 100 - max(age_x, age_y)
    termannuity = j_termannuity_advance(age_x + t, age_y + t, term - t, interest)
    
    if (t == 0)
      reserves[1] = ( (S*wla + bonus*ia) * (1 + percent_claim) +  prem * (percent_init + percent_renew * (termannuity-1)) ) - (prem * termannuity)
    else
      reserves[t+1] = ( (S*wla + bonus*ia) * (1 + percent_claim) +  prem * percent_renew * termannuity) - prem * termannuity
  }
  return(reserves)
}

# whole life assur - single prem - iod: 

jres_wholelifeassurance_single_iod = function(prem, age_x, age_y, IR, S, b, percent_init, percent_claim){
  interest = IR
  
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:(100 - max(age_x, age_y))){
    wla = j_wholelifeassurance_iod(age_x + t, age_y + t, interest)
    ia = ij_wholelifeassurance_iod(age_x + t, age_y + t, interest)
    
    if (t == 0)
      reserves[1] = ( (S*wla + bonus*ia) * (1 + percent_claim) +  prem * percent_init ) - prem
    else
      reserves[t+1] = (S*wla + bonus*ia) * (1 + percent_claim)
  }
  return(reserves)
}
  
# whole life assur - level prem - iod: 

jres_wholelifeassurance_level_iod = function(prem, age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  percent_init = percent_init / 100
  percent_claim = percent_claim / 100
  percent_renew = percent_renew / 100
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:(100 - max(age_x, age_y))){
    wla = j_wholelifeassurance_iod(age_x + t, age_y + t, interest)
    ia = ij_wholelifeassurance_iod(age_x + t, age_y + t, interest)
    term = 100 - max(age_x, age_y)
    termannuity = j_termannuity_advance(age_x + t, age_y + t, term - t, interest)
    
    if (t == 0)
      reserves[1] = ( (S*wla + bonus*ia) * (1 + percent_claim) +  prem * (percent_init + percent_renew * (termannuity-1)) ) - (prem * termannuity)
    else
      reserves[t+1] = ( (S*wla + bonus*ia) * (1 + percent_claim) +  prem * percent_renew * termannuity ) - prem * termannuity
  }
  return(reserves)
}

# term assur - single - eoyod: 

jres_termassurance_single_eoyod = function(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:term){
    ta = j_termassurance_eoyod(age_x + t, age_y + t, term - t, interest)
    ia = ij_termassurance_eoyod(age_x + t, age_y + t, term - t, interest)
    
    if (t == 0)
      reserves[1] = ( ( (S*ta + bonus*ia) * (1 + percent_claim) ) + prem * percent_init ) - prem
    else
      reserves[t+1] = (S*ta + bonus*ia) * (1 + percent_claim)
  }
  return(reserves)
}

# term assur - level - eoyod: 

jres_termassurance_level_eoyod = function(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  percent_renew = percent_renew/100
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:term){
    ta = j_termassurance_eoyod(age_x + t, age_y + t, term - t, interest)
    ia = ij_termassurance_eoyod(age_x + t, age_y + t, term - t, interest)
    termannuity = j_termannuity_advance(age_x + t, age_y + t, term - t, interest)
    
    if (t == 0) {
      reserves[1] = ( ( (S*ta + bonus*ia) * (1 + percent_claim) ) + prem*(percent_init + percent_renew*(termannuity-1)) ) - prem*termannuity
    } else {
      reserves[t+1] = ( ( (S*ta + bonus*ia) * (1 + percent_claim) ) + prem*percent_renew*termannuity ) - prem*termannuity
    }
  }
  return(reserves)
}

# term assur - single - iod: 

jres_termassurance_single_iod = function(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:term){
    ta = j_termassurance_iod(age_x + t, age_y + t, term - t, interest)
    ia = ij_termassurance_iod(age_x + t, age_y + t, term - t, interest)
    
    if (t == 0)
      reserves[1] = ( ( (S*ta + bonus*ia) * (1 + percent_claim) ) + prem* percent_init ) - prem
    else
      reserves[t+1] = (S*ta + bonus*ia) * (1 + percent_claim)
  }
  return(reserves)
}

# term assur - level - iod: 

jres_termassurance_level_iod = function(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew){
  interest = IR
  
  percent_init = percent_init/100
  percent_claim = percent_claim/100
  percent_renew = percent_renew/100
  bonus = b/100 * S
  
  reserves = c()
  
  for (t in 0:term){
    ta = j_termassurance_iod(age_x + t, age_y + t, term - t, interest)
    ia = ij_termassurance_iod(age_x + t, age_y + t, term - t, interest)
    termannuity = j_termannuity_advance(age_x + t, age_y + t, term - t, interest)
    
    if (t == 0) {
      reserves[1] = ( ( (S*ta + bonus*ia) * (1 + percent_claim) ) + prem*(percent_init + percent_renew*(termannuity-1)) ) - prem*termannuity
    } else {
      reserves[t+1] = ( ( (S*ta + bonus*ia) * (1 + percent_claim) ) + prem*percent_renew*termannuity ) -  prem*termannuity
    }
  }
  return(reserves)
}

############################################### RESERVES PLOT ###############################################

res_plot = function(reserves1, reserves2 = NULL){
  if (is.null(reserves2) == TRUE){
    reserves1 = round(reserves1,2)
    data = data.frame("Time" = c(0:(length(reserves1)-1)), "Reserve" = reserves1)
    g = ggplot(data, aes(x = Time, y = Reserve)) + 
      geom_line(color = "lightblue", size = 1) + 
      geom_point(color = "lightblue") +
      ggtitle("Evolution of Yearly Prospective Reserve Over Policy Period") +
      xlab("Policy Seniority (Years)") + 
      ylab("Required Reserve ($)") + 
      theme_minimal() + 
      theme(plot.title = element_text(size = 16, face = "bold")) + 
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12)) 
    
    return(g)
  }
   else {
     reserves1 = round(reserves1,2)
     reserves2 = round(reserves2,2)
     data = data.frame("Time" = c(0:(length(reserves1)-1)), "main_group" = reserves1, "comparison_group" = reserves2)
     g = ggplot(data, aes(x = Time)) + 
       geom_line(aes(y = main_group), color = "lightblue", size = 1) + 
       geom_point(aes(y = main_group), color = "lightblue") +
       geom_line(aes(y = comparison_group), color = "lightgreen", size = 1) +
       geom_point(aes(y = comparison_group), color = "lightgreen") +
       ggtitle("Evolution of Yearly Prospective Reserve Over Policy Period") +
       xlab("Policy Seniority (Years)") + 
       ylab("Required Reserve ($)") + 
       theme_minimal() + 
       theme(plot.title = element_text(size = 16)) + 
       theme(plot.title = element_text(size = 16, face = "bold")) + 
       theme(axis.text=element_text(size=12),
             axis.title=element_text(size=12)) 
     
     
     return(g)
   }
}

############################################### UI ############################################### 


ui = fluidPage(
    titlePanel(h1(strong("Insurance Premium & Reserve Calculation Tool"), align = "center"), windowTitle = "Insurance Pricing & Reserving Tool"),
    theme = shinytheme("cerulean"),
    
    tabsetPanel(
      
      tabPanel(h4("Information"),
               icon = icon("info-circle"),
               fluidPage(
                 tags$head(
                   tags$style(HTML("
            code {
                display:block;
                padding: 1px;
                margin:0 0 1px;
                margin-top: 1px;
                font-size:20px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#FFFFFF;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:calibri;
            }"))),
                 fluidRow(
                   br(),
                   column(12, code(h4("Welcome to the Insurance Pricing & Reserve Calculation Tool. This page will introduce you to the basic features and products incorporated in the app.", align = "center")))
                 ),
                 br(),
                 br(),
                 fluidRow(
                   column(4, h3(strong("Features")), br(), h4("Our pricing tool supports two types of insurance policies:"), br(), h4("1. Single life"), h4("2. Joint life"), br(), h4("Within each group, you can first specify the policyholder age and their group (only applicable to single life products). Under each type of policy, you can choose between:"), br(), h4("1. Annuity"), h4("2. Assurance"), br(), h4("The tool will then display the gross premium amount, as well as an evolution plot for the prospective reserves, for which you can choose to compare the reserves against the remaining group (only applicable to single life products).")),
                   column(4, h3(strong("Single products")), br(), h4("Single life annuities include:"), br(), h4("1. Whole life annuity"), h4("2. Temporary annuity"), br(), h4("You can customize: benefit payable in arrears/continuously, interest rate, sum assured, compound rate of annuity inflation*, initial expense*, annuity benefit expense*. Only single premium applies in this case."), br(), h4("Single life assurances include:"), br(), h4("1. Pure endowment"), h4("2. Term assurance"), h4("3. Endowment assurance"), h4("4. Whole life assurance"), br(), h4("You can customize: benefit payable at end of year of death/immediately upon death (both n/a for pure endowment), benefit term (n/a for whole life assurance), interest rate, sum assured, simple bonus rate*, level annual/single premium, initial expense, renewal expense* (n/a for single premium), claim expense*.")),
                   column(4, h3(strong("Joint products")), br(), h4("Joint life annuities include:"), br(), h4("1. Whole life annuity"), h4("2. Temporary annuity"), br(), h4("Both are payable in arrears and go with single premium. You can customize: interest rate, sum assured, annuity inflation rate, initial expense, annuity benefit fee"), br(), h4("Joint assurances include:"), br(), h4("1. Whole life assurance"), h4("2. Term assurance"), br(), h4("You can customize: benefit payable at EOYOD/IOD, term (n/a for whole life assurance), interest rate, sum assured, simple bonus rate, initial expense, renewal expense (n/a for single premium), claim expense."))
                 ),
                 br(),
                 fluidRow(
                   h3(strong("Assumptions and notes")), br(), h4("1. The mortality of policyholder group (x) obeys the standard AM92 Ultimate mortality. The mortality of group (y) is derived from that of group X, where ly = l(x-3). This means that ly at age 20 corresponds to lx at age 17.", h4("2. For joint products, the mortality of the first and second policyholder is that of group (x) and (y) respectively."), h4("3. The maximum age at which contracts are sold is 100."), h4("4. For single products: the maximum term is (100 - policyholder's age)"), h4("5. For joint products: the maximum term is (100 - age of older policyholder)"), br(), h4("*Compound rate of annuity inflation: for annuities, non-zero rate means annuity payments will increase at a compound rate from the second year onwards reflecting inflation."), h4("*Simple bonus rate: for assurances, non-zero rate means product includes a simple bonus vesting at the start of each year including the first year."), h4("*Initial expense: in percentage of the gross premium, payable at the start of the contract."), h4("*Annuity benefit expense: in percentage of the annuity benefit, payable at the same time."), h4("*Renewal expense: excluding the first year, in percentage of the gross premium, payable at the same time as the level annual premium."), h4("*Claim expense: in percentage of the assurance claim, payable at the same time."))
                 ), 
                 br(), br()
               )),

        # tab 1: single life 
        
        tabPanel(h4("Single Life"),
                 icon = icon("user"),
                 sidebarLayout(
                     sidebarPanel(
                         tabsetPanel(
                             tabPanel(h4("Policyholder Info"),
                                      tags$br(),
                                      # choose X / Y
                                      radioButtons(inputId = "PolicyHolder", label= "Policyholder Group", choices = list("Group X", "Group Y"), selected = "Group X"),
                                      
                                      # age 
                                      sliderInput(inputId="S_Age",label="Age", value = 20,min=20,max=100)),
                             
                             tabPanel(h4("Product Info"),
                                      tags$br(),
                                      
                                      # single life prod: 
                                      radioButtons(inputId = "S_Prod", label = "Products", choices = list("Annuity", "Assurance"), selected = "Annuity"),
                                      
                                      # annuity
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Annuity'",
                                          # annuity prod: 
                                          selectInput(inputId = "SA_SubProd", label = "Annuity Products", choices = list("Whole Life Annuity", "Temporary Annuity"), selected = "Whole Life Annuity"),
                                          # arrears / continuous 
                                          selectInput(inputId = "SA_PmtPattern",label="Payment Pattern", choices = list("Arrears","Continuous"), selected = "Arrears"),
                                          
                                          # term: 
                                          conditionalPanel(
                                              condition = "input.SA_SubProd == 'Temporary Annuity'",
                                              sliderInput(inputId = "SA_Term", label = "Term", value = 10, min = 1, max = 104))
                                         
                                          
                                      ),
                                      
                                      # assurance
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Assurance'",
                                          selectInput(inputId = "SS_SubProd", label = "Assurance Products", choices = list("Pure Endowment", "Term Assurance", "Endowment Assurance", "Whole Life Assurance"), selected = "Pure Endowment"),
                                          conditionalPanel(
                                              condition = "input.SS_SubProd != 'Pure Endowment'",
                                              selectInput(inputId = "S_PmtTime", label = "Payment Time", choices = list("End of Year of Death", "Immediately Upon Death"), selected = "End of Year of Death")),
                                          conditionalPanel(
                                              condition = "input.SS_SubProd != 'Whole Life Assurance'",
                                              sliderInput(inputId = "SS_Term", label = "Term", value = 10, min = 1, max = 104)),
                                          
                                      ),
                                      
                                      # interest: 
                                      sliderInput(inputId = "S_IR",label="Interest Rate",value=4,min=0,max =50, post = "%"),
                                      
                                 
                                      
                                      # sum assured 
                                      numericInput(inputId = "S_Sum", label = "Assured Sum", value= 1000, min = 100, step = 100),
                                      
                                      # inflation-indexed annuity: 
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Annuity'",
                                          sliderInput(inputId = "SA_Inflation",label="Rate of Annuity Inflation",value=4,min=0,max =50, post = "%")
                                      ),
                                      
                                      # simple bonus rate for assurance: 
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Assurance' && input.SS_SubProd != 'Pure Endowment'",
                                          sliderInput(inputId = "SS_Bonus",label="Simple Bonus Rate",value=4,min=0,max =50, post = "%")
                                      ),
                                      
                                      # single premium for annuity:
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Annuity'",
                                          selectInput(inputId = "SA_Prem",label="Premium Type", choices = list("Single"), selected = "Single")),
                                      
                                      # level or single premium for assurance: 
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Assurance'",
                                          selectInput(inputId = "SS_Prem",label="Premium Type", choices = list("Level Annual", "Single"), selected = "Level Annual")),
                                      
                                      ),
                                      
                                    
                                      
                             tabPanel(h4("Expenses"),
                                      tags$br(),
                                      
                                      # initial exp.
                                      sliderInput(inputId = "S_InitExp", label = "Initial Expenses (Percentage of Gross Premium)", value = 0, min = 0, max = 50, post = "%"),
                                      
                                      # renewal exp.
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Assurance' && input.SS_Prem == 'Level Annual'",
                                          sliderInput(inputId = "SS_RenewExp", label = "Renewal Expenses (Percentage of Gross Premium)", value = 0, min = 0, max = 50, post = "%")),
                                      
                                      # claim exp.
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Assurance'",
                                          sliderInput(inputId = "SS_ClaimExp", label = "Claim Expenses (Percentage of Claim)", value = 0, min = 0, max = 50, post = "%")),
                                      
                                      # annuity benefit fee: 
                                      conditionalPanel(
                                          condition = "input.S_Prod == 'Annuity'",
                                          sliderInput(inputId = "SA_BenefitFee", label = "Annuity Benefit Fees (Percentage of Benefit)", value = 0, min = 0, max = 50, post = "%")),
                                      
                             ))),
                     mainPanel(
                       tabsetPanel(
                         tabPanel(h4("Reserve & Premium"),
                                  tags$br(),
                                  fluidRow(
                                           column(9, plotlyOutput("single_plot", width = 1000, height = 470)), 
                                           column(3, materialSwitch(inputId = "compare",label = h4("Compare with the remaining group"),value = FALSE))),
                                  tags$br(),
                                  tags$h3(strong("Gross Premium:")),
                                  tags$h3(strong(textOutput("single_premium")))
                                  )
                       ),
                         
                         
                     )
                 ),
        ),
        
        
        # joint life
        tabPanel(h4("Joint Life"),
                 icon = icon("user-friends"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         tabsetPanel(
                             tabPanel(h4("Policyholders Info"),
                                      tags$br(),
                                      
                                      # 1st age
                                      sliderInput(inputId="J_Age1",label="First Policyholder's Age", value = 20,min=20,max=100),
                                      
                                      # 2nd age 
                                      sliderInput(inputId="J_Age2",label="Second Policyholder's Age", value = 20,min=20,max=100)),
                             
                             tabPanel(h4("Product Info"),
                                      tags$br(),
                                      
                                      # joint products: 
                                      radioButtons(inputId = "J_Prod", label = "Products", choices = list("Annuity", "Assurance"), selected = "Annuity"),
                                      
                                      # annuity
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Annuity'",
                                          selectInput(inputId = "JA_SubProd", label = "Annuity Products", choices = list("Joint Whole Life Annuity", "Joint Temporary Annuity"), selected = "Joint Whole Life Annuity"),
                                          
                                          # arrears / continuous 
                                          selectInput(inputId = "JA_PmtPattern",label="Payment Pattern", choices = list("Arrears"), selected = "Arrears"),
                                          
                            
                                          # term: 
                                          conditionalPanel(
                                              condition = "input.JA_SubProd == 'Joint Temporary Annuity'",
                                              sliderInput(inputId = "JA_Term", label = "Term", value = 10, min = 1, max = 104))),
                                      
                                      # assurance:
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Assurance'",
                                          
                                          # assurance prod
                                          selectInput(inputId = "JS_SubProd", label = "Assurance Products", choices = list("Joint Whole Life Assurance", "Joint Term Assurance"), selected = "Joint Whole Life Assurance"),
                                          
                                          # pmt time 
                                          selectInput(inputId = "JS_PmtTime", label = "Payment Time", choices = list("End of Year of Death", "Immediately Upon Death"), selected = "End of Year of Death"),
                                          
                                          # term: 
                                          conditionalPanel(
                                              condition = "input.JS_SubProd == 'Joint Term Assurance'",
                                              sliderInput(inputId = "JS_Term", label = "Term", value = 10, min = 1, max = 104))),
                                      
                                      # interest: 
                                      sliderInput(inputId = "J_IR",label="Interest Rate",value=4,min=0,max =50, post = "%"),
                                      
                                      # sum assured 
                                      numericInput(inputId = "J_Sum", label = "Assured Sum", value= 1000, min = 100, step = 100),
                                      
                                      # inflation rate for annuity: 
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Annuity'",
                                          sliderInput(inputId = "JA_Inflation",label="Rate of Annuity Inflation",value=4,min=0,max =50, post = "%")),
                                      
                                      # simple bonus rate for assurance: 
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Assurance'",
                                          sliderInput(inputId = "JS_Bonus", label="Simple Bonus Rate",value=4,min=0,max =50, post = "%")),
                                      
                                      # single premium for annuity: 
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Annuity'",
                                          selectInput(inputId = "JA_Prem",label="Premium Type", choices = list("Single"), selected = "Single")),
                                      
                                      # level / single premium for assurance: 
                                      
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Assurance'",
                                          selectInput(inputId = "JS_Prem",label="Premium Type", choices = list("Level Annual", "Single"), selected = "Level"))),
                             
                             tabPanel(h4("Expenses"),
                                      tags$br(),
                                      # initial exp.
                                      sliderInput(inputId = "J_InitExp", label = "Initial Expenses (Percentage of Gross Premium)", value = 0, min = 0, max = 50, post = "%"),
                                      
                                      # renewal exp. for assurance with level prem.
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Assurance' && input.JS_Prem == 'Level Annual'",
                                          sliderInput(inputId = "JS_RenewExp", label = "Renewal Expenses (Percentage of Gross Premium)", value = 0, min = 0, max = 50, post = "%")),
                                      
                                      # claim exp. for assurance: 
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Assurance'",
                                          sliderInput(inputId = "JS_ClaimExp", label = "Claim Expenses (Percentage of Benefit)", value = 0, min = 0, max = 50, post = "%")),
                                      
                                      # benefit fee for annuity: 
                                      # claim exp.
                                      conditionalPanel(
                                          condition = "input.J_Prod == 'Annuity'",
                                          sliderInput(inputId = "JA_BenefitFee", label = "Annuity Benefit Fees (Percentage of Benefit)", value = 0, min = 0, max = 50, post = "%"))
                             ))),
                     
                     mainPanel(
                       tabsetPanel(tabPanel(h4("Reserve & Premium"),
                                            br(),
                         plotlyOutput("joint_plot", width = 1000, height = 470),
                         tags$h3(strong("Gross Premium:")),
                         tags$h3(strong(textOutput("joint_premium")))
                       ))))),
        
        tabPanel(
          icon = icon('envelope'),
          h4("Contact Us"),
          sidebarPanel(
            tags$h4("If you have any questions regarding or suggestions on how to improve this app, we'd love to hear from you!"),
            tags$br(),
            textInput("from", "From:", value="your email here"),
            textInput("to", "To:", value="admin@actuarial.monash.edu"),
            tags$em("Note: this is not a real email account, only for demonstration purposes"),
            tags$p(),
            textInput("subject", "Subject:", value=""),
            
            actionButton("send", "Send mail")),
          
          mainPanel(    
            shinyAce::aceEditor("message", value="")))),
    materialSwitch(
      inputId = "themeToggle",
      label = "Toggle Mode"
    ),
    tags$script(
        "
        // define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/slate.min.css',
            light: 'shinythemes/css/cerulean.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraLightThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {color: black!important;} .paginate_button { background: black!important;} thead { color: black;}'
        const extraLightThemeElement = document.createElement('style');
        extraLightThemeElement.appendChild(document.createTextNode(extraLightThemeCSS));
        head.appendChild(extraLightThemeElement);


        // define event - checked === 'dark'
        toggle.addEventListener('input', function(event) {
            // if checked, switch to dark theme
            if (toggle.checked) {
                removeLink(themes.light);
                head.removeChild(extraLightThemeElement);
                head.appendChild(darkTheme);
            }  else {
                // else add lighttheme
                removeLink(themes.dark);
                head.appendChild(extraLightThemeElement)
                head.appendChild(lightTheme);
            }
        })
        "
    ),
    tags$head(tags$style(HTML('* {font-family: "Calibri"};'))),
    tags$style(type = "text/css", "label{font-size: 18px}"),
    tags$footer("Developed by Nhi Le", align = "right"),
    br(),
    br()
    
)  


############################################### SERVER ############################################### 


server = function(input,output, session) {
    
    # change term and guaranteed period dynamically depending on age
    observe({
        val = input$S_Age
        updateSliderInput(session, "SA_Term", max = 104 - val)
        updateSliderInput(session, "SS_Term", max = 104 - val)
    })
    
    
    joint_ages = reactiveValues()
    
    observe({
        joint_ages$age1 = input$J_Age1
        joint_ages$age2 = input$J_Age2
        
        if (joint_ages$age1 >= joint_ages$age2){
            updateSliderInput(session, "JA_Term", max = 104 - joint_ages$age1)
            updateSliderInput(session, "JS_Term", max = 104 - joint_ages$age1)
        }
        else{
            updateSliderInput(session, "JA_Term", max = 104 - joint_ages$age2)
            updateSliderInput(session, "JS_Term", max = 104 - joint_ages$age2)
        }
    })
    
    # email: 
    observe({
      if(is.null(input$send) || input$send==0) return(NULL)
      from <- isolate(input$from)
      to <- isolate(input$to)
      subject <- isolate(input$subject)
      msg <- isolate(input$message)
      sendmail(from, to, subject, msg)
    })
    
    ############################################################ PREMIUM OUTPUT ############################################################ 
  
        output$single_premium = renderText( {
          
          ################################### PREMIUM - SINGLE ANNUITIES ################################### 
          
          # whole life annuity in arrears: 
            if (input$S_Prod == "Annuity" & input$SA_SubProd == "Whole Life Annuity" & input$SA_PmtPattern == "Arrears"){
                if (input$PolicyHolder == "Group X"){L = Lx}
                if (input$PolicyHolder == "Group Y"){L = Ly}
                age = input$S_Age
                IR = input$S_IR
                S = input$S_Sum
                b = input$SA_Inflation
                percent_init = input$S_InitExp
                percent_fee = input$SA_BenefitFee
                prem = prem_wholelifeannuity_single_eoyod(L, age, IR, S, b, percent_init, percent_fee)
                print(paste0("$ ", round(prem,2)))}
          
          # whole life annuity - continuous
          else if (input$S_Prod == "Annuity" & input$SA_SubProd == "Whole Life Annuity" & input$SA_PmtPattern == "Continuous"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SA_Inflation
            percent_init = input$S_InitExp
            percent_fee = input$SA_BenefitFee
            prem = prem_wholelifeannuity_single_iod(L, age, IR, S, b, percent_init, percent_fee)
            print(paste0("$ ", round(prem,2)))
          }
          
          # temporary annuity - arrears: 
          else if (input$S_Prod == "Annuity" & input$SA_SubProd == "Temporary Annuity" & input$SA_PmtPattern == "Arrears"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            term = input$SA_Term
            IR = input$S_IR
            S = input$S_Sum
            b = input$SA_Inflation
            percent_init = input$S_InitExp
            percent_fee = input$SA_BenefitFee
            
            prem = prem_termannuity_single_eoyod(L, age, term, IR, S, b, percent_init, percent_fee)
            print(paste0("$ ", round(prem,2)))}
          
          # temporary annuity - continuous: 
          else if (input$S_Prod == "Annuity" & input$SA_SubProd == "Temporary Annuity" & input$SA_PmtPattern == "Continuous"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            term = input$SA_Term
            IR = input$S_IR
            S = input$S_Sum
            b = input$SA_Inflation
            percent_init = input$S_InitExp
            percent_fee = input$SA_BenefitFee
            
            prem = prem_termannuity_single_iod(L, age, term, IR, S, b, percent_init, percent_fee)
            
            print(paste0("$ ", round(prem,2)))
          }
          
          ################################### PREMIUM - SINGLE ASSURANCES ###################################  
          
          
          # whole life assurance - single - eoyod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "End of Year of Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            prem = prem_wholelifeassurance_single_eoyod(L, age, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))
          }
          
          # whole life assurance - level prem - eoyod 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "End of Year of Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            prem = prem_wholelifeassurance_level_eoyod(L, age, IR, S, b, percent_init, percent_claim, percent_renew)

            print(paste0("$ ", round(prem,2)))
          }
          
          # whole life assurance - single prem - iod:
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            prem = prem_wholelifeassurance_single_iod(L, age, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # whole life assurance - level prem - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            prem = prem_wholelifeassurance_level_iod(L, age, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # term assur - single - eoyod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "End of Year of Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            prem = prem_termassurance_single_eoyod(L, age, term, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # term assur - level - eoyod: 
          
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "End of Year of Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            prem = prem_termassurance_level_eoyod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # term assur - single - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            prem = prem_termassurance_single_iod(L, age, term, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # term assur - level - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            prem = prem_termassurance_level_iod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # pure endow - single: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Pure Endowment" & input$SS_Prem == "Single"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            prem = prem_pureendowment_single(L, age, term, IR, S, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # pure endowment - level: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Pure Endowment" & input$SS_Prem == "Level Annual"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            prem = prem_pureendowment_level(L, age, term, IR, S, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # endow assur - single - eoyod:
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "End of Year of Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            prem = prem_endowmentassurance_single_eoyod(L, age, term, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # endow assur - level - eoyod:
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "End of Year of Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            prem = prem_endowmentassurance_level_eoyod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # endow assur - single - iod:
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            prem = prem_endowmentassurance_single_iod(L, age, term, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # endow assur - level - iod:
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            prem = prem_endowmentassurance_level_iod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          })
        
        ########################################################## JOINT PREM ########################################################## 
        
        # joint whole life annuity
        output$joint_premium = renderText ({
          if (input$J_Prod == 'Annuity' & input$JA_SubProd == "Joint Whole Life Annuity" & input$JA_PmtPattern == "Arrears" & input$JA_Prem == "Single"){
            
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JA_Inflation
            percent_init = input$J_InitExp
            percent_fee = input$JA_BenefitFee
            prem = jprem_wholelifeannuity_eoyod(age_x, age_y, IR, S, b, percent_init, percent_fee)
            print(paste0("$ ", round(prem,2)))}
          
          # joint temporary annuity: 
          else if (input$J_Prod == 'Annuity' & input$JA_SubProd == "Joint Temporary Annuity" & input$JA_PmtPattern == "Arrears" & input$JA_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JA_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JA_Inflation
            percent_init = input$J_InitExp
            percent_fee = input$JA_BenefitFee
            prem = jprem_termannuity_eoyod(age_x, age_y, term, IR, S, b, percent_init, percent_fee)
            print(paste0("$ ", round(prem,2)))}
          
          # joint whole life assur - single - eoyod: 
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_wholelifeassurance_single_eoyod(age_x, age_y, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # joint whole life assur - level - eoyod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_wholelifeassurance_level_eoyod(age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # joint whole life assur - single - iod 
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_wholelifeassurance_single_iod(age_x, age_y, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # joint whole life assur - level - iod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_wholelifeassurance_level_iod(age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # joint term assur - single - eoyod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_termassurance_single_eoyod(age_x, age_y, term, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # joint term assur - level - eoyod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_termassurance_level_eoyod(age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
          
          # joint term assur - single - iod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_termassurance_single_iod(age_x, age_y, term, IR, S, b, percent_init, percent_claim)
            print(paste0("$ ", round(prem,2)))}
          
          # joint term assur - level - iod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_termassurance_level_iod(age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew)
            print(paste0("$ ", round(prem,2)))}
        })
        
        
        ######################################################### SINGLE RESERVES ######################################################### 
        
        output$single_plot = renderPlotly({
          
          ##################################### SINGLE ANNUITIES ##################################### 
          
          # whole life annuity in arrears: 
          
          if (input$S_Prod == "Annuity" & input$SA_SubProd == "Whole Life Annuity" & input$SA_PmtPattern == "Arrears"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SA_Inflation
            percent_init = input$S_InitExp
            percent_fee = input$SA_BenefitFee
            
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_wholelifeannuity_single_eoyod(L, age, IR, S, b, percent_init, percent_fee)
              reserves = res_wholelifeannuity_eoyod(prem, L, age, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_wholelifeannuity_single_eoyod(L, age, IR, S, b, percent_init, percent_fee)
              reserves1 = res_wholelifeannuity_eoyod(prem1, L, age, IR, S, b, percent_init, percent_fee)
              
              prem2 = prem_wholelifeannuity_single_eoyod(L_compare, age, IR, S, b, percent_init, percent_fee)
              reserves2 = res_wholelifeannuity_eoyod(prem2, L_compare, age, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # whole life annuity - continuous:
          
          else if (input$S_Prod == "Annuity" & input$SA_SubProd == "Whole Life Annuity" & input$SA_PmtPattern == "Continuous"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SA_Inflation
            percent_init = input$S_InitExp
            percent_fee = input$SA_BenefitFee
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_wholelifeannuity_single_iod(L, age, IR, S, b, percent_init, percent_fee)
              reserves = res_wholelifeannuity_iod(prem, L, age, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_wholelifeannuity_single_iod(L, age, IR, S, b, percent_init, percent_fee)
              reserves1 = res_wholelifeannuity_iod(prem1, L, age, IR, S, b, percent_init, percent_fee)
              
              prem2 = prem_wholelifeannuity_single_iod(L_compare, age, IR, S, b, percent_init, percent_fee)
              reserves2 = res_wholelifeannuity_iod(prem2, L_compare, age, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # term annuity - single - eoyod: 
          else if (input$S_Prod == "Annuity" & input$SA_SubProd == "Temporary Annuity" & input$SA_PmtPattern == "Arrears"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SA_Inflation
            term = input$SA_Term
            percent_init = input$S_InitExp
            percent_fee = input$SA_BenefitFee
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_termannuity_single_eoyod(L, age, term, IR, S, b, percent_init, percent_fee)
              reserves = res_termannuity_eoyod(prem, L, age, term, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_termannuity_single_eoyod(L, age, term, IR, S, b, percent_init, percent_fee)
              reserves1 = res_termannuity_eoyod(prem1, L, age, term, IR, S, b, percent_init, percent_fee)
              
              prem2 = prem_termannuity_single_eoyod(L_compare, age, term, IR, S, b, percent_init, percent_fee)
              reserves2 = res_termannuity_eoyod(prem2, L_compare, age, term, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # term annuity - single - iod: 
          else if (input$S_Prod == "Annuity" & input$SA_SubProd == "Temporary Annuity" & input$SA_PmtPattern == "Continuous"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SA_Inflation
            term = input$SA_Term
            percent_init = input$S_InitExp
            percent_fee = input$SA_BenefitFee
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_termannuity_single_iod(L, age, term, IR, S, b, percent_init, percent_fee)
              reserves = res_termannuity_iod(prem, L, age, term, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_termannuity_single_iod(L, age, term, IR, S, b, percent_init, percent_fee)
              reserves1 = res_termannuity_iod(prem1, L, age, term, IR, S, b, percent_init, percent_fee)
              
              prem2 = prem_termannuity_single_iod(L_compare, age, term, IR, S, b, percent_init, percent_fee)
              reserves2 = res_termannuity_iod(prem2, L_compare, age, term, IR, S, b, percent_init, percent_fee)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # whole life assur - single - eoyod: 
    
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "End of Year of Death"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_wholelifeassurance_single_eoyod(L, age, IR, S, b, percent_init, percent_claim)
              reserves = res_wholelifeassurance_single_eoyod(prem, L, age, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_wholelifeassurance_single_eoyod(L, age, IR, S, b, percent_init, percent_claim)
              reserves1 = res_wholelifeassurance_single_eoyod(prem1, L, age, IR, S, b, percent_init, percent_claim)
              
              prem2 = prem_wholelifeassurance_single_eoyod(L_compare, age, IR, S, b, percent_init, percent_claim)
              reserves2 = res_wholelifeassurance_single_eoyod(prem2, L_compare, age, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # whole life assur - level - eoyod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "End of Year of Death"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_wholelifeassurance_level_eoyod(L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves = res_wholelifeassurance_level_eoyod (prem, L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_wholelifeassurance_level_eoyod(L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves1 = res_wholelifeassurance_level_eoyod (prem1, L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              
              prem2 = prem_wholelifeassurance_level_eoyod(L_compare, age, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves2 = res_wholelifeassurance_level_eoyod (prem2, L_compare, age, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # whole life assur - single - iod: 
          
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "Immediately Upon Death"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_wholelifeassurance_single_iod(L, age, IR, S, b, percent_init, percent_claim)
              reserves = res_wholelifeassurance_single_iod(prem, L, age, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_wholelifeassurance_single_iod(L, age, IR, S, b, percent_init, percent_claim)
              reserves1 = res_wholelifeassurance_single_iod(prem1, L, age, IR, S, b, percent_init, percent_claim)
              
              prem2 = prem_wholelifeassurance_single_iod(L_compare, age, IR, S, b, percent_init, percent_claim)
              reserves2 = res_wholelifeassurance_single_iod(prem2, L_compare, age, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # whole life assur - level - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Whole Life Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "Immediately Upon Death"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_wholelifeassurance_level_iod(L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves = res_wholelifeassurance_level_iod(prem, L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_wholelifeassurance_level_iod(L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves1 = res_wholelifeassurance_level_iod(prem1, L, age, IR, S, b, percent_init, percent_claim, percent_renew)
              
              prem2 = prem_wholelifeassurance_level_iod(L_compare, age, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves2 = res_wholelifeassurance_level_iod(prem2, L_compare, age, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # term assur - single - eoyod: 
          
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "End of Year of Death"){
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp

            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_termassurance_single_eoyod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves = res_termassurance_single_eoyod(prem, L, age, term, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem = prem_termassurance_single_eoyod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves_1 = res_termassurance_single_eoyod(prem, L, age, term, IR, S, b, percent_init, percent_claim)
              
              prem = prem_termassurance_single_eoyod(L_compare, age, term, IR, S, b, percent_init, percent_claim)
              reserves_2 = res_termassurance_single_eoyod(prem, L_compare, age, term, IR, S, b, percent_init, percent_claim)
              
              g = res_plot(reserves_1, reserves_2)
              plotly::ggplotly(g)
            }}
          
          # term assur - level - eoyod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "End of Year of Death"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            term = input$SS_Term
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_termassurance_level_eoyod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves = res_termassurance_level_eoyod(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_termassurance_level_eoyod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves1 = res_termassurance_level_eoyod(prem1, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              
              prem2 = prem_termassurance_level_eoyod(L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves2 = res_termassurance_level_eoyod(prem2, L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          
          # term assur - single - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "Immediately Upon Death"){
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            b = input$SS_Bonus
            term = input$SS_Term
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_termassurance_single_iod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves = res_termassurance_single_iod(prem, L, age, term, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem = prem_termassurance_single_iod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves1 = res_termassurance_single_iod(prem, L, age, term, IR, S, b, percent_init, percent_claim)
              
              prem = prem_termassurance_single_iod(L_compare, age, term, IR, S, b, percent_init, percent_claim)
              reserves2 = res_termassurance_single_iod(prem, L_compare, age, term, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # term assur - level - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Term Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "Immediately Upon Death"){
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_termassurance_level_iod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves = res_termassurance_level_iod(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)}
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem = prem_termassurance_level_iod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves_1 = res_termassurance_level_iod(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              
              prem = prem_termassurance_level_iod(L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves_2 = res_termassurance_level_iod(prem, L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              
              g = res_plot(reserves_1, reserves_2)
              plotly::ggplotly(g)
            }}
          
          
          # pure endowment - single: 
          
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Pure Endowment" & input$SS_Prem == "Single"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            term = input$SS_Term
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp

            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_pureendowment_single(L, age, term, IR, S, percent_init, percent_claim)
              reserves = res_pureendowment_single(prem, L, age, term, IR, S, percent_init, percent_claim)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_pureendowment_single(L, age, term, IR, S, percent_init, percent_claim)
              reserves1 = res_pureendowment_single(prem1, L, age, term, IR, S, percent_init, percent_claim)
              
              prem2 = prem_pureendowment_single(L_compare, age, term, IR, S, percent_init, percent_claim)
              reserves2 = res_pureendowment_single(prem2, L_compare, age, term, IR, S, percent_init, percent_claim)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # pure endowment - level: 
          
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Pure Endowment" & input$SS_Prem == "Level Annual"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            S = input$S_Sum
            term = input$SS_Term
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_pureendowment_level(L, age, term, IR, S, percent_init, percent_claim, percent_renew)
              reserves = res_pureendowment_level(prem, L, age, term, IR, S, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_pureendowment_level(L, age, term, IR, S, percent_init, percent_claim, percent_renew)
              reserves1 = res_pureendowment_level(prem1, L, age, term, IR, S, percent_init, percent_claim, percent_renew)
              
              prem2 = prem_pureendowment_level(L_compare, age, term, IR, S, percent_init, percent_claim, percent_renew)
              reserves2 = res_pureendowment_level(prem2, L_compare, age, term, IR, S, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }} 
          
          # endow assur - single - eoyod: 
          
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "End of Year of Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_endowmentassurance_single_eoyod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves = res_endowmentassurance_single_eoyod(prem, L, age, term, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_endowmentassurance_single_eoyod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves1 = res_endowmentassurance_single_eoyod(prem1, L, age, term, IR, S, b, percent_init, percent_claim)
              
              prem2 = prem_endowmentassurance_single_eoyod(L_compare, age, term, IR, S, b, percent_init, percent_claim)
              reserves2 = res_endowmentassurance_single_eoyod(prem2, L_compare, age, term, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # endowment assur - level - eoyod: 
          
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "End of Year of Death"){
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            
            
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_endowmentassurance_level_eoyod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves = res_endowmentassurance_level_eoyod(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_endowmentassurance_level_eoyod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves1 = res_endowmentassurance_level_eoyod(prem1, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              
              prem2 = prem_endowmentassurance_level_eoyod(L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves2 = res_endowmentassurance_level_eoyod(prem2, L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # endow assur - single - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Single" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_endowmentassurance_single_iod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves = res_endowmentassurance_single_iod(prem, L, age, term, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_endowmentassurance_single_iod(L, age, term, IR, S, b, percent_init, percent_claim)
              reserves1 = res_endowmentassurance_single_iod(prem1, L, age, term, IR, S, b, percent_init, percent_claim)
              
              prem2 = prem_endowmentassurance_single_iod(L_compare, age, term, IR, S, b, percent_init, percent_claim)
              reserves2 = res_endowmentassurance_single_iod(prem2, L_compare, age, term, IR, S, b, percent_init, percent_claim)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          # endow assur - level - iod: 
          else if (input$S_Prod == "Assurance" & input$SS_SubProd == "Endowment Assurance" & input$SS_Prem == "Level Annual" & input$S_PmtTime == "Immediately Upon Death"){
            if (input$PolicyHolder == "Group X"){L = Lx}
            if (input$PolicyHolder == "Group Y"){L = Ly}
            age = input$S_Age
            IR = input$S_IR
            term = input$SS_Term
            S = input$S_Sum
            b = input$SS_Bonus
            percent_init = input$S_InitExp
            percent_claim = input$SS_ClaimExp
            percent_renew = input$SS_RenewExp
            
            
            if (input$compare == FALSE){
              if (input$PolicyHolder == "Group X"){L = Lx}
              if (input$PolicyHolder == "Group Y"){L = Ly}
              prem = prem_endowmentassurance_level_iod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves = res_endowmentassurance_level_iod(prem, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves, NULL)
              plotly::ggplotly(g)
            }
            
            else {
              if (input$PolicyHolder == "Group X"){
                L = Lx
                L_compare = Ly}
              if (input$PolicyHolder == "Group Y"){
                L = Ly
                L_compare = Lx}
              
              prem1 = prem_endowmentassurance_level_iod(L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves1 = res_endowmentassurance_level_iod(prem1, L, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              
              prem2 = prem_endowmentassurance_level_iod(L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              reserves2 = res_endowmentassurance_level_iod(prem2, L_compare, age, term, IR, S, b, percent_init, percent_claim, percent_renew)
              g = res_plot(reserves1, reserves2)
              plotly::ggplotly(g)
            }}
          
          })
        
       ########################################################## JOINT RESERVES PLOT ###################################################################
        
        output$joint_plot = renderPlotly({
          
          # joint whole life annuity
          if (input$J_Prod == 'Annuity' & input$JA_SubProd == "Joint Whole Life Annuity" & input$JA_PmtPattern == "Arrears" & input$JA_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JA_Inflation
            percent_init = input$J_InitExp
            percent_fee = input$JA_BenefitFee
            prem = jprem_wholelifeannuity_eoyod(age_x, age_y, IR, S, b, percent_init, percent_fee)
            reserves = jres_wholelifeannuity_eoyod(prem, age_x, age_y, IR, S, b, percent_init, percent_fee)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint temporary annuity: 
          else if (input$J_Prod == 'Annuity' & input$JA_SubProd == "Joint Temporary Annuity" & input$JA_PmtPattern == "Arrears" & input$JA_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JA_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JA_Inflation
            percent_init = input$J_InitExp
            percent_fee = input$JA_BenefitFee
            prem = jprem_termannuity_eoyod(age_x, age_y, term, IR, S, b, percent_init, percent_fee)
            reserves = jres_termannuity_eoyod(prem, age_x, age_y, term, IR, S, b, percent_init, percent_fee)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint whole life assur - single - eoyod: 
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_wholelifeassurance_single_eoyod(age_x, age_y, IR, S, b, percent_init, percent_claim)
            reserves = jres_wholelifeassurance_single_eoyod(prem, age_x, age_y, IR, S, b, percent_init, percent_claim)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint whole life assur - level - eoyod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_wholelifeassurance_level_eoyod(age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew)
            reserves = jres_wholelifeassurance_level_eoyod(prem, age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint whole life assur - single - iod 
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_wholelifeassurance_single_iod(age_x, age_y, IR, S, b, percent_init, percent_claim)
            reserves = jres_wholelifeassurance_single_iod(prem, age_x, age_y, IR, S, b, percent_init, percent_claim)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint whole life assur - level - iod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Whole Life Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_wholelifeassurance_level_iod(age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew)
            reserves = jres_wholelifeassurance_level_iod(prem, age_x, age_y, IR, S, b, percent_init, percent_claim, percent_renew)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint term assur - single - eoyod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_termassurance_single_eoyod(age_x, age_y, term, IR, S, b, percent_init, percent_claim)
            reserves = jres_termassurance_single_eoyod(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint term assur - level - eoyod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "End of Year of Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_termassurance_level_eoyod(age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew)
            reserves = jres_termassurance_level_eoyod(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint term assur - single - iod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Single"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            prem = jprem_termassurance_single_iod(age_x, age_y, term, IR, S, b, percent_init, percent_claim)
            reserves = jres_termassurance_single_iod(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
          # joint term assur - level - iod
          else if (input$J_Prod == 'Assurance' & input$JS_SubProd == "Joint Term Assurance" & input$JS_PmtTime == "Immediately Upon Death" & input$JS_Prem == "Level Annual"){
            age_x = input$J_Age1
            age_y = input$J_Age2
            term = input$JS_Term
            IR = input$J_IR
            S = input$J_Sum
            b = input$JS_Bonus
            percent_init = input$J_InitExp
            percent_claim = input$JS_ClaimExp
            percent_renew = input$JS_RenewExp
            prem = jprem_termassurance_level_iod(age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew)
            reserves = jres_termassurance_level_iod(prem, age_x, age_y, term, IR, S, b, percent_init, percent_claim, percent_renew)
            g = res_plot(reserves)
            plotly::ggplotly(g)}
          
        })
}


############################################### APP ############################################### 


shinyApp(ui = ui , server = server)


############################################### REFERENCE ############################################### 


# toggle dark / light mode: https://stackoverflow.com/questions/61632272/r-shiny-light-dark-mode-switch, https://dreamrs.github.io/shinyWidgets/index.html
# all shinythemes: https://rstudio.github.io/shinythemes/
# font: https://stackoverflow.com/questions/45359552/change-font-family-throughout-entire-r-shiny-app-css-html
# layout guide: https://shiny.rstudio.com/articles/layout-guide.html
# shiny widgets: http://shinyapps.dreamrs.fr/shinyWidgets/
# icon: https://fontawesome.com/v5.15/icons?d=gallery&p=2&q=user
# email: https://stackoverflow.com/questions/20857068/sending-email-from-shiny
# animation: https://plotly.com/r/animations/


