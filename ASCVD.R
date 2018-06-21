dat<-read.table("table.dat", row.names=1)
const<-dat[3,]
smokc<-0; smokcov<-0;   BPc<-const$ln_untreated_BP; BPcov<-0; diab<-0
age<-55; numTC<-213; hdl<-50; numBP<-120
calc<-log(age)*const$ln_age+log(age)*log(age)*const$ln_age_squared+
  log(numTC)*const$ln_total_cholest+log(age)*log(numTC)*const$ln_age_totcholest+
  log(hdl)*const$ln_hdlC+log(age)*log(hdl)*const$ln_age_hdlC+smokc+
  smokcov*log(age)*const$ln_age_smoker+log(numBP)*BPc+log(age)*log(numBP)*BPcov+diab
ASCVD<-round(100*(1-(const$baseline^exp(calc-const$meancoef))),2)
ASCVD


#####male
const<-dat[3,]
smokc<-1;  diab<-1
age<-55; numTC<-213; hdl<-50; numBP<-120



  
cal=12.344*log(age)+11.853*log(numTC)-2.664*log(age)*log(numTC)-7.990*log(hdl)+1.769*log(age)*log(hdl)+
  1.764*log(numBP)+7.837*smokc-1.795*log(age)*smokc+0.658*diab

ASCVD<-round(100*(1-(0.9144^exp(cal-61.18))),2)
ASCVD


###female

smokc<-0;  
diab<-0
age<-55; numTC<-213; hdl<-50; numBP<-120

ascvdf= function(age,numTC,hdl,numBP,smokc,diab){
cal=-29.799*log(age)+4.884*log(age)*log(age)+13.540*log(numTC)-3.114*log(age)*log(numTC)-13.578*log(hdl)+3.149*log(age)*log(hdl)+
  1.957*log(numBP)+7.574*smokc-1.665*log(age)*smokc+0.661*diab


ASCVD<-round(100*(1-(0.9665^exp(cal+29.18))),2)
ASCVD}


library(tidyverse)
cohort=cohort %>% mutate(smokc=ifelse(Smoker=="smoker",1,0),diab=ifelse(Diabetes=="Diabetes",1,0))



ascvdf= function(age,numTC,hdl,numBP,smokc,diab){
  cal=-29.799*log(age)+4.884*log(age)*log(age)+13.540*log(numTC)-3.114*log(age)*log(numTC)-13.578*log(hdl)+3.149*log(age)*log(hdl)+
    1.957*log(numBP)+7.574*smokc-1.665*log(age)*smokc+0.661*diab
  
  
  ASCVD<-round(100*(1-(0.9665^exp(cal+29.18))),2)
  ASCVD}

ascvdm = function(age,numTC,hdl,numBP,smokc,diab){
  cal=12.344*log(age)+11.853*log(numTC)-2.664*log(age)*log(numTC)-7.990*log(hdl)+1.769*log(age)*log(hdl)+
  1.764*log(numBP)+7.837*smokc-1.795*log(age)*smokc+0.658*diab

ASCVD<-round(100*(1-(0.9144^exp(cal-61.18))),2)
ASCVD
}



ascvdf(age,numTC,hdl,numBP,smokc,diab)

ascvdm(age,numTC,hdl,numBP,smokc,diab)


cohort=cohort %>% mutate(ASCVD=case_when(
  sex=="male"~ascvdm(age=Age,numTC=Total_Cholesterol,hdl=HDL,numBP=SBP,smokc=smokc,diab=diab),
  sex=="female"~ascvdf(age=Age,numTC=Total_Cholesterol,hdl=HDL,numBP=SBP,smokc=smokc,diab=diab))) 


ggplot(cohort,aes(x=hypertensives,y=ASCVD,fill=hypertensives))+geom_boxplot()