
library(tidyverse)

set.seed(123)
cohort= tibble(

Age=c(rnorm(35,40,10.1),rnorm(35,43,10),rnorm(35,48,9.8),rnorm(25,41,7)),

Height=c(rnorm(35,170,7.2),rnorm(35,168,9.4),rnorm(35,166,9.1),rnorm(25,167,7.2)),

Weight= c(rnorm(35,62,10.1),rnorm(35,64,10.2),rnorm(35,68,11),rnorm(25,57,9.2)),

Abdominal_Circumfrence=c(rnorm(35,85,10.1),rnorm(35,88.3,10.1),rnorm(35,91,12),rnorm(25,87,8.9)),

SBP =c(rnorm(35,144.1,8),rnorm(35,164.2,10),rnorm(35,182,18),rnorm(25,116.3,9)),

DBP =c(rnorm(35,94,4),rnorm(35,102,4),rnorm(35,112,8),rnorm(25,79.1,10)),

Total_Cholesterol =c(rnorm(35,4.9,1.1),rnorm(35,5,0.9),rnorm(35,5.5,0.9),rnorm(25,4.8,1)),


HDL =c(rnorm(35,1.34,0.5),rnorm(35,1.31,0.6),rnorm(35,1.34,1),rnorm(25,1.28,0.4)),

LDL =c(rnorm(35,3.1,0.7),rnorm(35,3.1,1),rnorm(35,3.51,0.8),rnorm(25,2.8,0.4)),

VLDL =c(rnorm(35,0.6,0.2),rnorm(35,0.5,0.3),rnorm(35,0.59,0.3),rnorm(25,0.5,0.1)),


Triglycerides =c(rnorm(35,1.27,0.6),rnorm(35,1.2,0.4),rnorm(35,1.25,0.3),rnorm(25,1.09,0.4)),

Lipoprotein_A =c(rnorm(35,30,7),rnorm(35,34,8),rnorm(35,36.2,6),rnorm(25,20,10)),


sex=c(sample(c("male","female"),35,replace = TRUE,prob=c(0.52,0.48)),
      sample(c("male","female"),35,replace = TRUE,prob=c(0.53,0.47)),
      sample(c("male","female"),35,replace = TRUE,prob=c(0.4,0.6)),
      sample(c("male","female"),25,replace = TRUE,prob=c(0.5,0.5))),

Smoker= c(sample(c("smoker","Non_smoker"),35,replace = TRUE,prob=c(0.25,0.75)),
          sample(c("smoker","Non_smoker"),35,replace = TRUE,prob=c(0.3,0.70)),
          sample(c("smoker","Non_smoker"),35,replace = TRUE,prob=c(0.32,0.68)),
          sample(c("smoker","Non_smoker"),25,replace = TRUE,prob=c(0.22,0.78))),


hypertensives= c(rep("HTN_grade_1",35),rep("HTN_grade_2",35),rep("HTN_grade_3",35),rep("Control",25))

)


cohort=cohort %>% mutate(Total_Cholesterol=38.67*Total_Cholesterol,
                  LDL=38.67*LDL,
                  HDL=38.67*HDL,
                  Triglycerides=88.67*Triglycerides,
                  VLDL=38.67*VLDL)



cohort=cohort %>% mutate_if(is.numeric,round)

##cohort %>% select_if(is.numeric) %>% map_df(~round(.,digits=0))


cohort= cohort %>% mutate(type=ifelse(hypertensives=="Controls","controls","cases"))


summary(cohort$HDL)       


cohort=cohort %>% mutate(Total_Cholesterol=LDL+HDL+round(Triglycerides/5)) 


cohort=cohort %>% mutate(BMI=round(Weight/(Height/100)^2,2),MAP=round((SBP+2*DBP)/3,2)) 

          


source('anovat.R')

options(scipen = 999)

library(magrittr)



cohort %$% anovafinal(Lipoprotein_A,hypertensives)

sink("anova.txt")

cohort %>% select_if(is.numeric) %>% map(~anovafinal(.,cohort$hypertensives))

sink()



library(pander)


aa=cohort %>% select_if(is.numeric) %>% names()

aa
bb= "hypertensives"
cc=expand.grid(aa,bb)

dd= function(x,y){
  x <- enquo(x)
  y <- enquo(y)
  
  
  cohort %>% filter() anovafinal(!!x,!!y)
}

dd(Lipoprotein_A,hypertensives)


bare_to_quo_in_func <- function( var) {
  var_enq <- enquo(var)
  mtcars%>% select(!!var_enq) %>% head(1)
}
bare_to_quo_in_func( mpg)


#######plots#########

plots= cohort%>% select_if(is.numeric) %>% names() %>% map(function(y)ggplot(cohort,aes(x=hypertensives,aes_string(y=y),fill="orange")))+ geom_boxplot()+guides(fill=FALSE)
paths <- stringr::str_c("Hypertensives",1:length(plots), ".png")
pwalk(list(paths, plots), ggsave, path = getwd())

paths
tempdir()

getwd()

length(plots)


cc

dx= function(x,y){
  x <- enquo(x)
  y <- enquo(y)
  
  ggplot(cohort,aes(x=aes_string(quo_name(x)),y=aes_string(quo_name(y)),fill=aes_string(quo_name(x))))+geom_boxplot()+guides(fill=FALSE)
}

dx("hypertensives","Age")

rm(age)
