
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
  
  
  HDL =c(rnorm(35,38.67*1.34,38.67*0.5),rnorm(35,38.67*1.31,38.67*0.6),rnorm(35,38.67*1.34,38.67*1),rnorm(25,38.67*1.28,38.67*0.4)),
  
  LDL =c(rnorm(35,38.67*3.1,38.67*0.7),rnorm(35,38.67*3.1,38.67*1),rnorm(35,38.67*3.51,38.67*0.8),rnorm(25,38.67*2.8,38.67*0.4)),
  
  VLDL =c(rnorm(35,38.67*0.6,38.67*0.2),rnorm(35,38.67*0.5,38.67*0.3),rnorm(35,38.67*0.59,38.67*0.3),rnorm(25,38.67*0.5,38.67*0.1)),
  
  
  Triglycerides =c(rnorm(35,88.67*1.27,88.67*0.6),rnorm(35,88.67*1.2,88.67*0.4),rnorm(35,88.67*1.25,88.67*0.3),rnorm(25,88.67*1.09,88.67*0.4)),
  
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

cohort=cohort %>% mutate(Total_Cholesterol=LDL+HDL+round(Triglycerides/5)) 

cohort$Diabetes= c(sample(c("Diabetes","Non_Diabetes"),35,replace = TRUE,prob=c(0.32,0.78)),
                   sample(c("Diabetes","Non_Diabetes"),35,replace = TRUE,prob=c(0.2,0.8)),
                   sample(c("Diabetes","Non_Diabetes"),35,replace = TRUE,prob=c(0.35,0.65)),
                   sample(c("Diabetes","Non_Diabetes"),25,replace = TRUE,prob=c(0.2,0.8)))




####ANOVA########
summary(cohort$HDL)       

summary(cohort$Triglycerides)     

TukeyHSD(aov(ASCVD~hypertensives,data=cohort))

expand=c(rnorm(35,30,10),rnorm(35,40,10),rnorm(35,50,10),rnorm(25,20,4))


cohort$Triglycerides=cohort$Triglycerides+expand

cohort$HDL =ifelse(cohort$HDL<20,20,cohort$HDL)

cohort$HDL =ifelse(cohort$HDL>62,62,cohort$HDL)

cohort$Triglycerides =ifelse(cohort$Triglycerides<100,100,cohort$Triglycerides)







