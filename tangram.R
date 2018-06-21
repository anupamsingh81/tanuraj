
str_c(aa)



paste(aa,collapse="+") #+ format for tangram

cohort1=as.data.frame(cohort) # convert to normal tibble for work

library(tangram)

tangram("hypertensives~Age+Height+Weight+Abdominal_Circumfrence+SBP+DBP+Total_Cholesterol+HDL+LDL+VLDL+Triglycerides+Lipoprotein_A+ASCVD+BMI+MAP+Smoker+Diabetes",data=cohort1)


tangram("hypertensives~Age+Height+Weight+Abdominal_Circumfrence+SBP+DBP+Total_Cholesterol+HDL+LDL+VLDL+Triglycerides",data=cohort)


tangram("hypertensives~Age + Height",data=cohort1)

summary(cohort)

saveRDS(cohort1,"cohort1.RDS")