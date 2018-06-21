options(scipen = 999)
#https://github.com/tidyverse/forcats/issues/16
#library(forcats)

x= c(rnorm(60,30,7))
out= c(rep("high",20),rep("med",20),rep("low",20))

library(pander)

out= factor(out) 
out=reorder(out,x,mean) # arranging control

aa = aov(x~out)

aov_residuals <- residuals(object = aa)

shapiro.test(aov_residuals)$p.value # checking for normality of residuals

bt = bartlett.test(x,out)   # 

pander(aa)
pander(TukeyHSD(aa))

oneway.test(x~out)

n=length(levels(out))
bt$p.value
sw$p.value



for(i in seq_along(length(levels(out)))){
  cat('The mean of ',levels(out)[i],'group was',round(mean(x[out==levels(out)[i]],2)),'.')
}



livi= function(i) {
 k= paste('The mean of ',levels(out)[i],'group was',round(mean(x[out==levels(out)[i]],2)),'. The SD of ',levels(out)[i],'group was',round(sd(x[out==levels(out)[i]],2)),'.')
 # Dont use cat here 
  k }


str(livi(1)) # always check output of function first


library(purrr)
kk= list
sapply(c(1:n),livi)

unlist(map(c(1:n),livi))

### same as map

output <- vector("list", n)
for (i in seq_along(levels(out))) {
  
  output[[i]] <- livi(i)
}

outputter = unlist(output)
writeLines(outputter) #https://stackoverflow.com/questions/24010547/print-cat-paste-in-r-separated-by-newline-character

###########
aovnormhomog = function(x,out){
  out= factor(out) 
  out=reorder(out,x,mean) # arranging control
  livi= function(i) {
    k= paste('The mean of ',levels(out)[i],'group was',round(mean(x[out==levels(out)[i]],2)),'. The SD of ',levels(out)[i],'group was',round(sd(x[out==levels(out)[i]],2)),'.')
    # Dont use cat here 
    k }
  writeLines(unlist(sapply(c(1:length(levels(out))),livi)))
  
  
  aa = aov(x~out)
  
  livi= function(i) {
    k= paste('The mean of ',levels(out)[i],'group was',round(mean(x[out==levels(out)[i]],2)),'. The SD of ',levels(out)[i],'group was',round(sd(x[out==levels(out)[i]],2)),'.')
    # Dont use cat here 
    k }
  
  

cat('Since assumptions of normality of residuals and homogeneity of variance were met, ')

k1=deparse(substitute(x))
k2=deparse(substitute(out))
aa1 = summary(aa)[[1]]$`Pr(>F)`[1]

pval = ifelse(aa1<0.0001,"<0.0001",round(summary(aa)[[1]]$`Pr(>F)`[1],4))


aa2 = ifelse(aa1<0.05,"significant","non-significant")

cat(paste('A one-way between subjects ANOVA was conducted to compare the effect of groups on ',k1,'\n'))

cat(paste('There was a  statistically ',aa2,'effect of groups ',k1,'at the p<.05 level for all',length(levels(out)),' groups .'))
0
cat(paste('[F(',summary(aa)[[1]]$Df[1],',',summary(aa)[[1]]$Df[2],')=',round(summary(aa)[[1]]$`F value`[1],2),',p =',pval,'.]'))
cat('\n')
if (summary(aa)[[1]]$`Pr(>F)`[1]<0.05){
cat('We conducted a priori specifiied Tukey test for carrying out multiple post-hoc comparisons between groups \n ')
cat(' P values were adjusted for multiple comparisons. Output is as follows \n')

suppressWarnings(pander(TukeyHSD(aa))) }

}


aovnormhomog(x,out)

aovnormhetrog(x,out)
aovnormhetrog = function(x,out){
  out= factor(out) 
  out=reorder(out,x,mean) # arranging control
  livi= function(i) {
    k= paste('The mean of ',levels(out)[i],'group was',round(mean(x[out==levels(out)[i]],2)),'. The SD of ',levels(out)[i],'group was',round(sd(x[out==levels(out)[i]],2)),'.')
    # Dont use cat here 
    k }
  writeLines(unlist(sapply(c(1:length(levels(out))),livi)))
  
  
  ow = oneway.test(x~out)
  
  livi= function(i) {
    k= paste('The mean of ',levels(out)[i],'group was',round(mean(x[out==levels(out)[i]],2)),'. The SD of ',levels(out)[i],'group was',round(sd(x[out==levels(out)[i]],2)),'.')
    # Dont use cat here 
    k }
  
  
  
  cat('Since assumptions of normality of residuals were met  but  homogeneity of variance was violated, ')
  
  k1=deparse(substitute(x))
  k2=deparse(substitute(out))
  aa1 = round(ow$p.value,4)
  
  pval = ifelse(ow$p.value<0.0001,"<0.0001",round(ow$p.value,4))
  
  
  aa2 = ifelse(ow$p.value<0.05,"significant","non-significant")
  
  cat(paste('A one-way between subjects one-way ANOVA with Welch Correction  was conducted to compare the effect of groups on ',k1,'\n'))
  
  cat(paste('There was a  statistically',aa2,'effect of groups ',k1,'at the p<.05 level for all',length(levels(out)),' groups .'))
  
  cat(paste('[F(',ow$parameter[[1]],',',ow$parameter[[2]],')=',round(ow$statistic,2),',p =',pval,'.]'))
  cat('\n')
  if (ow$p.value<0.05){
    cat('We conducted a priori specifiied Games-Howell test for carrying out multiple post-hoc comparisons between groups \n ')
    cat(' P values were adjusted for multiple comparisons. Output is as follows \n')
    
    suppressWarnings(posthoc.tgh(x=out,y=x)) }
  
  
}


ky=posthoc.tgh(y,out)
ow=oneway.test(x~out)
ow$parameter[[1]]
ow$parameter[[2]]
ow$p.value
ow$
deparse(substitute(out))

    
    y= c(rnorm(20,10,5),rnorm(20,70,5),rnorm(20,20,10))
    
    kw =kruskal.test(y~as.factor(out))
    
    library(FSA)
    dunnTest(y~as.factor(out))
    
    #########
    
    anovakw = function(x,out){
      
      out= factor(out) 
      out=reorder(out,x,median) # arranging control
      
      cat('Since the assumption of normality was not met, we performed non-parametric Kruskal Wallis test')
    cat('\n')
    
    pval = ifelse(kw$p.value<0.0001,"<0.0001",round(kw$p.value,4))
    
    
    aa2 = ifelse(kw$p.value<0.05,"significant","non-significant")
    
    livi= function(i) {
      k= paste('The median of ',levels(out)[i],'group was',round(median(x[out==levels(out)[i]],2)),'. The IQR of ',levels(out)[i],'group was',round(IQR(x[out==levels(out)[i]]),2),'.')
      # Dont use cat here 
      k }
    writeLines(unlist(sapply(c(1:length(levels(out))),livi)))
    k1=deparse(substitute(x))
    
    cat(paste('A Kruskal- Wallis test was conducted to compare the effect of groups on ',k1,'\n'))
    
    cat(paste('There was a  statistically',aa2,'effect of groups ',k1,'at the p<.05 level for all',length(levels(out)),' groups .'))
    
    cat(paste('[F(',kw$parameter[[1]],')=',round(kw$statistic,2),',p =',pval,'.]'))
    cat('\n')
    cat('\n')
    if (kw$p.value<0.05){
      cat('We conducted a priori specifiied Dunn test for carrying out multiple post-hoc comparisons between groups \n ')
      cat(' P values were adjusted for multiple comparisons. Output is as follows \n')
      library(FSA)
      suppressWarnings(dunnTest(x~out)) }
    
    
    }
    
    kw$parameter
    
    anovakw(y,out)
    
    dunnTest(x~out)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    anovafinal = function(x,out){
      
      aa = aov(x~out)
      
      aov_residuals <- residuals(object = aa)
      
     sss = ifelse(shapiro.test(aov_residuals)$p.value<0.05,"significant","non-significant")
     sss1 = ifelse(shapiro.test(aov_residuals)$p.value<0.0001,"<0.0001",shapiro.test(aov_residuals)$p.value)
      
      bt = bartlett.test(x,out)
      bbb = ifelse(bt$p.value<0.05,"significant","non-significant")
      bbb1 = ifelse(bt$p.value<0.0001,"<0.0001",bt$p.value)
      
      cat('The Shapiro test for normality was statistically ',sss,'with p value of ',sss1,'.')
      cat('The Barlett test for homogeneity of variance  was statistically  ',bbb,'with p value of ',bbb1,'.')
      cat('\n')
      
      
      if(shapiro.test(aov_residuals)$p.value<0.05){
        anovakw(x,out)
      } else if(bt$p.value <0.05){
        aovnormhetrog(x,out)
        }
      else {
        aovnormhomog(x,out)
      }
    }
    
    
    anovafinal(x,out)
    anovafinal(y,out)