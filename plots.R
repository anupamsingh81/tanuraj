

aa=cohort %>% select_if(is.numeric) %>% names()

aa
bb= "hypertensives"
cc=expand.grid(aa,bb)

####multiple colored ggplot
###https://colinfay.me/tidyeval-1/


###multiple 
gg_top <- function(col_group, col_plot){
  enquo_col_group <- enquo(col_group)
  enquo_col_plot <- enquo(col_plot)  
  cohort%>%
    
    ggplot(aes_string(quo_name(enquo_col_group),quo_name(enquo_col_plot),fill=quo_name(enquo_col_group))) + 
    geom_boxplot()+guides(fill=FALSE)
}

cc


gg_top(hypertensives,Age)


plotsa=map2(.x=cc$Var2,.y=cc$Var1,.f=gg_top)
pathsa <- stringr::str_c(cc$Var1,1:length(plots), ".png")
pwalk(list(pathsa, plotsa), ggsave, path = getwd())


###multiple scatterplots

#https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
  x <- unique(x)
  
  y <- unique(y)
  
  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  
  do.call(rbind, lapply(seq_along(x), g))
}

sp=expand.grid(aa,aa)
spa= sp[!duplicated(t(apply(sp, 1, sort))),]  #remove same combination of rows.

spa1= as_data_frame(expand.grid.unique(aa,aa))

spa1$name=paste(spa1$V1,spa1$V2,sep="_")


gg_topa <- function(col_group, col_plot){
  enquo_col_group <- enquo(col_group)
  enquo_col_plot <- enquo(col_plot)  
  t= cohort %>% select(!!enquo_col_plot,!!enquo_col_group)  %>% 
    cor %>%  as_data_frame() %>% pull(!!enquo_col_plot)
  ta=round(t[2],4)
 
  cohort%>%
    
    ggplot(aes_string(quo_name(enquo_col_group),quo_name(enquo_col_plot))) + 
    geom_point(aes(color="red"))+geom_smooth(method = lm, se = FALSE)+guides(color=FALSE)+
   annotate("text", x=Inf, y = Inf,label=paste("R = ", ta),vjust=1, hjust=1)
}

gg_topa(Age,LDL)

library(lsr)

###correlation
gg_tor <- function(col_group, col_plot){
  enquo_col_group <- enquo(col_group)
  enquo_col_plot <- enquo(col_plot) 
 t= cohort %>% select(!!enquo_col_plot,!!enquo_col_group)  %>% 
    cor %>%  as_data_frame() %>% pull(!!enquo_col_plot)
 t[2]
}

##
plotsr=map2(.x=spa1$V1,.y=spa1$V2,.f=gg_topa)
pathsr <- stringr::str_c(spa1$name,1:length(plotsr), ".png")
pwalk(list(pathsr, plotsr), ggsave, path = getwd())

