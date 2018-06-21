dat<-read.table("table.dat", row.names=1)
shinyServer(function(input, output) {

  rown<-reactive({
     if(input$sex=="Male") 
       if (input$race=="White") ("white_male") else ("afroamer_male")
     else
       if (input$race=="White") ("white_female") else ("afroamer_female")})
  const<-reactive({dat[rown(),]})
  smokc<-reactive({if("Current smoker," %in% input$checkGroup) const()$smoker else const()$nonsmoker})
  smokcov<-reactive({if("Current smoker," %in% input$checkGroup) 1 else 0})
  BPc<-reactive({if("Treated systolic blood pressure," %in% input$checkGroup) const()$ln_treated_BP else const()$ln_untreated_BP})
  BPcov<-reactive({if("Treated systolic blood pressure," %in% input$checkGroup) const()$ln_age_BP else const()$ln_age_ln_untreated_BP})
  diab<-reactive({if("Diabetes," %in% input$checkGroup) const()$diabetes else const()$nondiabetes})
  meancoef<-reactive({const()$meancoef  })
  calc<-reactive({
      log(input$numage)*const()$ln_age+log(input$numage)*log(input$numage)*const()$ln_age_squared+
      +log(input$numTC)*const()$ln_total_cholest+log(input$numage)*log(input$numTC)*const()$ln_age_totcholest+
      +log(input$hdl)*const()$ln_hdlC+log(input$numage)*log(input$hdl)*const()$ln_age_hdlC+smokc()+
      +smokcov()*log(input$numage)*const()$ln_age_smoker+log(input$numBP)*BPc()+log(input$numage)*log(input$numBP)*BPcov()+
      +diab() })
   ascvd<-reactive({round(
     100*(1-(const()$baseline^exp(calc()-meancoef()))),2)
        })
     output$ascvd<-ascvd
     output$totalchol<-renderText({ input$numTC })
     output$BP<-renderText({ input$numBP })
     output$HDL<-renderText({ input$hdl })
     output$checkedgroup<-renderText({input$checkGroup})

  })