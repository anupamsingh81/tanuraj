library(shiny)

shinyUI(fluidPage(
  titlePanel("The risk of getting Atherosclerotic cardiovascular disease over next 10 years"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Please enter data about yourself:"),
      numericInput("numage", label = h5("Your age"), value = 55, min=0, max=100),
      selectInput("sex", label = h5("Your sex"), 
                  choices = c("Female" , "Male" ), selected = "Male"),
      selectInput("race", label = h5("Your race"), 
                choices = c("White", "African American"), selected = "White"),
      numericInput("numTC", label = h5("Total cholesterol, mg/dL"), value = 213, min=100, max=300),
      numericInput("hdl", label = h5("HDL-cholesterol, mg/dL"), value = 50, min=20, max=100),
      numericInput("numBP", label = h5("Systolic Blood Pressure, mmHg"), value = 120, min=80, max=200),
      checkboxGroupInput("checkGroup", label = h5("Check if valid for you:"), 
                         choices =c("Treated systolic blood pressure,","Current smoker,", "Diabetes,"))),
    
    mainPanel("You entered:",
              br(),
      fluidRow(column(3, "Blood Pressure:", verbatimTextOutput("BP")),
            column(3, "Total cholesterol:", verbatimTextOutput("totalchol")),
             column(3,"HDL-C", verbatimTextOutput("HDL"))
            ),
            br(),
          "Conditions:", verbatimTextOutput("checkedgroup"),
          br(),
          #verbatimTextOutput("calcul"),
         h3("Your 10-year ASCVD risk in numerical value, %: "),
          verbatimTextOutput("ascvd"),
      br(),
        h3("Number provides an estimate of 10-year risk for a first hard atherosclerotic cardiovascular disease (ASCVD) among patiens without pre-existing cardiovascular disease. Try changing conditions, like stop smoking, and see how risks change. Risks >7.5% considered as elevated.")
         #plotOutput("main_plot")     
  )
)))

