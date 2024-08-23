
################################################################################

#References:
#https://vincentarelbundock.github.io/Rdatasets/doc/mosaicData/HELPrct.html
#https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
#https://fontawesome.com/icons
################################################################################


#Releated Libraries
library(shiny)
library(mosaicData) 
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(dashboardthemes)
library(fontawesome)
#install.packages("ggpubr")
library(ggpubr) 

#loading data in mosaicData library
mydata <- HELPrct %>% select(age, hospitalizations, pcs, mcs, anysubstatus, sex, g1b, cesd)
mydata2 <- HELPrct %>% select(age, hospitalizations, pcs, mcs, cesd)
mydata3 <- HELPrct %>% select(anysubstatus,sex, g1b)  
mydata4 <- HELPrct %>% select(g1b)
mydata5 <- HELPrct %>% select(hospitalizations)

ui <-  dashboardPage(
  dashboardHeader(title = "STAT292 Final Project",
                  titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem("Description", tabName = "description", icon = icon("info-sign", lib = "glyphicon")),
                     menuItem("General Information", tabName = "info", icon=icon("zoom-in", lib="glyphicon")),
                     menuItem("Plots", tabName = "plot", icon = icon("align-left", lib = "glyphicon")),
                     menuItem("Analysis", tabName = "analysis", icon = icon("pencil", lib = "glyphicon")),
                     menuItem("References", tabName = "references", icon=icon("list"))
                   )
  ),
  dashboardBody(tags$head(
    tags$style(HTML("
      #sidebarItemExpanded > ul > :last-child {
        position: absolute;
        bottom: 0;
        width: 100%;
        background-color: blue;
      }

    "))),
    shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      tabItem(tabName = "description",
              #tabPanel for our description
              tabsetPanel(
                tabPanel("Description",icon=icon("caret-right", lib = "font-awesome"), 
                         p("The HELPrct dataset in the MosaicData library is a dataset containing variables from adult patients in the detoxification unit."),
                         p("This dataset consists of 453 observations and 30 columns."),
                         p("Of these 30 variables, 19 are integers, 9 are factors, and 2 are numeric. Name of variables age(in years), anysub, cesd, d1, hospitalizations, daysanysub, dayslink, drugrisk, e2b, female, sex, g1b, homeless, i1, i2, id, indtot, linkstatus, link, pcs, mcs, pss_fr, racegrp, satreat, sexrisk, substance, and treat."),
                         p("The aim of this study is to examine the relationships between age, hospitalizations, pcs , mcs, g1b, cesd, anysubstatus, and sex."),
                         p(icon("caret-right", lib = "font-awesome"),code("age"), "of the variables we will use is the age of the subjects in years"),
                         p(icon("caret-right", lib = "font-awesome"),code("hospitalizations"),"lifetime number of hospitalizations for medical problems (measured at baseline)"),
                         p(icon("caret-right", lib = "font-awesome"),code("anysubstatus"),"use of any substance post-detox: 0 for no, 1 for yes"),
                         p(icon("caret-right", lib = "font-awesome"),code("cesd"),"Center for Epidemiologic Studies Depression measure at baseline (high scores indicate more depressive symptoms)"),
                         p(icon("caret-right", lib = "font-awesome"),code("g1b"),"experienced serious thoughts of suicide in last 30 days (measured at baseline): a factor with levels no yes"),
                         p(icon("caret-right", lib = "font-awesome"),code("pcs"),"SF-36 Physical Component Score (measured at baseline, lower scores indicate worse status)"),
                         p(icon("caret-right", lib = "font-awesome"),code("mcs"),"SF-36 Mental Component Score (measured at baseline, lower scores indicate worse status)"),
                         p(icon("caret-right", lib = "font-awesome"),code("sex"),"a factor with levels male female."))
              )),
      tabItem(tabName = "info",
              #tabPanel for our data summary 
              tabsetPanel(
                tabPanel("Data Summary", icon=icon("tv", lib = "font-awesome"),
                         verbatimTextOutput("summary")),
                tabPanel("Data", icon=icon("table", lib = "font-awesome"),
                         DT::dataTableOutput("table"))
              )),
      tabItem(tabName = "plot",
              #tabPanel for our interactive scatter plots our variable 
              tabsetPanel(
                tabPanel("Scatter Plot",icon=icon("line-chart", lib = "font-awesome"),
                         h2(),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(
                               inputId = "xaxis", 
                               label = "Choose a Variable for the X-axis of the First Graph", 
                               choices = colnames(mydata2),
                               selected = "cesd"
                             ),
                             selectInput(
                               inputId = "yaxis", 
                               label = "Choose a Variable for the Y-axis of the First Graph", 
                               choices = colnames(mydata2),
                               selected = "mcs"
                             )
                           ),
                           mainPanel(
                             plotOutput(outputId = "scatterplot"))
                         ),
                ),
                #tabPanel for continous and discrete variable for interactive histogram
                tabPanel("Histogram and Bar Graph",icon=icon("chart-bar", lib = "font-awesome"), 
                         sidebarLayout(
                           sidebarPanel(
                             radioButtons(
                               inputId = "variable_cont",
                               label = "Choose the name of continous variable for the histogram",
                               choices = colnames(mydata2)
                             )
                           ),
                           mainPanel(
                             plotOutput(outputId = "hist")
                           ),
                         ),
                         sidebarLayout(
                           sidebarPanel(
                             radioButtons(
                               inputId = "variable_disc",
                               label = "Choose the name of discrete variable for the bar graph",
                               choices = colnames(mydata3)
                             )
                           ),
                           mainPanel(
                             plotOutput(outputId = "hist2")
                           ),
                         )
                )
              )),
      tabItem(tabName = "analysis",
              #tabPanel for our logistic and linear regressions for our variable 
              tabsetPanel(
                tabPanel("Logistic Regression",
                         sidebarLayout(sidebarPanel(
                           uiOutput("model_select"),
                           uiOutput("var1_select"),
                           uiOutput("rest_var_select")),
                           mainPanel( helpText("Model form"),
                                      verbatimTextOutput("other_val_show")))),
                tabPanel("Linear Regression",
                         sidebarLayout(sidebarPanel(
                           uiOutput("model_select_1"),
                           uiOutput("var1_select_1"),
                           uiOutput("rest_var_select_1")),
                           mainPanel( helpText("Model form"),
                                      verbatimTextOutput("other_val_show_1"))))
              )
      ), 
      tabItem(tabName = "references",
              #tabsetPanel for references
              tabsetPanel(
                tabPanel("References", icon=icon("list", lib = "font-awesome"),
                         mainPanel(h2(strong("REFERENCES")),
                                   p("https://vincentarelbundock.github.io/Rdatasets/doc/mosaicData/HELPrct.html"),
                                   p("https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp"),
                                   p("https://fontawesome.com/icons"))
                )
              )), 
      tabItem(tabName = "aboutus"
      )
    )
  )
)  
#We define server logic required to choose data
server <- function(input,output) { 
  data <- reactive({
    HELPrct %>% select(age, hospitalizations, pcs, mcs, anysubstatus, sex, g1b, cesd)
  })  
  
  data2 <- reactive({
    HELPrct %>% select(sex)
  })
  output$table <- renderDataTable({
    data()
  },options = list(pageLength = 7))
  output$tb1 <- renderUI({
    dataTableOutput("table")
  })
  #Logistec Regression for compare experienced serious thoughts of suicide in last 30 days and our independent variables using renderUI
  output$model_select<-renderUI({
    selectInput("modelselect","Choose Algorithm",choices = c("Logistic regression"="logreg"))
  })
  output$var1_select<-renderUI({
    selectInput("dep_var_select","Response Variable", choices = as.list(names(mydata4)) ,multiple = FALSE)
  })
  output$rest_var_select<-renderUI({
    checkboxGroupInput("other_var_select","Choose independent variable(s)",choices =as.list(names(mydata2)), selected = "age")
  })
  output$other_val_show<-renderPrint({
    input$other_var_select
    input$dep_var_select
    sel_data<-data()
    
    library(caret)
    form <- sprintf("%s~%s",input$dep_var_select,paste0(input$other_var_select,collapse="+"))
    print(form)
    
    logreg <-glm(as.formula(form),family=binomial(),data=sel_data)
    print(summary(logreg))
  })
  
  
  #Linear Regression forhospitalizations and our independent variables using renderUI
  output$model_select_1<-renderUI({
    selectInput("modelselect_1","Choose Algorithm",choices = c("Linear regression"="linreg"))
  })
  output$var1_select_1<-renderUI({
    selectInput("dep_var_select_1","Response Variable", choices = as.list(names(mydata5)) ,multiple = FALSE,)
  })
  output$rest_var_select_1<-renderUI({
    checkboxGroupInput("other_var_select_1","Choose independent variable(s)",choices =as.list(names(mydata2)), selected = "pcs")
  })
  output$other_val_show_1<-renderPrint({
    input$other_var_select_1
    input$dep_var_select_1
    sel_data_1<-data()
    
    library(caret)
    form_1 <- sprintf("%s~%s",input$dep_var_select_1,paste0(input$other_var_select_1,collapse="+"))
    print(form_1)
    
    linreg <-lm(as.formula(form_1),data=sel_data_1)
    print(summary(linreg))
  })
  #using renderPrint for summary
  output$summary <- renderPrint({
    summary(data())
  })
  
  plot_data <- reactive({
    HELPrct %>% select(age, pcs, mcs, hospitalizations)
  })
  
  output$plot_select <- renderUI({
    selectInput("graph_select", "Choose a graph to plot", choices = as.list(names(plot_data())))
  })
  #using renderPlot for histogram with continuous variable
  output$hist <- renderPlot({
    req(input$variable_cont)
    ggplot(mydata, aes_string(x=paste0("`", input$variable_cont,"`"))) + geom_histogram(fill="lightgreen", binwidth = 1)
    
  })
  #using renderPlot for histogram with discrete variable
  output$hist2 <- renderPlot({
    req(input$variable_disc)
    ggplot(mydata, aes_string(x=paste0("`", input$variable_disc,"`"))) + geom_bar(fill="lightgreen", binwidth = 1, stat = "count", bins = 50)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data()
  }))
  #using renderPlot for scatter plot 
  output$scatterplot <- renderPlot({
    req(input$xaxis)
    req(input$yaxis)
    ggplot(mydata, aes_string(x = paste0("`", input$xaxis, "`"), 
                              y = paste0("`", input$yaxis, "`"))) + geom_point(color ="red") + 
      scale_x_log10()+scale_y_log10()+ stat_cor(method="pearson", label.y.npc = "bottom")
  })
  
}


shinyApp(ui=ui,server=server) #We used shinyapp to see UI and server together

