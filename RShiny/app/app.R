library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(forecast)
library(demography)
library(fitdistrplus)
library(MASS)
library(survival)
library(npsurv)
library(lsei)


#Import Data


italy = read.demogdata(file="Mx_1x1.txt", popfile="Exposures_1x1.txt",type="mortality", label="Italy")
summary(italy)

chosen_cohort=1955

library(lifecontingencies)
require(StMoMo)
horizon=20
ages.fit = 0:100
fyears.fit = 1955:2018
myears.fit = 1959:2018

ita.StMoMoData<-StMoMoData(data=italy, series = "total",type="central") 
ita.StMoMoDataF<-StMoMoData(data=italy, series = "female",type="central")
ita.StMoMoDataM<-StMoMoData(data=italy, series = "male",type="central")

LC <- lc(link = "logit") 
LCfit <- fit(LC, data = central2initial(ita.StMoMoData), ages.fit = ages.fit, years.fit = fyears.fit)
LCfitF <- fit(LC, data = central2initial(ita.StMoMoDataF), ages.fit = ages.fit, years.fit = fyears.fit)
LCfitM <- fit(LC, data = central2initial(ita.StMoMoDataM), ages.fit = ages.fit, years.fit = myears.fit)

ita.StMoMoData.IniF<-central2initial(ita.StMoMoDataF) 
ita.StMoMoData.IniM<-central2initial(ita.StMoMoDataM)



LCforF<- forecast( LCfitF, h = horizon) 
LCfor <- forecast(LCfit, h = horizon) 
LCforM<- forecast( LCfitM, h = horizon)

historical1955F <- extractCohort(fitted(LCfitF, type = "rates"),
                                 cohort = chosen_cohort)
forecasted1955F<- extractCohort(LCforF$rates,
                                cohort = chosen_cohort)
rates1955F <- c(historical1955F,forecasted1955F)
qx1955F<-mx2qx(rates1955F)#(mx2qx convertion de taux de mortalite en probabilité de decces)

historical1959M <- extractCohort(fitted(LCfitM, type = "rates"),
                                 cohort = chosen_cohort)
forecasted1959M<- extractCohort(LCforM$rates,
                                cohort = chosen_cohort)
rates1959M <- c(historical1955M,forecasted1959M)
qx1959M<-mx2qx(rates1959M)


historical1955t <- extractCohort(fitted(LCfit, type = "rates"),
                                 cohort = chosen_cohort)
forecasted1955t<- extractCohort(LCfor$rates,
                                cohort = chosen_cohort)
rates1955t <- c(historical1955t,forecasted1955t)
qx1955t<-mx2qx(rates1955t)


################################

lifetableF<-probs2lifetable(probs=qx1955F,type = "qx",name =paste("LC","1955","lt",sep="_"))
lifetableM<-probs2lifetable(probs=qx1959M,type = "qx",name =paste("LC","1959","lt",sep="_"))
lifetablet<-probs2lifetable(probs=qx1955t,type = "qx",name =paste("LC","1955","lt",sep="_"))


acttablet<-new("actuarialtable",x=lifetablet@x,
               lx=lifetablet@lx,
               interest = 0.05)
# transformation de la lifetable en table actuarielle :
#Taux d'interet = 0.05 
acttableF<-new("actuarialtable",x=lifetableF@x,
               lx=lifetableF@lx,
               interest = 0.05)

# transformation de la lifetable en table actuarielle pour les hommes  :
acttableM<-new("actuarialtable",x=lifetableM@x,
               lx=lifetableM@lx,
               interest = 0.05)



button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI

ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Projet Actuariat Vie - Sujet 2",theme = shinythemes::shinytheme("lumen")
,
             tabPanel("Calcul Actuariel", fluid = TRUE, icon = icon("calculator"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel(""),
                          #shinythemes::themeSelector(),
                          
                          
                          # Set Time Range
                          fluidRow(column(5,
                                          textInput("age", label = "Age :", placeholder = "age"),
                          ),
                          ),
                          
                          
                          hr(),
                          sliderInput(inputId = "contrat",
                                      label = "Contrat",
                                      min = 1,
                                      max = 20,
                                      value = 3,
                                      width = "320px"),
                          actionButton("submit", label = "Confirmer"),
                          
                        ),
                        mainPanel(
                          
                          # hr(),
                          
                          hr(),
                          h5("Esperance de vie :"),
                          textOutput("esp"),
                          h5("Probabilite de deces :"),
                          textOutput("dec"),
                          h5("VAP :"),
                          textOutput("vap"),
                          h5("Prime Pure :"),
                          textOutput("pp"),
                          plotOutput("plot"),
                          br(),
                        )
                      )
             ), 
             tabPanel("Modele de Lee Carter", fluid = TRUE, icon = icon("leanpub"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel(""),
                          #shinythemes::themeSelector(),
                          
                          
                          # Set Time Range
                          fluidRow(column(5,
                                          h5("Homme :"),
                                          actionButton("homme", label = "Visualiser Plot"),
                                          actionButton("hommep", label = "Visualiser Projection"),
                                          h5("Femme:"),
                                          actionButton("femme", label = "Visualiser Plot"),
                                          actionButton("femmep", label = "Visualiser Projection"),
                                          h5("Total"),
                                          actionButton("total", label = "Visualiser Plot"),
                                          actionButton("totalp", label = "Visualiser Projection"),
                          ),
                          ),
                          
                          
                          hr(),
                          
                          
                        ),
                        mainPanel(
                          
                          # hr(),
                          
                          hr(),
                          plotOutput("homme"),
                          
                          
                          
                          br(),
                        )
                      )
             ),
             
             tabPanel("Mortalite", fluid = TRUE, icon = icon("users"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel(""),
                          #shinythemes::themeSelector(),
                          
                          
                          # Set Time Range
                          fluidRow(column(5,
                                          h5("Total | 1872-2018 :"),
                                          actionButton("totalb", label = "Visualiser "),
                                          
                                          h5("Femme | 1872-2018 :"),
                                          actionButton("femmeb", label = "Visualiser"),
                                          
                                          h5("Homme | 1872-2018 :"),
                                          actionButton("hommeb", label = "Visualiser"),
                                          
                          ),
                          ),
                          
                          
                          hr(),
                          
                          
                        ),
                        mainPanel(
                          
                          # hr(),
                          
                          hr(),
                          plotOutput("tot"),
                          
                          
                          
                          br(),
                        )
                      )
             ),
             
             navbarMenu("Plus", icon = icon("info-circle"),
                        tabPanel("A propos", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("A propos du Projet")),
                                          h5(p("Dans le contexte du vieillissement de la population, de l'amelioration de la longevite et de la ")),
                                          h5(p("faiblesse des taux de mortalite, la question du financement des regimes de retraite et de la  ")), 
                                          h5(p("planification financiere optimale nous mene projet academique actuarielle dont l'objectif   ")), 
                                          h5(p("principal est de visualiser et de projeter l'evolution de la mortalite en Italie qui est considere   ")), 
                                          h5(p("comme un pays ayant un niveau de vie parmi les plus eleves sur la base de l'indice de   ")), 
                                          h5(p("developpement humain.   ")), 
                                          #hr(),
                                          
                                   ),
                                   column(6,
                                          
                                          
                                          
                                          
                                          br()
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Sources:"),
                                 
                                 h5("Built with",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    "by",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                    ".")
                        )
             )
  )
)

# Define server
server <- function(input, output, session) {
  
  
  esperance <- eventReactive( input$submit, {
    exn(lifetablet,x=as.numeric(input$age))
  })
  
  deces <- eventReactive( input$submit, {
    qxt(lifetablet, x=as.numeric(input$age), t=as.numeric(input$contrat)) 
  })
  
  vap <- eventReactive( input$submit, {
    axn(acttablet,x=62,n=20)
    #qxt(lifetablet, x=as.numeric(input$age), t=as.numeric(input$contrat)) 
  })
  
  pp <- eventReactive( input$submit, {
    axn(acttablet, x=as.numeric(input$age), m=as.numeric(input$contrat)) / axn(acttablet, x=as.numeric(input$age), m=1,n=as.numeric(input$contrat))
    #qxt(lifetablet, x=as.numeric(input$age), t=as.numeric(input$contrat)) 
  })
  
  #Plot
  observe({ if(input$homme) output$homme <- renderPlot(plot(LCfitM,col="blue")) })
  observe({ if(input$femme) output$homme <- renderPlot(plot(LCfitF,col="red")) })
  observe({ if(input$total) output$homme <- renderPlot(plot(LCfit))})
  #Projection
  observe({ if(input$hommep) output$homme <- renderPlot(plot(LCforM,col="blue")) })
  observe({ if(input$femmep) output$homme <- renderPlot(plot(LCforF,col="red")) })
  observe({ if(input$totalp) output$homme <- renderPlot(plot(LCfor))})
  
  
  
  # text output
  output$esp <- renderText({
    esperance()
  })
  
  output$dec <- renderText({
    deces()
  })
  output$vap <- renderText({
    vap()
  })
  output$pp <- renderText({
    pp()
  })
  
  
  observe({ if(input$totalb) output$tot <- renderPlot(plot(italy, series = 'total'))})
  observe({ if(input$femmeb) output$tot <- renderPlot(plot(italy, series = 'female'))})
  observe({ if(input$hommeb) output$tot <- renderPlot(plot(italy, series = 'male'))})
  
  
  
  
  
}  
# Run the application 
shinyApp(ui = ui, server = server)

