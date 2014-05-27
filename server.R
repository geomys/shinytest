# server.R
if("MASS" %in% rownames(installed.packages()) == FALSE) {
install.packages("MASS")}
if("shinyIncubator" %in% rownames(installed.packages()) == FALSE) {
devtools::install_github("shiny-incubator", "rstudio")}
if("shiny" %in% rownames(installed.packages()) == FALSE) {
 install.packages('httpuv', repos=c(RStudio='http://rstudio.org/_packages', CRAN='http://cran.rstudio.com'))
 install.packages('devtools')  # if you don't already have devtools installed
 devtools::install_github('shiny', 'rstudio')
}
library(shinyIncubator)
library(shiny)
library(MASS)

N.year <- 100
rfdelta <- 0.33 #decreases in probability of renesting after the first nest attempt
rsdelta <- 0.1
rbfdelta <- 0.625



BS <- function(f,Chmort,E) 1-(1-f*Chmort)^(E/2) #brood survival.  f=baseline chick survival, Chmort = survival of threat

P11 <- function(ns,bs,rs,rf,rbf){ #probability of no second nest attempt
    ns*bs*(1-rs) + (1-ns)*(1-rf) + ns*(1-bs)*(1-rbf) 
    }

P12 <- function(ns,bs,rs,rf,rbf){ #probability of second nest attempt
    ns*bs*rs + (1-ns)*rf + ns*(1-bs)*rbf
}

P22 <- function(ns,bs,rs,rf,rbf) {  #probability of no third nest attempt
       ns*bs*(1-rs*rsdelta) + (1-ns)*(1-rf*rfdelta)+ns*(1-bs)*(1-rbf*rbfdelta) 
}

P23 <- function(ns,bs,rs,rf,rbf) {  #probability of third nest attempt
         ns*bs*rs*rsdelta + (1-ns)*rf*rfdelta + ns*(1-bs)*rbf*rbfdelta
}    

shinyServer(function(input,output,session){                                  
  
  options(shiny.usecairo=FALSE)
  output$Na0 <- renderUI({
     numericInput("Na0",label=h6("Starting abundance adults:"),100,min=0.00,max=1000,step=1)
  })
  output$Ns0 <- renderUI({
    numericInput("Ns0",label=h6("Starting abundance subadults:"),50,min=0.00,max=1000,step=1)
  })
  
  output$ya <- renderUI({
    numericInput("ya", label=h6("Nesting rate:"),0.85,min=0.00,max=1.00,step=0.01 )
  }) 
  output$Ea <- renderUI({
    numericInput("Ea", label=h6("Clutch size:"),2.85,min=0.00,max=3.00,step=0.01 )
  })
  output$nsa <- renderUI({
    numericInput("nsa", label=h6("Nest survival:"),0.6,min=0.00,max=1.00,step=0.01 )
  }) 
  output$K <- renderUI({
    numericInput("K", label=h6("Carrying capacity:"),250,min=0.00,max=1000,step=1 )
  }) 
  output$Rfa <- renderUI({ 
         numericInput("Rfa", label=h6("Renest after nest failure:"),0.92,min=0.00,max=1.00,step=0.01 )
  })
  output$Rbfa <- renderUI({ 
    numericInput("Rbfa", label=h6("Renest after brood failure:"),0.24,min=0.00,max=1.00,step=0.01 )
  })
  output$Rsa <- renderUI({ 
    numericInput("Rsa", label=h6("Renest after success:"),0.2,min=0.00,max=1.00,step=0.01 )
  })
  output$fa <- renderUI({ 
    numericInput("fa", label=h6("Chick survival:"),0.4,min=0.00,max=1.00,step=0.01 )
  })
  output$sfla<- renderUI({ 
         numericInput("sfla", label=h6("Baseline fledgling survival:"),0.9,min=0.00,max=1.00,step=0.01 )
  })
  output$sba<- renderUI({  
         numericInput("sba", label=h6("Base adult breeding surv.:"),1,min=0.00,max=1.00,step=0.01 )
  })
  output$WASY<- renderUI({  
         numericInput("WASY", label=h6("Adult winter survival:"), 0.85, min = 0.00, max = 1.00,step=0.01)   
  })
  output$WSAa<- renderUI({  
         numericInput("WSAa", label=h6("Juvenile winter survival:"), 0.59, min = 0.00, max = 1.00,step=0.01)   
  })
  output$Psi12Ja<- renderUI({   
    numericInput("Psi12Ja", label=h6("Juvenile movement rate to Site 2:"), 0, min = 0.00, max = 1.00,step=0.01)   
  })
  output$ChMort2a<- renderUI({  
         numericInput("ChMort2a", label=h6("Pre-fledging Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$fMort2a<- renderUI({   
         numericInput("fMort2a", label=h6("Fledgling Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$sMort2a<- renderUI({   
         numericInput("sMort2a", label=h6("Adult Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$ys <- renderUI({
    numericInput("ys", label=h6("Nesting rate:"),0.85,min=0.00,max=1.00,step=0.01 )
  }) 
  output$E <- renderUI({
    numericInput("E", label=h6("Clutch size:"),2.85,min=0.00,max=3.00,step=0.01 )
  })
  output$ns <- renderUI({
    numericInput("ns", label=h6("Nest survival:"),0.6,min=0.00,max=1.00,step=0.01 )
  }) 
  output$Rf <- renderUI({ 
    numericInput("Rf", label=h6("Renest after nest failure:"),0.92,min=0.00,max=1.00,step=0.01 )
  })
  output$Rbf <- renderUI({ 
    numericInput("Rbf", label=h6("Renest after brood failure:"),0.24,min=0.00,max=1.00,step=0.01 )
  })
  output$Rs <- renderUI({ 
    numericInput("Rs", label=h6("Renest after success:"),0.2,min=0.00,max=1.00,step=0.01 )
  })
  output$f <- renderUI({ 
    numericInput("f", label=h6("Chick survival:"),0.4,min=0.00,max=1.00,step=0.01 )
  })
  output$sfl<- renderUI({ 
    numericInput("sfl", label=h6("Baseline fledgling survival:"),0.9,min=0.00,max=1.00,step=0.01 )
  })
  output$sb<- renderUI({  
    numericInput("sb", label=h6("Base subadult breeding surv.:"),1,min=0.00,max=1.00,step=0.01 )
  })
  output$WSY<- renderUI({  
    numericInput("WSY", label=h6("Subdult winter survival:"), 0.85, min = 0.00, max = 1.00,step=0.01)   
  })
  output$WSA<- renderUI({  
    numericInput("WSA", label=h6("Juvenile winter survival:"), 0.59, min = 0.00, max = 1.00,step=0.01)   
  })
  output$Psi12A<- renderUI({   
    numericInput("Psi12A", label=h6("Subadult movement rate to Site 2:"), 0, min = 0.00, max = 1.00,step=0.01)   
  })
  output$Psi12J<- renderUI({   
    numericInput("Psi12J", label=h6("Juvenile movement rate to Site 2:"), 0, min = 0.00, max = 1.00,step=0.01)   
  })
  output$ChMort2<- renderUI({  
    numericInput("ChMort2", label=h6("Pre-fledging Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$fMort2<- renderUI({   
    numericInput("fMort2", label=h6("Fledgling Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$sMort2<- renderUI({   
    numericInput("sMort2", label=h6("Subdult Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  
  output$Na02 <- renderUI({
    numericInput("Na02",label=h6("Starting abundance adults:"),100,min=0.00,max=1000,step=1)
  })
  output$Ns02 <- renderUI({
    numericInput("Ns02",label=h6("Starting abundance subadults:"),50,min=0.00,max=1000,step=1)
  })
  
  output$ya2 <- renderUI({
    numericInput("ya2", label=h6("Nesting rate:"),0.85,min=0.00,max=1.00,step=0.01 )
  }) 
  output$Ea2 <- renderUI({
    numericInput("Ea2", label=h6("Clutch size:"),2.85,min=0.00,max=3.00,step=0.01 )
  })
  output$nsa2 <- renderUI({
    numericInput("nsa2", label=h6("Nest survival:"),0.6,min=0.00,max=1.00,step=0.01 )
  }) 
  output$K2 <- renderUI({
    numericInput("K2", label=h6("Carrying capacity:"),250,min=0.00,max=1000,step=1 )
  }) 
  output$Rfa2 <- renderUI({ 
    numericInput("Rfa2", label=h6("Renest after nest failure:"),0.92,min=0.00,max=1.00,step=0.01 )
  })
  output$Rbfa2 <- renderUI({ 
    numericInput("Rbfa2", label=h6("Renest after brood failure:"),0.24,min=0.00,max=1.00,step=0.01 )
  })
  output$Rsa2 <- renderUI({ 
    numericInput("Rsa2", label=h6("Renest after success:"),0.2,min=0.00,max=1.00,step=0.01 )
  })
  output$fa2 <- renderUI({ 
    numericInput("fa2", label=h6("Chick survival:"),0.4,min=0.00,max=1.00,step=0.01 )
  })
  output$sfla2<- renderUI({ 
    numericInput("sfla2", label=h6("Baseline fledgling survival:"),0.9,min=0.00,max=1.00,step=0.01 )
  })
  output$sba2<- renderUI({  
    numericInput("sba2", label=h6("Base adult breeding surv.:"),1,min=0.00,max=1.00,step=0.01 )
  })
  output$WASY2<- renderUI({  
    numericInput("WASY2", label=h6("Adult winter survival:"), 0.85, min = 0.00, max = 1.00,step=0.01)   
  })
  output$WSAa2<- renderUI({  
    numericInput("WSAa2", label=h6("Juvenile winter survival:"), 0.59, min = 0.00, max = 1.00,step=0.01)   
  })
  output$Psi21Ja<- renderUI({   
    numericInput("Psi21Ja", label=h6("Juvenile movement rate to Site 1:"), 0, min = 0.00, max = 1.00,step=0.01)   
  })
  output$ChMort2a2<- renderUI({  
    numericInput("ChMort2a2", label=h6("Pre-fledging Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$fMort2a2<- renderUI({   
    numericInput("fMort2a2", label=h6("Fledgling Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$sMort2a2<- renderUI({   
    numericInput("sMort2a2", label=h6("Adult Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$ys2 <- renderUI({
    numericInput("ys2", label=h6("Nesting rate:"),0.85,min=0.00,max=1.00,step=0.01 )
  }) 
  output$E2 <- renderUI({
    numericInput("E2", label=h6("Clutch size:"),2.85,min=0.00,max=3.00,step=0.01 )
  })
  output$ns2 <- renderUI({
    numericInput("ns2", label=h6("Nest survival:"),0.6,min=0.00,max=1.00,step=0.01 )
  }) 
  output$Rf2 <- renderUI({ 
    numericInput("Rf2", label=h6("Renest after nest failure:"),0.92,min=0.00,max=1.00,step=0.01 )
  })
  output$Rbf2 <- renderUI({ 
    numericInput("Rbf2", label=h6("Renest after brood failure:"),0.24,min=0.00,max=1.00,step=0.01 )
  })
  output$Rs2 <- renderUI({ 
    numericInput("Rs2", label=h6("Renest after success:"),0.2,min=0.00,max=1.00,step=0.01 )
  })
  output$f2 <- renderUI({ 
    numericInput("f2", label=h6("Chick survival:"),0.4,min=0.00,max=1.00,step=0.01 )
  })
  output$sfl2<- renderUI({ 
    numericInput("sfl2", label=h6("Baseline fledgling survival:"),0.9,min=0.00,max=1.00,step=0.01 )
  })
  output$sb2<- renderUI({  
    numericInput("sb2", label=h6("Base subadult breeding surv.:"),1,min=0.00,max=1.00,step=0.01 )
  })
  output$WSY2<- renderUI({  
    numericInput("WSY2", label=h6("Subadult winter survival:"), 0.85, min = 0.00, max = 1.00,step=0.01)   
  })
  output$WSA2<- renderUI({  
    numericInput("WSA2", label=h6("Juvenile winter survival:"), 0.59, min = 0.00, max = 1.00,step=0.01)   
  })
  output$Psi21A<- renderUI({   
    numericInput("Psi21A", label=h6("Subadult movement rate to Site 1:"), 0, min = 0.00, max = 1.00,step=0.01)   
  })
  output$Psi21J<- renderUI({   
    numericInput("Psi21J", label=h6("Juvenile movement rate to Site 1:"), 0, min = 0.00, max = 1.00,step=0.01)   
  })
  output$ChMort22<- renderUI({  
    numericInput("ChMort22", label=h6("Pre-fledging Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$fMort22<- renderUI({   
    numericInput("fMort22", label=h6("Fledgling Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  output$sMort22<- renderUI({   
    numericInput("sMort22", label=h6("Subdult Survival of Hazard:"), 1, min = 0.00, max = 1.00,step=0.01)   
  })
  
  
  observe({  #resets values to defaults on user click of reset button
  
  if (as.numeric(input$reset_button) > 0) {

        updateNumericInput(session,"Na0", ,100,min=0.00,max=1000,step=1 )
        updateNumericInput(session,"Ns0", ,50,min=0.00,max=1000,step=1 )
        updateNumericInput(session,"ya", ,0.85,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Ea", ,2.85,min=0.00,max=3.00,step=0.01 )
        updateNumericInput(session,"nsa", ,0.6,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"K", ,250,min=0,max=1000,step=1 )
        updateNumericInput(session,"Rfa", ,0.92,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Rbfa", ,0.24,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Rsa", ,0.2,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"fa", ,0.4,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"sfla", ,0.9,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"sba", ,1,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"WASY", ,0.85,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"WSAa", ,0.59,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Psi12Ja", ,0,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"ChMort2a", ,1,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"fMort2a", ,1,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"sMort2a", ,1,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"ys", ,0.85,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"E", ,2.85,min=0.00,max=3.00,step=0.01 )
        updateNumericInput(session,"ns", ,0.6,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Rf", ,0.92,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Rbf", ,0.24,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Rs", ,0.2,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"f", ,0.4,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"sfl", ,0.9,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"sb", ,1,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"WSY", ,0.85,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"WSA", ,0.59,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Psi12A", ,0,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"Psi12J", ,0,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"ChMort2", ,1,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"fMort2", ,1,min=0.00,max=1.00,step=0.01 )
        updateNumericInput(session,"sMort2", ,1,min=0.00,max=1.00,step=0.01 )
  }
  if (as.numeric(input$reset_button2) > 0) {
    updateNumericInput(session,"Na02", ,100,min=0.00,max=1000,step=1 )
    updateNumericInput(session,"Ns02", ,50,min=0.00,max=1000,step=1 )
    updateNumericInput(session,"ya2", ,0.85,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Ea2", ,2.85,min=0.00,max=3.00,step=0.01 )
    updateNumericInput(session,"nsa2", ,0.6,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"K2", ,250,min=0,max=1000,step=1 )
    updateNumericInput(session,"Rfa2", ,0.92,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Rbfa2", ,0.24,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Rsa2", ,0.2,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"fa2", ,0.4,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sfla2", ,0.9,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sba2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"WASY2", ,0.85,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"WSAa2", ,0.59,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi21Ja", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"ChMort2a2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"fMort2a2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sMort2a2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"ys2", ,0.85,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"E2", ,2.85,min=0.00,max=3.00,step=0.01 )
    updateNumericInput(session,"ns2", ,0.6,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Rf2", ,0.92,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Rbf2", ,0.24,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Rs2", ,0.2,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"f2", ,0.4,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sfl2", ,0.9,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sb2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"WSY2", ,0.85,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"WSA2", ,0.59,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi21A", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi21J", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"ChMort22", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"fMort22", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sMort22", ,1,min=0.00,max=1.00,step=0.01 )
  }
  
  if (as.numeric(input$States) == 1) {
    updateNumericInput(session,"Psi21Ja", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi21J", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi21A", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi12Ja", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi12J", ,0,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"Psi12A", ,0,min=0.00,max=1.00,step=0.01 )
  }
  
  if (as.numeric(input$Mort) == 1) {
    updateNumericInput(session,"sMort2a", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"ChMort2a", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"fMort2a", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sMort2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"fMort2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"ChMort2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sMort2a2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"ChMort2a2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"fMort2a2", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"sMort22", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"fMort22", ,1,min=0.00,max=1.00,step=0.01 )
    updateNumericInput(session,"ChMort22", ,1,min=0.00,max=1.00,step=0.01 )
  }
  })
  
  
  

#STRATUM 1  
#adults stratum 1

ba1  <- reactive({BS(input$fa,input$ChMort2a,input$Ea)})
p01a <- reactive({input$ya})
p11a <- reactive({P11(input$nsa,ba1(),input$Rsa,input$Rfa,input$Rbfa)})
p12a <- reactive({P12(input$nsa,ba1(),input$Rsa,input$Rfa,input$Rbfa)})
p22a <- reactive({P22(input$nsa,ba1(),input$Rsa,input$Rfa,input$Rbfa)})
p23a <- reactive({P23(input$nsa,ba1(),input$Rsa,input$Rfa,input$Rbfa)})


p0a  <- reactive({1-p01a()}) #probability of not nesting
p1a = reactive({p01a()*p11a()}) #probability of a single nest attempt, adults
p2a = reactive({p01a()*p12a()*p22a()}) #probability of 2 nest attempts, adults
p3a = reactive({p01a()*p12a()*p23a()}) #probability of 3 nest attempts, adults
pveca <- reactive({c(p0a(),p1a(),p2a(),p3a())})
Aa <- reactive ({t(c(0,1,2,3))%*%(pveca())}) #mean number of nest attempts, adults

Fa <- reactive({(input$Ea/2)*Aa()*input$fa*input$ChMort2a*input$fMort2a})  #male young per Adult male
Phiba = reactive({input$sba*input$sMort2a}) #adult survival of breeding season including threat
Phia = reactive({Phiba()*input$WASY}) #adult annual survival, breeding * winter


#subadults stratum 1
bs1  <- reactive({BS(input$f,input$ChMort2,input$E)})
p01s <- reactive({input$ys})
p11s <- reactive({P11(input$ns,bs1(),input$Rs,input$Rf,input$Rbf)})
p12s <- reactive({P12(input$ns,bs1(),input$Rs,input$Rf,input$Rbf)})
p22s <- reactive({P22(input$ns,bs1(),input$Rs,input$Rf,input$Rbf)})
p23s <- reactive({P23(input$ns,bs1(),input$Rs,input$Rf,input$Rbf)})


p0s  <- reactive({1-p01s()}) #probability of not nesting
p1s <- reactive({p01s()*p11s()}) #probability of a single nest attempt, adults
p2s <- reactive({p01s()*p12s()*p22a()}) #probability of 2 nest attempts, adults
p3s <- reactive({p01s()*p12s()*p23s()}) #probability of 3 nest attempts, adults
pvecs <- reactive({c(p0s(),p1s(),p2s(),p3s())})
As <- reactive ({t(c(0,1,2,3))%*%(pvecs())}) #mean number of nest attempts, adults

Fs <- reactive({(input$E/2)*Aa()*input$f*input$ChMort2*input$fMort2})  #male young per Adult male
Phibs <- reactive({input$sb*input$sMort2}) #adult survival of breeding season including threat
Phis <- reactive({Phibs()*input$WSY}) #adult annual survival, breeding * winter



#Leslie matrix


Lesmat1  <- reactive({matrix(c(Fs()*input$WSA,Phis(),
                               Fa()*input$WSAa,Phia()),2,2,byrow=FALSE)})

lambda1 <- reactive({eigen(Lesmat1())$values[1]})



#STRATUM 2  
#adults stratum 2

ba2  <- reactive({BS(input$fa2,input$ChMort2a2,input$Ea2)})
p01a2 <- reactive({input$ya2})
p11a2 <- reactive({P11(input$nsa2,ba2(),input$Rsa2,input$Rfa2,input$Rbfa2)})
p12a2 <- reactive({P12(input$nsa2,ba2(),input$Rsa2,input$Rfa2,input$Rbfa2)})
p22a2 <- reactive({P22(input$nsa2,ba2(),input$Rsa2,input$Rfa2,input$Rbfa2)})
p23a2 <- reactive({P23(input$nsa2,ba2(),input$Rsa2,input$Rfa2,input$Rbfa2)})


p0a2  <- reactive({1-p01a()}) #probability of not nesting
p1a2 = reactive({p01a()*p11a()}) #probability of a single nest attempt, adults
p2a2 = reactive({p01a()*p12a()*p22a()}) #probability of 2 nest attempts, adults
p3a2 = reactive({p01a()*p12a()*p23a()}) #probability of 3 nest attempts, adults
pveca2 <- reactive({c(p0a2(),p1a2(),p2a2(),p3a2())})
Aa2 <- reactive ({t(c(0,1,2,3)) %*% pveca2()})
  
Fa2 <- reactive({(input$Ea2/2)*Aa2()*input$fa2*input$ChMort2a2*input$fMort2a2})  #male young per Adult male
Phiba2 = reactive({input$sba2*input$sMort2a2}) #adult survival of breeding season including threat
Phia2 = reactive({Phiba2()*input$WASY2}) #adult annual survival, breeding * winter


#subadults stratum 2
bs2  <- reactive({BS(input$f2,input$ChMort22,input$E2)})
p01s2 <- reactive({input$ys})
p11s2 <- reactive({P11(input$ns2,bs2(),input$Rs2,input$Rf2,input$Rbf2)})
p12s2 <- reactive({P12(input$ns2,bs2(),input$Rs2,input$Rf2,input$Rbf2)})
p22s2 <- reactive({P22(input$ns2,bs2(),input$Rs2,input$Rf2,input$Rbf2)})
p23s2 <- reactive({P23(input$ns2,bs2(),input$Rs2,input$Rf2,input$Rbf2)})


p0s2  <- reactive({1-p01s2()}) #probability of not nesting
p1s2 <- reactive({p01s2()*p11s2()}) #probability of a single nest attempt, adults
p2s2 <- reactive({p01s2()*p12s2()*p22a2()}) #probability of 2 nest attempts, adults
p3s2 <- reactive({p01s2()*p12s2()*p23s2()}) #probability of 3 nest attempts, adults
pvecs2 <- reactive({c(p0s2(),p1s2(),p2s2(),p3s2())})
As2 <- reactive ({t(c(0,1,2,3))%*%(pvecs2())}) #mean number of nest attempts, adults

Fs2 <- reactive({(input$E2/2)*As2()*input$f2*input$ChMort22*input$fMort22})  #male young per Adult male
Phibs2 <- reactive({input$sb2*input$sMort22}) #adult survival of breeding season including threat
Phis2 <- reactive({Phibs2()*input$WSY2}) #adult annual survival, breeding * winter


#Leslie matrix


Lesmat2  <- reactive({matrix(c(Fs2()*input$WSA2,Phis2(),
                               Fa2()*input$WSAa2,
                               Phia2()),2,2,byrow=FALSE)})

lambda2 <- reactive({eigen(Lesmat2())$values[1]})

#Lesmattotal <- reactive({matrix(c(1*1*1,1*1,1,0,
#         1,1*1,0,1,
#         1,0,1,1,
#         0,1,1,1),4,4,byrow=FALSE)})


Lesmattotal  <- reactive({matrix(c(
  Fs()*input$WSA*(1-input$Psi12J),Fa()*input$WSAa*(1-input$Psi12Ja),input$Psi21A,0,
  Phis()*(1-input$Psi12A),Phia()*(1-input$Psi12A),0,input$Psi21A,
  input$Psi12A,0,Fs2()*input$WSA2*(1-input$Psi21J),Fa2()*input$WSAa2*(1-input$Psi12Ja),
  0, input$Psi12A,Phis2()*(1-input$Psi21A), Phia2()*(1-input$Psi12A)),

  4,4,byrow=TRUE)})

# Lesmattotal  <- reactive({matrix(c(
#   Fs()*input$WSA*(1-input$Psi12J),Fa()*input$WSAa*(1-input$Psi12Ja),input$Psi21A,0,
#   Phis()*(1-input$Psi12A),Phia()*(1-input$Psi12A),0,input$Psi21A,
#   input$Psi12A,0,Fs2()*1*(1-input$Psi21J),Fa()*1*(1-input$Psi12Ja),
#   0, input$Psi12A,1*(1-input$Psi21A), 1*(1-input$Psi12A)),
#   
#   4,4,byrow=TRUE)})


# Lesmattotal  <- reactive({matrix(c(
#   Fs()*input$WSA*(1-input$Psi12J),
#   Phis()*(1-input$Psi12A),
#   input$Psi12A,
#   0, 
#   Fa()*input$WSAa*(1-input$Psi12Ja),
#   Phia()*(1-input$Psi12A),
#   0, 
#   input$Psi12A,
#   input$Psi21A,
#   0,
#   Fs2()*input$WSA2*(1-input$Psi21J),
#   Phis()*(1-input$Psi21A),
#   0,
#   input$Psi21A,
#   Fa2()*input$WSAa2*(1-input$Psi12Ja),
#   Phia2()*(1-input$Psi12A)),
#   4,4,byrow=FALSE)})

lambdatot <- reactive({eigen(Lesmattotal())$values[1]})




# Generate a p-dimensional Poisson (Yahav and Shmueli 2008)
# p = the dimension of the distribution
# samples = the number of observations
# R = correlation matrix p X p
# lambda = rate vector p X 1

GMP<-function(p, samples, R, lambda){
            normal_mu=rep(0, p)
            normal = mvrnorm(samples, normal_mu, R)
            pois = normal
            j=pnorm(normal)
            if (samples>1) {
            for (s in 1:p){pois[,s]=qpois(j[,s], lambda[s])}
            }
            if (samples==1) {
              for (s in 1:p){pois[s]=qpois(j[s], lambda[s])}
            }
            return(pois)}



#withProgress(session, min=0, max=1, {
# setProgress(message = "Loading user data...")
# Let's say we normally expect the user data loading to take 20% of the total time
#setProgress(value = 0.2, message = "Calculating...") 
#jobLength = 10
#for (i in 1:jobLength) {
# Do work
#  setProgress(value = i)
#}
# })

trajectory <- reactive({
  withProgress(session, min=0, max=1, {
   setProgress(message = "Calculating...")
  setProgress(value = 0.2, message = "Calculating, do not change inputs...") 
   
 
 Extinct = 0
 Nmatrix <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100)
 Nbmatrix <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100)
 Nmatrix2 <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100)
 Nbmatrix2 <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100)
 Nmatrixtotal <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100)
 Nbmatrixtotal <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100)
 K <- input$K
 K2 <- input$K2
 CV <- 0.1
 corrsurv <- 0.9 #correlation among survival rates
 corrF <- 0.9

 for (iter in 1:as.numeric(input$maxiter)){  
   if (input$States==1) { 
  
  popmat1 <- matrix(0,2,N.year)

  popmat1[1,1] <- input$Ns0
  popmat1[2,1] <- input$Na0
  Ntotal1 <- c(popmat1[1,1]+popmat1[2,1],rep(0,N.year-1))
  Ntotaltot <- c(popmat1[1,1]+popmat1[2,1],rep(0,N.year-1))
  Nbreed1 <- c(min(popmat1[1,1]+popmat1[2,1],K),rep(0,N.year-1))
  Ntotalbtot <- c(min(popmat1[1,1]+popmat1[2,1],K),rep(0,N.year-1))
  for (i in 2:N.year) {

        rcat <- rbinom(1,1,0.05) #probability of no reproductive catastrophe
        sdWSA <- CV*input$WSA
        sdWSAa <- CV*input$WSAa
        sdPhis <- CV*Phis()
        sdPhia <- CV*Phia()
        sdWSAlogit <- sdWSA/(input$WSA*(1-input$WSA))
        sdWSAalogit <- sdWSAa/(input$WSAa*(1-input$WSAa))
        sdPhislogit <- sdPhis/(Phis()*(1-Phis()))
        sdPhialogit <- sdPhis/(Phia()*(1-Phia()))
vcovWSurv <- #variance-covariance matrix for survival rates
  matrix(c(sdWSAlogit^2,sdWSAlogit*sdWSAalogit*corrsurv,
           sdWSAlogit*sdWSAalogit*corrsurv,sdWSAalogit^2),
            2,2,byrow=TRUE)
vcovBSurv <- #variance-covariance matrix for survival rates
  matrix(c(sdPhislogit^2,sdPhislogit*sdPhialogit*corrsurv,
           sdPhislogit*sdPhialogit*corrsurv,sdPhialogit^2),
         2,2,byrow=TRUE)
Fmean <- GMP(2,1,matrix(c(1,corrF,corrF,1),2,2,byrow=TRUE),c(Fa(),Fs())) 

    Wsurvmean <- plogis(mvrnorm(1,c(qlogis(input$WSA),qlogis(input$WSAa)),vcovWSurv))
    Bsurvmean <- plogis(mvrnorm(1,c(qlogis(Phis()),qlogis(Phia())),vcovBSurv))

      WSAt <- Wsurvmean[1]#plogis(rnorm(1,qlogis(input$WSA),qlogis(input$WSA)*CV))
       WSAat <- Wsurvmean[2]#plogis(rnorm(1,qlogis(input$WSAa),qlogis(input$WSAa)*CV))
       Phist <- Bsurvmean[1]#plogis(rnorm(1,qlogis(Phis()),qlogis(Phis())*CV))
       Phiat <- Bsurvmean[2]#plogis(rnorm(1,qlogis(Phia()),qlogis(Phia())*CV))
       #LesmatDD  <- matrix(c(Fat*WSAat-1,Phist,Fst*WSAt,Phiat),2,2,byrow=FALSE)
       #print(isolate(c(popmat1[2,i-1],popmat1[1,i-1],Ntotal[i-1],K,Fat,Fst)))
       Fat <- Fmean[1]
       Fst <- Fmean[2]
       if (Ntotal1[i-1] > K) {
         Fat <- (popmat1[2,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fat) #all adults over K have fecundity of 0
         Fst <- (popmat1[1,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fst) #all subadults over K have fecundity of 0
       }
       Lesmat1  <- matrix(c(Fst*WSAt*(1-input$Psi12J),Phist*(1-input$Psi12A),
                            Fat*WSAat*(1-input$Psi12Ja),Phiat*(1-input$Psi12A)),2,2,byrow=FALSE)

      #DDmat <- matrix(c((1-(Ntotal[i-1]/K)),0,0,1),2,2,byrow=TRUE)
      
      #popmat1[1:2,i] <- c(popmat1[1,i-1],0)+DDmat%*%LesmatDD%*%popmat1[1:2,i-1]
       popmat1[1:2,i] <- Lesmat1%*%popmat1[1:2,i-1]
      Ntotal1[i] <- popmat1[1,i]+popmat1[2,i]
      Ntotal1[i][Ntotal1[i-1]<1]=0
      Nbreed1[i] <- min(Ntotal1[i],K)
      Ntotaltot[i] <- Ntotal1[i]
      Ntotalbtot[i] <- Nbreed1[i]
    }
  }
##################TWO INPUT STATES#####################
if (input$States==2) {
  popmat1 <- matrix(0,2,N.year)
  popmat2 <- matrix(0,2,N.year)
  popmattotal <- matrix(0,4,N.year)
  popmat1[1,1] <- input$Ns0
  popmat1[2,1] <- input$Na0
  popmat2[1,1] <- input$Ns02
  popmat2[2,1] <- input$Na02
  popmattotal[1,1] <- input$Ns0
  popmattotal[2,1] <- input$Na0
  popmattotal[3,1] <- input$Ns02
  popmattotal[4,1] <- input$Na02
  
  Ntotal1 <- c(popmat1[1,1]+popmat1[2,1],rep(0,N.year-1))
  Nbreed1 <- c(min(popmat1[1,1]+popmat1[2,1],K),rep(0,N.year-1))
  Ntotal2 <- c(popmat2[1,1]+popmat2[2,1],rep(0,N.year-1))
  Nbreed2 <- c(min(popmat2[1,1]+popmat2[2,1],K2),rep(0,N.year-1))
  Ntotaltot <- c(popmattotal[1,1]+popmattotal[2,1]+popmattotal[3,1]+popmattotal[4,1],rep(0,N.year-1))
  Ntotalbtot <- c(min(popmattotal[1,1]+popmattotal[2,1]+popmattotal[3,1]+popmattotal[4,1],(K+K2)),rep(0,N.year-1))
  for (i in 2:N.year) {
    
    rcat <- rbinom(1,1,0.05) #probability of no reproductive catastrophe
    
    corrsurv <- 0.9 #correlation among survival rates
    corrF <- 0.9
    sdWSA <- CV*input$WSA
    sdWSAa <- CV*input$WSAa
    sdPhis <- CV*Phis()
    sdPhia <- CV*Phia()
    sdWSAlogit <- sdWSA/(input$WSA*(1-input$WSA))
    sdWSAalogit <- sdWSAa/(input$WSAa*(1-input$WSAa))
    sdPhislogit <- sdPhis/(Phis()*(1-Phis()))
    sdPhialogit <- sdPhis/(Phia()*(1-Phia()))
    sdWSA2 <- CV*input$WSA2
    sdWSAa2 <- CV*input$WSA2
    sdPhis2 <- CV*Phis2()
    sdPhia2 <- CV*Phia2()
    sdWSAlogit2 <- sdWSA2/(input$WSA2*(1-input$WSA2))
    sdWSAalogit2 <- sdWSAa2/(input$WSAa2*(1-input$WSAa2))
    sdPhislogit2 <- sdPhis2/(Phis2()*(1-Phis2()))
    sdPhialogit2 <- sdPhis2/(Phia2()*(1-Phia2()))
    
    
    vcovBSurv <- #variance-covariance matrix for survival rates
      matrix(c(sdPhislogit^2,sdPhislogit*sdPhialogit*corrsurv,
               sdPhislogit*sdPhialogit*corrsurv,sdPhialogit^2),
             2,2,byrow=TRUE)
    vcovBSurv2 <- #variance-covariance matrix for survival rates
      matrix(c(sdPhislogit2^2,sdPhislogit2*sdPhialogit2*corrsurv,
               sdPhislogit2*sdPhialogit2*corrsurv,sdPhialogit2^2),
             2,2,byrow=TRUE)
    
    vcovWSurv <- #variance-covariance matrix for survival rates
      matrix(c(sdWSAlogit^2,sdWSAlogit*sdWSAalogit*corrsurv,sdWSAlogit*sdWSAlogit2*corrsurv,sdWSAlogit*sdWSAalogit2*corrsurv,
               sdWSAlogit*sdWSAalogit*corrsurv,sdWSAalogit^2,sdWSAalogit*sdWSAlogit2*corrsurv,sdWSAalogit*sdWSAalogit2*corrsurv,
               sdWSAlogit*sdWSAlogit2*corrsurv,sdWSAalogit*sdWSAalogit2*corrsurv,sdWSAlogit2^2,sdWSAlogit2*sdWSAalogit2*corrsurv,
               sdWSAlogit*sdWSAalogit2*corrsurv,sdWSAalogit*sdWSAalogit2*corrsurv,sdWSAlogit2*sdWSAalogit2*corrsurv,sdWSAalogit2^2),
             4,4,byrow=TRUE)
    Fmean <- GMP(2,1,matrix(c(1,corrF,corrF,1),2,2,byrow=TRUE),c(Fa(),Fs())) 
    Fmean2 <- GMP(2,1,matrix(c(1,corrF,corrF,1),2,2,byrow=TRUE),c(Fa2(),Fs2())) 
    
    Wsurvmean <- plogis(mvrnorm(1,c(qlogis(input$WSA),qlogis(input$WSAa),qlogis(input$WSA2),qlogis(input$WSAa2)),vcovWSurv))
    Bsurvmean1 <- plogis(mvrnorm(1,c(qlogis(Phis()),qlogis(Phia())),vcovBSurv))
    Bsurvmean2 <- plogis(mvrnorm(1,c(qlogis(Phis2()),qlogis(Phia2())),vcovBSurv2))
    

    
    WSAt <- Wsurvmean[1]#plogis(rnorm(1,qlogis(input$WSA),qlogis(input$WSA)*CV))
    WSAat <- Wsurvmean[2]#plogis(rnorm(1,qlogis(input$WSAa),qlogis(input$WSAa)*CV))
    WSAt2 <- Wsurvmean[3]#plogis(rnorm(1,qlogis(input$WSA),qlogis(input$WSA)*CV))
    WSAat2 <- Wsurvmean[4]#plogis(rnorm(1,qlogis(input$WSAa),qlogis(input$WSAa)*CV))   
    
    Phist <- Bsurvmean1[1]#plogis(rnorm(1,qlogis(Phis()),qlogis(Phis())*CV))
    Phiat <- Bsurvmean1[2]#plogis(rnorm(1,qlogis(Phia()),qlogis(Phia())*CV))
    Phist2 <- Bsurvmean2[1]#plogis(rnorm(1,qlogis(Phis()),qlogis(Phis())*CV))
    Phiat2 <- Bsurvmean2[2]#plogis(rnorm(1,qlogis(Phia()),qlogis(Phia())*CV))

    Fat <- Fmean[1]
    Fst <- Fmean[2]
    Fat2 <- Fmean2[1]
    Fst2 <- Fmean2[2]
    if (Ntotal1[i-1] > K) {
      Fat <- (popmat1[2,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fat) #all adults over K have fecundity of 0
      Fst <- (popmat1[1,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fst) #all subadults over K have fecundity of 0
    }
    if (Ntotal2[i-1] > K2) {
      Fat2 <- (popmat2[2,i-1]/Ntotal2[i-1])*((Ntotal2[i-1]-K2)/Ntotal2[i-1]*0 + K2/Ntotal2[i-1]*Fat2) #all adults over K have fecundity of 0
      Fst2 <- (popmat2[1,i-1]/Ntotal2[i-1])*((Ntotal2[i-1]-K2)/Ntotal2[i-1]*0 + K2/Ntotal2[i-1]*Fst2) #all subadults over K have fecundity of 0
    }
    Lesmattotal  <- matrix(c(Fst*WSAt*(1-input$Psi12J),Phist*(1-input$Psi12A),input$Psi12A,0,
                         Fat*WSAat*(1-input$Psi12Ja),Phiat*(1-input$Psi12A),0,input$Psi12A,
                         input$Psi21A,0,Fst2*WSAt2*(1-input$Psi21J),Phist2*(1-input$Psi21A),
                         0,input$Psi21A,Fat2*WSAat2*(1-input$Psi12Ja),Phiat2*(1-input$Psi12A)),4,4,byrow=FALSE)
    
    #DDmat <- matrix(c((1-(Ntotal[i-1]/K)),0,0,1),2,2,byrow=TRUE)
    
    #popmat1[1:2,i] <- c(popmat1[1,i-1],0)+DDmat%*%LesmatDD%*%popmat1[1:2,i-1]
    popmattotal[1:4,i] <- Lesmattotal%*%popmattotal[1:4,i-1]
    Ntotal1[i] <- popmattotal[1,i]+popmattotal[2,i]
    Ntotal2[i] <- popmattotal[3,i]+popmattotal[4,i]
    Ntotaltot[i] <- popmattotal[1,i]+popmattotal[2,i]+popmattotal[3,i]+popmattotal[4,i]
    Ntotaltot[i][Ntotaltot[i-1]<1]=0
    Nbreed1[i] <- min(Ntotal1[i],K)
    Nbreed2[i] <- min(Ntotal2[i],K)
    Ntotalbtot[i] <- min(Ntotaltot[i],K+K2)
 
    
  }
}


  Extinct[Ntotaltot[100]==0]=Extinct+1
  Nmatrix[iter,] <- Ntotal1[]
  Nbmatrix[iter,] <- Nbreed1[]

 if (input$States==2){
  Nmatrix2[iter,] <- Ntotal2[]
  Nbmatrix2[iter,] <- Nbreed2[]}

  Nmatrixtotal[iter,] <- Ntotaltot[]
  Nbmatrixtotal[iter,] <- Ntotalbtot[]
 }
    Nave <- colMeans(Nmatrixtotal)
    Nbave <- colMeans(Nbmatrixtotal)
    Nsd <- apply(Nmatrixtotal, 2, sd)
    Nbsd <- apply(Nbmatrixtotal, 2, sd)
    Nlow <- Nave - 1.96*Nsd
    Nhi <- Nave + 1.96*Nsd
    Nblow <- Nbave - 1.96*Nbsd
    Nbhi <- Nbave + 1.96*Nbsd

Nave1 <- colMeans(Nmatrix)
Nbave1 <- colMeans(Nbmatrix)
Nsd1 <- apply(Nmatrix, 2, sd)
Nbsd1 <- apply(Nbmatrix, 2, sd)
Nlow1 <- Nave1 - 1.96*Nsd1
Nhi1 <- Nave1 + 1.96*Nsd1
Nblow1 <- Nbave1 - 1.96*Nbsd1
Nbhi1 <- Nbave1 + 1.96*Nbsd1

Nave2 <- colMeans(Nmatrix2)
Nbave2 <- colMeans(Nbmatrix2)
Nsd2 <- apply(Nmatrix2, 2, sd)
Nbsd2 <- apply(Nbmatrix2, 2, sd)
Nlow2 <- Nave2 - 1.96*Nsd2
Nhi2 <- Nave2 + 1.96*Nsd2
Nblow2 <- Nbave2 - 1.96*Nbsd2
Nbhi2 <- Nbave2 + 1.96*Nbsd2


    Eprob = Extinct/as.numeric(input$maxiter)
jobLength = 10
for (k in 1:jobLength) {
# Do work
  setProgress(value = k)
}
  
  

    list(Nave=Nave,Nbave=Nbave,Nlow=Nlow,Nhi=Nhi,Nblow=Nblow,Nbhi=Nbhi,Eprob=Eprob,K=K,K2=K2,
          Nave1=Nave1,Nbave1=Nbave1,Nblow1=Nblow1,Nbhi1=Nbhi1,Nhi1=Nhi1,Nlow1=Nlow1,
         Nave2=Nave2,Nbave2=Nbave2,Nblow2=Nblow2,Nbhi2=Nbhi2,Nhi2=Nhi2,Nlow2=Nlow2)
  }) 

})



output$lambda1 <- renderText({
                            if(input$States==2) {return(NULL)}
                            if(is.null(input$Na0)) {return(NULL)}
                            paste("Leslie matrix lambda (1 = stationary population): ",round(lambda1(),3))
                            })
output$lambda1b <- renderText({
  if(input$States==1) {return(NULL)}
  if(is.null(input$Na0)) {return(NULL)}
  paste("Site 1 Leslie matrix lambda assuming geographic closure (1 = stationary population): ",round(lambda1(),3))
})
                            


output$lambda2 <- renderText({
  if(is.null(input$Na0)) {return(NULL)}
  if(input$States==1) {return(NULL)}
  paste("Site 2 Leslie matrix lambda assuming geographic closure (1 = stationary population): ",round(lambda2(),3))})


output$lambdatot <- renderText({
  if(input$States==1) {return(NULL)}
  if(is.null(input$Na0)) {return(NULL)}
  
  paste("Metapopulation transition matrix lambda (1 = stationary population): ",round(lambdatot(),3))})

output$pextinct1 <- renderText({
                              if(is.null(input$Na0)) {return(NULL)}
                              paste("Probability of extinction: ",trajectory()$Eprob)})


output$growthtotal <- renderPlot({
                      
                               if(is.null(input$Na0)) {return(NULL)}
                              if(is.null(min(trajectory()$Nlow))) {return(NULL)}
                            
                              
                              if (as.numeric(input$maxiter) > 1) {
                              par(xpd=T, mar=par()$mar+c(0,0,0,6))
                              if (input$States==1){
                              plot(1:N.year,trajectory()$Nave,ylab="N",xlab="Year",
                                   main=expression(paste("Mean projected male abundance ",symbol("\261")," 95% prediction intervals")),type="l",
                                           ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K,trajectory()$Nhi)))
                              }
                              if (input$States==2){
                                plot(1:N.year,trajectory()$Nave,ylab="N",xlab="Year",
                                     main=expression(paste("Mean projected male abundance ",symbol("\261")," 95% prediction intervals")),type="l",
                                     ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K+max(trajectory()$K2),trajectory()$Nhi)))
                              }
                              
                              lines(2:N.year,trajectory()$Nhi[2:N.year],lty=5,col="black")
                              lines(2:N.year,trajectory()$Nlow[2:N.year],lty=5,col="black")
                              par(new=T)
                              
                              if (input$States==1){
                              plot(1:N.year,trajectory()$Nbave,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
                                   ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K,trajectory()$Nhi)))
                              }
                              if (input$States==2){
                                plot(1:N.year,trajectory()$Nbave,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
                                     ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K+trajectory()$K2,trajectory()$Nhi)))
                              }
                              lines(2:N.year,trajectory()$Nbhi[2:N.year],lty=5,col="red")
                              lines(2:N.year,trajectory()$Nblow[2:N.year],lty=5,col="red")
                           
                              if(input$States==1){
                                lines(1:N.year,rep(trajectory()$K,N.year),col="blue",lty=5)
                              }
                              
                              if(input$States==2){
                              lines(1:N.year,rep(trajectory()$K+trajectory()$K2,N.year),col="blue",lty=5)
                              }
                              lines(1:N.year,rep(0,N.year),col="black",lty=3)
                              legend(105,max(trajectory()$Nhi),c("Total N", "Breeding N","K"), col=c("black","red","blue"),lty = 1) 
                              par(mar=c(5, 4, 4, 2) + 0.1)
                              }
                              
                            if (as.numeric(input$maxiter)==1){
                              par(xpd=T, mar=par()$mar+c(0,0,0,6))
                              
                              if (input$States==1){
                              plot(1:N.year,trajectory()$Nave,ylab="N",xlab="Year",
                                   main=expression(paste("Projected male abundance ")),type="l",
                                   ylim=c(0,max(trajectory()$K,trajectory()$Nave)))
                              }
                              if (input$States==2){
                                plot(1:N.year,trajectory()$Nave,ylab="N",xlab="Year",
                                     main=expression(paste("Projected male abundance ")),type="l",
                                     ylim=c(0,max(trajectory()$K+trajectory()$K2,trajectory()$Nave)))
                              }
                              
                            
                              par(new=T)
                              if (input$States==1){
                              plot(1:N.year,trajectory()$Nbave,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
                                   ylim=c(0,max(trajectory()$K,trajectory()$Nave)))
                              }
                              if (input$States==2){
                                plot(1:N.year,trajectory()$Nbave,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
                                     ylim=c(0,max(trajectory()$K+trajectory()$K2,trajectory()$Nave)))
                              }
                              if (input$States==1) {
                                lines(1:N.year,rep(trajectory()$K,N.year),col="blue",lty=5)
                              }                             
                              if (input$States==2) {
                              lines(1:N.year,rep(trajectory()$K+trajectory()$K2,N.year),col="blue",lty=5)
                              }
                              
                              lines(1:N.year,rep(0,N.year),col="black",lty=3)
                              legend(105,max(trajectory()$Nave),c("Total N", "Breeding N","K"), col=c("black","red","blue"),lty = 1) 
                              par(mar=c(5, 4, 4, 2) + 0.1)
                              }
                              
                              })


output$growth1 <- renderPlot({
  if(is.null(input$Na0)) {return(NULL)}
  if(input$States==1) {return(NULL)}
  if(is.null(min(trajectory()$Nlow1))) {return(NULL)}
  
  
  if (as.numeric(input$maxiter) > 1) {
    par(xpd=T, mar=par()$mar+c(0,0,0,6))
    plot(1:N.year,trajectory()$Nave1,ylab="N",xlab="Year",
         main=expression(paste("Mean projected male abundance ",symbol("\261")," 95% prediction intervals, site 1")),type="l",
         ylim=c(min(0,trajectory()$Nlow1),max(trajectory()$K,trajectory()$Nhi1)))
    lines(2:N.year,trajectory()$Nhi1[2:N.year],lty=5,col="black")
    lines(2:N.year,trajectory()$Nlow1[2:N.year],lty=5,col="black")
    par(new=T)
    plot(1:N.year,trajectory()$Nbave1,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
         ylim=c(min(0,trajectory()$Nlow1),max(trajectory()$K,trajectory()$Nhi1)))
    lines(2:N.year,trajectory()$Nbhi1[2:N.year],lty=5,col="red")
    lines(2:N.year,trajectory()$Nblow1[2:N.year],lty=5,col="red")
    lines(1:N.year,rep(trajectory()$K,N.year),col="blue",lty=5)
    lines(1:N.year,rep(0,N.year),col="black",lty=3)
    legend(105,max(trajectory()$Nhi1),c("Total N", "Breeding N","K"), col=c("black","red","blue"),lty = 1) 
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
  
  if (as.numeric(input$maxiter)==1){
    par(xpd=T, mar=par()$mar+c(0,0,0,6))
    plot(1:N.year,trajectory()$Nave1,ylab="N",xlab="Year",
         main=expression(paste("Projected male abundance, site 1 ")),type="l",
         ylim=c(0,max(trajectory()$K,trajectory()$Nave1)))
    par(new=T)
    plot(1:N.year,trajectory()$Nbave1,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
         ylim=c(0,max(trajectory()$K,trajectory()$Nave1)))
    lines(1:N.year,rep(trajectory()$K,N.year),col="blue",lty=5)
    lines(1:N.year,rep(0,N.year),col="black",lty=3)
    legend(105,max(trajectory()$Nave1),c("Total N", "Breeding N","K"), col=c("black","red","blue"),lty = 1) 
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
  
})

output$growth2 <- renderPlot({
  if(is.null(input$Na0)) {return(NULL)}
  if(input$States==1) {return(NULL)}
  if(is.null(min(trajectory()$Nlow2))) {return(NULL)}
  
  
  if (as.numeric(input$maxiter) > 1) {
    par(xpd=T, mar=par()$mar+c(0,0,0,6))
    plot(1:N.year,trajectory()$Nave2,ylab="N",xlab="Year",
         main=expression(paste("Mean projected male abundance ",symbol("\261")," 95% prediction intervals, site 2")),type="l",
         ylim=c(min(0,trajectory()$Nlow2),max(trajectory()$K2,trajectory()$Nhi2)))
    lines(2:N.year,trajectory()$Nhi2[2:N.year],lty=5,col="black")
    lines(2:N.year,trajectory()$Nlow2[2:N.year],lty=5,col="black")
    par(new=T)
    plot(1:N.year,trajectory()$Nbave2,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
         ylim=c(min(0,trajectory()$Nlow2),max(trajectory()$K2,trajectory()$Nhi2)))
    lines(2:N.year,trajectory()$Nbhi2[2:N.year],lty=5,col="red")
    lines(2:N.year,trajectory()$Nblow2[2:N.year],lty=5,col="red")
    lines(1:N.year,rep(trajectory()$K2,N.year),col="blue",lty=5)
    lines(1:N.year,rep(0,N.year),col="black",lty=3)
    legend(105,max(trajectory()$Nhi2),c("Total N", "Breeding N","K"), col=c("black","red","blue"),lty = 1) 
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
  
  if (as.numeric(input$maxiter)==1){
    par(xpd=T, mar=par()$mar+c(0,0,0,6))
    plot(1:N.year,trajectory()$Nave2,ylab="N",xlab="Year",
         main=expression(paste("Projected male abundance, site 2 ")),type="l",
         ylim=c(0,max(trajectory()$K2,trajectory()$Nave2)))
    par(new=T)
    plot(1:N.year,trajectory()$Nbave2,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
         ylim=c(0,max(trajectory()$K2,trajectory()$Nave2)))
    lines(1:N.year,rep(trajectory()$K2,N.year),col="blue",lty=5)
    lines(1:N.year,rep(0,N.year),col="black",lty=3)
    legend(105,max(trajectory()$Nave2),c("Total N", "Breeding N","K"), col=c("black","red","blue"),lty = 1) 
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
  
})


}) 
