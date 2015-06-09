# server.R   This is the part of the shiny code that processes inputs and outputs and runs R functions.
#the first part of the coded checks to see if the user has the proper R packages installed, 
#and installs and loads them if not.
if("MASS" %in% rownames(installed.packages()) == FALSE) {
  install.packages("MASS")}
if("shinyIncubator" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("shiny-incubator", "rstudio")}
if("shiny" %in% rownames(installed.packages()) == FALSE) {
  install.packages('httpuv', repos=c(RStudio='http://rstudio.org/_packages', CRAN='http://cran.rstudio.com'))
  install.packages('devtools') # if you don't already have devtools installed
  devtools::install_github('shiny', 'rstudio')
}
library(shinyIncubator)
library(shiny)
library(MASS)

#the following code sets values for global constants.  The user doesn't yet have access to them.
N.year <- 100
rfdelta <- 0.33 #proportional decrease in probability of renesting following the second nest failure.  Gets multiplied by rf.
rsdelta <- 0.1 #proportional decrease in probability of renesting following the success of the second brood.  Gets multiplied by rs. 
rbfdelta <- 0.625 #proportional decrease in probability of renesting following the failure of the second brood. Gets multiplied by rbf. 


#the following code defines functions to be used later
#calculated brood survival
BS <- function(f,Chmort,E) 1-(1-f*Chmort)^(E/2) #f=baseline chick survival, Chmort = survival of threat


#proportion of males with 1 successful nest attempt
PS1 <- function(ns,bs,rs,rf,rbf){
  
  ns*bs*(1-rs) + ns*(1-bs)*(1-rbf) + 
  ns*bs*rs*(1-ns)*rf*rfdelta*(1-ns)  + ns*bs*rs*(1-ns)*(1-rf*rfdelta)+
ns*(1-bs)*rbf*(1-ns)*rf*rfdelta*(1-ns)  + ns*(1-bs)*rbf*(1-ns)*(1-rf*rfdelta) +
  (1-ns)*rf *ns*bs*(1-rs*rsdelta) + (1-ns)*rf*ns*(1-bs)*(1-rbf*rbfdelta) +
  (1-ns)*rf *ns*bs*(1-ns)+ (1-ns)*rf*ns*(1-bs)*(rbf*rbfdelta) *(1-ns)+
    (1-ns)*rf*(1-ns)*rf*rfdelta*ns
}

#proportion of males with 2 successful nest attempts
PS2 <- function(ns,bs,rs,rf,rbf){
  ns*bs*rs*ns*bs*rs*rsdelta*(1-ns)     + ns*bs*rs*ns*bs*(1-rs*rsdelta) +  
  ns*(1-bs)*rbf*ns*bs*rs*rsdelta*(1-ns) +   ns*(1-bs)*rbf*ns*bs*(1-rs*rsdelta)+
  ns*bs*rs*ns*(1-bs)*rbf*rbfdelta*(1-ns) + ns*bs*rs*ns*(1-bs)*(1-rbf*rbfdelta)+
  ns*(1-bs)*rbf*ns*(1-bs)*rbf*rbfdelta*(1-ns) +  ns*(1-bs)*rbf*ns*(1-bs)*(1-rbf*rbfdelta)    + 
  (1-ns)* rf*ns*bs*rs*rsdelta*ns + (1-ns)* rf*ns*(1-bs)*rbf*rbfdelta*ns  +
  ns*bs*rs*(1-ns)*rf*rfdelta*ns + ns*(1-bs)*rbf*(1-ns)*rf*rfdelta*ns
}

#proportion of males with 3 successful nest attempts
PS3 <- function(ns,bs,rs,rf,rbf){
  ns*bs*rs*ns*bs*rs*rsdelta*ns +
  ns*bs*rs*ns*(1-bs)*rbf*rbfdelta*ns +
  ns*(1-bs)*rbf*ns*bs*rs*rsdelta*ns +
  ns*(1-bs)*rbf*ns*(1-bs)*rbf*rbfdelta*ns
}

#The function below is used to draw random Poisson values for fecundity
#allowing for covariance between fecundity ofr adult and subadult males.
# Generate a p-dimensional Poisson (Yahav and Shmueli 2008)
# p = the dimension of the distribution (the number of fecundity rates in this case there are 2)
# samples = the number of observations (just 1, this is the random draws)
# R = correlation matrix p X p (correlation matrix for the fecundity rates)
# lambda = rate vector p X 1 (the vector of mean fecundity values)

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


#shinyServer is the function within which all the other functions get executed
shinyServer(function(input,output,session){ #standard first line
  options(shiny.usecairo=FALSE) #turns off the default graphics library which makes poor-looking output
  
#this next code defines the elements for the user interface.  
#"output" is the object containing the user-editable variables, after the "$" is the name of the variable.
#"numericInput" creates a box where the user can type a number.  If the App is run in a web browser,
#the user will also see arrows they can use to increment the number up or down by the value of "step"
#the default value follows the label.

#Adults in stratum 1 
  output$Na0 <- renderUI({ 
    numericInput("Na0",label=h6("Starting abundance adults:"),100,min=0.00,max=1000,step=1)
  })
  output$ya <- renderUI({
    numericInput("ya", label=h6("Nesting rate:"),1.0,min=0.00,max=1.00,step=0.01 )
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

#subadults in Stratum 1
output$Ns0 <- renderUI({
  numericInput("Ns0",label=h6("Starting abundance subadults:"),50,min=0.00,max=1000,step=1)
})
  output$ys <- renderUI({
    numericInput("ys", label=h6("Nesting rate:"),1.0,min=0.00,max=1.00,step=0.01 )
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


#Adults stratum 2
  output$Na02 <- renderUI({
    numericInput("Na02",label=h6("Starting abundance adults:"),100,min=0.00,max=1000,step=1)
  })
  output$ya2 <- renderUI({
    numericInput("ya2", label=h6("Nesting rate:"),1.0,min=0.00,max=1.00,step=0.01 )
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

#subadults stratum 2

output$Ns02 <- renderUI({
  numericInput("Ns02",label=h6("Starting abundance subadults:"),50,min=0.00,max=1000,step=1)
})
  output$ys2 <- renderUI({
    numericInput("ys2", label=h6("Nesting rate:"),1.0,min=0.00,max=1.00,step=0.01 )
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


#the following code resets user inputs to defaults upon user click of reset button
#observe tells shiny to check the value of the input when the user interacts with it and to run the code 
  observe({ 
    if (as.numeric(input$reset_button) > 0) { #the reset button value gets incremented by 1 when it is clicked
      updateNumericInput(session,"Na0", ,100,min=0.00,max=1000,step=1 )
      updateNumericInput(session,"Ns0", ,50,min=0.00,max=1000,step=1 )
      updateNumericInput(session,"ya", ,1.0,min=0.00,max=1.00,step=0.01 )
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
      updateNumericInput(session,"ys", ,1.0,min=0.00,max=1.00,step=0.01 )
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
    
    #reset button for stratum 2
    if (as.numeric(input$reset_button2) > 0) {
      updateNumericInput(session,"Na02", ,100,min=0.00,max=1000,step=1 )
      updateNumericInput(session,"Ns02", ,50,min=0.00,max=1000,step=1 )
      updateNumericInput(session,"ya2", ,1.0,min=0.00,max=1.00,step=0.01 )
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
      updateNumericInput(session,"ys2", ,1.0,min=0.00,max=1.00,step=0.01 )
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
    
    #resets the transition parameter values to zero if States gets set back to 1
    if (as.numeric(input$States) == 1) {
      updateNumericInput(session,"Psi21Ja", ,0,min=0.00,max=1.00,step=0.01 )
      updateNumericInput(session,"Psi21J", ,0,min=0.00,max=1.00,step=0.01 )
      updateNumericInput(session,"Psi21A", ,0,min=0.00,max=1.00,step=0.01 )
      updateNumericInput(session,"Psi12Ja", ,0,min=0.00,max=1.00,step=0.01 )
      updateNumericInput(session,"Psi12J", ,0,min=0.00,max=1.00,step=0.01 )
      updateNumericInput(session,"Psi12A", ,0,min=0.00,max=1.00,step=0.01 )
    }
    
    #resets the values of additional hazards to zero if number of hazards gets set back to 1
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

#Now comes the code that calculates the values in the Leslie matrix
  #STRATUM 1
  #adults stratum 1
  #reactive is the shiny function that causes the values of variables to change, or creates variables that can later be changed
  ba1 <- reactive({BS(input$fa,input$ChMort2a,input$Ea)}) #calculates brood survival

 #the probabilities that an adult male will have 1, 2, or 3 nests that hatch, stratum 1
  ps1a <- reactive({PS1(input$nsa,ba1(),input$Rsa,input$Rfa,input$Rbfa)})
  ps2a <- reactive({PS2(input$nsa,ba1(),input$Rsa,input$Rfa,input$Rbfa)})
  ps3a <- reactive({PS3(input$nsa,ba1(),input$Rsa,input$Rfa,input$Rbfa)})


  pveca <- reactive({c(ps1a(),ps2a(),ps3a())}) #vector of the above 3 probabiliities
  Aa <- reactive ({t(c(1,2,3))%*%(pveca())}) #mean number of successful nest attempts, adults in stratum 1
  Fa <- reactive({input$ya*(input$Ea/2)*Aa()*input$fa*input$sfla*input$ChMort2a*input$fMort2a}) #male young per Adult male stratum 1
  Phiba = reactive({input$sba*input$sMort2a}) #adult survival of breeding season including threat
  Phia = reactive({Phiba()*input$WASY}) #adult annual survival, breeding * winter
 
  
  #subadults stratum 1
  
  bs1 <- reactive({BS(input$f,input$ChMort2,input$E)}) #brood survival, subadults, stratum 1
  
#the probabilities that a subadult male will have 1, 2, or 3 nests that hatch, stratum 1
  ps1 <- reactive({PS1(input$ns,bs1(),input$Rs,input$Rf,input$Rbf)})
  ps2 <- reactive({PS2(input$ns,bs1(),input$Rs,input$Rf,input$Rbf)})
  ps3 <- reactive({PS3(input$ns,bs1(),input$Rs,input$Rf,input$Rbf)})
  

  pvecs <- reactive({c(ps1(),ps2(),ps3())})#vector of the above 3 probabiliities
  As <- reactive ({t(c(1,2,3))%*%(pvecs())}) #mean number of nest attempts, subadults, stratum 1
  Fs <- reactive({input$ys*(input$E/2)*As()*input$f*input$sfl*input$ChMort2*input$fMort2}) #male young per subAdult male
  Phibs <- reactive({input$sb*input$sMort2}) #subadult survival of breeding season including threat
  Phis <- reactive({Phibs()*input$WSY}) #subadult annual survival, breeding * winter

  #Leslie matrix for stratum 1
  Lesmat1 <- reactive({matrix(c(Fs()*input$WSA,Phis(),
                                Fa()*input$WSAa,Phia()),2,2,byrow=FALSE)})
#population growth rate for stratum 1
  lambda1 <- reactive({eigen(Lesmat1())$values[1]})
  
  
  #STRATUM 2
  #adults stratum 2
  ba2 <- reactive({BS(input$fa2,input$ChMort2a2,input$Ea2)}) 
  ps1a2 <- reactive({PS1(input$nsa2,ba2(),input$Rsa2,input$Rfa2,input$Rbfa2)})
  ps2a2 <- reactive({PS2(input$nsa2,ba2(),input$Rsa2,input$Rfa2,input$Rbfa2)})
  ps3a2 <- reactive({PS3(input$nsa2,ba2(),input$Rsa2,input$Rfa2,input$Rbfa2)})
  
  
  pveca2 <- reactive({c(ps1a2(),ps2a2(),ps3a2())})
  Aa2 <- reactive ({t(c(1,2,3)) %*% pveca2()})
  Fa2 <- reactive({input$ya2*(input$Ea2/2)*Aa2()*input$fa2*input$sfla2*input$ChMort2a2*input$fMort2a2}) #male young per Adult male, stratum 2
  Phiba2 = reactive({input$sba2*input$sMort2a2}) #adult survival of breeding season including threat, stratum 2
  Phia2 = reactive({Phiba2()*input$WASY2}) #adult annual survival, breeding * winter, stratum 2
  
  #subadults stratum 2
  bs2 <- reactive({BS(input$f2,input$ChMort22,input$E2)})
  
  ps12 <- reactive({PS1(input$ns2,bs2(),input$Rs2,input$Rf2,input$Rbf2)})
  ps22 <- reactive({PS2(input$ns2,bs2(),input$Rs2,input$Rf2,input$Rbf2)})
  ps32 <- reactive({PS3(input$ns2,bs2(),input$Rs2,input$Rf2,input$Rbf2)})
  
  
  pvecs2 <- reactive({c(ps12(),ps22(),ps32())})
  As2 <- reactive ({t(c(1,2,3))%*%(pvecs2())}) #mean number of nest attempts, subadults stratum 2
  Fs2 <- reactive({input$ys2*(input$E2/2)*As2()*input$f2*input$sfla2*input$ChMort22*input$fMort22}) #male young per subAdult male, stratum 2
  Phibs2 <- reactive({input$sb2*input$sMort22}) #subadult survival of breeding season including threat, stratum 2
  Phis2 <- reactive({Phibs2()*input$WSY2}) #subadult annual survival, breeding * winter, stratum 2

  #Leslie matrix
  Lesmat2 <- reactive({matrix(c(Fs2()*input$WSA2,Phis2(),
                                Fa2()*input$WSAa2,
                                Phia2()),2,2,byrow=FALSE)})
  lambda2 <- reactive({eigen(Lesmat2())$values[1]})
  
#The code below creates a 4 by 4 transition matrix that includes movements:
#Fs1    Fa1    Psi21A  0
#Phis1  Phia1  0       Psi21A
#0      Psi12A Fs2     Fa2
#Psi12A 0      Phis2   Phia2
#This matrix is used for the metapopulation

  Lesmattotal <- reactive({matrix(c(
    Fs()*input$WSA*(1-input$Psi12J),Fa()*input$WSAa*(1-input$Psi12Ja),input$Psi21A,0,
    Phis()*(1-input$Psi12A),Phia()*(1-input$Psi12A),0,input$Psi21A,
    input$Psi12A,0,Fs2()*input$WSA2*(1-input$Psi21J),Fa2()*input$WSAa2*(1-input$Psi12Ja),
    0, input$Psi12A,Phis2()*(1-input$Psi21A), Phia2()*(1-input$Psi12A)),
    4,4,byrow=TRUE)})

#Metapopulation growth rate
  lambdatot <- reactive({eigen(Lesmattotal())$values[1]})

#The following code calculates population size for each time step

  trajectory <- reactive({
    withProgress(session, min=0, max=1, {  #creates a progress bar so the user can see that the App is busy
      setProgress(message = "Calculating...")
      setProgress(value = 0.2, message = "Calculating, do not change inputs...")
      
      #sets up variables for use in the projection model
      Extinct1 = 0 #initialized a counter for the number of runs where the population went extinct, Stratum 1
      Extinct2 = 0 #same but for population 2
      Extinctm = 0 #same but for metapopulation
      
      Nmatrix <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100) #a matrix to hold the pop size for stratum 1
      Nbmatrix <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100) #matrix to hold the breeding pop size, stratum 1
      Nmatrix2 <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100) #a matrix to hold the pop size for stratum 2
      Nbmatrix2 <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100) #matrix to hold the breeding pop size, stratum 2
      Nmatrixtotal <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100) #a matrix to hold the pop size for the metapopulation
      Nbmatrixtotal <- matrix(0,nrow=as.numeric(input$maxiter),ncol=100) # a matrix to hold the breeding metapop size
      K <- input$K  #user-defined carrying capacity, stratum 1
      K2 <- input$K2 #user-defined carrying capacity, stratum 2
      
      #these 3 lines contain the variances and covariances for stochasticity in vital rates.
      #the user doesn't have access to them at this time, and they are arbitrary 
      
      CV <- 0.1 #coefficient of variation for survival and movement rates, used to calculate variances on these rates.  
                #at 0.1 this is a moderate amount of variability
      corrsurv <- 0.9 #correlation among survival rates, at 0.9 this makes adult and subadult survival very correlated
      corrF <- 0.9 #correlation among fecundity rates, at 0.9 this makes adult and subadult fecundity very correlated
      
      
      #now start looping through the simulation runs.  "maxiter" is the user input for the number of simulation runs
      for (iter in 1:as.numeric(input$maxiter)){
        if (input$States==1) { #this code is used if there is only 1 stratum
          
          #seed Year 0
          #popmat1 is a matrix that holds the population vector for each year, row 1 = subadults, row 2 = adults
          popmat1 <- matrix(0,2,N.year)
          popmat1[1,1] <- input$Ns0 
          popmat1[2,1] <- input$Na0
          #update the total and breeding population size matrices
          Ntotal1 <- c(popmat1[1,1]+popmat1[2,1],rep(0,N.year-1))
          Ntotaltot <- c(popmat1[1,1]+popmat1[2,1],rep(0,N.year-1))
          Nbreed1 <- c(min(popmat1[1,1]+popmat1[2,1],K),rep(0,N.year-1))
          Ntotalbtot <- c(min(popmat1[1,1]+popmat1[2,1],K),rep(0,N.year-1))
          
          #now for a given simulation run, loop through the years
          for (i in 2:N.year) {
            rcat <- rbinom(1,1,0.95) #probability of no reproductive catastrophe (where fecundity = 0)
            #calculate the standard deviations for the vital rates
            sdWSA <- CV*input$WSA
            sdWSAa <- CV*input$WSAa
            sdPhis <- CV*Phis()
            sdPhia <- CV*Phia()
            
            #put standard deviations on the logit scale
            sdWSAlogit <- sdWSA/(input$WSA*(1-input$WSA))
            sdWSAalogit <- sdWSAa/(input$WSAa*(1-input$WSAa))
            sdPhislogit <- sdPhis/(Phis()*(1-Phis()))
            sdPhialogit <- sdPhis/(Phia()*(1-Phia()))
            
          
            vcovWSurv <- #variance-covariance matrix for survival rates on logit scale
              matrix(c(sdWSAlogit^2,sdWSAlogit*sdWSAalogit*corrsurv,
                       sdWSAlogit*sdWSAalogit*corrsurv,sdWSAalogit^2),
                     2,2,byrow=TRUE)
            vcovBSurv <- #variance-covariance matrix for survival rates on logit scale
              matrix(c(sdPhislogit^2,sdPhislogit*sdPhialogit*corrsurv,
                       sdPhislogit*sdPhialogit*corrsurv,sdPhialogit^2),
                     2,2,byrow=TRUE)
            Fmean <- GMP(2,1,matrix(c(1,corrF,corrF,1),2,2,byrow=TRUE),c(Fa(),Fs())) #generate random fecundities in a vector
            
            #generate random winter and breeding season survival rates on the probability scale, in a vector
            Wsurvmean <- plogis(mvrnorm(1,c(qlogis(input$WSA),qlogis(input$WSAa)),vcovWSurv))
            Bsurvmean <- plogis(mvrnorm(1,c(qlogis(Phis()),qlogis(Phia())),vcovBSurv))
            #get the rates for each stage class out of the vectorsr
            WSAt <- Wsurvmean[1]
            WSAat <- Wsurvmean[2]
            Phist <- Bsurvmean[1]
            Phiat <- Bsurvmean[2]
            Fat <- Fmean[1]
            Fst <- Fmean[2]
            
            #sets fecundity to 0 for males in excess of carrying capacity (they cannot get a territory)
            if (Ntotal1[i-1] > K) {
              Fat <- (popmat1[2,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fat) #all adults over K have fecundity of 0
              Fst <- (popmat1[1,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fst) #all subadults over K have fecundity of 0
            }
            
            Fat <- Fat*max(rcat,0.5) #include reproductive catastrophe
            Fst <- Fst*max(rcat,0.5)
            
            #year-specific Leslie matrix with the randomly-generated rates
            Lesmat1 <- matrix(c(Fst*WSAt*(1-input$Psi12J),Phist*(1-input$Psi12A),
                                Fat*WSAat*(1-input$Psi12Ja),Phiat*(1-input$Psi12A)),2,2,byrow=FALSE)
    
            
            #projects population size for year t, and stores in the relevant matrices
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
          #seed Year 0 for both populations
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
          Ntotalbtot <- c((Nbreed1+Nbreed2),rep(0,N.year-1))
          
          for (i in 2:N.year) { #loop through the years
            rcat <- rbinom(1,1,0.95) #probability of no reproductive catastrophe, wherer fecundity = 0
            
            #the stochasticity parameters, chosen arbitrarily
            CV = 0.1 #covariate for survival and transition rates
            corrsurv <- 0.9 #correlation among survival rates
            corrF <- 0.9
            
            #standard deviations for vital rates stratum 1
            sdWSA <- CV*input$WSA
            sdWSAa <- CV*input$WSAa
            sdPhis <- CV*Phis()
            sdPhia <- CV*Phia()
            #put SD's on logit scale
            sdWSAlogit <- sdWSA/(input$WSA*(1-input$WSA))
            sdWSAalogit <- sdWSAa/(input$WSAa*(1-input$WSAa))
            sdPhislogit <- sdPhis/(Phis()*(1-Phis()))
            sdPhialogit <- sdPhis/(Phia()*(1-Phia()))
            
            #repeat for stratum 2
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
            #generate random fecundities for stratum 1 and stratum 2 in vectors
            Fmean <- GMP(2,1,matrix(c(1,corrF,corrF,1),2,2,byrow=TRUE),c(Fa(),Fs()))
            Fmean2 <- GMP(2,1,matrix(c(1,corrF,corrF,1),2,2,byrow=TRUE),c(Fa2(),Fs2()))
            #generate random survival rates in vectors
            Wsurvmean <- plogis(mvrnorm(1,c(qlogis(input$WSA),qlogis(input$WSAa),qlogis(input$WSA2),qlogis(input$WSAa2)),vcovWSurv))
            Bsurvmean1 <- plogis(mvrnorm(1,c(qlogis(Phis()),qlogis(Phia())),vcovBSurv))
            Bsurvmean2 <- plogis(mvrnorm(1,c(qlogis(Phis2()),qlogis(Phia2())),vcovBSurv2))
  
            #get rates from the vectors
            WSAt <- Wsurvmean[1]
            WSAat <- Wsurvmean[2]
            WSAt2 <- Wsurvmean[3]
            WSAat2 <- Wsurvmean[4]
            Phist <- Bsurvmean1[1]
            Phiat <- Bsurvmean1[2]
            Phist2 <- Bsurvmean2[1]
            Phiat2 <- Bsurvmean2[2]
            Fat <- Fmean[1]
            Fst <- Fmean[2]
            Fat2 <- Fmean2[1]
            Fst2 <- Fmean2[2]
            
            #set fecundity to 0 for all males in excess of carrying capacity (they cannot get a territory)
            if (Ntotal1[i-1] > K) {
              Fat <- (popmat1[2,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fat) #all adults over K have fecundity of 0
              Fst <- (popmat1[1,i-1]/Ntotal1[i-1])*((Ntotal1[i-1]-K)/Ntotal1[i-1]*0 + K/Ntotal1[i-1]*Fst) #all subadults over K have fecundity of 0
            }
            if (Ntotal2[i-1] > K2) {
              Fat2 <- (popmat2[2,i-1]/Ntotal2[i-1])*((Ntotal2[i-1]-K2)/Ntotal2[i-1]*0 + K2/Ntotal2[i-1]*Fat2) #all adults over K have fecundity of 0
              Fst2 <- (popmat2[1,i-1]/Ntotal2[i-1])*((Ntotal2[i-1]-K2)/Ntotal2[i-1]*0 + K2/Ntotal2[i-1]*Fst2) #all subadults over K have fecundity of 0
            }
            
            Fat <- Fat*max(rcat,0.5) #include reproductive catastrophe
            Fst <- Fst*max(rcat,0.5)
            Fat2 <- Fat2*max(rcat,0.5)
            Fst2 <- Fst2*max(rcat,0.5)
            
            #create the 4 by 4 transition matrix for time t, using the randomly generated values
            #note, stochasticity in movement rates isn't included
            Lesmattotal <- matrix(c(Fst*WSAt*(1-input$Psi12J),Phist*(1-input$Psi12A),input$Psi12A,0,
                                    Fat*WSAat*(1-input$Psi12Ja),Phiat*(1-input$Psi12A),0,input$Psi12A,
                                    input$Psi21A,0,Fst2*WSAt2*(1-input$Psi21J),Phist2*(1-input$Psi21A),
                                    0,input$Psi21A,Fat2*WSAat2*(1-input$Psi12Ja),Phiat2*(1-input$Psi12A)),4,4,byrow=FALSE)
  
            #project the population for the current year, and store the abundances in the relevant matrices
            popmattotal[1:4,i] <- Lesmattotal%*%popmattotal[1:4,i-1]
            Ntotal1[i] <- popmattotal[1,i]+popmattotal[2,i]
            Ntotal2[i] <- popmattotal[3,i]+popmattotal[4,i]
            Ntotaltot[i] <- popmattotal[1,i]+popmattotal[2,i]+popmattotal[3,i]+popmattotal[4,i]
            Ntotaltot[i][Ntotaltot[i-1]<1]=0
            Nbreed1[i] <- min(Ntotal1[i],K)
            Nbreed2[i] <- min(Ntotal2[i],K)
            Ntotalbtot[i] <- Nbreed1[i]+Nbreed2[i]
          }
        }
        Extinct1[Ntotal1[100]<1]=Extinct1+1 #adds up the number of runs where the population went extinct
        if (input$States==2){
        Extinct2[Ntotal2[100]<1]=Extinct2+1 #adds up the number of runs where the population went extinct
        Extinctm[Ntotaltot[100]<1]=Extinctm+1} #adds up the number of runs where the population went extinct
    
        
        #matrices that hold the population sizes across years for all the iterations
        Nmatrix[iter,] <- Ntotal1[]
        Nbmatrix[iter,] <- Nbreed1[]
        if (input$States==2){
          Nmatrix2[iter,] <- Ntotal2[]
          Nbmatrix2[iter,] <- Nbreed2[]}
        Nmatrixtotal[iter,] <- Ntotaltot[]
        Nbmatrixtotal[iter,] <- Ntotalbtot[]
      }
      
      #calculate mean and confidence bound across all iterations, for each year, for plotting
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
      Eprob1 = Extinct1/as.numeric(input$maxiter)  #extinction probability
      Eprob2 = Extinct2/as.numeric(input$maxiter)  #extinction probability
      Eprobm = Extinctm/as.numeric(input$maxiter)  #extinction probability
      jobLength = 10
      for (k in 1:jobLength) { #closes the loop for the progress bar
        setProgress(value = k)
      }
      
      list(Nave=Nave,Nbave=Nbave,Nlow=Nlow,Nhi=Nhi,Nblow=Nblow,Nbhi=Nbhi,Eprob1=Eprob1,Eprob2=Eprob2,Eprobm=Eprobm,K=K,K2=K2,
           Nave1=Nave1,Nbave1=Nbave1,Nblow1=Nblow1,Nbhi1=Nbhi1,Nhi1=Nhi1,Nlow1=Nlow1,
           Nave2=Nave2,Nbave2=Nbave2,Nblow2=Nblow2,Nbhi2=Nbhi2,Nhi2=Nhi2,Nlow2=Nlow2) #list of things for the function to output
    })
  })
  

#sets up the output to send to the user interfact Output tab.  
  #these 2 lines are used for error trapping, and are not currently displayed in the output
  #I used it to display the results of calculations when it seemed something was going wrong in one of the steps
  output$errcheck <- renderText({
    if(input$States==1) {return(NULL)}
    paste("extinct2 :",)
  })
  
  #RESULTS TO BE DISPLAYED
  #First, numeric text results
  output$lambda1 <- renderText({ #lambda for population 1, when number of states is 1
    if(input$States==2) {return(NULL)} #this prevents the display of nonsense if certain user input isn't available
    if(is.null(input$Na0)) {return(NULL)}  #same as abovev
    paste("Leslie matrix lambda (1 = stationary population): ",round(lambda1(),3))
  })

  output$lambda1b <- renderText({ #lambda for population 1, when number of states is 2
    if(input$States==1) {return(NULL)}
    if(is.null(input$Na0)) {return(NULL)}
    paste("Site 1 Leslie matrix lambda assuming geographic closure (1 = stationary population): ",round(lambda1(),3))
  })

  output$lambda2 <- renderText({#lambda for population 2
    if(is.null(input$Na0)) {return(NULL)}
    if(input$States==1) {return(NULL)}
    paste("Site 2 Leslie matrix lambda assuming geographic closure (1 = stationary population): ",round(lambda2(),3))})

  output$lambdatot <- renderText({#lambda for metapopulation
    if(input$States==1) {return(NULL)}
    if(is.null(input$Na0)) {return(NULL)}
    paste("Metapopulation transition matrix lambda (1 = stationary population): ",round(lambdatot(),3))})


  output$pextinct1 <- renderText({#probability of extinction for stratum 1, where there is only 1 stratum
    if(is.null(input$Na0)) {return(NULL)}
    if(input$States==2) {return(NULL)}
    paste("Probability of extinction: ",trajectory()$Eprob1)})

output$pextinct1b <- renderText({#probability of extinction for stratum 1, where there are 2 strata
  if(is.null(input$Na0)) {return(NULL)}
  if(input$States==1) {return(NULL)}
  paste("Probability of extinction for site 1: ",trajectory()$Eprob1)})

output$pextinct2 <- renderText({#probability of extinction for stratum 2
  if(is.null(input$Na0)) {return(NULL)}
  if(input$States==1) {return(NULL)}
  paste("Probability of extinction for site 2: ",trajectory()$Eprob2)})

output$pextinctm <- renderText({#probability of fextinction for metapopulation
  if(input$States==1) {return(NULL)}
  if(is.null(input$Na0)) {return(NULL)}
  paste("Probability of extinction for entire population: ",trajectory()$Eprobm)})
 

#POPULATION PROJECTION PLOTS
output$growthtotal <- renderPlot({
    if(is.null(input$Na0)) {return(NULL)} #suppresses undesireable output while variable values are undefined
    if(is.null(min(trajectory()$Nlow))) {return(NULL)}
    if (as.numeric(input$maxiter) > 1) { #if the user sets iterations > 1, our plots will need 95% confidence limits
      par(xpd=T, mar=par()$mar+c(0,0,0,6))#this sets up thhe margins of the plots
      
       #plots for total population
      #When there is only 1 stratum
      if (input$States==1){
        plot(1:N.year,trajectory()$Nave,ylab="N",xlab="Year",
             main=expression(paste("Mean projected male abundance ",symbol("\261")," 95% prediction intervals")),type="l",
             ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K,trajectory()$Nhi)))
      }
      
      #when there are 2 strata, need to set the upper limit of the y-axis based on the sum of N for both strata
      if (input$States==2){
        plot(1:N.year,trajectory()$Nave,ylab="N",xlab="Year",
             main=expression(paste("Mean projected male abundance ",symbol("\261")," 95% prediction intervals")),type="l",
             ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K+max(trajectory()$K2),trajectory()$Nhi)))
      }
      lines(2:N.year,trajectory()$Nhi[2:N.year],lty=5,col="black")#plots the 95% confidence intervals
      lines(2:N.year,trajectory()$Nlow[2:N.year],lty=5,col="black")
      par(new=T)
      
      #adds the plots for total breeding population
      #only 1 stratum
      if (input$States==1){
        plot(1:N.year,trajectory()$Nbave,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
             ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K,trajectory()$Nhi)))
      }
      
      #if there are 2 strata
      if (input$States==2){
        plot(1:N.year,trajectory()$Nbave,ylab="N",xlab="Year",main="",type="l",ann=FALSE,axes=FALSE,col="red",
             ylim=c(min(0,trajectory()$Nlow),max(trajectory()$K+trajectory()$K2,trajectory()$Nhi)))
      }
      lines(2:N.year,trajectory()$Nbhi[2:N.year],lty=5,col="red")
      lines(2:N.year,trajectory()$Nblow[2:N.year],lty=5,col="red")
      
      #adds a reference line for carrying capacity
      if(input$States==1){
        lines(1:N.year,rep(trajectory()$K,N.year),col="blue",lty=5)
      }
      if(input$States==2){
        lines(1:N.year,rep(trajectory()$K+trajectory()$K2,N.year),col="blue",lty=5)
      }
      
      #adds a black reference  line for zero (extinction)
      lines(1:N.year,rep(0,N.year),col="black",lty=3)
      
      legend(105,max(trajectory()$Nhi),c("Total N", "Breeding N","K"), col=c("black","red","blue"),lty = 1)
      par(mar=c(5, 4, 4, 2) + 0.1)#resets the margins to the typical for R, iirc
    }
    
    #Repeats the above code for the case where the user sets iterations to 1.  In this case there are no 95% confidence limits
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

#repeats the above code, but this does the population plots for site 1 only if there are 2 strata
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

#now we make the plots for stratum 2
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
