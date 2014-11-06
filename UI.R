shinyUI( #this function sets up the user interface
 
 #tabsetPanel sets up a tab-style page.   This tab is used for inputs 
tabsetPanel(tabPanel("Input",
 fluidPage(  #shiny function for setting up a page of information
   tags$head( #this provides formatting information for text boxes
     tags$style(type="text/css", "#States {width: 50px; }"),
     tags$style(type="text/css", "#Mortality {width: 50px; }"),
     tags$style(type="text/css", "#Na0 {width: 50px; }"),
     tags$style(type="text/css", "#Ns0 {width: 50px; }"),
     tags$style(type="text/css", "#K {width: 50px; }")
   ),
   fluidRow( #on shiny pages, objects are set up in rows.  The first row acacepts input on the number of breeding sites
      #this input is a pulldown menu, the object type is "selectInput"
     column(3,selectInput("States", label = h3("Breeding sites"), 
                          choices = list("One" = 1, "Two" = 2),selected = 1))),
   
  titlePanel("Site One Input"), #panels are sections on the page
   
   
    mainPanel(           
      progressInit(),
    
    tags$head(
      tags$style(type='text/css', ".span4 { max-width: 1000px;}")
    ),
    
    
#in this row the user selects the number of mortality sources, sets the initial population sizes,
#and carrying capacity
    fluidRow( 

    column(3,selectInput("Mort", label = h3("Mortality Sources"), 
                choices = list("One" = 1, "Two" = 2),
                               selected = 1)),
   column(3, offset=0.5,
           uiOutput("Na0")
    ),
    column(3, 
           uiOutput("Ns0")
    ),
   column(3, 
          uiOutput("K")
   )
    ),


 
   tags$head( #more textbox formatting
     tags$style(type="text/css", "#Rs {width: 50px; }"),
     tags$style(type="text/css", "#Rf {width: 50px; }"),
     tags$style(type="text/css", "#Rbf {width: 50px; }"),
     tags$style(type="text/css", "#ys {width: 50px; }"),
     tags$style(type="text/css", "#E {width: 50px; }"),
     tags$style(type="text/css", "#f {width: 50px; }"),
     tags$style(type="text/css", "#ns {width: 50px; }"),
     tags$style(type="text/css", "#sfl {width: 50px; }"),
     tags$style(type="text/css", "#sb {width: 50px; }"),
     tags$style(type="text/css", "#WSY {width: 50px; }"),
     tags$style(type="text/css", "#WSA {width: 50px; }"),
     tags$style(type="text/css", "#ChMort2 {width: 50px; }"),
     tags$style(type="text/css", "#fMort2 {width: 50px; }"),
     tags$style(type="text/css", "#sMort2 {width: 50px; }"),
     tags$style(type="text/css", "#Psi12 {width: 50px; }"),
     tags$style(type="text/css", "#Rsa {width: 50px; }"),
     tags$style(type="text/css", "#Rfa {width: 50px; }"),
     tags$style(type="text/css", "#Rbfa {width: 50px; }"),
     tags$style(type="text/css", "#ya {width: 50px; }"),
     tags$style(type="text/css", "#Ea {width: 50px; }"),
     tags$style(type="text/css", "#fa {width: 50px; }"),
     tags$style(type="text/css", "#nsa {width: 50px; }"),
     tags$style(type="text/css", "#sfla {width: 50px; }"),
     tags$style(type="text/css", "#sba {width: 50px; }"),
     tags$style(type="text/css", "#WASY {width: 50px; }"),
     tags$style(type="text/css", "#WSAa {width: 50px; }"),
     tags$style(type="text/css", "#ChMort2a {width: 50px; }"),
     tags$style(type="text/css", "#fMort2a {width: 50px; }"),
     tags$style(type="text/css", "#sMort2a {width: 50px; }"),
     tags$style(type="text/css", "#Psi12Ja {width: 50px; }"),
     tags$style(type="text/css", "#Psi12J {width: 50px; }"),
     tags$style(type="text/css", "#Psi12A {width: 50px; }")
   ),
   

   fluidRow(# a row that just contains a label
    
    column(5, h4("Adults Site 1"),
                  helpText(""))
   ),

#now come all the rows for user input of demographic data, site 1 adults
   fluidRow(
     column(3, 
            uiOutput("ya")
     ),
     column(3, 
            uiOutput("Ea")
     ),
     column(3, 
            uiOutput("nsa")
     )
   ),
     
   fluidRow(   
     column(3, 
            uiOutput("Rfa")
     ),
     column(3, 
            uiOutput("Rbfa")
     ),
     column(3, 
            uiOutput("Rsa")
     ),
     column(3, 
            uiOutput("fa")
     )
    ),

   fluidRow(   
     column(3, 
            uiOutput("sfla")
     ),
     column(3, 
            uiOutput("sba")
     ),
     column(3, 
            uiOutput("WASY")   
     ),
     column(3, 
            uiOutput("WSAa")   
     ),
   conditionalPanel( #conditional panels (page sections) only appear if a variable take a particular value
                    # in this case, if the user makes the number of sites = 2, the input for movement rates appears
    condition = "input.States == 2",
    column(3, 
           uiOutput("Psi12A")   
    ),
    column(3, 
           uiOutput("Psi12Ja")   
    )
   )
  ),
# Only show this panel if Two mortality sources are selected
  conditionalPanel(
    condition = "input.Mort == 2",
  fluidRow(
    column(3, 
           uiOutput("ChMort2a")   
    ),  
    column(3, 
           uiOutput("fMort2a")   
    ),
    column(3, 
           uiOutput("sMort2a")   
    )
  )
  ),

#inputs for subadults for site 1
  fluidRow(
    column(5, h4("Subadults Site 1"),
         helpText("")
    )
  ),
fluidRow(
  column(3, 
         uiOutput("ys")
  ),
  column(3, 
         uiOutput("E")
  ),
  column(3, 
         uiOutput("ns")
  )
),

fluidRow(   
  column(3, 
         uiOutput("Rf")
  ),
  column(3, 
         uiOutput("Rbf")
  ),
  column(3, 
         uiOutput("Rs")
  ),
  column(3, 
         uiOutput("f")
  )
),

fluidRow(   
  column(3, 
         uiOutput("sfl")
  ),
  column(3, 
         uiOutput("sb")
  ),
  column(3, 
         uiOutput("WSY")   
  ),
  column(3, 
         uiOutput("WSA")   
  ),
  conditionalPanel(
    condition = "input.States == 2",
    column(3, 
           uiOutput("Psi12J")   
    )

  )
),
# Only show this panel if Two mortality sources are selected
conditionalPanel(
  condition = "input.Mort == 2",
  fluidRow(
    column(3, 
           uiOutput("ChMort2")   
    ),  
    column(3, 
           uiOutput("fMort2")   
    ),
    column(3, 
           uiOutput("sMort2")   
    )
  )
  ),
  column(3,actionButton("reset_button", "Reset Defaults Site 1")),

#adds a blank row for spacing
fluidRow(
  column(5, h1(""),
         helpText(""))),    

         
conditionalPanel( #this panel contains the inputs for site 2, and only appears if the user sets number of sites = 2
      condition = "input.States == 2",        
         
fluidRow(#blank row
  column(5, h1(""),
         helpText(""))),
fluidRow(#label row
  column(12, h1("Site Two Input"),
         helpText(""))),
fluidRow(
  column(5, h3(""),
         helpText(""))),
           
       
             
             tags$head(
          
               tags$style(type="text/css", "#Mortality2 {width: 50px; }"),
               tags$style(type="text/css", "#Na02 {width: 50px; }"),
               tags$style(type="text/css", "#Ns02 {width: 50px; }"),
               tags$style(type="text/css", "#K2 {width: 50px; }")
             ),
             fluidRow(

               column(3,selectInput("Mort2", label = h3("Mortality Sources"), 
                                    choices = list("One" = 1, "Two" = 2),
                                    selected = 1)),
               column(3, offset=0.5,
                      uiOutput("Na02")
               ),
               column(3, 
                      uiOutput("Ns02")
               ),
               column(3, 
                      uiOutput("K2")
               )
             ),
             
             
             
             tags$head(
               tags$style(type="text/css", "#Rs2 {width: 50px; }"),
               tags$style(type="text/css", "#Rf2 {width: 50px; }"),
               tags$style(type="text/css", "#Rbf2 {width: 50px; }"),
               tags$style(type="text/css", "#ys2 {width: 50px; }"),
               tags$style(type="text/css", "#E2 {width: 50px; }"),
               tags$style(type="text/css", "#f2 {width: 50px; }"),
               tags$style(type="text/css", "#ns2 {width: 50px; }"),
               tags$style(type="text/css", "#sfl2 {width: 50px; }"),
               tags$style(type="text/css", "#sb2 {width: 50px; }"),
               tags$style(type="text/css", "#WSY2 {width: 50px; }"),
               tags$style(type="text/css", "#WSA2 {width: 50px; }"),
               tags$style(type="text/css", "#ChMort22 {width: 50px; }"),
               tags$style(type="text/css", "#fMort22 {width: 50px; }"),
               tags$style(type="text/css", "#sMort22 {width: 50px; }"),
               tags$style(type="text/css", "#Psi21 {width: 50px; }"),
               tags$style(type="text/css", "#Rsa2 {width: 50px; }"),
               tags$style(type="text/css", "#Rfa2 {width: 50px; }"),
               tags$style(type="text/css", "#Rbfa2 {width: 50px; }"),
               tags$style(type="text/css", "#ya2 {width: 50px; }"),
               tags$style(type="text/css", "#Ea2 {width: 50px; }"),
               tags$style(type="text/css", "#fa2 {width: 50px; }"),
               tags$style(type="text/css", "#nsa2 {width: 50px; }"),
               tags$style(type="text/css", "#sfla2 {width: 50px; }"),
               tags$style(type="text/css", "#sba2 {width: 50px; }"),
               tags$style(type="text/css", "#WASY2 {width: 50px; }"),
               tags$style(type="text/css", "#WSAa2 {width: 50px; }"),
               tags$style(type="text/css", "#ChMort2a2 {width: 50px; }"),
               tags$style(type="text/css", "#fMort2a2 {width: 50px; }"),
               tags$style(type="text/css", "#sMort2a2 {width: 50px; }"),
               tags$style(type="text/css", "#Psi21Ja {width: 50px; }"),
               tags$style(type="text/css", "#Psi21J {width: 50px; }"),
               tags$style(type="text/css", "#Psi21A {width: 50px; }")
             ),
             
             
             fluidRow(
               
               column(5, h4("Adults Site 2"),
                      helpText(""))
             ),
             fluidRow(
               column(3, 
                      uiOutput("ya2")
               ),
               column(3, 
                      uiOutput("Ea2")
               ),
               column(3, 
                      uiOutput("nsa2")
               )
             ),
             
             fluidRow(   
               column(3, 
                      uiOutput("Rfa2")
               ),
               column(3, 
                      uiOutput("Rbfa2")
               ),
               column(3, 
                      uiOutput("Rsa2")
               ),
               column(3, 
                      uiOutput("fa2")
               )
             ),
             
             fluidRow(   
               column(3, 
                      uiOutput("sfla2")
               ),
               column(3, 
                      uiOutput("sba2")
               ),
               column(3, 
                      uiOutput("WASY2")   
               ),
               column(3, 
                      uiOutput("WSAa2")   
               ),
               conditionalPanel(
                 condition = "input.States == 2",
                 column(3, 
                        uiOutput("Psi21A")   
                 ),
                 column(3, 
                        uiOutput("Psi21Ja")   
                 )
               )
             ),
             # Only show this panel if Two mortality sources are selected
             conditionalPanel(
               condition = "input.Mort2 == 2",
               fluidRow(
                 column(3, 
                        uiOutput("ChMort2a2")   
                 ),  
                 column(3, 
                        uiOutput("fMort2a2")   
                 ),
                 column(3, 
                        uiOutput("sMort2a2")   
                 )
               )
             ),
             
             fluidRow(
               column(5, h4("Subadults Site 2"),
                      helpText("")
               )
             ),
             fluidRow(
               column(3, 
                      uiOutput("ys2")
               ),
               column(3, 
                      uiOutput("E2")
               ),
               column(3, 
                      uiOutput("ns2")
               )
             ),
             
             fluidRow(   
               column(3, 
                      uiOutput("Rf2")
               ),
               column(3, 
                      uiOutput("Rbf2")
               ),
               column(3, 
                      uiOutput("Rs2")
               ),
               column(3, 
                      uiOutput("f2")
               )
             ),
             
             fluidRow(   
               column(3, 
                      uiOutput("sfl2")
               ),
               column(3, 
                      uiOutput("sb2")
               ),
               column(3, 
                      uiOutput("WSY2")   
               ),
               column(3, 
                      uiOutput("WSA2")   
               ),
               conditionalPanel(
                 condition = "input.States == 2",
                 column(3, 
                        uiOutput("Psi21J")   
                 )
               )
             ),
             # Only show this panel if Two mortality sources are selected
             conditionalPanel(
               condition = "input.Mort2 == 2",
               fluidRow(
                 column(3, 
                        uiOutput("ChMort22")   
                 ),  
                 column(3, 
                        uiOutput("fMort22")   
                 ),
                 column(3, 
                        uiOutput("sMort22")   
                 )
               )
             ),
             column(3,actionButton("reset_button2", "Reset Defaults Site 2"))
        )  
           ) #mainpanel
         )#fluidpage
        )#tabpanel

,


#now create a new tab for the modeling outputs
  tabPanel("Output",
         mainPanel(
           #this first row is an input: the number of simulation runs
           fluidRow(
             column(3,selectInput("maxiter", label = h6("Iterations"), 
                                  choices = list("1" = 1, "10" = 10,"100"=100,"1000"=1000),
                                  selected = 10))
                                      )
           ,
           fluidRow(),#blank row

#Now comes the displayed output.   First, the lambda and exinct probabilities.
#The results for site 2 won't appear in the output if the user has selected 1 site
         #  textOutput("errcheck"), used in debugging, not currently displayed

           textOutput("lambda1"), #growth rate for site 1, if 1 site is selected
         textOutput("lambda1b"), #growth rate for site 1, if 2 sites are selected
         textOutput("pextinct1"), #exctinction probability for site 1, if 1 site is selected
         textOutput("pextinct1b"), #extinction probability for site 1, if 2 sites are selected
         textOutput("lambda2"), #growth rate for site 2
         textOutput("pextinct2"), #extinction prob. for site 2
         textOutput("lambdatot"), #growth rate for metapopulation
         textOutput("pextinctm"), #extinction probability for metapopulation

#next come the graphs
           plotOutput("growthtotal"),
         plotOutput("growth1"),
         plotOutput("growth2")
           )

   ),

#a new tab containing the development notes/instructions
   tabPanel("Notes",
            fluidPage(
              titlePanel("Current Development Notes"),
              mainPanel(
                fluidRow(
                  column(12, h3(""),
                         helpText("The model is for the male population, fecundity is per male."))
                ),
                fluidRow(
                  column(12, h3(""),
                         helpText("The model assumes a pre-breeding census and uses two stage classes, subadults which
                                    are first-time breeders and adults which are in their second or later potential
                                    breeding season.   On the input screen, vital rates under 'Subadults' means that
                                    the parents are subadults.  In that case, juvenile winter survival and juvenile
                                    movement rates would be rates for young raised by a subadult male parent.  Subadult
                                    breeding survival means survival of a first-time breeding male parent during the
                                    breeding season.  Subadult winter survival means the survival of the male parent after 
                                    its first breeding season but before its second season, at which point it's an adult.
                                    Subadult movement rate is the probability of a first-time breeder dispersing after
                                    its first breeding season."))
                ),
                fluidRow(
                  column(12, h3(""),
                         helpText("Carrying capacity (K) is the total number of male territories per site per year.
                                   Any males in excess of this number will have 0 fecundity.  The population grows
                                   exponentially until it reaches K"))
                  ),
                fluidRow(
                  column(12, h3(""),
                         helpText("Baseline survival is the survival rate of all hazards except the management 
                                   variable of interest (e.g., road, crab).  If you are not examining the effects
                                   of a mortality factor, leave 'Mortality Sources' set to 1.  If you are examining
                                  the effects of a mortality factor, set 'Mortality Sources' to 2.   In that case,
                                   baseline survival is survival as if the mortality factor did not exist.  Then 
                                   'survival of hazard' is the chances of not being killed by the mortality factor given
                                    that an individual has survived all other potential threats.  For example, if survival
                                    in the absence of road mortality is 0.9, but then 20% of birds are killed in the road,
                                     set baseline survival to 0.9 and survival of hazard to 0.8.  This leads to a total
                                     breeding season survival 0f 0.8 x 0.9 = 0.72"))
                ),
                
                fluidRow(
                  column(12, h3(""),
                         helpText("Users have access to probabilities of second nest attempts.  Users do not currently 
                                    have access to probabilities of third nest attempts, which are modeled as percentages
                                     of probabilities of second nest attempts, with the percentages coming from eastern
                                      FL Panhandle data given below.  Probabilities of nest attempts after the third are
                                      modeled as zero.
                                  
                                      Prob. renest after failure of second nest = 0.33*Prob. renest after failure of first nest.
                                      Prob. renest after failure of brood from second nest = 0.625*Prob. renest after failure
                                                               of brood from first nest.
                                      Prob. renest after fledging brood from second nest = 0.1*prob. renest after fledging
                                        brood from first nest."))
                ),
                
                fluidRow(
                  column(12, h3(""),
                         helpText("The user does not currently have access to variances in vital rates or covariances
                                   among them.  This can be implemented in the future.  Currently, stochasticity in 
                                    survival rates is modeled by assuming a coefficient of variation (SD/mean) of 0.06,
                                    which is arbitrary but leads to reasonable variability for plovers.  When available we
                                    will use standard errors from local studies.

                                    Fecundity rates are modeled as 90% correlated between adults and subadults within sites but
                                    0% correlated between sites, except there is a 5% chance each year of total reproductive
                                    failure for all sites (e.g., due to weather).  

                                    Breeding season adult and subadult survival are modeled as 90% correlated within sites
                                    but 0% correlated among them.  Winter survival rates are modeled as 90% correlated 
                                    between sites.  Fecundity rates are not currently modeled as correlated
                                    with survival.  The above variance-covariance structures are arbitrary but can be made
                                    more realistic in the future based on local studies and literature."))
                )
              )
            )
            )
            
) #tabsetPanel
) #shinyUI
