
# loading essential packages
library(shiny)
library(tidyverse)
library(stats)
library(RColorBrewer)

# connecting to google sheets to save input data
source('global.R')

# loading plotting functions
source('01_functions.R')

# Define UI for application
ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
   # Application title
   titlePanel("Examining Plots"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
           
      sidebarPanel(
              h3('Instructions and Controls:'),
              HTML("<p>
              To the side have simulated data of 100 people's favorite food for dinner: Tacos, Burritos, or Nachos. 
              Visually estimate the ratio of Taco to Burrito fans. 
              <p> For example, if the visualization displays twice as many tacos as burritos, guess 2. If there are half as many guess 0.5. 
              Decimals are encouraged! - Results are posted below the visual after submitting and results are stored"),
              
              # conditional panel, only show guess option once the app has been started
              conditionalPanel(
                      # only display once the action button has been hit once
                      condition = "input.button > 0",
                      # allow users to guess! 
                      numericInput('guess', 'Guess!', NA, 
                                   min = 0, max = 100, step = 1,
                                   width = 100)),
              # submit button
              actionButton("button", "Begin/Submit")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h3('Visualization:'),
         #This is a tutorial aiming to highlight the differences in interpretibility 
         # effectiveness between different
         #visualization tecniques. <p>

         # show plot
         plotOutput("distPlot"),
         # print text output 
         textOutput('prev_result'),
         HTML("<p><p><p>
                   Made by <a href='https://mattkcole.com/'>Matt Cole</a>
                   <p>
                   <a href='https://mattkcole.com/2019/01/17/pie-charts-what-s-the-big-deal/'>Blog post here</a>
                   <p>
                   <a href='https://mattkcole.com/about/'>Contact</a>
                   <p>
                   All data (time, guess, correct ratio) stored by this app will be open sourced for all whom are interested!"
              )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
p = NA
 # p = rbeta(n = 1, shape1 = 1, shape2 = 1)
 # p = gtools::rdirichlet(1, c(1,1,1)) %>%
 #         as.vector()
dat = NA
 # dat = sample(0:2, 100, prob = p, replace = TRUE) 

        # observe input button being hit
        observeEvent(input$button,{
                
                # compute the actual correct ratio
                correct_ans = sum(dat == 0) / sum(dat == 1)
                
                # when the input button has been hit at least once, display the correct ratio
                if(input$button > 0){
                        print(paste('Actual Taco:Burrito Ratio:', correct_ans, 'Guess:', input$guess))
                }
                
                # prepare the data for storing in google sheets
                l = list(time = as.character.Date(Sys.time()),
                         tacos = sum(dat == 0),
                         burritos = sum(dat == 1),
                         nachos = sum(dat == 2),
                         guess = input$guess,
                         user = 'prod') %>%
                        as_data_frame()
                
                # add the row of data to google sheets
                gs_add_row(ss, ws = 1, input = l, verbose = TRUE)
                
                xyz = paste('Actual Taco:Burrito Ratio:', round(correct_ans,2), 
                            'Guess:', input$guess)
                # this block collects and displays the most previous response/correct answer
                output$prev_result <- renderText({
                        # this makes sure the application waits for the button to be hit
                        input$button
                        # below is passed as output$prev_result
                        xyz
                        })

                # a rdirichlet is used to sample p's
                # note, alpha is 1.5,1.5,1.5 to reduce but not eliminate extreeme high and low p's
                p <<- gtools::rdirichlet(n = 1, alpha = c(1.5,1.5,1.5)) %>%
                        as.vector()
                
                #dat = rbinom(n = 1000, size = 1, prob = p) 
                dat <<- sample(0:2, 100, prob = p, replace = TRUE)
                
                
                output$distPlot <- renderPlot({
                        
                        # randomly select, a pie chart or a bar chart?
                        if(rbinom(1,1,0.5) == 1){
                                return(make_pie(dat))
                        }else{
                                return(make_bar(dat) )
                        }
                })
        } 
                     )
   

}

# Run the application 
shinyApp(ui = ui, server = server)

