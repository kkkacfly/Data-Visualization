library(shiny)

# Define UI for application that plots NHANES dataset
shinyUI(fluidPage(
  img(src = "premier_league_logo.png",height = 45, width = 82.5),
  # Application title
  titlePanel("36721 Project II"),
  # title = "Premier League 2011-12 Top 5 Team Stats",
  
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("rounds",
                  "Rounds:",
                  min = 1,
                  max = 38,
                  value = 2,
                  step = 1),
      
      # team selection option
      selectInput("Team", label = h4("Top 5 Teams"), 
                  choices = list("Manchester City" = 1, 
                                 "Manchester United" = 2,
                                 "Arsenal" = 3, 
                                 "Tottenham Hotspur" = 4, 
                                 "Newcastle United" = 5),
                                  selected = 1),
      
      # attack info selection option
      selectInput("AttStat", label = h4("Attack Stats"), 
                  choices = list("total goals" = 1, 
                                 "goals from inside box" = 2,
                                 "goals from outside box" = 3, 
                                 "headed goals" = 4, 
                                 "left foot goals" = 5,
                                 "right foot goals" = 6,
                                 "goals from substitutions" = 7,
                                 "assists" = 8),selected = 1),
      
      # defense info selection option 
      selectInput("DefStat", label = h4("Defense Stats"), 
                  choices = list("total duels won" = 1, 
                                 "aerial duels won" = 2,
                                 "ground duels won" = 3, 
                                 "tackles won" = 4, 
                                 "total clearances" = 5,
                                 "total blocks" = 6,
                                 "total fouls conceded" = 7,
                                 "yellow cards" = 8),selected = 1),
      
      # trends
      checkboxInput("trends", label = "show trends", value = FALSE),
      
      # general description
      h3("Description"),
      p("This app analyzess the statistics of the top 5 teams in Premier League
         during season 2011 - 12 from three aspects: attack, defense, and 
         attack-defense correlationships."),
      br(),
      helpText(p("The source of the data could be found:", a(href = 
                "http://datahub.io/dataset/uk-premier-league-match-by-match-2011-2012",
                "here.", target = "_blank"))),
      helpText(p("This is developed by",span("Xuan Li", style = "color:blue"),
                 "(xuanli1), ", "please contact", a(href = "mailto:xuanli1@andrew.cmu.edu",
                 "me", target = "_blank"), "if you feel this project is cool!"))
    ),
    
    
    # Show the generated scatterplot
    mainPanel(
      tabsetPanel(
        tabPanel("Attack", plotOutput("attack")), 
        tabPanel("Defense", plotOutput("defense")), 
        tabPanel("Correlation Analysis", plotOutput("correlation"))
      )
    )
  )
))