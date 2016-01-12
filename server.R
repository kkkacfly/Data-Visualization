# Name: Xuan Li
# Andrew ID: xuanli1
# Project 2 for 36721

# -----------------------------------------------------------------------
# notice:
# dear TA: please install the following packages before running the program
# Thank!!!
# -----------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(tidyr)
library(data.table)

# --------------------predefined plotting functons ----------------------#

# plotting function for trend analysis for attack & defense
trendPlot <- function(vector_x,vector_y){
  a <- cbind(vector_x, vector_y)
  a <- data.frame(a)
  
  q1 <- ggplot(a, environment = environment(),aes(x=vector_x, y=vector_y)) +
        geom_step(data=a, mapping=aes(x=vector_x, y=vector_y)) +
        # geom_step(data=a, mapping=aes(x=vector_x, y=vector_y), direction="vh", linetype=3) +
        geom_point(data=a, mapping=aes(x=vector_x, y=vector_y), color="red") +
        ylab("Chosen Stats") + xlab("Number of Rounds") + theme_bw() +
        scale_x_discrete(breaks=c(1:max(vector_x)),limits = c(1:max(vector_x))) +
        theme(axis.text.y = element_text(size = rel(1.2)),
              axis.text.x = element_blank(),
              axis.title.x = element_text(size = rel(1.5)),
              axis.title.y = element_text(size = rel(1.5)),
              plot.margin=unit(c(1, 1, 2, 0.5), "lines")) +
 
        # geom_smooth(method=lm,se=FALSE)  
        stat_smooth(size=1, alpha = 0.05)
  
  print(q1)
}

# plotting function for no-trend analysis for attack & defense
noTrendplot <- function(vector_x,vector_y){
  a <- cbind(vector_x, vector_y)
  a <- data.frame(a)
  
  q1 <- ggplot(a, environment = environment(),aes(x=vector_x, y=vector_y)) +
    geom_step(data=a, mapping=aes(x=vector_x, y=vector_y)) +
    # geom_step(data=a, mapping=aes(x=vector_x, y=vector_y), direction="vh", linetype=3) +
    geom_point(data=a, mapping=aes(x=vector_x, y=vector_y), color="red") +
    ylab("Chosen Stats") + xlab("Number of Rounds") + theme_bw() +
    scale_x_discrete(breaks=c(1:max(vector_x)),limits = c(1:max(vector_x))) +
    theme(axis.text.y = element_text(size = rel(1.2)),
          axis.text.x = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
          plot.margin=unit(c(1, 1, 2, 0.5), "lines"))
  
  print(q1)
}

# plotting function for Correlation Analysis
easyplot <- function(temp_vector_x,temp_vector_y){
  
  a <- cbind(temp_vector_x, temp_vector_y)
  a <- data.frame(a)
  
  a$temp_vector_x <- as.factor(a$temp_vector_x)
  
  
  q <- ggplot(a, aes(x = a$temp_vector_x, y = a$temp_vector_y, 
                     colour = factor(a$temp_vector_x)),
                     environment = environment())
  q <- q + geom_boxplot(outlier.colour = NA, width  =0.5) + theme(legend.position="none")
  q <- q + ylab("Defense Stats") + xlab("Attack Stats") + theme_bw()
  q <- q + theme(legend.position="none", 
                 axis.text.x = element_text(size = rel(1.5)),
                 axis.text.y = element_text(size = rel(1.2)),
                 axis.title.x = element_text(size = rel(1.5)),
                 axis.title.y = element_text(size = rel(1.5)))
  print(q)
  
}


# ------------------------ raw data processing -------------------------- #
# read in data
totaldata <- read.csv("Premier League 2011-12 Match by Match1.csv")
# transfer to table
totaldata <- data.table(totaldata)
# find seasonal team info
teamInfo <- totaldata[, lapply(.SD, sum), by=list(Team)]
# find team info by match
matchInfo <- totaldata[, lapply(.SD, sum), by=list(Date,Team)]
matchInfo <- matchInfo[order(Team,Date)]

# ------------------------------------------------------------
# attack info by round for each team
attack_by_round <- matchInfo[,.SD,.SDcols=c(1,2,16,31,35,39,43,47,76,79)]
attack_by_round <- attack_by_round[order(Date)]

# only consider the top 5 teams
attack_1 <- filter(attack_by_round, Team == c("Manchester City"))
attack_1 <- mutate(attack_1,rounds = c(1:38))
attack_2 <- filter(attack_by_round, Team == c("Manchester United"))
attack_2 <- mutate(attack_2,rounds = c(1:38))
attack_3 <- filter(attack_by_round, Team == c("Arsenal"))
attack_3 <- mutate(attack_3,rounds = c(1:38))
attack_4 <- filter(attack_by_round, Team == c("Tottenham Hotspur"))
attack_4 <- mutate(attack_4,rounds = c(1:38))
attack_5 <- filter(attack_by_round, Team == c("Newcastle United"))
attack_5 <- mutate(attack_5,rounds = c(1:38))


# defense info by round for each team
defense_by_round <- matchInfo[,.SD,.SDcols=c(1,2,144,146,148,150,153,157,160,169)]
defesne_by_round <- defense_by_round[order(Date)]

# only consider the top 5 teams
defense_1 <- filter(defense_by_round, Team == c("Manchester City"))
defense_1 <- mutate(defense_1,rounds = c(1:38))
defense_2 <- filter(defense_by_round, Team == c("Manchester United"))
defense_2 <- mutate(defense_2,rounds = c(1:38))
defense_3 <- filter(defense_by_round, Team == c("Arsenal"))
defense_3 <- mutate(defense_3,rounds = c(1:38))
defense_4 <- filter(defense_by_round, Team == c("Tottenham Hotspur"))
defense_4 <- mutate(defense_4,rounds = c(1:38))
defense_5 <- filter(defense_by_round, Team == c("Newcastle United"))
defense_5 <- mutate(defense_5,rounds = c(1:38))


# ---------------------------- attack info ----------------------------- #
# attack info for the total 38 rounds 
attack <- teamInfo[,.SD,.SDcols=c(1,16,31,35,39,43,47,76,79)]
attack <- attack[order(Team)]
# insert ranking
attack[, c("ranking") := c(3,16,19,18,6,7,9,8,1,2,5,12,17,14,13,11,4,10,15,20)]
attack <- attack[order(ranking)]
# only consider the top 5 teams
attack <- filter(attack, ranking == c(1:5))
# names for all the team according to seasonal ranking
names <- attack$Team
names <- as.character(names)

# ---------------------------- defense info ---------------------------- #
# defense info for the total 38 rounds 
defense <- teamInfo[,.SD,.SDcols=c(1,144,146,148,150,153,157,160,169)]
defense <- defense[order(Team)]
# insert ranking
defense[, c("ranking") := c(3,16,19,18,6,7,9,8,1,2,5,12,17,14,13,11,4,10,15,20)]
defense <- defense[order(ranking)]
# only consider the top 5 teams
defense <- filter(defense, ranking == c(1:5))

# ------------------------------- SERVER -------------------------------- #
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a scatterplot. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$attack <- renderPlot({
    # Draw the scatterplot for the specified data subset
    #with(subset(mdata, Player.Surname == input$player),
    #     plot(mdata$Player.Surname, mdata$Time.Played)
    #)
    # define input parameters
    num_of_rounds <- input$rounds
    team <- input$Team
    to_be_plotted_a <- input$AttStat
    
    if(team == 1) {temp_team_a <- attack_1}
    if(team == 2) {temp_team_a <- attack_2}
    if(team == 3) {temp_team_a <- attack_3}
    if(team == 4) {temp_team_a <- attack_4}
    if(team == 5) {temp_team_a <- attack_5}
    
    temp_a <- slice(temp_team_a, 1:num_of_rounds)
    
    if(to_be_plotted_a == 1) 
    { temp_vector_a <- as.vector(temp_a$Goals)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
     }
    
    if(to_be_plotted_a == 2) 
    { temp_vector_a <- as.vector(temp_a$Goals.from.Inside.Box)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
    }
    
    if(to_be_plotted_a == 3) 
    { temp_vector_a <- as.vector(temp_a$Goals.from.Outside.Box)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
    }
    
    if(to_be_plotted_a == 4) 
    { temp_vector_a <- as.vector(temp_a$Headed.Goals)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
    }
    
    if(to_be_plotted_a == 5) 
    { temp_vector_a <- as.vector(temp_a$Left.Foot.Goals)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
    }
    
    if(to_be_plotted_a == 6) 
    { temp_vector_a <- as.vector(temp_a$Right.Foot.Goals)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
    }
    
    if(to_be_plotted_a == 7) 
    { temp_vector_a <- as.vector(temp_a$Goals.as.a.substitute)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
    }
    
    if(to_be_plotted_a == 8) 
    { temp_vector_a <- as.vector(temp_a$Assists)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector_a)}
      else {noTrendplot(vector_x,temp_vector_a)}
    }
    
        
  })
  
  output$defense <- renderPlot({
    # Draw the scatterplot for the specified data subset
    #with(subset(mdata, Player.Surname == input$player),
    #     plot(mdata$Player.Surname, mdata$Time.Played)
    #)
    num_of_rounds <- input$rounds
    team <- input$Team
    to_be_plotted <- input$DefStat
    
    if(team == 1) {temp_team <- defense_1}
    if(team == 2) {temp_team <- defense_2}
    if(team == 3) {temp_team <- defense_3}
    if(team == 4) {temp_team <- defense_4}
    if(team == 5) {temp_team <- defense_5}
    
    temp <- slice(temp_team, 1:num_of_rounds)
    
    if(to_be_plotted == 1) 
    { temp_vector <- as.vector(temp$Duels.won)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
    
    if(to_be_plotted == 2) 
    { temp_vector <- as.vector(temp$Aerial.Duels.won)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
    
    if(to_be_plotted == 3) 
    { temp_vector <- as.vector(temp$Ground.Duels.won)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
    
    if(to_be_plotted == 4) 
    { temp_vector <- as.vector(temp$Tackles.Won)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
    
    if(to_be_plotted == 5) 
    { temp_vector <- as.vector(temp$Total.Clearances)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
    
    if(to_be_plotted == 6) 
    { temp_vector <- as.vector(temp$Blocks)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
    
    if(to_be_plotted == 7) 
    { temp_vector <- as.vector(temp$Total.Fouls.Conceded)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
    
    if(to_be_plotted == 8) 
    { temp_vector <- as.vector(temp$Yellow.Cards)
      vector_x = 1:num_of_rounds
      if(input$trends == TRUE) {trendPlot(vector_x,temp_vector)}
      else {noTrendplot(vector_x,temp_vector)}
    }
        
  })
  
  output$correlation <- renderPlot({
    # Draw the scatterplot for the specified data subset
    #with(subset(mdata, Player.Surname == input$player),
    #     plot(mdata$Player.Surname, mdata$Time.Played)
    #)
    total_rounds <- 38
    team <- input$Team
    to_be_plotted_x <- input$AttStat
    to_be_plotted_y <- input$DefStat
    
    if(team == 1) 
        {temp_team_t_x <-  attack_1
         temp_team_t_y <- defense_1}
 
    if(team == 2) 
        {temp_team_t_x <-  attack_2
         temp_team_t_y <- defense_2}
    
    if(team == 3) 
        {temp_team_t_x <-  attack_3
         temp_team_t_y <- defense_3}
    
    if(team == 4) 
        {temp_team_t_x <-  attack_4
         temp_team_t_y <- defense_4}
    
    if(team == 5) 
        {temp_team_t_x <-  attack_5
         temp_team_t_y <- defense_5}
    
    
    # plot case by case 1 ~ 8 
    if(to_be_plotted_x == 1 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      # quickplot(temp_vector_x,temp_vector_y)
      easyplot(temp_vector_x,temp_vector_y)
    }
      
    if(to_be_plotted_x == 1 & to_be_plotted_y == 2) 
    {  temp_vector_x <- as.vector(temp_team_t_x$Goals)
       temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      # quickplot(temp_vector_x,temp_vector_y)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 1 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
      
    if(to_be_plotted_x == 1 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
     
    
    if(to_be_plotted_x == 1 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
     
    
    if(to_be_plotted_x == 1 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
     
    
    if(to_be_plotted_x == 1 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    
    if(to_be_plotted_x == 1 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
        
    # plot case by case 9 ~ 16 
    if(to_be_plotted_x == 2 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 2 & to_be_plotted_y == 2) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 2 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 2 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 2 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 2 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 2 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 2 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Inside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    
    # plot case by case 17 ~ 24 
    if(to_be_plotted_x == 3 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 3 & to_be_plotted_y == 2) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 3 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 3 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 3 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 3 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 3 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 3 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.from.Outside.Box)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    
    # plot case by case 25 ~ 32 
    if(to_be_plotted_x == 4 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 4 & to_be_plotted_y == 2) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 4 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 4 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 4 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 4 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 4 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 4 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Headed.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    # plot case by case 33 ~ 40 
    if(to_be_plotted_x == 5 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 5 & to_be_plotted_y == 2) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 5 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 5 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 5 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 5 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 5 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 5 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Left.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    # plot case by case 41 ~ 48 
    if(to_be_plotted_x == 6 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 6 & to_be_plotted_y == 2) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 6 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 6 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 6 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 6 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 6 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 6 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Right.Foot.Goals)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    # plot case by case 49 ~ 56 
    if(to_be_plotted_x == 7 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 7 & to_be_plotted_y == 2) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 7 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 7 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 7 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 7 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 7 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 7 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Goals.as.a.substitute)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    # plot case by case 57 ~ 64 
    if(to_be_plotted_x == 8 & to_be_plotted_y == 1) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 8 & to_be_plotted_y == 2) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Aerial.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 8 & to_be_plotted_y == 3) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Ground.Duels.won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 8 & to_be_plotted_y == 4) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Tackles.Won)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 8 & to_be_plotted_y == 5) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Clearances)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 8 & to_be_plotted_y == 6) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Blocks)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 8 & to_be_plotted_y == 7) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Total.Fouls.Conceded)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
    if(to_be_plotted_x == 8 & to_be_plotted_y == 8) 
    { temp_vector_x <- as.vector(temp_team_t_x$Assists)
      temp_vector_y <- as.vector(temp_team_t_y$Yellow.Cards)
      easyplot(temp_vector_x,temp_vector_y)
    }
    
  })
     
})



