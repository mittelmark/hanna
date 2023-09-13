\name{simul$season} %{{{ 
\alias{simul_season} 
\alias{simul$season} 
\title{ Create matches for everyone against everyone using the given model. } 
\description{ 
  This function creates pairings for a tournament where in every round. 
  The actual match will give chances based on a certain amount of tokens in dependence  
  of the given model. 
} 
\usage{ `simul$season(x,token=rep(length(x),length(x)),model="null", 
                           min.value=4,memory=NULL,memory.length=1, game.prob=NULL, season="season")` } 
\arguments{ 
  \item{x}{ vector of teams } 
  \item{token}{ vector of token for each team, which might influence the match outcone, depending on the given model, defaults: 5 } 
  \item{model}{ the model given as string, possible values are 'null', 'chance','gain' or 'last', default: 'null' } 
  \item{min.value}{ for the model 'last' how low is the minimal value for each team, default: 4} 
  \item{memory}{optional vector of last results for each agent} 
  \item{memory.length}{how many of last results should be stored, default: 1} 
  \item{season}{type of season either regular - "season" or "random" for random pairings, default: "season"} 
} 
\details{ 
    This function allows you to create matches for all against all in a season and performs the matches 
    based on the given model and the current number of tokens for each team. 
} 
\value{list with three components:  
  \itemize{ 
     \item{M}{matrix of results where 1 is a win, -1 is a loss and 0 is a draw} 
     \item{token}{vector of current tokens for each team} 
     \item{model}{the choosen model} 
     \item{memory}{list with last results for each agent} 
  } 
} 
\examples{ 
set.seed(123) 
res=simul$season(LETTERS[1:6],model="null")  
res 
} 
 
