library(dplyr)
league_size <- 7
league_duration <- 38
team_minscore <- 10
team_maxscore <- 100
seasons_simulated <- 300

# Generates random scores ("gameweek points") for all teams,
# designating team 1 as AVERAGE
generate_round_scores <- function(n = league_size, minscore = team_minscore, maxscore = team_maxscore){
  scores <- c(0)
  for (i in 1:n){
    scores <- append(scores, sample(minscore:maxscore,1))
  }
  # Team 1 will always be the AVERAGE team
  scores[1] <- round(mean(scores[-1]))
  return(scores)
}

# Calculates head-to-head points based on each team's score
match_points <- function(team_a, team_b){
  if (team_a == team_b){
    return (c(1,1))
  } else if (team_a > team_b){
    return (c(3,0))
  } else {
    return (c(0,3))
  }
}

# Generates a set of all permutations of fixtures given n teams
# to be pulled from when generating the actual head-to-head rounds
possible_fixtures <- function(n_teams){
  # This will only make sense for even numbers of teams
  if (n_teams %% 2 != 0) return(NA)
  
  n_pairs <- round(n_teams/2)
  
  # Make a list of all possible team pairings in the league
  possible_pairs <- t(combn(1:n_teams,2))
  
  # Empty array of appropriate size to be populated in the loop
  possible_combos <- array(dim = c(n_teams-1, n_pairs, 2))
  
  # The total number of permutations will be n-1
  # since each team can face every other team but not itself
  for (i in 1:n_teams-1){
    
    # For each permutation, we will generate pairwise matchups until all teams are used
    for (j in 1:n_pairs){
      
      # We will pick a pair from the list of possible pairs that has not been used
      # To be a valid pair, neither team in the pair can be in any pairs already used
      # in the current permutation. When we have added a pair to the current permutation
      # the same pairing cannot be used in any permutation again. We therefore remove it
      # from the list of possible pairings.
      n_unpicked_pairs <- length(possible_pairs[,1])
      for (k in 1:n_unpicked_pairs){
        #print(possible_combos[i,j,1])
        if (anyNA(possible_combos[i,j,1])){
          if (!(possible_pairs[k,1] %in% possible_combos[i,,] || possible_pairs[k,2] %in% possible_combos[i,,])){
            possible_combos[i,j,] <- possible_pairs[k,]
            possible_pairs <- possible_pairs[-k,,drop=FALSE]
          }
        }
      }
    }  
  }
  return(possible_combos)
}

# Calculate points for all teams in a round given fixtures and scores per team
calc_round_points <- function (fixtures, round_scores) {
  points <- vector(mode="integer", length=length(fixtures[,1])*2)
  for (i in 1:length(fixtures[,1])) {
    match_points = calc_match_points(round_scores[fixtures[i,1]], round_scores[fixtures[i,2]])
    points[fixtures[i,1]] = match_points[1]
    points[fixtures[i,2]] = match_points[2]
  }
  return(points)
}

# Get list of all possible fixtures for n teams
season_fixtures <- function(n_teams = league_size+1, n_rounds = league_duration){
  pf <- possible_fixtures(n_teams)
  
  # Generate an empty array of appropriate size
  fixtures <- array(dim = c(n_rounds, length(pf[1,,1]), 2))
  for (round in 1:n_rounds){
    # Since our league probably has more rounds than the list of possible permutations,
    # we iterate over the same list multiple times until the league is filled
    i <- 1 + round %% length(pf[,1,1])
    fixtures[round,,] <- pf[i,,]
  }
  return (fixtures)
}

# Puts all together and create an entire season with score, points and position (rank)
# for each team for each round. 
season <- function(n_teams = league_size+1, n_rounds = league_duration){
  df <- data.frame(team=integer(), 
                   round=integer(), 
                   pos=integer(), 
                   points=integer(), 
                   score=integer())
  fixtures <- season_fixtures()
  score <- matrix(nrow=league_duration, ncol=n_teams)
  points <- matrix(nrow=league_duration, ncol=n_teams)
  for (round in 1:league_duration){
    score[round,] <- t(generate_round_scores())
    points[round,] <- t(calc_round_points(fixtures[round,,], score[round,]))
    for (team in 1:length(score[round,])) {
      if (round == 1) {
        total_score <- score[round,team]
        total_points <- points[round,team]
      } else {
        total_score <- df[df$team==team & df$round==round-1, "score"]  + score[round,team]
        total_points <- df[df$team==team & df$round==round-1, "points"] + points[round,team]
      }
      df <- rbind(df, data.frame(team=team, 
                                 round=round, 
                                 pos=NA, 
                                 points=total_points, 
                                 score=total_score))
    }
    
    # Use custom function rank2 to generate league ranks for the round
    # based on total points and secondarily by rank
    df$pos[df$round==round] <- rank2(df[df$round==round,],"points","score","min")[["rr"]]
  }
  return(df)
}

# Custom ranking function shamelessly stolen from stackoverflow because ordering is hard.
rank2 <- function(df, key1, key2, ties.method) {
  average <- function(x) mean(x)
  random <- function(x) sample(x, length(x))
  df$r <- order(order(-df[[key1]], -df[[key2]]))
  group_by_(df, key1, key2) %>% mutate(rr = get(ties.method)(r))  
}

# Simulate a bunch of season, only keeping final positions for AVERAGE (team1, remember)
simulate_average <- function(n_teams = league_size + 1, 
                             n_rounds = league_duration, 
                             n_iterations = seasons_simulated){
  df <- data.frame(final_pos=integer())
  for (season in 1:n_iterations){
    df_season <- season(n_teams=n_teams, n_rounds=n_rounds)
    df <- rbind(df, data.frame(final_pos = df_season$pos[df_season$round == n_rounds
                                                        & df_season$team == 1]))
  }
  return(df)
}

# Results
df <- simulate_average()
summary(df)