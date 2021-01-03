library(rjson)
library(dplyr)
library(openxlsx)
library(data.table)

#Declaring the vectors that will hold each of the variables we are after
game_id <- c()
team <- c()
period_id <- c()
seconds <- c()
player <- c()
start_x <- c()
start_y <- c()
end_x <- c()
end_y <- c()
actiontype <- c()
result <- c()
bodypart <- c()

fixZL <- function(x)
{
  try(
    if(x > 0)
    {
      return(x)
    }, silent = TRUE)
  
  temp <- data.frame(x)
  temp <- temp[is.null(temp)] <- NA
  
  return(temp)
}

get_actiontype <- function(x)
{
  a = "non_action"
  
  if(x$type$name == "Pass")
  {
    a = "pass"  # default
    
    ptype = fixZL(x$pass$type$name)
    height = fixZL(x$pass$height$name)
    
    if(is.na(ptype))
    {
      return(a)
    }
    
    #Determining whether or not the pass was a cross
    if("cross" %in% names(x$pass))
    {
      cross <- TRUE
    }
    else
    {
      cross <- FALSE
    }

    if (ptype == "Free Kick")
    {
      if(height == "High Pass" | cross)
      {
        a = "freekick_crossed"
      }
      else 
      {
        a = "freekick_short"
      }
    }
    else if(ptype == "Corner")
    {
      if(height == "High Pass" | cross)
      {
        a = "corner_crossed"
      }
      else
      {
        a = "corner_short"
      }
    }
    else if(ptype == "Goal Kick")
    {
      a = "goalkick"
    }
    else if(ptype == "Throw-in")
    {
      a = "throw_in"
    }
    else if(cross)
    {
      a = "cross"
    }
    else
    {
      a = "pass"
    }
  }    
  else if(x$type$name == "Dribble")
  {
    a = "take_on"
  }
  else if(x$type$name == "Carry")
  {
    a = "dribble"
  }
  else if(x$type$name == "Foul Committed")
  {
    a = "foul"
  }
  else if(x$type$name == "Duel" & fixZL(x$duel$type$name) == "Tackle")
  {
    a = "tackle"
  }
  else if(x$type$name == "Interception")
  {
    a = "interception"
  }
  else if(x$type$name == "Shot")
  {
    extra_type = fixZL(x$shot$type$name)
    if(extra_type == "Free Kick")
    {
      a = "shot_freekick"
    }
    else if(extra_type == "Penalty")
    {
      a = "shot_penalty"
    }
    else
    {
      a = "shot"
    }
  }
  else if(x$type$name == "Own Goal Against")
  {
    a = "shot"
  }
  else if(x$type$name == "Goal Keeper")
  {
    extra_type = fixZL(x$goalkeeper$type$name)
    
    if(extra_type == "Shot Saved")
    {
      a = "keeper_save"
    }
    else if(extra_type == "Collected" | extra_type == "Keeper Sweeper")
    {
      a = "keeper_claim"
    }
    else if(extra_type == "Punch")
    {
      a = "keeper_punch"
    }
    else
    {
      a = "non_action"
    }
  }
  else if(x$type$name == "Clearance")
  {
    a = "clearance"
  }
  else if(x$type$name == "Miscontrol")
  {
    a = "bad_touch"
  }
  else
  {
    a = "non_action"
  }
  
  return(a)
}



get_result <- function(x)
{
  if(x$type$name == "Pass")
  {
    pass_outcome = fixZL(x$pass$outcome$name)
    if(is.na(pass_outcome))
    {
      return(NA)
    }

    if(pass_outcome %in% c("Incomplete", "Out"))
    {
      r = "fail"
    }
    else if(pass_outcome == "Pass Offside")
    {
      r = "offside"
    }
    else
    {
      r = "success"
    }
  }
  else if(x$type$name == "Shot")
  {
    shot_outcome = fixZL(x$shot$outcome$name)
    if(shot_outcome == "Goal")
    {
      r = "success"
    }
    else if(shot_outcome %in% c("Blocked", "Off T", "Post", "Saved", "Wayward"))
    {
      r = "fail"
    }
    else
    {
      r = "fail"
    }
  }
  else if(x$type$name == "Dribble")
  {
    dribble_outcome = fixZL(x$dribble$outcome$name)
    if(dribble_outcome == "Incomplete")
    {
      r = "fail"
    }
    else if(dribble_outcome == "Complete")
    {
      r = "success"
    }
    else
    {
      r = "success"
    }
  }
  else if(x$type$name == "Foul Committed")
  {
    foul_card = fixZL(x$foul_commited$card$name)
    if("Yellow" %in% foul_card)
    {
      r = "yellow_card"
    }
    else if("Red" %in% foul_card)
    {
      r = "red_card"
    }
    else
    {
      r = "success"
    }
  }
  else if(x$type$name == "Duel")
  {
    duel_outcome = fixZL(x$duel$outcome$name)
    if(duel_outcome %in% c("Lost In Play", "Lost Out"))
    {
      r = "fail"
    }
    else if(duel_outcome %in% c("Success in Play", "Won"))
    {
      r = "success"
    }
    else
    {
      r = "success"
    }
  }
  else if(x$type$name == "Interception")
  {
    interception_outcome = fixZL(x$interception$outcome$name)
    if(interception_outcome %in% c("Lost In Play", "Lost Out"))
    {
      r = "fail"
    }
    else if(interception_outcome == "Won")
    {
      r = "success"
    }
    else
    {
      r = "success"
    }
  }
  else if(x$type$name == "Own Goal Against")
  {
    r = "owngoal"
  }
  else if(x$type$name == "Goal Keeper")
  {
    goalkeeper_outcome = fixZL(x$goalkeeper$outcome$name)
    if(goalkeeper_outcome %in% c("Claim", "Clear", "Collected Twice", "In Play Safe", "Success", "Touched Out"))
    {
      r = "success"
    }
    else if(goalkeeper_outcome %in% c("In Play Danger", "No Touch"))
    {
      r = "fail"
    }
    else
    {
      r = "success"
    }
  }
  else if(x$type$name == "Clearance")
  {
    r = "success"
  }
  else if(x$type$name == "Miscontrol")
  {
    r = "fail"
  }
  else
  {
    r = "success"
  }
  
  return(r)
}


#The function to determine what bodypart should be 
get_bodypart <- function(x)
{
  if(x$type$name == "Shot")
  {
    bp = fixZL(x$shot$body_part$name)
  }
  else if(x$type$name == "Pass")
  {
    bp = fixZL(x$pass$body_part$name)
  }
  else if(x$type$name == "Goal Keeper")
  {
    bp = fixZL(x$goalkeeper$body_part$name)
  }
  else
  {
    bp = NA
  }
  
  bp <- bp[bp == "NULL"] <- NA
  
  if(is.na(bp))
  {
    b = "foot"
  }
  else if(grepl("Head", bp))
  {
    b = "head"
  }
  else if(grepl("Foot", bp) | bp == "Drop Kick")
  {
    b = "foot"
  }
  else
  {
    b = "other"
  }
  
  return(b)
}

####Obtaining the Statsbomb Events####
event.files <- list.files(path="C:\\Users\\...", full.names = TRUE,recursive = TRUE)

#Getting only the Champoins League events
event.files <- subset(event.files, grepl("events/16", event.files))

for(i in 1:length(event.files)){
  event.temp <- fromJSON(file=event.files[i])
  
  match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
  
  #unique(unlist(lapply(event.temp,function(x) x$type$name))) | Let's us see the unique events that happen in a game
  
  teamids <- c() #Setting up the unique teamids participating in a match

  for(s in 1:2)
  {
    teamids <- c(teamids,event.temp[[s]]$team$id)
  }
  
  pass.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Pass" |
                      unlist(lapply(event.temp,function(x) x$type$name))=="Carry" |
                        unlist(lapply(event.temp,function(x) x$type$name))=="Shot") 
  
  #obtain the passes just for team1 (the first element in teamids)
  pass.team1 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
  
  for(p in 1:length(pass.team1)){
    pass.temp <- event.temp[[pass.team1[p]]]
    
    game_id <- append(game_id, as.numeric(match.id))
    team <- append(team, pass.temp$team$name)
    period_id <- append(period_id, pass.temp$period)
    seconds <- append(seconds, (pass.temp$minute * 60) + pass.temp$second)
    player <- append(player, pass.temp$player$name)
    start_x <- append(start_x, pass.temp$location[1])
    start_y <- append(start_y, pass.temp$location[2])
    end_x <- append(end_x, fixZL(pass.temp$pass$end_location[1]))
    end_y <- append(end_y, fixZL(pass.temp$pass$end_location[2]))
    actiontype <- append(actiontype, get_actiontype(pass.temp))
    result <- append(result, get_result(pass.temp))
    bodypart <- append(bodypart, get_bodypart(pass.temp))
  }
  
  pass.team2 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
  
  for(p in 1:length(pass.team2)){
    pass.temp <- event.temp[[pass.team2[p]]]
    
    game_id <- append(game_id, as.numeric(match.id))
    team <- append(team, pass.temp$team$name)
    period_id <- append(period_id, pass.temp$period)
    seconds <- append(seconds, (pass.temp$minute * 60) + pass.temp$second)
    player <- append(player, pass.temp$player$name)
    start_x <- append(start_x, pass.temp$location[1])
    start_y <- append(start_y, pass.temp$location[2])
    end_x <- append(end_x, fixZL(pass.temp$pass$end_location[1]))
    end_y <- append(end_y, fixZL(pass.temp$pass$end_location[2]))
    actiontype <- append(actiontype, get_actiontype(pass.temp))
    result <- append(result, get_result(pass.temp))
    bodypart <- append(bodypart, get_bodypart(pass.temp))
  }
}

#Creating a data frame from the data assembled above
finalPassData <- data.frame("game_id" = game_id, "team" = team, "period_id" = period_id, "seconds" = seconds,
                            "player" = player,"start_x" = start_x, "start_y" = start_y, "end_x" = end_x, 
                            "end_y" = end_y, "actiontype" = actiontype, "result" = result, "bodypart" = bodypart) 

#Finally, I'm going to correct for the lack of end_location data of dribbling
for(i in 1:(length(finalPassData$game_id) - 1))
{
  if(finalPassData$actiontype[i] == "dribble" & is.na(finalPassData$end_x[i] & 
    (finalPassData$player[i] == finalPassData$player[i + 1])))
  {
    finalPassData$end_x[i] <- finalPassData$start_x[i + 1]
    finalPassData$end_y[i] <- finalPassData$start_y[i + 1]
  }
  
}

#Writing the data we've assembled to a excel file
write.xlsx(finalPassData, "Statsbomb CL Open Data.xlsx")

