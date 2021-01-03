library(dplyr)
library(openxlsx)

#Setting the M*N grid that describes the football pitch
M <- 12 #width
N <- 16 #length

#Pitch length and width retrived from 
#https://github.com/ML-KULeuven/socceraction/blob/master/socceraction/spadl/config.py

 get_cell_indexes <- function(x, y, l, w)
{

xmin = 0
ymin = 0

xi = (x - xmin) / 105.0 * l
yj = (y - ymin) / 68.0 * w
xi <- round(pmax(pmin(xi, l), 1))
yj <- round(pmax(pmin(yj, w), 1))

return(list(xi, yj))
}

get_flat_indexes <- function(x,y,l,w)
{
  output <- c()
  indexes <- get_cell_indexes(x, y, l, w)

  for(i in 1:length(indexes[[1]]))
  {
    output <- append(output, w - indexes[[2]][i] + 1 + (12 * (indexes[[1]][i] - 1)))
  }

  return(output)
}


#Count the number of actions occurring in each cell of the grid.
#    :param x: The x-coordinates of the actions.
#    :param y: The y-coordinates of the actions.
#    :param l: Amount of grid cells in the x-dimension of the grid.
#    :param w: Amount of grid cells in the y-dimension of the grid.
#    :return: A matrix, denoting the amount of actions occurring in each cell. The top-left corner is the origin.

count <- function(x,y,l,w)
{
x = x[is.na(x) == FALSE & is.na(y) == FALSE]
y = y[is.na(x) == FALSE & is.na(y) == FALSE]

flat_indexes = get_flat_indexes(x, y, l, w)
vn <- table(flat_indexes)

#Creating and filling  the vector of occurrences
vector <- rep(0, l*w)

for(i in 1:(l*w))
{
  if(as.character(i) %in% names(vn))
  {
    vector[i] <- vn[[as.character(i)]]
  }
}


return(matrix(data = vector, nrow = w, ncol = l))
}

safe_divide <- function(a,b)
{
  output <- a/b
  output[which(is.nan(output))] <- 0
  return(output)
}

#Compute the probability of scoring when taking a shot for each cell.
#   :param actions: Actions, in SPADL format.
#   :param l: Amount of grid cells in the x-dimension of the grid.
#   :param w: Amount of grid cells in the y-dimension of the grid.
#   :return: A matrix, denoting the probability of scoring for each cell.

scoring_prob <- function(actions, l, w)
{
shot_actions = actions[which(actions$actiontype == "shot"),]
goals = shot_actions[which(shot_actions$result == "success"),]

shotmatrix = count(shot_actions$start_x, shot_actions$start_y, l, w)
goalmatrix = count(goals$start_x, goals$start_y, l, w)

return(safe_divide(goalmatrix, shotmatrix))
}

get_move_actions <- function(actions)
{
  return(actions[which(actions$actiontype == "pass" | 
                       actions$actiontype == "dribble" |
                       actions$actiontype == "cross"),])
}

#Compute the probability of taking an action in each cell of the grid. The options are: shooting or moving.
#   :param actions: Actions, in SPADL format.
#   :param l: Amount of grid cells in the x-dimension of the grid.
#   :param w: Amount of grid cells in the y-dimension of the grid.
#   :return: 2 matrices, denoting for each cell the probability of choosing to shoot
#   and the probability of choosing to move.
action_prob <- function(actions, l, w)
{
move_actions = get_move_actions(actions)
shot_actions = actions[which(actions$actiontype == "shot"),]

movematrix = count(move_actions$start_x, move_actions$start_y, l, w)
shotmatrix = count(shot_actions$start_x, shot_actions$start_y, l, w)
totalmatrix = movematrix + shotmatrix

return(list(safe_divide(shotmatrix, totalmatrix), safe_divide(movematrix, totalmatrix)))
}


#Compute the move transition matrix from the given actions.
#   This is, when a player chooses to move, the probability that he will
#   end up in each of the other cells of the grid successfully.
#   :param actions: Actions, in SPADL format.
#   :param l: Amount of grid cells in the x-dimension of the grid.
#   :param w: Amount of grid cells in the y-dimension of the grid.
#   :return: The transition matrix.
move_transition_matrix <- function(actions, l, w)
{
move_actions = get_move_actions(actions)
move_actions <- subset(move_actions, is.na(end_x) == FALSE & is.na(result) == FALSE)

X = data.frame("start_cell" = get_flat_indexes(move_actions$start_x, move_actions$start_y, l, w),
               "end_cell" = get_flat_indexes(move_actions$end_x, move_actions$end_y, l, w),
               "result_name" = move_actions$result)

vc = as.vector(table(X$start_cell))

transition_matrix = matrix(data = rep(0,w * l * w * l), ncol = w * l, nrow = w * l)

for (i in 1:(w * l))
{
  vc2 = table(X[which(X$start_cell == i & X$result_name == "success"),]$end_cell)
  

  for(j in 1:length(names(vc2)))
  {
    transition_matrix[i, as.numeric(names(vc2)[j])] = vc2[j] / vc[i]
  }
}

  return(transition_matrix)
}

################################################################################
#An implementation of Karun Singh's Expected Threat model (https://karun.in/blog/expected-threat.html).
setClass("expectedThreat", slots=list(l="numeric", w="numeric", eps="numeric", heatmaps="list", xT="matrix",
                                      scoring_prob_matrix="matrix", shot_prob_matrix="matrix",
                                      move_prob_matrix="matrix", transition_matrix="matrix"))

#The  "Constructor" for the expectedThreat class
expectedThreat <- function(l, w, eps)
{
  setClass("expectedThreat", slots=list(l="numeric", w="numeric", eps="numeric", heatmaps="list", xT="matrix",
                                        scoring_prob_matrix="matrix", shot_prob_matrix="matrix",
                                        move_prob_matrix="matrix", transition_matrix="matrix"))
  
  output <- new("expectedThreat", l=l, w=w, eps=eps, heatmaps=list(), xT=matrix(data=rep(0,w*l), nrow = w, ncol = l),
                scoring_prob_matrix = matrix(data=rep(0,w*l), nrow = w, ncol = l), 
                shot_prob_matrix = matrix(data=rep(0,w*l), nrow = w, ncol = l),
                move_prob_matrix = matrix(data=rep(0,w*l), nrow = w, ncol = l),
                transition_matrix = matrix(data=rep(0,w*l), nrow = w * l, ncol = w * l))
  
  return(output)
}


#Solves the expected threat equation with dynamic programming.
#      :param self: Object of Formal class expectedThreat
#      :param p_scoring (matrix, M x N): Probability of scoring at each grid cell, when shooting from that cell.
#      :param p_shot (matrix, M x N): For each grid cell, the probability of choosing to shoot from there.
#      :param p_move (matrix, M x N): For each grid cell, the probability of choosing to move from there.
#      :param transition_matrix (matrix, (M*N) x (M*N)): When moving, the probability of moving to each of the other zones.
#      :param iter_max (int): maximum number of iterations to be made 
solve <- function(self, p_scoring, p_shot, p_move, transition_matrix, iter_max)
{
gs = p_scoring * p_shot
diff = 1
it = 0
self@heatmaps <- append(self@heatmaps, self@xT)

while (TRUE %in% as.vector(diff > self@eps) & it < iter_max)
{  
  total_payoff <- matrix(data = rep(0, self@w * self@l), nrow = self@w, ncol = self@l)

  #Looping through every possibility in the transition matrix and using it to calculate the xT values
  for (y in 1:self@w)
  {
    for (x in 1:self@l)
    {
      for (q in 1:self@w)
      {
        for (z in 1:self@l)
        {
          total_payoff[y,x] <- total_payoff[y,x] + (transition_matrix[y + (12 * (x - 1)), q + (12 * (z - 1))]* self@xT[q, z])
        }
      }
    }
  }

newxT = gs + (p_move * total_payoff)
diff = newxT - self@xT
self@xT = newxT
self@heatmaps <- append(self@heatmaps, self@xT)
it <- it + 1
}

print(sprintf("# iterations: %s", it))
return(self)
}


#Fits the xT model with the given actions.
      #param self: object of Formal class expectedThreat
      #param actions: Actions, data.frame in SPADL format.
fit <- function(self, actions, iter_max)
{
self@scoring_prob_matrix <- scoring_prob(actions, self@l, self@w)

tempList <- action_prob(actions, self@l, self@w)
self@shot_prob_matrix <- tempList[[1]]
self@move_prob_matrix <- tempList[[2]]

self@transition_matrix = move_transition_matrix(actions, self@l, self@w)

self <- solve(self, self@scoring_prob_matrix, self@shot_prob_matrix, self@move_prob_matrix,
        self@transition_matrix, 5)

return(self)
}


interpolator <- function(self)
{
library(akima) #loading the package to run the interp function

cell_length = 105.0 / self@l
cell_width = 68.0 / self@w

x <- seq(from = 0.0, to = 105.0, length.out = 192) + 0.5 * cell_length
y <- seq(from = 0.0, to = 68.0, length.out = 192) + 0.5 * cell_width

result <- interp(x=x, y=y, z=self@xT, xo = seq(min(x),max(x), length.out = 680), yo = seq(min(y), max(y)
      , length.out = 1050),linear = FALSE, extrap = TRUE)

return(result[[3]])
}


#Predicts the xT values for the given actions.
#       :param actions: Actions, in SPADL format.
#       :param use_interpolation: Indicates whether to use bilinear interpolation when inferring xT values.
#       :return: Each action, including its xT value.
predict <- function(self, actions, use_interpolation = TRUE)
{
  if(use_interpolation == FALSE)
  {
    l = self@l
    w = self@w
    grid = self@xT
  }else
  {
    # Use interpolation to create a
    # more fine-grained 1050 x 680 grid
    #It is recommended to use a fairly large data set here to get accurate/reliable results
    l = 1050
    w = 680
    grid = interpolator(self)
}

start <- get_cell_indexes(actions$start_x, actions$start_y, l, w)
end <- get_cell_indexes(actions$end_x, actions$end_y, l, w)

xT_start <- c()
xT_end <- c()

for(i in 1:length(start[[2]]))
{
  xT_start <- append(xT_start, grid[w + 1 - start[[2]][i], start[[1]][i]])
  xT_end <- append(xT_end, grid[w + 1 - end[[2]][i], end[[1]][i]])
}

return(xT_end - xT_start)
}

################################################################################
#Implementing the functions defined above to fit the xThreat model
################################################################################
#Reading in event level data (in SPADL format)
data <- read.xlsx("Statsbomb CL Open Data.xlsx")

#Splitting the data into test and training sets
test <- subset(data, game_id == "16010" & is.na(end_x) == FALSE)
train <- subset(data, game_id != "16010")

#Creating the expectedThreat object
obj <- expectedThreat(l = N, w = M, eps = 1e-5)

#Fitting the xThreat model
obj <- fit(obj, train)

#Predicting the value of various actions using the model 
predicted <- predict(obj, test, use_interpolation = FALSE)

actionsFinal <- data.frame(test, "xThreat" = round(predicted,3))
