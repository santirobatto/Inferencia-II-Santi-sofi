
library(rstan)
library(tidyverse)
library(bayesrules)
library(janitor)

current <- 3
set.seed(8)
proposal <- runif(1, min = current - 1, max = current + 1)
proposal
proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
proposal_plaus
current_plaus  <- dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
current_plaus
#Proposal es el numerador
#Current es el denominador
#Lanzaremos en el siguiente paso una moneda sesgada que tiene como cociente el num/den)

alpha <- min(1, proposal_plaus / current_plaus)
alpha

#nO ENTENDI MUCHO, FUE EL PRIMER DIA EN MAPFRE Y ME DORMIA
next_stop <- sample(c(proposal, current),
                    size = 1, prob = c(alpha, 1-alpha))
next_stop


#Definimos una funcion para hacer metropolis hastings
one_mh_iteration <- function(w, current){
  # STEP 1: Propose the next chain location
  proposal <- runif(1, min = current - w, max = current + w)
  
  # STEP 2: Decide whether or not to go there
  proposal_plaus <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
  current_plaus  <- dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
  alpha <- min(1, proposal_plaus / current_plaus)
  next_stop <- sample(c(proposal, current), 
                      size = 1, prob = c(alpha, 1-alpha))
  
  # Return the results
  return(data.frame(proposal, alpha, next_stop))
}

set.seed(8)
one_mh_iteration(w = 1, current = 3)
#La probamos y da lo mismo



#Nueva funcion para hacer MH, donde generamos N valores de mu con ancho W

mh_tour <- function(N, w){
  # 1. Start the chain at location 3
  current <- 3
  
  # 2. Initialize the simulation
  mu <- rep(0, N)
  
  # 3. Simulate N Markov chain stops
  for(i in 1:N){    
    # Simulate one iteration
    sim <- one_mh_iteration(w = w, current = current)
    
    # Record next location
    mu[i] <- sim$next_stop
    
    # Reset the current location
    current <- sim$next_stop
  }
  
  # 4. Return the chain locations
  return(data.frame(iteration = c(1:N), mu))
}
set.seed(84735)
mh_simulation_1 <- mh_tour(N = 5000, w = 1)
mh_simulation_1

ggplot(mh_simulation_1, aes(x = iteration, y = mu)) + 
  geom_line()

ggplot(mh_simulation_1, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20) + 
  stat_function(fun = dnorm, args = list(4,0.6), color = "blue")

#El valor de w chico hace que le cueste mucho crecer y visitar toods los valores, 
#Un w demasiado gradne será demasiado erratico dado que podrá tomar valores en un recorrido demasiado amplio
