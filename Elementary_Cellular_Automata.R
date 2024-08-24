# Elementary Cellular Automata using R and Vectors

pacman::p_load("stringr", "ggplot2", "reshape2")

ElemCA <- function(initial_state, col_num, 
                       generations, 
                       key){
  
  #initial_state is the starting state or gen1 of the ElemCA
  #col_num is the length of the initial state vector, and all other generation vectors
  #generations are the number of rows in the Elem CA
  
  null_vector = c()
  
  #prints out the initial state or sample or input of our program
  #print(initial_state)
    
  len = length(initial_state)
  
  for (i in 1:len){
    if(i == 1){
      a_string = paste0(c(initial_state[len], initial_state[i], initial_state[i+1]), collapse = "")
    }
    else if(i <= len-1){
      a_string = paste0(c(initial_state[i-1], initial_state[i], initial_state[i+1]), collapse = "")
    }else{
      a_string = paste0(c(initial_state[i-1], initial_state[i], initial_state[1]), collapse = "")
    }
    #inside for loop
    # checks to make sure we can move trough the initial vector
    #print(a_string)
    
    matches <- str_detect(key, a_string)
    
    if(TRUE %in% matches){
      null_vector = c(null_vector, 1)
    }else{
      null_vector = c(null_vector, 0)
    }
  }
  # Outside for loop
  #print(null_vector)
  return(null_vector)
}

Run_ElemCA <- function(initial_state=0, col_num=5, 
                        generations = 5, 
                        key=c("100", "011", "010", "001")){
  
  if (is.vector(initial_state)==FALSE){
    initial_state = sample(c(0,1),col_num, replace = TRUE)
  }
  M <- matrix(initial_state, nrow = 1)
  #print(initial_state)
  #print(M)
  next_gen <- ElemCA(initial_state = initial_state, col_num = col_num, generations = generations, key = key)
  
  M <- rbind(M, next_gen)
  #print(next_gen)
  
  instance = 2
  
  while (instance < generations){
    next_gen <- ElemCA(initial_state = next_gen, col_num = col_num, generations = generations, key = key)
    instance = instance + 1
    M <- rbind(M, next_gen)
    #print(next_gen)
  }
  return(M)
}

vec <- c(rep(0,25), 1, rep(0,25))

M <- Run_ElemCA(initial_state = vec, col_num = length(vec), generations = 250)


image(t(M[nrow(M):1,]), axes=FALSE, zlim=c(0,1), col=grey(c(1,0)))

