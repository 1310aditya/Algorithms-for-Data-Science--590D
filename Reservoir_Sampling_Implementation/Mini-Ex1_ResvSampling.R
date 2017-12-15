resSample <- function(n){
  resv <- integer()
  counter <- vector(mode = "integer", length = 100)
  
  #N Runs of Algorithm
  for (i in 1:n){
    
    #Reservoir Sampling Algorithm (with reservoir= 1)
    for(i in 1:100){
      x <- i
      probability <- c(1/i, 1-1/i)
      decision <- sample(c(T,F), 1, prob = probability)
      switch(decision, resv <- x)
    }
    counter[[resv]] = counter[[resv]] + 1
  }
  
  if(n == 1)
    print(resv)
  else
    plot(counter, type = "l", col = "dark red")
}

#Running the algorithm- 1 Run, 1000 Runs, 10,000 Runs and 100,000 runs.
resSample(1)
resSample(1000)
resSample(10000)
resSample(100000)