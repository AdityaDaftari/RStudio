# ---
# title: "Factorial"
# author: "Aditya Daftari"
# date: "7/28/2020"
# ---
  
Factorial_loop <- function(n){
  # computes the factorial of an integer using looping (such as a for loop)
  if(n==0) return(1)
  fact <- 1
  for(i in seq(n))
  {
    fact <- fact*i
  }
  fact
}

Factorial_reduce <- function(n){ 
  # computes the factorial using the reduce() function in the purrr package.
  if(n==0) return(1)
  library(purrr)
  fact <- reduce(seq(n), function(a,b) a*b)
  fact
}

Factorial_func <- function(n){
  # uses recursion to compute the factorial.
  if(n==0) return(1)
  fact <- n*Factorial_func(n-1)
  fact
}

fact_vector <- c(1)
Factorial_mem <- function(n){
  # uses memoization to compute the factorial.
  if(n==0) return(1)
  if(!is.na(fact_vector[n])) return(fact_vector[n])
  fact_vector[n] <<- n*Factorial_mem(n-1)
  fact_vector[n]
}