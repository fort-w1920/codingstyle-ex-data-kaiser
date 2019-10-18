###################################
##### Codying Style Loesung #######
###################################

#-----------------------------
### Conditional complex swipe 

swipe <- function(swiper, picture, profile) {

  if (!exists(picture)) {
    stop("can't decide without a picture.")
  }
  if (!is_attractive(picture)) {
    return("swipe left")
  }
  if (!is_sober(swiper)) {
    return("swipe right")
  }
  if (!exists(profile)) {
    stop("can't decide without a profile.")
  }
  if (seems_too_weird(profile)) {
    return("swipe left")
  } 
  if (matches(picture, swiper[["preferences"]])) {
    return("swipe right")
   } 
  "swipe left"
}

#---------------------------
### refactor-dry

x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
v <-  z^2  

estimate_interval <- function(vector) {
  
  ## checking the input 
  if (!is.vector(vector)) {
    stop("input must be a vector")
  }
  if (!is.numeric(vector)) {
    stop("interval can be calculated only with numeric vector")
  }
  
  ## calculating and returning the 95% interval
  m <- mean(vector)
  s <- sd(vector)
  l <- length(vector)
  interval <- c(m - 1.96 * s / sqrt(s), m + 1.96 * s / sqrt(s))
  return(interval)
}

estimate_interval(x)
estimate_interval(y)
estimate_interval(z)
estimate_interval(v)

#--------------------------
### styleguid-sarnierung

