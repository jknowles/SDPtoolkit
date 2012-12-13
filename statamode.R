# This function is in the DEV version of my eeptools package
# which I need to push out soon. 
# It allows you to take the mode with different options about how to break
# ties. In this case you can choose the last value in a series, Stata's "."
# or you can randomly draw from among the tied values. 

statamode <- function(x,method=c("last", "stata","sample"))
{  
  x<-as.character(x)
  
  stata <- function(x){
    z <- table(as.vector(x))
    m<-names(z)[z == max(z)]
    if(length(m)==1){
      return(m)
    }
    return(".")
  }
  
  sample2 <- function(x){
    z <- table(as.vector(x))
    m<-names(z)[z == max(z)]
    if(length(m)==1){
      return(m)
    }
    else if (length(m)>1){
      return(sample(m,1))
    }
    else if (length(m)<1){
      return(NA)
    }
  }
  
  last <- function(x){
    z <- table(as.vector(x))
    m<-names(z)[z == max(z)]
    if(length(m)==1){
      return(m)
    }
    else if(length(m)>1){
      return(tail(m,1))
    }
    else if(length(m)<1){
      return(NA)
    }
  }
  
  method <- match.arg(method)
  
  switch(method,
         stata = temp <- stata(x),
         sample = temp <- sample2(x),
         last = temp <- last(x))
  
  return(temp)
}