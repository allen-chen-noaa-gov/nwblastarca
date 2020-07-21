LengthWeight <- function(length,   # Fork length in cm
                         sex,      # Sex, 1=female, 2=male
                         a.1,      # a for females
                         b.1,      # b for females
                         a.2,      # a for males
                         b.2,      # b for males
                         a.3,      # a for unsexed fish
                         b.3){     # b for unsexed fish
    #' LengthWeight
    #'
    #' LengthWeight
    #'
    #' @param length Fork length in cm
    #'
    #' @return weight weight
    #' @export
    #' @examples
    #'
    
  if(sex==1) { # parameters for females
    a <- a.1
    b <- b.1
  } else if(sex==2) { # parameters for males
    a <- a.2
    b <- b.2
  } else if(sex==3) { # parameters for unsexed fish
    a <- a.3
    b <- b.3
  }
  
  # calcualte weight in kg
  weight <- a*length^b
  
  # convert weight from kg to lbs
  weight <- weight*2.2046
  
  # return weight in lbs
  return(weight)
} 
