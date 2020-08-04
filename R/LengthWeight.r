LengthWeight <- function(lengthf, sex, a.1, b.1, a.2, b.2, a.3, b.3) {
    #' LengthWeight
    #'
    #' Length weight conversion
    #'
    #' @param lengthf Fork length in cm
    #' @param sex Sex, 1=female, 2=male
    #' @param a.1 a for females
    #' @param b.1 b for females
    #' @param a.2 a for males
    #' @param b.2 b for males
    #' @param a.3 a for unsexed fish
    #' @param b.3 b for unsexed fish
    #'
    #' @return weight weight
    #' @export
    #' @examples
    #'
    
    if(sex==1) { 
    # parameters for females
        <- a.1
        b <- b.1
    } else if(sex==2) { 
    # parameters for males
        a <- a.2
        b <- b.2
    } else if(sex==3) { 
    # parameters for unsexed fish
        a <- a.3
        b <- b.3
    }

    # calcualte weight in kg
    weight <- a*lengthf^b

    # convert weight from kg to lbs
    weight <- weight*2.2046

    # return weight in lbs
    return(weight)
} 
