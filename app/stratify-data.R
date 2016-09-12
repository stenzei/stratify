# The following code computes strata for Latin Hypercube Sampling.
rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
library(lazyeval)

# Recursive function to calculate strata. 
# Parameters: 
# dta:         data_frame with any number of parameters each
#              column is exspected to represent one parameter  
# numb.strat:  number of strata in each dimension
stratify <- function(dta, numb.strat=4) {
  parameters <- names(dta)                    # get the names of parameters,
  param <- parameters[1]                      # store the name of the first,
 
  n <- dta %>%                                # claculate the number of values 
    summarise(n())                            # (repetitions included),
  n <- n$`n()`                                # calculate the number of 
  s <- floor(n / numb.strat)                  # parameter values per stratum

  if (!is.integer(n / numb.strat)) {          # ommit randomly all values which
    n <- (s * numb.strat)                     # are too much to fit in the given 
    dta <- dta %>%                            # stratum,
     sample_n(n)
  }

  strata <- dta %>%                           # take the data,
    select_(param) %>%                        # select the first parameter,
    arrange_(param) %>%                       # sort param values,
    slice(c(1, seq.int(from = s,              # extract strata limits,
                       to = n, 
                       by = s)))
  
  results <- data_frame()
  for (i in 1:(numb.strat)) {                 # for each stratum of the first
    lower.bound <- interp(~v >= x,            # parameter calc lower boundary,
                          v = as.name(param), 
                          x = as.double(strata[i, 1]))
    upper.bound <- interp(~v < y,             # and calculate upper boundary,
                          v = as.name(param), 
                          y = as.double(strata[i + 1, 1]))
    not.param <- interp(~-v, v = as.name(param))
    next.strat <- dta %>% 
      filter_(lower.bound) %>%                # filter the stratum which is
      filter_(upper.bound) %>%                # to be stratified next,
      select_(not.param)                      # select the remaining parameters,

    if (length(parameters) > 1) {             # unstratified parameters left?
      next.strat <- stratify(next.strat,      # stratify them
                             numb.strat)
      new.col <- interp(~a, a = as.double(strata[i, 1]))
      next.strat <- next.strat %>%            # combine stratification results
        mutate_(.dots = setNames(list(new.col), param)) 
      results <- bind_rows(results, next.strat) # bind strata of param together
    } else {
      return(slice(strata, -1))               # recursion reached leaf of tree
    }            
  }
  return(results)                             # done. return the stratum
}