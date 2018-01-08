################################################################################
# File: MCMCIntro.R
# Course: MA480 Bayesian Data Analysis
# Description: Set up computations needed to perform MCMC step by step to
#              introduce the process.
#
# Author: Eric Reyes
# Date: 29 Dec 2017
# Modified:
#
# Notes:
#  1. To be incorporated into an RMarkdown file that students interact with
#     directly.

require(tidyverse, quietly = TRUE)

# Potential Names
#  These represent the individuals we could speak with.
.names <- c("",
            "Arturo", "Bentley", "Courtney", "Eric",
            "Jamie", "Mike", "Rachel", "Sue",
            "")


# Relative Wealth
#  Represents the relative wealth between each pair of individuals for which a
#  valid move is allowed.  We add a fake individual on each end to handle edge
#  cases.
.wealth <- matrix(c(0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 
                    0, 2, 0, 0.5, 0, 0, 0, 0, 0, 0, 
                    0, 0, 2, 0, 0.4, 0, 0, 0, 0, 0, 
                    0, 0, 0, 2.5, 0, 1, 0, 0, 0, 0, 
                    0, 0, 0, 0, 1, 0, 2.5, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0.4, 0, 2, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0.5, 0, 2, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0), 
                  nrow = 8, ncol = 10, byrow = TRUE)


# function: First.Name
# description: Generate the name of the first individual we will speak with.
First.Name <- function(){
  sample(.names[-c(1,length(.names))], 1)
}


# function: Coin.Flip
# description: Generates the result of a fair coin flip.
Flip.Coin <- function(){
  sample(c("H", "T"), size = 1)
}


# function: Relative.Wealth
# description: Given the current individual and the candidate, determine the
#              wealth of the candidate relative to the current individual.
Relative.Wealth <- function(current.name, coin.flip = c("H", "T")){
  position <- which(.names == current.name)
  direction <- c("H" = -1, "T" = 1)[coin.flip]

  data.frame(Candidate = .names[position+direction],
             `Relative Wealth` = .wealth[position-1, position+direction],
             stringsAsFactors = FALSE)
}
