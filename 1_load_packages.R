################
# name packages
################

packages <- c("hdftsa", "sde", "ftsa", "xtable", "dplyr", "quantreg") 

## Now load or install and load all 

package_check <- lapply(
  packages,
  FUN <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

require(hdftsa)
require(sde)
require(ftsa)
require(xtable)
require(dplyr)
require(quantreg)
