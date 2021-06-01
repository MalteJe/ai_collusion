# repository packages
list_of_packages <- c("rlist",
                    "tidyverse",
                    "parallel",
                    "future.apply",
                    "memoise",
                    "nnet",
                    "janitor",
                    "glue",
                    "rlang",
                    "xtable",
                    "ggbeeswarm"
                   )

# check against available packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]

if(length(new_packages) > 0){
  install.packages(new_packages)
}