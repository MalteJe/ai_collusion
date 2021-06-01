# Collusion among autonomous pricing algorithms utilizing function approximation methods - Guide


## Setup

**I recommend to always open the project file `ai_collusion.Rproj` in *RStudio* first. This ensures the working directory is correctly specified.**

`config.R` checks which project packages are available and installs missing packages.

## User Guide

This is a brief description of the folder hierarchy of this project repository. It is useful to think of this as a sequential 4-step process:

1. Simulate experiments
2. Aggregate data
3. Visualize data
4. Generate report

### Simulate experiments

The highest-level script is  `master.R`.  It executes all simulations. At the very top, you are able to specify the desired number of cores to parallelize execution over. `number_of_cores = 1` evokes sequential execution.

To keep the size of the master script manageable, `master.R` invokes several functions that are not defined in the script itself, but in *helper scripts*. Those are:

* `experiment_helper.R`: The function calls in `master.R` execute a series of experiments. `experiment-helper.R` defines the functions invoked in `master.R` and separates the command to run a series of experiments into lower order commands to run isolated experiments and, another level lower, into single runs. The command to run a single run invokes the function `single_run` defined in `gd.R`.
* `gd.R` (gradient descent) defines a single function that executes a single simulation, including the run's setup and initialization. It invokes functions from several lower level scripts, most importantly the loop over time periods:
  * `algorithm_loop.R`: This script is the heart of a single simulation. There are 4 functions that loops over time episodes  (expected SARSA, on-policy SARSA, and two scripts executing the deviation experiments). Each function returns the entire environment back to `single_run`. The functions in `algorithm_loop.R` itself utilize lower level functions from these scripts:
    1. `feature_extraction.R` contains functions that set up specific feature extraction methods at the beginning of a run and, more importantly, functions that extract features from the state set.
    2. `selection_methods_and_td.R` contains function to select actions (e.g. based on an $\epsilon$-greedy policy) and to calculate the temporal-difference error.
    3. `helper.R` contains 3 more helper functions to estimate the value of a state-action combination and to determine whether a run has converged
  * `economic_environment.R` contains functions to (a) calculate Logit demand and profits as well as (b) calculate the static Nash solution and monopoly profits through means of numerical optimization.

After a single run is executed, the function `single_run` looks for an subdirectory with an appropriate name for the experiment in `simulation_results/`. If  none exists, it creates one. For instance, once the first run in a series of experiments with varying $\alpha$ concludes, a file with the simulation results is saved in `simulaton_results/Alpha/*FEM*_*alpha value*_*run id*.RData `. That file contains the history of prices and profits as well as information about the specifications. These files are required for the next step.

### Aggregate data

`aggregate_files.R`  aggregates data of individual runs. For a series of experiments saved in a subdirectory of `simulation_results` (e.g. `simulation_results/Alpha_final/`) it screens for all files in that folder, aggregates them and stores a single compressed file `aggregated.R` in the same folder. These files are used in the subsequent step

### Visualize data

`static_visualization.R` creates the graphs and tables used in the report (and some more). It invokes several lower-level wrappers defined in `visualization_helper.R`.  First, it loads `aggregated.R` files from a subdirectory in `simulaton_results`. Then, a number of plots and tables are created. Some of them are saved in `report/plots/` and `report/tables` respectively.

### Generate report

`master.tex` in `report` generates the entire report by sourcing the other `.tex` files in the folder. Naturally, the `plots ` and `tables` subdirectories must contain the plots.