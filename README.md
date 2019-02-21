# Search as a simple take-the-best heuristic

Complete experimental code, simulation code, evaluation code, and data for replicating the results from:
``Search as a simple take-the-best heuristic``

## Getting Started

The project structure consists of three main parts
 1) The experimental paradigm in *experiment*
 2) The experimental data in *data*
 3) Evaluation and simulation code in *data_analysis*
 
## Experiment

The experiment requires Java 1.7 for the backend server and a web browser supporting websockets on the participants device.  
To run the experiment:
 1) change the IP address (``$IP``) in ``webapp/js/index.js`` to the correct ``$IP``
 2) change the pathPrefix in ``java/World.java`` (``$filepath``)
 3) build java artifact 
 4) copy artifact to webapp
 5) copy landscapes to ``$filepath``
 6) SSH to server and copy webapp to server
 7) set jetty home (``JETTY_HOME=``)
 8) go to jetty home (``cd $JETTY_HOME``)
 9) start server in screen and detach (``screen -d -m java -jar $JETTY_HOME/start.jar jetty.http.port=8082``)
 10) check if server is running (open browser to ``$IP:8082``)
 
## Data

All data is saved in csv files. With the exception of the three landscapes*.csv files all data used is [tidy](https://en.wikipedia.org/wiki/Tidy_data).

### movement.csv

Decision data from all participants

```
time, phase, level, step ,user id,decision type, position in grid, payoff
```

### participant.csv

Demographic information of all participants

```
starting time, userid id, age, gender
```

### structure.csv

Relationship between level and landscape

```
user id, level, number of rounds (always 30), deprecated (always false), isRich?, experimental phase, landscape id
```

### landscapes*.csv

Landscapes for each of the three experimental phases  

one line is one landscape

## Data analysis

The data analysis uses R version 3.4.1. It heavely relies on the [tidyverse](https://www.tidyverse.org/)

### load and preprocess experimental data

run ``_basic.R ``

### Simulation

To fit the parameters of the simulation to the experimental data run ``run_and_fit_simulation.R``

### Figures

To generate the raw versions of all figures used in the paper run ``figures_all.R``

### Numbers

To calculate all the numeric values (such as mean and sd) used in the paper run ``numbers_all.R``

## Authors

See the list of [contributors](contributors.txt) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details