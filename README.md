# many-analysts-variations-revisit
A revisit of "Many Analysts, One Data Set: Making Transparent How Variations in Analytic Choices Affect Results" Silberzahn et al. 2018

# Repository Instructions

## Data

The folder contains in raw format the initial data received by the
research teams + the analyses after the experiment.

⚠️ **Important**: Reading the teams' PDFs reveals that during the
experiment there were two versions of the data. The available one is the
latest.\
The difference lies in the "raters" values which were initially between
1 and 5 but are now between 0 and 1.

------------------------------------------------------------------------

## Personal

Draft folder with random notes I took during my research.

------------------------------------------------------------------------

## Pipeline

Folder containing the pipeline prototype.\
It requires an **OpenRouter API key** to be placed in a `secrets.env`
file at the root of the repository.\
It also needs the team's PDF and the available team code, if possible
stored in the `pipeline` folder.

------------------------------------------------------------------------

## Teams

Folder containing files related to the different teams.\
In each team's folder there is:\
- a `hand_work` folder with my personal codes,\
- an `output` folder,\
- and a `source` folder containing all the documents provided by the
team.

------------------------------------------------------------------------

## Frictions

The file `frictions.md` contains the frictions encountered during the
replication task using different approaches.
