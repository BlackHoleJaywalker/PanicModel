### README

This project contains the computational model developed and evaluated in "Advancing the Network Theory of Mental Disorders: A Computational Model of Panic Disorder" (preprint: <https://psyarxiv.com/km37w/>). The computational model is a formalized theory of panic disorder. This project can be used to simulate the behavior that follows from this theory. 

### Folders

-   `/R` contains the computational model (simPanic), default initial values and parameter values, the equations that make up the model, and supporting functions that help evaluate model output.
-   `/man` contains manual files for the simPanic function

### Scripts

-   `Showcase.Rmd` illustrates how to simulate data from the model, including how to modify initial values and parameter values, how to incorporate perturbations into the model simulations, how to iterate the simulation across a set of initial values or parameter values, and how to incorporate cognitive behavioral treatment components into the simulation.


### R

-   `simPanic.R` contains the computational model of panic disorder
-   `Defaults.R` contains the default initial values and default parameter models for all model equations.
-   `DifferentialEquations.R` contains all equations used in the computational model.
-   `SupportingFunctions.R` contains two functions that can be used for analyzing model output: a function for identifying panic attacks in the simulated time series data (detectPanic) and a function for computing the time to recover to baseline following a perturbation (detectRecover).
