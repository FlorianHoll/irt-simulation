# IRT simulation

The goal of this simulation is to simulate the behavior of multidimensional Item Response Theory (IRT) estimation in R 
with the package `mirt` under different sample sizes, number of items and parameters.
The ultimate goal is to find out the minimal sample size that is necessary in order to obtain sensible estimates for multidimensional IRT models.

## Goal
Item Response Theory is a method to infer the latent traits or abilities of persons, given their reponse to closed-format
questions.
This simulation deals with uni- and multidimensional 2PL models. 

The goal is to **find out a minimal necessary sample size** to obtain sensible estimates for multidimensional 2PL IRT models, given some parameters.

## Run the simulation
The simulation is controlled with a `.yml` file in which each simulation is represented as a key and the parameters as values. For an exemplary .yml file, see `./configs/configs.yml`.
The path of this .yml file needs to passed as an argument to the script `simulation.R`.

Therefore, the simulation can be run using:

```bash
Rscript simulation.R configs/configs.yml
```

## Parameters
The parameters that can be manipulated in this simulation are:
* Data parameters:
	* `nr_items`: The number of items that shall be calculated in the model.
	* `nr_persons`: The number of persons whose responses shall be simulated.
* Model parameters:
	- `nr_dimensions`: The number of dimensions. If the model only has only one dimension, 
	it is a unidimensional model. In all cases where `nr_dimensions > 1`, it is a multidimensional 
	IRT model with a general factor (sometimes also called "G factor") that *all* items load onto and
	`nr_dimensions` subfactors where each subfactors has some amount of items loading onto it.
	- `priors`: Use priors for the parameter estimation or not. `mirt` has the ability to specify 
	priors for each parameter.
	- `model_type`: Either "Rasch" (1PL) or "2PL". The 1PL model is a 2PL model with the alpha parameter
	(item discrimination) constrained to be 1 for all items.

## Logic of  the simulation
The general logic of the simulation is as follows:
* Do the following `nr_simulation` times:
	* **Simulate some "true parameters"** - that is, parameters for the items and persons. This step is subject to a number of parameters such as the number of items, the number of persons, the number of dimensions etc. Specifically, the following parameters are simulated:
		* The discrimination (alpha) parameter(s) for each of the items and each dimension. If there are 20 items, for example and 3 (sub-)dimensions (which means 1 general factor and three subfactors, i.e. 4 dimensions overall), the number of simulated alpha parameters is 20 x 4 = 80.
		* The difficulty (delta) parameter for each item. This is not affected by the dimensionality, i.e. for 20 items and 3 (sub-) dimensions, 20 parameters are simulated.
		* The ability (theta) parameters. There is an ability on each dimension, i.e. for the general as well as the sub-dimension. This means that for `nr_persons=100` and `nr_dimensions=3`, 400 parameters will be simulated: For each of the 100 persons, an ability is simulated for the general factor as well as the three subfactors (i.e. 4 ability parameters per person).
	* **Simulate some data, based on the probabilistic processes that IRT assumes.** IRT assumes that the probability of 
	a person 
	* Pass the data to an IRT-model estimated with `mirt`. The estimated model is also subject to some general parameters.
	* Retrieve the *estimated* person abilities, item discriminations and difficulties and compare them to the *actual*, i.e. true, parameters by calculating the deviation between the true and the estimated parameters.
	* Return the deviations along with some parameters.

