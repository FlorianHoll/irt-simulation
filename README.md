# IRT simulation

The goal of this simulation is to simulate the behavior of multidimensional Item Response Theory (IRT) estimation in R 
with the package `mirt` under different sample sizes, number of items and parameters.
The ultimate goal is to find out the minimal sample size that is necessary in order to obtain sensible estimates for multidimensional IRT models.

## Goal
Item Response Theory is a method to infer the latent traits or abilities of persons, given their reponse to closed-format
questions. Item Response Theory can be seen as a factor analysis and as a type of Structural Equation Model.
There are unidimensional (i.e. only one latent trait) and multidimensional (multiple latent traits) IRT models. Furthermore, one can choose between a 1PL model (the so-called "Rasch Model") or the 2PL model (the so-called "Birnbaum Model).
This simulation deals with uni- and multidimensional 2PL models. The goal is to find out a minimal sample size that is 
necessary in order to obtain sensible estimates for multidimensional 2PL IRT models.

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

