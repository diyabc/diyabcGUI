# The signification of the `noob` parameter

The median global/local statistics and (global) confidence intervals for parameter estimation need a number of "Out-Of-Bag" (OOB) samples (parameter `noob`) to be reliable (typically 30% of the size of the dataset is sufficient).

Be aware that using the whole set (i.e. assigning `noob` the same than the number of samples in the training set to consider `n_rec`) for weights predictions (Raynal et al., 2018) could be very costly, memory and cpu-wise. 

If your dataset is large in number of samples, we advice to reduce the `noob` parameter.

## References

Raynal, Louis, Jean-Michel Marin, Pierre Pudlo, Mathieu Ribatet, Christian P Robert, and Arnaud Estoup. 2018. "ABC random forests for Bayesian parameter inference." Bioinformatics 35 (10): 1720–28. <https://doi.org/10.1093/bioinformatics/bty867>.