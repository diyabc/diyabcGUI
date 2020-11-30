# The signification of the `noob` parameter

The median global/local statistics and confidence intervals (global)
measures for parameter estimation need a number of OOB samples
(`--noob`) to be reliable (typlially 30% of the size of the dataset is
sufficient). Be aware than computing the whole set (i.e. assigning
`--noob` the same than for `--nref`) for weights predictions (Raynal et
al. [2018](#ref-raynal2016abc)) could be very costly, memory and
cpu-wise, if your dataset is large in number of samples, so it could be
adviseable to compute them for only choose a subset of size `noob`.