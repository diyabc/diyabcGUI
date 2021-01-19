# Summary statistics

**Warning!** All summary statistics implemented in the program will be computed and included in the training dataset.

## For SNP loci (both IndSeq and PoolSeq SNPs)

For both IndSeq and PoolSeq SNP loci, the following set of summary statistics has been implemented.

1. Proportion of monomorphic loci for each population, as well as for each pair and triplet of populations (ML1p, ML2p, ML3p)

Mean and variance (over loci) values are computed for all subsequent summary statistics.

2. Heterozygosity for each population (HW) and for each pair of populations (HB)
3. FST-related statistics for each population (FST1), for each pair (FST2), triplet (FST3), quadruplet (FST4) and overall (FSTall) populations (when the dataset includes more than four populations)
4. Patterson’s f-statistics for each triplet (f3-statistics; F3) and quadruplet (f4-statistics; F4) of populations
5. Nei’s distance (NEI) for each pair of populations
6. Maximum likelihood coefficient of admixture (AML) computed for each triplet of populations.

---

## For Microsatellite loci

For microsatellite loci, the following set of summary statistics has been implemented.

### Single sample statistics

1. Mean number of alleles across loci (NAL)
2. Mean gene diversity across loci (HET)
3. Mean allele size variance across loci (VAR)
4. Mean $M$ index across loci (MGW)

### Two sample statistics

1. Mean number of alleles across loci (two samples) (N2P)
2. Mean gene diversity across loci (two samples) (H2P)
3. Mean allele size variance across loci (two samples) (V2P)
4. $F_{ST}$ between two samples (FST)
5. Mean index of classification (two samples) (LIK)
6. Shared allele distance between two samples (DAS)
7. Distance between two samples (DM2)

### Three sample statistics

1. Maximum likelihood coefficient of admixture (AML)

---

## For DNA sequence loci

For DNA sequence loci, the following set of summary statistics has been implemented.

### Single sample statistics

1. Number of distinct haplotypes (NHA)
2. Number of segregating sites (NSS)
3. Mean pairwise difference (MPD)
4. Variance of the number of pairwise differences (VPD)
5. Tajima’s D statistics (DTA)
6. Number of private segregating sites (PSS)
7. Mean of the numbers of the rarest nucleotide at segregating sites (MNS)
8. Variance of the numbers of the rarest nucleotide at segregating sites (VNS)

### Two sample statistics

1. Number of distinct haplotypes in the pooled sample (NH2)
2. Number of segregating sites in the pooled sample (NS2)
3. Mean of within sample pairwise differences (MP2)
4. Mean of between sample pairwise differences (MPB)
5. $F_{ST}$ between two samples (HST)

### Three sample statistics

1. Maximum likelihood coefficient of admixture (SML)