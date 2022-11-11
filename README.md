
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ECoHeN

<!-- badges: start -->
<!-- badges: end -->

The ECoHeN package is composed many useful functions when working with
heterogeneous networks. Functionality includes:

-   Extracting communities from a heterogeneous network (ECoHeN) with
    ECoHeN: `cluster_ECoHeN`,
-   Extracting a community from an arbitrary seed set (or list of seed
    sets): `extract_ECoHeN`
-   Extracting statistically significant communities (ESSC) with ESSC:
    `cluster_ESSC`,
-   Refining extracted communities: `refine_ECoHeN`,
-   Partitioning a heterogeneous network with ZCMod: `cluster_ZCMod`,
-   Evaluating the assortativity of a discovered community:
    `eval_community`,
-   Sampling random heterogeneous networks with the same collection of
    heterogeneous degree sequences: `sample_heterogeneous_dcm`,
-   Sampling random heterogeneous networks with specified block
    structure: `sample_heterogeneous_sbm`,
-   Visualizing the block structure of sampled networks with block
    structure: `viz_heterogeneous_sbm`,
-   Evaluating the overlap between simulated community structure and
    discovered communities: `eval_jaccard`,
-   Converting back and forth between membership vectors (reported by
    partitioning methods) and a collection of communities (reported by
    extraction methods).

The extraction procedure of the ECoHeN algorithm in written in C++, and
the algorithm can easily be parallelized across initialized seed sets
using the `future` package.

## Installation

You can install the development version of ECoHeN from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ConGibbs10/ECoHeN")
```

## Example Usage

We will recreate the empirical data analysis presented in
