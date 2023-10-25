# TransProR
[![R-CMD-check](https://github.com/SSSYDYSSS/TransProR/workflows/R-CMD-check/badge.svg)](https://github.com/SSSYDYSSS/TransProR/actions)
<!-- badges: start -->

<!-- badges: end -->

The goal of TransProR is to ...

## Installation

You can install the released version of TransProR from CRAN with:

``` r
install.packages("TransProR")
```

You can install the development version of TransProR like so:

``` r
install.packages("devtools")
devtools::install_github("SSSYDYSSS/TransProR")

install.packages("remotes")
devtools::install_github("SSSYDYSSS/TransProR")
```

## System Requirements

-   Python version 3.10 or higher is required.

## Python Dependencies

This package requires several Python libraries to function properly. The following are the main dependencies:

-   TransProPy

Please ensure these Python libraries are installed in your environment before you proceed with using this package. You can quickly install them using the command below:

### Installation and Environment Setup

To run this project, we recommend using `conda`, a popular Python environment and package manager. Please follow the steps below to set up your environment:

### Creating and Activating the Environment

First, if you have not installed `conda` yet, please visit the [Miniconda](https://docs.conda.io/en/latest/miniconda.html) website and follow the instructions for installation. Once you have `conda` installed, you can create a new environment named "TransPro" that includes all the dependencies required to run this project.

Run the following command in your command line to create a new environment:

``` bash
conda create --name TransPro python=3.10 # or higher version if preferred.
pip3 install TransProPy 
```

This version helps guide users through the installation process more smoothly, taking into account various common scenarios they might encounter. It suggests considering the environment management system and potential system-specific quirks, improving the overall user experience.

For more detailed information or troubleshooting, please refer to the documentation of each individual Python package.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(TransProR)
## basic example code
```

## Code of Conduct

Please note that the TransProR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms. "\# TransProR"
