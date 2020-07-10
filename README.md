# Graphical User Interface for DIYABC-RF software

We provide a graphical user interface (GUI) for the DIYABC-RF software [1], 
called DIYABC-RF GUI.

Please check the project [website](https://diyabc.github.io/) for 
additional information and detailed documentation.

DIYABC-RF GUI is available as a standalone application, or in a R package called `diyabcGUI` as a `shiny` web app. You can either install the standalone app, or the `diyabcGUI` package and run the DIYABC-RF GUI as a standard `shiny` app, c.f. [below](#r-package-installation).

DIYABC-RF GUI is a set of tools implementing Approximate Bayesian 
Computation (ABC) combined with supervised machine learning based on 
Random Forests (RF), for model choice and parameter inference in the context 
of population genetics analysis.`diyabcGUI` provides a user-friendly interface 
for command-line softwares `diyabc` (https://github.com/diyabc/diyabc) and 
`abcranger` (https://github.com/diyabc/abcranger).

## Installation

### Requirements

- `zip` program

### Standalone app

For Windows and MacOS users, please download the latest release at 
<https://github.com/diyabc/diyabcGUI/releases/latest>.

For the moment, the standalone app is not available for Linux users. 
Nonetheless, Linux users can install the `diyabcGUI` package, c.f. 
[below](#r-package-installation), and 
run the DIYABC-RF GUI as a standard `shiny` app.

> **Note:** if encountering instability in the standalone app, we recommend to install and use the `shiny` app available in the `diyabcGUI` R package, c.f. [below](#r-package-installation).


### R package installation

1. Install `devtools` package if not available
```R
install.packages("devtools")
```

> **Note:** if you encounter any issue when installing `devtools`, please check the [next section](#potential-issue-with-devtools).

2. Install `diyabcGUI` package
```R
devtools::install_github(
    "diyabc/diyabcGUI",
    subdir = "R-pkg"
)
```

3. **The first time after installation**, you need to download required binary files (e.g. `diyabc` and `abcranger` command line tools) by running
```R
library(diyabcGUI)
diyabcGUI::dl_all_latest_bin()
```

> **Note:** you can run this command from time to time to update required binary files in case new versions were released.

4. Launch the interface
```R
library(diyabcGUI)
diyabcGUI::diyabc()
```

The function `diyabc()` will launch DIYABC-RF GUI as a standard `shiny` web app, that you will be able to use either in your web browser or in the Rstudio `shiny` app viewer.

To run simultaneously mutliple instances of DIYABC-RF GUI, e.g. to simultaneously manage and run multiple projects, you just need to run several times the function `diyabc()` from R (this is not possible from RStudio).

### Potential issue with devtools

You may encounter some issue when installing devtools, please check the official 
[devtools page](https://github.com/r-lib/devtools).

Following `devtools` recommandations, make sure you have a working development environment.

- Windows: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
- Mac: Install Xcode from the Mac App Store.
- Linux: Install a compiler and various development libraries (details vary across different flavors of Linux).

For Ubuntu users [here](https://www.digitalocean.com/community/tutorials/how-to-install-r-packages-using-devtools-on-ubuntu-18-04) is a guide to install devtools requirement (users of other Linux distributions may still find it useful).


### Shiny server installation

As a `shiny` app, DIYABC-RF GUI can be installed and run from a Shiny server. To do so, you just need (on Unix system, please adapt for Windows server) to:

1. install the `diyabcGUI` package on your system, c.f. [above](#r-package-installation)
2. manage the file access rights so that the Shiny server has access to the R package installation directory
3. Create a symbolic link to the directory given by the R command `system.file("application", package = "diyabcGUI")` inside the `site_dir` folder configured in `/etc/shiny-server/shiny-server.conf` (by default `/srv/shiny-server`), e.g.:
```bash
ln -s /path/to/R_LIBS/diyabcGUI/application /srv/shiny-server/diyabc
```
4. DIYABC-RF GUI is now available on your server at `https://my.shiny.server.address/diyabc`

---

## Reference

[1] Collin F-D, Raynal L, Durif G, Gautier M, Vitalis R, Lombaert E., Marin J-M, Estoup A (2020) DIYABC Random Forest v1.0: extending approximate Bayesian computation with supervised machine learning to infer demographic history from genetic polymorphisms. Submitted to Molecular Ecology Resources.