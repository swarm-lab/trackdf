# trackdf - A data frame class for tracking data

[![Travis-CI Build Status](https://travis-ci.org/swarm-lab/trackdf.svg?branch=master)](https://travis-ci.org/swarm-lab/trackdf)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/swarm-lab/trackdf?branch=master&svg=true)](https://ci.appveyor.com/project/swarm-lab/trackdf)

**Please submit all suggestions and bug reports 
[here](https://github.com/swarm-lab/trackdf/issues). Thanks!**

---

## 1 - Description <img src="reference/figures/logo.png" align="right" alt="" width="120" />

[`trackdf`](https://swarm-lab.github.io/trackdf/) is a R package that aims to 
standardize and accelerate the processing of data describing animal trajectories 
captured in the lab (e.g., video tracking) or in the field (e.g., GPS trackers). 
`trackdf` provides a data frame-like class based on the popular 
[tibble](https://cran.r-project.org/web/packages/tibble/) class and is 
compatible with both base R and most of the functions from the 
[tidyverse](https://www.tidyverse.org/) (e.g., [dplyr](https://dplyr.tidyverse.org/), 
[ggplot2](https://ggplot2.tidyverse.org/)). 

[`trackdf`](https://github.com/swarm-lab/swaRm) is a work in progress. Functions 
are not yet in a stable state and are likely to change as the package gets 
developed. 

---

## 2 - Quick start guides

+ [1 - Installing `trackdf`](https://swarm-lab.github.io/trackdf/articles/z1_install.html)
+ [2 - Building a track table](https://swarm-lab.github.io/trackdf/articles/z2_build.html)
+ [3 - Manipulating a track table](https://swarm-lab.github.io/trackdf/articles/z3_manipulate.html)

---

## 3 - FAQ

**How can I help?**

`trackdf` is an open-source project, meaning that you can freely modify its code
and implement new functionalities. If you have coding skills, you are more than 
welcome to contribute new code or code improvement by submitting pull requests 
on the GitHub repository of the project at: https://github.com/swarm-lab/trackdf. 
I will do my best to review and integrate them quickly. 

If you do not feel like contributing code, you can also help by submitting bug 
reports and feature requests using the issue tracker on the GitHub repository of 
the project at: https://github.com/swarm-lab/trackdf/issues. These are extremely 
helpful to catch and correct errors in the code, and to guide the development of 
`trackdf` by integrating functionalities requested by the community. 
