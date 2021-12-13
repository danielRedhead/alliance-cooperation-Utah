# alliance-cooperation-Utah


Data and code associated with "Political Alliance Formation and Cooperation Networks in the Utah State Legislature" published in Human Nature
----------------------------

# Requirements for analyses:

- R (v3.6.3 or greater): https://cran.r-project.org
- RSiena (1.2-25 or greater): https://www.stats.ox.ac.uk/~snijders/siena/
- dplyr: https://dplyr.tidyverse.org


# Packages used for visualisation:

- iGraph: https://igraph.org/r/
- Rcolorbrewer: https://cran.r-project.org/web/packages/RColorBrewer/index.html
- tidyverse: https://www.tidyverse.org

# Instructions to reproduce analyses:

In R, set the working directory to that containing this readme file. On Mac, for example, you could say

```
setwd('~/Desktop/alliance-cooperation-Utah')
```

Check to see if you're in the right place by typing dir() and see whether this readme file is present.


The analyses use six data files as input:

```

'2005_matrix.csv' - The co-sponsorship network at time point 1 (2005).
'2006_matrix.csv' - The co-sponsorship network at time point 2 (2006).
'2007_matrix.csv' - The co-sponsorship network at time point 3 (2007).
'2008_matrix.csv' - The co-sponsorship network at time point 4 (2008).
'composition.txt' - A formatted composition list of when legislators join and leave the study. 
'covariates.csv' - Data for all legislators who appear in the study.
```

We also include a composition list that is not formatted:

```
'join_and_leave.csv' - A non-formatted composition list of when legislators join and leave the study. 
```

Within the  ```covariates.csv``` file there are the following variables:

```
**leg_name:** string ID variable of legislator's name
**leader:** binary for if legislator was member of republican leadership that year (0=not a leader, 1=leader)
**female:** binary for sex (0=male, 1=female)
**democrat:** binary for party (0=republican, 1=democrat)
**senator:** binary for chamber(0=house representative, 1=senator)
**intro:** count for number of bills the legislator introduced as an author in that year
**prior_year:** count for number of years the legislator served in office prior to that year
**sponsored:** count for number of bills the legislator floor sponsored for another legislator in that year
```

When the project folder is the working directory, you may run the  longitudinal analysis (assuming that you have installed all of the dependencies) by calling

```
source('./code/1-analyses.R')
```

We recommend, however, that you go into '1-analyses.R' and run the code line-by-line yourself.

To run the analyses with no composition change, and then complete the goodness-of-fit checks, either run

```
source('./code/2-goodness-of-fit.R')
```

or open the analysis script and run it in blocks.

The analyses may take some time. The total time until completion may vary by machine.

To allow for reproduction of the figures and tables in the manuscript, we have included the following script:

```
3-descriptives-plots.R
```
We advise to run the script in blocks once the analyses have finished running. 

# Manuscript folder

Alongside the materials to reproduce our analyses, we have also include output figures and tables--and a non-formatted pdf of our manuscript--in the folder '2-manuscript'.


The project is maintained by Daniel Redhead (daniel_redhead@eva.mpg.de) and is hosted at https://github.com/danielRedhead
