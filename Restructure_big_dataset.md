restructure_dataset
================
2025-04-21

## R Markdown

importing the huge data set that needs lots of restructuring.

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.2.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.2.3

``` r
library(reshape2)
```

    ## Warning: package 'reshape2' was built under R version 4.2.3

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:reshape2':
    ## 
    ##     smiths

``` r
#import the data
tabn331_10 <- read_excel("tabn331.10.xlsx", 
      skip = 4, n_max = 43)
```

    ## New names:
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...31`
    ## • `` -> `...32`

``` r
#make a copy of your data set

big_dataset <- tabn331_10

#remove the rows and columns that are only NA values

big_dataset <- big_dataset[-c(2, 6, 14, 18, 22, 25, 39), ]


big_dataset <- big_dataset[, colSums(is.na(big_dataset)) != nrow(big_dataset)]

#rename the columns
names(big_dataset) <- c("category", "total_in_thousands", "total_anyaid", "se_total_anyaid", "federal_anyaid", "se_federal_anyaid", "nonfederal_anyaid", "se_nonfederal_anyaid", "total_grants", "se_total_grant", "federal_grants", "se_federal_grants", "nonfederal_grants", "se_nonfederal_grants", "total_loans", "se_total_loans", "federal_loans", "se_federal_loans", "nonfederal_loans","remove_1" ,"se_nonfederal_loans", "total_workstudy","remove_2" ,"se_total_workstudy")

#remove those final columns
big_dataset <- big_dataset %>% select(-remove_1, -remove_2)

#add a column for the year of the dataset
big_dataset <- big_dataset %>% mutate(year = "2019-20")
```

Now I have the data cleaned with all the NA’s removed. I am going to
create subsets of the data for each category such as age, gender, race,
etc.

``` r
#dataset for gender
big_dataset_gender <- big_dataset[2:4, ]

#dataset for 
big_dataset_race <- big_dataset[5:11, ]

#for age
big_dataset_age <- big_dataset[12:14, ]

#for marital status
big_dataset_marital <- big_dataset[15:17, ]

#for attendance status
big_dataset_attendance <- big_dataset[18:19, ]

#for dependency status and income (income by dependency status)
big_dataset_dependency_and_income <- big_dataset[20:32, ]

#for housing status
big_dataset_housing <- big_dataset[33:36, ]
```

The next step is to do this for another year of data and merge
