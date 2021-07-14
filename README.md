# covid_eus

This repository contains R code to download, harmonize, and join various sources of data to enable demographic analyses of the COVID-19 situation in Spanish autnomous communities. 

Instructions to replicate:
1) install [git](https://git-scm.com/downloads) and [Rstudio](https://www.rstudio.com/products/rstudio/download/) on your computer
2) Clone this repo: Open RStudio. In the menu, click `file` | `new project` | `version control` | `git` | now copy `https://github.com/timriffe/covid_eus.git` into the repository url, and select a location on your computer and click `create repository`
3) open `data_prep.R`. Install any missing R packages listed in the header.
4) the first time running code, step through it, so you see the data sources and operations. Executing the whole thing creates a file `data_ccaa.rds`

It should follow a format like this:

```
   year_iso week_iso fecha      CCAA_iso sexo   edad value    pob variable        tasa stand_nac stand_pv CCAA     
      <dbl>    <dbl> <date>     <chr>    <chr> <dbl> <dbl>  <int> <chr>          <dbl>     <dbl>    <dbl> <chr>    
 1     2020        2 2020-01-06 AN       H         0  0.25 193268 exceso    0.00000129    0.0207   0.0193 Andalucía
 2     2020        2 2020-01-06 AN       H         5  0    225718 exceso    0             0.0243   0.0235 Andalucía
 3     2020        2 2020-01-06 AN       H        10 -0.5  254427 exceso   -0.00000197    0.0267   0.0251 Andalucía
 4     2020        2 2020-01-06 AN       H        15  3    240375 exceso    0.0000125     0.0255   0.0239 Andalucía
 5     2020        2 2020-01-06 AN       H        20 -0.25 232121 exceso   -0.00000108    0.0248   0.0223 Andalucía
 6     2020        2 2020-01-06 AN       H        25  0    244391 exceso    0             0.0263   0.0217 Andalucía
```

Note the variable called `tasa` in fact is not a rate. Rather it is defined as `value / pob`. If you want to use it as a rate then on a week timescale you should multiply `tasa` by `365.25 / 7` . If instead you want the cumulative rate at the end of the series, then multiply population by `k = fecha %>% range() %>% diff() %>% as.numeric()` or similar. It would be preferable to have interpolated population estimates at a few time points, but given the width of age groups it wouldn't make much difference. Note the `excess` is given in 5-year age groups, whereas all other (stacked) variables are given in 10-year age groups. For our figures this makes no difference since we use age-standardized summary measures, but if you want to repurpose this data object you might want to either split the isciii data into 5-year age groups or else group excess to 10 year age groups. For further advice along these lines shoot me a line.

5) the script `report_figures.R` assumes you have `data_ccaa.rds` created from the previous step. 
  Some of the annotations in the ridge plots created in that script will be misplaced if date ranges change...

