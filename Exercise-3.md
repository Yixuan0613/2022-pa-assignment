Exercise 3
================

``` r
# set path for R to find our data
data_path <- "D:/R/Assignment/PA assignment/"
```

## 1. Load data

``` r
library(arrow) # to be able to load data in the .parquet format
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

``` r
app_data_sample
```

    ## # A tibble: 2,018,477 x 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ... with 2,018,467 more rows, and 12 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>

## Get gender for examiners

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names
```

    ## # A tibble: 2,595 x 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # ... with 2,585 more rows

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
examiner_names_gender
```

    ## # A tibble: 1,822 x 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # ... with 1,812 more rows

``` r
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)
# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4551308 243.1    7560935 403.8  4949270 264.4
    ## Vcells 49539144 378.0   92446242 705.4 79854901 609.3

## Guess the examiner’s race

``` r
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```

    ## # A tibble: 3,806 x 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # ... with 3,796 more rows

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 3,806 x 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198
    ## # ... with 3,796 more rows

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))
examiner_race
```

    ## # A tibble: 3,806 x 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333      0.643 white
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372      0.861 white
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309      0.702 white
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185      0.947 white
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271      0.827 white
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324      0.687 white
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463      0.574 black
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313      0.620 white
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318      0.666 white
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198      0.945 Asian
    ## # ... with 3,796 more rows

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4965978 265.3    7560935 403.8  7560935 403.8
    ## Vcells 53338303 407.0   92446242 705.4 90509913 690.6

## Examiner’s tenure

``` r
library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates
```

    ## # A tibble: 2,018,477 x 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # ... with 2,018,467 more rows

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018)
```

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
examiner_dates
```

    ## # A tibble: 5,649 x 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # ... with 5,639 more rows

``` r
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  4980171 266.0   13552308  723.8  13552308  723.8
    ## Vcells 65717241 501.4  133298588 1017.0 132275769 1009.2

## Art Unit size

``` r
table(app_data_sample$examiner_art_unit)
```

    ## 
    ##  1600  1609  1611  1612  1613  1614  1615  1616  1617  1618  1619  1620  1621 
    ##   101    54  6961  9164  5842  9100 14133 15721 11921 10661  6292     1 20440 
    ##  1622  1623  1624  1625  1626  1627  1628  1629  1631  1632  1633  1634  1635 
    ##  6356  8188 24586 25419 24930 10816 12476  8178 10382  8586  9857 14249  6670 
    ##  1636  1637  1638  1639  1641  1642  1643  1644  1645  1646  1647  1648  1649 
    ##  9766 13028 13096  5226  7415  9044  8235 14135 11770 10829 11959 12339  7616 
    ##  1651  1652  1653  1654  1655  1656  1657  1658  1661  1662  1663  1671  1672 
    ## 13422 13939  9542  8103 10199 11205  8531   449   839  3679  4248  7440  3643 
    ##  1673  1674  1675  1676  1677  1678  1700  1709  1711  1712  1713  1714  1715 
    ##  5324  4568  5362  3424  2221  3372    10    35 15735 11869 14152 11705  7493 
    ##  1716  1717  1718  1721  1722  1723  1724  1725  1726  1727  1728  1729  1731 
    ##  7537  5889  2164  8490 11967  9786 13140 13102 10318  5726   902  5764 12032 
    ##  1732  1733  1734  1735  1736  1741  1742  1743  1744  1745  1746  1747  1751 
    ## 12600 13949 10690  6667  8866  8189 14990 10628 11202 12181 11343  7065  6592 
    ##  1752  1753  1754  1755  1756  1757  1758  1759  1761  1762  1763  1764  1765 
    ##  4991  4569  8985  9120 10741  2782  4162  6265 17590 18222 10115 10656 18347 
    ##  1766  1767  1768  1771  1772  1773  1774  1775  1776  1777  1778  1779  1781 
    ##  6081  7033  3332 11677 11344 11151 13195  7657 10558  7103  7094  3487  5953 
    ##  1782  1783  1784  1785  1786  1787  1788  1789  1791  1792  1793  1794  1795 
    ##  6396  6662  7196  6294  7556  7491  6091  4501 17251 14829 18513 13728 17280 
    ##  1796  1797  1798  1799  2100  2109  2111  2112  2113  2114  2115  2116  2117 
    ## 19589 24128  4832  3274    46    11  7403  9786  9277  7953  7706  7763  8062 
    ##  2118  2121  2122  2123  2124  2125  2126  2127  2128  2129  2131  2132  2133 
    ##  2568  9263  5506  6984  1825  4662  4764  4358  7635  7683  3368  4202  5000 
    ##  2134  2135  2136  2137  2138  2139  2141  2142  2143  2144  2145  2146  2151 
    ##  1939  3603  3486  3368  2909  2382  4558  4050  3985  3098  2107   166  1373 
    ##  2152  2153  2154  2155  2156  2157  2158  2159  2161  2162  2163  2164  2165 
    ##  5602  2712  4650  6137  5166  5285  4175  5129  6772  7814  6980  5277  5788 
    ##  2166  2167  2168  2169  2171  2172  2173  2174  2175  2176  2177  2178  2179 
    ##  5438  6078  6216  5417  4993  3738  8531  5545  5436  6043  3352  5807  5327 
    ##  2181  2182  2183  2184  2185  2186  2187  2188  2189  2191  2192  2193  2194 
    ##  6826  6694  7577  7168  7389  5781  5759  4797  4983  6853  7223  8362  7261 
    ##  2195  2196  2197  2198  2199  2400  2403  2411  2412  2413  2414  2415  2416 
    ##  5430  4420  3500  1880  3118    71     1  2956  2633  1608  2574  2009  5002 
    ##  2419  2421  2422  2423  2424  2425  2426  2427  2431  2432  2433  2434  2435 
    ##  2809  4797  5948  3303  4696  4401  3457  3641  8878  4036  4815  5941  6089 
    ##  2436  2437  2438  2439  2441  2442  2443  2444  2445  2446  2447  2448  2449 
    ##  6189  4887  5087  4708  5810  3575  5262  4998  4555  2882  7375  5376  2380 
    ##  2451  2452  2453  2454  2455  2456  2457  2458  2459  2461  2462  2463  2464 
    ##  5495  4438  4411  6955  5252  4307  7680  2591  1118  5741  6242  5302 12724 
    ##  2465  2466  2467  2468  2469  2471  2472  2473  2474  2475  2476  2477  2478 
    ##  4931  5659  6197  4053  4037  7570  6155  5965  6033  4261  5973  5121  4733 
    ##  2479  2481  2482  2483  2484  2485  2486  2487  2488  2489  2491  2492  2493 
    ##  2417  4456  4740  3974  5649  3832  7801  4495  4320  1752  3508  3753  3857 
    ##  2494  2495  2496  2497  2498 
    ##  3011  2693  1625  2515  1457

row (1,3,5,7,9,……):art unit row (2,4,6,8,10,…..):art unit size

``` r
count = table(app_data_sample$examiner_art_unit)
art_unit = attr(count,"dimnames")[[1]]
art_unit_size_list = as.numeric(count)
art_unit_size_df = data.frame(art_unit = art_unit, art_unit_size = art_unit_size_list)
```

``` r
merge_df = merge(app_data_sample, art_unit_size_df, by.x = "examiner_art_unit", by.y = "art_unit")
```

## Art Unit gender ratio

``` r
ratio_table = table(merge_df$gender,merge_df$examiner_art_unit)
```

``` r
male = as.numeric(ratio_table[2,1:291]) 
female = as.numeric(ratio_table[1,1:291]) 
art_unit = attr(ratio_table,"dimnames")[[2]]
gender_df = data.frame(art_unit = art_unit,male = male,female = female)
gender_df$gender_ratio = male/female
```

``` r
merge_df_2 = merge(merge_df, gender_df, by.x = "examiner_art_unit", by.y = "art_unit")
```

The ratio is male:female

## OLS regression model

Because the OLS model requires each variable is independent, I chose not
to put gender and art unit gender ratio in the same model.

## OLS regression model 1

Gender + Race + Art Unit Size

``` r
library(caTools)
model_1 = lm(tenure_days ~ gender+race+art_unit_size,data = merge_df_2)
summary(model_1)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ gender + race + art_unit_size, data = merge_df_2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5532.5  -549.9   448.1   762.8  1407.6 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)    5.171e+03  2.512e+00 2058.903   <2e-16 ***
    ## gendermale    -7.992e+01  1.741e+00  -45.902   <2e-16 ***
    ## raceblack      1.454e+02  4.714e+00   30.836   <2e-16 ***
    ## raceHispanic  -3.070e+02  4.907e+00  -62.568   <2e-16 ***
    ## raceother     -5.673e+02  5.026e+01  -11.288   <2e-16 ***
    ## racewhite      1.736e+01  1.940e+00    8.951   <2e-16 ***
    ## art_unit_size  4.159e-02  1.552e-04  267.926   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1068 on 1714611 degrees of freedom
    ##   (303859 observations deleted due to missingness)
    ## Multiple R-squared:  0.0478, Adjusted R-squared:  0.04779 
    ## F-statistic: 1.434e+04 on 6 and 1714611 DF,  p-value: < 2.2e-16

## OLS regression model 2

Race + Art Unit Size + Art Unit Gender Ratio

``` r
merge_df_3 = merge_df_2[!is.na(merge_df_2$gender_ratio),]
merge_df_3 = merge_df_3[!is.infinite(merge_df_3$gender_ratio),]
model_2 = lm(tenure_days ~ race+art_unit_size+gender_ratio,data = merge_df_3)
summary(model_2)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ race + art_unit_size + gender_ratio, 
    ##     data = merge_df_3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5610.7  -534.4   430.3   765.6  1398.3 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)    5.093e+03  1.894e+00 2689.197   <2e-16 ***
    ## raceblack      9.819e+01  3.852e+00   25.491   <2e-16 ***
    ## raceHispanic  -3.057e+02  4.611e+00  -66.304   <2e-16 ***
    ## raceother     -5.806e+02  5.048e+01  -11.503   <2e-16 ***
    ## racewhite      3.148e+01  1.701e+00   18.502   <2e-16 ***
    ## art_unit_size  4.326e-02  1.407e-04  307.551   <2e-16 ***
    ## gender_ratio  -2.083e-02  9.309e-03   -2.238   0.0252 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1073 on 2014150 degrees of freedom
    ## Multiple R-squared:  0.04973,    Adjusted R-squared:  0.04973 
    ## F-statistic: 1.757e+04 on 6 and 2014150 DF,  p-value: < 2.2e-16

## Which model is a better fit?

Over all, this two models don’t have a very significant different.

1.  If we look at the residuals of two models, both of them have a
    similar symmetry status. The median of the model 2 is closer to the
    mean, which is 0, but not a big difference compared to model 1.
2.  The significant codes of all variables in model 1 are \*\**. The
    significant codes of “gender ratio” in model 2 is *.
3.  The residual standard error of model 2 is bigger than that of model
    1.
4.  The adjusted R-squared of model 1 is slightly smaller than that of
    model 2.
5.  Therefore, I think model 1 fits the data a little bit better than
    model 2.

## The effects of the variables in the models

1.  As we can tell from the “Coefficients” section in the model outputs.
    Race has a much more significant impact on tenure days than other
    variables. Being white and black is positive related with tenure
    days, and being other races is negative related.
2.  Gender also has an impact. Male stay longer than female.

## What recommendations can you offer?

1.  The organization should look into its policy regarding minorities.
    Whether the environment is friendly enough for races other than
    black and white? Obviously white people is the major race in the US
    and usually has a better survival conditions. As well as the right
    of African American people, we all know that the US is making
    efforts to accommodate them. However, what about the rights of
    others minority groups?
2.  The organization also need to look into its policy regarding female
    employees. What makes them stay shorter than male employees?
3.  Since there are different results regarding different gender and
    races, the organization should pay attention that do these
    differences impact the organization negatively?
