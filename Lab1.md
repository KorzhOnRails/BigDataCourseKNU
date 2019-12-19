Lab1
================
Oleksandr Korzh
5/2/2019

1.  За допомогою download.file() завантажте любий excel файл з порталу
    <http://data.gov.ua> та зчитайте його (xls, xlsx – бінарні формати,
    тому встановить mode = “wb”. Виведіть перші 6 строк отриманого
    фрейму даних.

<!-- end list -->

``` r
library(readxl)
Sys.setlocale("LC_CTYPE", "russian")
```

    ## [1] "Russian_Russia.1251"

``` r
download.file('https://data.gov.ua/dataset/c445c6ea-f0c3-4167-abb1-5afb4a0e5499/resource/d55eebcf-4660-4919-96b3-4894be5a6cda/download/nuclear_safety_q1_2019.xlsx',destfile = "rad_data.xlsx","auto",TRUE,"wb")
rad_data <- read_excel("rad_data1.xlsx")
head(rad_data, n = 6)
```

    ## # A tibble: 6 x 16
    ##    year quarter station   irg irg_index `iodine_ radion~ `iodine_ radion~
    ##   <dbl>   <dbl> <chr>   <dbl>     <dbl>            <dbl> <chr>           
    ## 1  2018       1 ЗАЕС       89      0.13            260   <0,01           
    ## 2  2018       1 РАЕС      105      0.16            147   <0,01           
    ## 3  2018       1 ЮУАЕС      45      0.1              76   <0,01           
    ## 4  2018       1 ХАЕС       31      0.07             26.8 <0,01           
    ## 5  2018       2 ЗАЕС       84      0.12            262   <0,01           
    ## 6  2018       2 РАЕС      113      0.17             73   <0,01           
    ## # ... with 9 more variables: stable_radionuclides <dbl>, `stable_
    ## #   radionuclides_index` <chr>, cs_137_emission <dbl>, `co_60_
    ## #   emission` <dbl>, cs_137_dump <dbl>, co_60_dump <dbl>, volume <dbl>,
    ## #   index_radioactive_releas <dbl>, index_dump <chr>

2.  За допомогою download.file() завантажте файл
    getdata\_data\_ss06hid.csv за посиланням
    <https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv>
    та завантажте дані в R. Code book, що пояснює значення змінних
    знаходиться за посиланням
    <https://www.dropbox.com/s/dijv0rlwo4mryv5/PUMSDataDict06.pdf?dl=0>
    Необхідно знайти, скільки property мають value
$1000000+

<!-- end list -->

``` r
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv',"Fss06hid.csv","auto",TRUE,"w")
fss06hid <- read.csv("Fss06hid.csv")
sum(fss06hid$VAL == 24, na.rm = TRUE)
```

    ## [1] 53

3.  Зчитайте xml файл за посиланням
    <http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml>
    Скільки ресторанів мають zipcode 21231?

<!-- end list -->

``` r
library(XML)
download.file('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml',"restaurants.xml","auto",TRUE,"w")
restaurants <- xmlParse("restaurants.xml")
zipcodes <- xpathApply(restaurants,'//zipcode',xmlValue)
length(zipcodes[zipcodes == '21231'])
```

    ## [1] 127
