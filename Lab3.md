Лабораторна робота № 3. Зчитування даних з WEB.
================
Oleksandr Korzh
5/2/2019

В цій лабораторній роботі необхідно зчитати WEB сторінку з сайту
IMDB.com зі 100 фільмами 2017 року виходу за посиланням
«<http://www.imdb.com/search/title?count=100&release_date=2017,2017&title_ty>
pe=feature». Необхідно створити data.frame «movies» з наступними даними:
номер фільму (rank\_data), назва фільму (title\_data), тривалість
(runtime\_data). Для виконання лабораторної рекомендується використати
бібліотеку «rvest». CSS селектори для зчитування необхідних даних:
rank\_data: «.text-primary», title\_data: «.lister-item-header a»,
runtime\_data: «.text-muted .runtime». Для зчитування url
використовується функція read\_html, для зчитування даних
по CSS селектору – html\_nodes і для перетворення зчитаних html даних в
текст - html\_text. Рекомендується перетворити rank\_data та
runtime\_data з тексту в числові значення. При формуванні дата фрейму
функцією data.frame рекомендується використати параметр
«stringsAsFactors = FALSE».

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## Registered S3 method overwritten by 'rvest':
    ##   method            from
    ##   read_xml.response xml2

``` r
html = read_html("http://www.imdb.com/search/title?count=100&release_date=2017,2017&title_type=feature")
rank_data <- html_text(html_nodes(html,'.text-primary'))
rank_data = as.numeric(rank_data)
title_data <- html_text(html_nodes(html,'.lister-item-header a'))
runtime_data <- html_text(html_nodes(html,'.text-muted .runtime'))
runtime_data = as.numeric(gsub(" min", "", runtime_data))
movies <- data.frame(Rank = rank_data, Title = title_data, Runtime = runtime_data, stringsAsFactors = FALSE)
```

1.  Виведіть перші 6 назв фільмів дата
    фрейму.

<!-- end list -->

``` r
head(movies, 6)$Title
```

    ## [1] "<U+0422><U+043E><U+0440>: <U+0420><U+0430><U+0433><U+043D><U+0430><U+0440><U+043E><U+043A>" "<U+041B><U+044E><U+0434><U+0438><U+043D><U+0430>-<U+043F><U+0430><U+0432><U+0443><U+043A>: <U+041F><U+043E><U+0432><U+0435><U+0440><U+043D><U+0435><U+043D><U+043D><U+044F> <U+0434><U+043E><U+0434><U+043E><U+043C><U+0443>"
    ## [3] "<U+0412><U+0430><U+0440><U+0442><U+043E><U+0432>i <U+0413><U+0430><U+043B><U+0430><U+043A><U+0442><U+0438><U+043A><U+0438> 2" "Unicorn Store"                  
    ## [5] "1+1: <U+041D><U+043E><U+0432><U+0430> i<U+0441><U+0442><U+043E><U+0440>i<U+044F>" "<U+041D><U+0430><U+0439><U+0432><U+0435><U+043B><U+0438><U+0447><U+043D>i<U+0448><U+0438><U+0439> <U+0448><U+043E><U+0443><U+043C><U+0435><U+043D>"

2.  Виведіть всі назви фільмів с тривалістю більше 120
    хв.

<!-- end list -->

``` r
subset(movies, Runtime > 120)$Title
```

    ##  [1] "<U+0422><U+043E><U+0440>: <U+0420><U+0430><U+0433><U+043D><U+0430><U+0440><U+043E><U+043A>"
    ##  [2] "<U+041B><U+044E><U+0434><U+0438><U+043D><U+0430>-<U+043F><U+0430><U+0432><U+0443><U+043A>: <U+041F><U+043E><U+0432><U+0435><U+0440><U+043D><U+0435><U+043D><U+043D><U+044F> <U+0434><U+043E><U+0434><U+043E><U+043C><U+0443>"
    ##  [3] "<U+0412><U+0430><U+0440><U+0442><U+043E><U+0432>i <U+0413><U+0430><U+043B><U+0430><U+043A><U+0442><U+0438><U+043A><U+0438> 2"
    ##  [4] "1+1: <U+041D><U+043E><U+0432><U+0430> i<U+0441><U+0442><U+043E><U+0440>i<U+044F>"
    ##  [5] "<U+0414><U+0438><U+0432><U+043E>-<U+0416>i<U+043D><U+043A><U+0430>"
    ##  [6] "<U+0417><U+043E><U+0440><U+044F><U+043D>i <U+0432>i<U+0439><U+043D><U+0438>: <U+0415><U+043F>i<U+0437><U+043E><U+0434> 8 - <U+041E><U+0441><U+0442><U+0430><U+043D><U+043D>i <U+0414><U+0436><U+0435><U+0434><U+0430>i"
    ##  [7] "<U+0412><U+043E><U+043D><U+043E>"        
    ##  [8] "<U+0422><U+043E><U+0439>, <U+0445><U+0442><U+043E> <U+0431>i<U+0436><U+0438><U+0442><U+044C> <U+043F><U+043E> <U+043B><U+0435><U+0437><U+0443> 2049"
    ##  [9] "<U+041F>i<U+0440><U+0430><U+0442><U+0438> <U+041A><U+0430><U+0440><U+0438><U+0431><U+0441><U+044C><U+043A><U+043E><U+0433><U+043E> <U+043C><U+043E><U+0440><U+044F>: <U+041F><U+043E><U+043C><U+0441><U+0442><U+0430> <U+0421><U+0430><U+043B><U+0430><U+0437><U+0430><U+0440><U+0430>"
    ## [10] "<U+0424><U+043E><U+0440><U+043C><U+0430> <U+0432><U+043E><U+0434><U+0438>"
    ## [11] "Call Me by Your Name"                    
    ## [12] "<U+0414><U+0436><U+043E><U+043D> <U+0423>i<U+043A> 2"
    ## [13] "<U+0427><U+0443><U+0436><U+0438><U+0439>: <U+0417><U+0430><U+043F><U+043E><U+0432>i<U+0442>"
    ## [14] "<U+041A><U+0440><U+0430><U+0441><U+0443><U+043D><U+044F> i <U+0427><U+0443><U+0434><U+043E><U+0432><U+0438><U+0441><U+044C><U+043A><U+043E>"
    ## [15] "<U+041B><U+043E><U+0433><U+0430><U+043D>: <U+0420><U+043E><U+0441><U+043E><U+043C><U+0430><U+0445><U+0430>"
    ## [16] "<U+041A><U+043E><U+0440><U+043E><U+043B><U+044C> <U+0410><U+0440><U+0442><U+0443><U+0440>: <U+041B><U+0435><U+0433><U+0435><U+043D><U+0434><U+0430> <U+043C><U+0435><U+0447><U+0430>"
    ## [17] "<U+0424><U+043E><U+0440><U+0441><U+0430><U+0436> 8"
    ## [18] "<U+041C><U+0430><U+0442><U+0438>!"       
    ## [19] "<U+0422><U+0440><U+0430><U+043D><U+0441><U+0444><U+043E><U+0440><U+043C><U+0435><U+0440><U+0438>: <U+041E><U+0441><U+0442><U+0430><U+043D><U+043D>i<U+0439> <U+043B><U+0438><U+0446><U+0430><U+0440>"
    ## [20] "The Shack"                               
    ## [21] "Kingsman: <U+0417><U+043E><U+043B><U+043E><U+0442><U+0435> <U+043A>i<U+043B><U+044C><U+0446><U+0435>"
    ## [22] "<U+0413><U+0440><U+0430> <U+041C><U+043E><U+043B><U+043B>i"
    ## [23] "<U+0412><U+0430><U+043B><U+0435><U+0440>i<U+0430><U+043D> i <U+043C>i<U+0441><U+0442><U+043E> <U+0442><U+0438><U+0441><U+044F><U+0447>i <U+043F><U+043B><U+0430><U+043D><U+0435><U+0442>"
    ## [24] "<U+041F><U+043E><U+0441><U+0442><U+0440>i<U+043B> <U+0432> <U+0431><U+0435><U+0437><U+043E><U+0434><U+043D><U+044E>"
    ## [25] "<U+041C><U+0435><U+0442><U+0435><U+043B><U+0438><U+043A>"
    ## [26] "<U+041F><U+0440><U+0438><U+043C><U+0430><U+0440><U+043D><U+0430> <U+043D><U+0438><U+0442><U+043A><U+0430>"
    ## [27] "<U+0412><U+043E><U+0440><U+043E><U+0433><U+0438>"
    ## [28] "Brawl in Cell Block 99"                  
    ## [29] "<U+0412><U+0431><U+0438><U+0432><U+0441><U+0442><U+0432><U+043E> <U+0441><U+0432><U+044F><U+0449><U+0435><U+043D><U+043D><U+043E><U+0433><U+043E> <U+043E><U+043B><U+0435><U+043D><U+044F>"
    ## [30] "Only the Brave"                          
    ## [31] "<U+0422><U+0435><U+043C><U+043D>i <U+0447><U+0430><U+0441><U+0438>"
    ## [32] "Saban's <U+041C><U+043E><U+0433><U+0443><U+0442><U+043D>i <U+0440><U+0435><U+0439><U+043D><U+0434><U+0436><U+0435><U+0440><U+0438>"
    ## [33] "<U+0421>i<U+043C> <U+0441><U+0435><U+0441><U+0442><U+0435><U+0440>"
    ## [34] "<U+0423><U+0441>i <U+0433><U+0440><U+043E><U+0448>i <U+0441><U+0432>i<U+0442><U+0443>"
    ## [35] "The Glass Castle"                        
    ## [36] "<U+0412>i<U+0439><U+043D><U+0430> <U+0437><U+0430> <U+043F><U+043B><U+0430><U+043D><U+0435><U+0442><U+0443> <U+043C><U+0430><U+0432><U+043F>"
    ## [37] "<U+0414><U+0440><U+0443><U+0436><U+0438><U+043D><U+0430> <U+0434><U+043E><U+0433><U+043B><U+044F><U+0434><U+0430><U+0447><U+0430> <U+0437><U+043E><U+043E><U+043F><U+0430><U+0440><U+043A><U+0443>"

3.  Скільки фільмів мають тривалість менше 100 хв.

<!-- end list -->

``` r
nrow(movies[movies$Runtime < 100, ])
```

    ## [1] 15
