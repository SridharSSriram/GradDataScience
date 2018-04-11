
Assignment 1: 24 January 2018
=============================

### Question 1

1.  For the longest time, I have been incredibly passionate about analyzing the way people speak. As a closeted writer and novelist during my childhood, I would continuously listen in on the conversations surrounding me on a local, state-wide, national, and global level. Through each of these “tiers,” I would make it a point to understand how the communication methods of leaders at each of these stages changed. As a policy student, I can now see that these variations in communication and discourse also inevitably affect the communities, populations, and people surrounding the respective leaders. As a result, I am genuinely interested in analyzing the way public figures speak by developing a political sentiment analyzer that extends beyond simply “conservative” or “liberal” tags for bodies of text. The data for such a project exists — there are datasets collected by Harvard researchers that harvested a database of 280 million tweets during the 2016 election that were either tweeted directly from or relating to the accounts of the Democratic Party, Republican Party, and all of their respective candidates. Using this information, I want to devise a multivariate classifier that breaks down the intricacies of political ideologies and creates classifications such as “center-left”, “moderate”, “center-right”, and other tags for the tweets and social media posts of public figures. In doing so, I wish to then draw a parallel between the social media presence of elected officials and their constituents, and hopefully draw insights relating to how the political polarization of a public figure’s social presence impacts the ideologies of the people they represent.

### Question 2

dataset link: <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FPDI7IN>

Associated with this link are a series of documents, all containing **only** tweet IDs, relating to: \* Candidates and key election hashtags \* Democratic candidates \* Democratic Convention \* Democratic Party \* Election Day \* First presidential debate \* GOP Convention \* Republican candidates \* Republican Party \* Second presidential debate \* Third presidential debate \* Vice Presidential debate

Beyond this, I would also like to collect the tweet IDs/account information relating to the people directly represented by some of the candidates during the election.

### Question 3

citation: <http://delivery.acm.org/10.1145/2900000/2896396/a11_Elghazaly.pdf?ip=165.230.224.61&id=2896396&acc=ACTIVE%20SERVICE&key=7777116298C9657D%2E56792D02161BECDE%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35&__acm__=1516816318_33159649ae79c9860b85184fa5efbf84>

The paper “Political Sentiment Analysis Using Twitter Data,” published by Tarek Elghazaly, Amal Mahmoud, and Hesham A. Hefny of Cairo University (Egypt) laid out the foundations of what a political sentiment analyzer would look like. The publication provides an in-depth explanation of the methodologies used — tokenization, stop-word removal, normalization, etc. — and discusses the frameworks of two different classifiers that were tested for the implementation of the project: A Naive Bayes Classifier and a Support Vector Machine. The data set provided in question 2 does not necessarily deal with too many of the theoretical elements expressed in this paper, as the dataset is a collection of tweet IDs. Nevertheless, the dataset does provide the “corpora” — the body of text — for my project that will help to build out the context for the classifiers.

### Question 4

    The biggest flaw with this dataset is the genuine lack of specificity, coupled with the breadth of the data. Because the information provided is a collection of tweets, there isn’t a whole lot of context/information for me to build complex classifiers on simply because tweets do not generally contain much information. Nevertheless, I think using Twitter data is necessary because analyzing how people talk on Twitter lets me look at how people converse in their “normal” lives, as opposed to the polished language that is reserved for articles.
    Another issue is that because the dataset is incredibly extensive — nearly 280 million tweet IDs are provide — creating a classifier that generates any concrete, statistically consistent insights might be difficult given the fickle nature of Twitter users. Similarly, because this project will be analyzing text, I will be challenging myself in adapting the methods that we devise in class for text-based analysis.

### Question 5

1.  

-   **presidents**
    -   This dataset is a collection of quarterly approval ratings for US Presidents from 1945 to 1974. The data is structured in a 120-value time series.
-   **UCBAdmissions**
    -   This dataset is a collection of the admission results/information regarding the Admit status, gender, and department of 4526 students during 1973.

1.  The "Seatbelts" dataset contains 192 entries with 8 columns: `DriversKilled`,`drivers`, `front`, `rear`,`kms`,`PetrolPrice`, `VanKilled`, `law` detailing information from 1969 to 1985.

``` r
str(Seatbelts)
```

    ##  Time-Series [1:192, 1:8] from 1969 to 1985: 107 97 102 87 119 106 110 106 107 134 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr [1:8] "DriversKilled" "drivers" "front" "rear" ...

``` r
summary(Seatbelts)
```

    ##  DriversKilled      drivers         front             rear      
    ##  Min.   : 60.0   Min.   :1057   Min.   : 426.0   Min.   :224.0  
    ##  1st Qu.:104.8   1st Qu.:1462   1st Qu.: 715.5   1st Qu.:344.8  
    ##  Median :118.5   Median :1631   Median : 828.5   Median :401.5  
    ##  Mean   :122.8   Mean   :1670   Mean   : 837.2   Mean   :401.2  
    ##  3rd Qu.:138.0   3rd Qu.:1851   3rd Qu.: 950.8   3rd Qu.:456.2  
    ##  Max.   :198.0   Max.   :2654   Max.   :1299.0   Max.   :646.0  
    ##       kms         PetrolPrice        VanKilled           law        
    ##  Min.   : 7685   Min.   :0.08118   Min.   : 2.000   Min.   :0.0000  
    ##  1st Qu.:12685   1st Qu.:0.09258   1st Qu.: 6.000   1st Qu.:0.0000  
    ##  Median :14987   Median :0.10448   Median : 8.000   Median :0.0000  
    ##  Mean   :14994   Mean   :0.10362   Mean   : 9.057   Mean   :0.1198  
    ##  3rd Qu.:17202   3rd Qu.:0.11406   3rd Qu.:12.000   3rd Qu.:0.0000  
    ##  Max.   :21626   Max.   :0.13303   Max.   :17.000   Max.   :1.0000

``` r
dim(Seatbelts)
```

    ## [1] 192   8

``` r
nrow(Seatbelts)
```

    ## [1] 192

``` r
ncol(Seatbelts)
```

    ## [1] 8

1.  

`UKDriverDeaths` is simply a time series detailing the *monthly totals* of car-related deaths in Great Britain, while `Seatbelts` provides a much more extensive description of the information through with 8 variables for each of the 192 entries.

1.  

``` r
Seatbelts[1,1]
```

    ## DriversKilled 
    ##           107

returns the name of the first column: `DriversKilled`

``` r
Seatbelts[29,5]
```

    ##   kms 
    ## 13587

returns the 29th entry for column 5 (kilometers)

1.  

If I was interested in combining both `UKDriverDeaths` and `Seatbelts`, then I would probably combine the two datasets in a graph to see how the progression of time affected both the monthly totals and the 8 respective variables for each entry in the `Seatbelts` dataset.

### Question 6

1.  Comments should be the perfect combination of brief and descriptive. There is no need to write a narrative for the code snippets -- just enough for any reader to understand the code that the comment is referring to

2.  Preceding functions, comments should give a descriptive explanation of:
    -   the purpose of the function
    -   the parameters passed into the functin
    -   the return value of the function

3.  Comments should explain *why* the code was written, not *what* the code actually is

4.  All comments begin with \# followed by a space, and inline comments require two spaces before the `#`

5.  Comments should be thought of as sending a text/tweet to whatever reader shows up 6 months, 1 year, or 5 years down the line

### Question 7

1.  

``` r
`^`(3,2)
```

or

``` r
3^2
```

1.  

``` r
#sqrt(x)
sqrt(9)
```

    ## [1] 3

1.  

``` r
#log(x)
log(2)
```

    ## [1] 0.6931472

R uses the natural logarithm as the default logarithm `log` function. Therefore, the base is the mathematical constant `e`. There are options to play around with the inputs / declarations of the `log` function (`log`,`logb`,`log10`,`log2`).

1.  

``` r
5.432*10^3
```

    ## [1] 5432

1.  While `<-` and `=` are perfectly equivalent when used as assignment operators, the difference between the `=` and the `<-` operators is that `=` sets the value of an object/variable to a respective value in the local scope of a function (if being used), while `<-` allows for a declaration of the value to be accessed anywhere in the code. For example, declaring `testing=1:10` within the median function like: `median(testing=1:10)` and then accessing `testing` afterwards results in an error, while:

``` r
median(testing <- 1:10)
```

    ## [1] 5.5

``` r
testing
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10

does not. Furthermore, `=` is generally used to set the values of parameters in functions and other declarations.

1.  

``` r
x <- c(1, 5, 8, 4)
```

1.  

``` r
y <- c(1, 2, -2, 5)
```

1.  

``` r
x+y
```

    ## [1] 2 7 6 9

1.  

``` r
x-y
```

    ## [1]  0  3 10 -1

1.  

``` r
x*y
```

    ## [1]   1  10 -16  20

1.  

``` r
length(x)
```

    ## [1] 4

1.  

``` r
z = seq(from=1, to=8, by =2)
z
```

    ## [1] 1 3 5 7

1.  

``` r
z+x
```

    ## [1]  2  8 13 11

1.  

``` r
length(z)
```

    ## [1] 4

1.  

``` r
X <- matrix(1:4, nrow = 2, ncol = 2, byrow = F)
X
```

    ##      [,1] [,2]
    ## [1,]    1    3
    ## [2,]    2    4

1.  

``` r
2*X
```

    ##      [,1] [,2]
    ## [1,]    2    6
    ## [2,]    4    8

1.  

``` r
X%*%X
```

    ##      [,1] [,2]
    ## [1,]    7   15
    ## [2,]   10   22

1.  

``` r
solve(X)
```

    ##      [,1] [,2]
    ## [1,]   -2  1.5
    ## [2,]    1 -0.5

`solve()` essentially solves the system of equations given by the inputted square matrix.

1.  

``` r
X[,2]
```

    ## [1] 3 4

1.  

``` r
X[1,]
```

    ## [1] 1 3

### Question 8

1.  

``` r
1 == 2
```

    ## [1] FALSE

-   The `==` is an equivalence check. If the elements on either side of the `==` are equal, then the expression returns `TRUE` and `FALSE` otherwise
-   Trying to assign any values to `TRUE` returns the following error: `invalid(do_set) left-hand side to assignment`, but assigning values to 'True' works perfectly.

``` r
True <- 5
```

1.  

``` r
1!=2
```

    ## [1] TRUE

-   The `!` is a sign meaning "not." In the context of `!=`, this is a check for inequality.

1.  

``` r
Bob <- True
Bob
```

    ## [1] 5

Typing `Bob` returns 5 because we assigned the value `True` to `Bob`. Because in problem a, we assigned 5 to True, True is now an object with assigned valued 5.

``` r
Bob + Bob
```

    ## [1] 10

Typing `Bob + Bob` will return `10` because, based on the explanation above, Bob represents the value 5, so adding Bob to itself will give you 2 times the value of Bob.

1.  Because R likes to operate on vectors of the same length, R will "recycle" a vector on during vector operations if the given vectors are of unequal lengths. For example, if there are two vectors `a = (1,2,3,4)` and `b = (9,10,2)`, and we wanted to apply `a+b` then the resulting vector would be `(10, 12, 5, 13)` because the first element of vector `y` is added to the last element of vector `x` in order to complete the operation.

``` r
a <- c(1,2,3,4)
b <- c(9,10,2)
a+b
```

    ## Warning in a + b: longer object length is not a multiple of shorter object
    ## length

    ## [1] 10 12  5 13

We'll take vector `c = (1,9)` and vector `d = (10, 9, 8, 7, 1)`

``` r
c <- c(1,9)
d <- c(10, 9, 8, 7, 1)
c==d
```

    ## Warning in c == d: longer object length is not a multiple of shorter object
    ## length

    ## [1] FALSE  TRUE FALSE FALSE  TRUE

As discussed above, R simply repeats the smaller vector -- in this case, `c` -- in its equality operation until the end of vector `d`. As a result, the output is `FALSE TRUE FALSE FALSE TRUE` because the first element of `c` is checked with the last element of `d`.

1.  

``` r
v <- c(1:100)
v
```

    ##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
    ##  [18]  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
    ##  [35]  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51
    ##  [52]  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
    ##  [69]  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
    ##  [86]  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100

``` r
str(v>50)
```

    ##  logi [1:100] FALSE FALSE FALSE FALSE FALSE FALSE ...

### Question 9

1.  

``` r
STR <- "This is a character string."
```

1.  

-   is 'alpha' `==` 'beta'?

    ``` r
    "alpha"=="beta"
    ```

        ## [1] FALSE

-   is 'alpha' `>` 'beta'?

    ``` r
    "alpha">"beta"
    ```

        ## [1] FALSE

-   is 'alpha' `>` 'Alpha'?

    ``` r
    "alpha">"Alpha"
    ```

        ## [1] FALSE

    -   R treats all capitalized letters as having higher ordering than all lowercased letters.

    ``` r
    "alpha" > "Zebra"
    ```

        ## [1] FALSE

1.  

``` r
nchar("alpha")
```

    ## [1] 5

``` r
nchar("beta")
```

    ## [1] 4

`nchar` simply acts as a character length function.

1.  

``` r
D <- c("A","B","C","z","e")
sort(D)
```

    ## [1] "A" "B" "C" "e" "z"

1.  

``` r
x1 <- c(1 , 2 , 3)
x2 <- c("a" , "b")
c(x1,x2)
```

    ## [1] "1" "2" "3" "a" "b"

``` r
c(x2,x1)
```

    ## [1] "a" "b" "1" "2" "3"

The first concatenate function appends the elements in x2 to x1, and vice versa.

``` r
Combine <- c(x1,x2)
str(Combine)
```

    ##  chr [1:5] "1" "2" "3" "a" "b"

`str()` returns the internal structure of any given R object. In other words, the function returns the data type and the associated values wihtin the structure. To determine the datatype, `str()` refers to the data type determined by the `mode()` function.

### Question 10

1.  It took me a around 3 hours to complete the homewokr (with researching papers, etc.)
2.  Not too difficult at all.

3.  The parts that challenged us to think about the code / analysis really helped, while the parts that were simply copy and paste provided minimal help.

4.  I think I would like to see this week's homework assignment before giving any instrumental advice/feedback. Overall, the assignment was great!
