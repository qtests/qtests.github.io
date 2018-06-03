---
layout: post
title: QATAA With Bands
date: 2018-06-03
categories: tests
tags: [SIT, Systematic, Asset Allocation, QATAA]
---

### Quantitative Approach To TAA With Bands

this is a variation of the investment strategy discussed [earlier](http://qtests.github.io/QATAA/) that was originally designed and published by [Mebane T. Faber](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461). The main modification is the introduction of the +5%/-5% bands around the 10 month moving average to avoid whipsaws. Here the overall idea and the code are taken from the the [Systematic Investor's Blog](http://systematicinvestor.github.io/strategy/Strategy-TAA-BANDS).

### Data

``` r
# Get the data
# More details - http://systematicinvestor.github.io/Data-Proxy
#                and http://qtests.github.io/Starting-with-SIT/
# make.data.proxy ()
load('data/data.proxy.raw.Rdata')

tickers = '
US.STOCKS        = VTI + VTSMX
FOREIGN.STOCKS   = VEU + FDIVX
US.10YR.GOV.BOND = IEF + VFITX
REAL.ESTATE      = VNQ + VGSIX
COMMODITIES      = DBC + CRB
CASH             = BND + VBMFX
'

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data,
                 raw.data = data.proxy.raw, set.symbolnames = T, auto.assign = T)
```

``` r
for(i in data$symbolnames)
  data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

cat ("\nStart Dates: \n")
print(bt.start.dates(data, ""))
```

    ## Start Dates:
    ##                              
    ## CASH             "1986-12-11"
    ## US.10YR.GOV.BOND "1991-10-28"
    ## FOREIGN.STOCKS   "1991-12-27"
    ## US.STOCKS        "1992-04-27"
    ## COMMODITIES      "1994-01-03"
    ## REAL.ESTATE      "1996-05-13"

``` r
cat ("\nEnd Dates: \n")
print(bt.end.dates(data, ""))
```

    ## End Dates:
    ##                              
    ## US.10YR.GOV.BOND "2018-06-01"
    ## REAL.ESTATE      "2018-06-01"
    ## FOREIGN.STOCKS   "2018-06-01"
    ## CASH             "2018-06-01"
    ## COMMODITIES      "2018-06-01"
    ## US.STOCKS        "2018-06-01"

``` r
bt.prep(data, align='remove.na', fill.gaps = T)

cat ("\nLast Prices: \n")
print(last(data$prices))
```

    ## Last Prices:
	##
    ##            US.STOCKS FOREIGN.STOCKS US.10YR.GOV.BOND REAL.ESTATE COMMODITIES   CASH
    ## 2018-06-01     141.5          54.09           101.93       79.24       17.91  79.09

<br>
### Setup

``` r
# Setup
# -------------------------------------------------------------
data$universe <- data$prices > 0

# do not allocate to CASH
data$universe$CASH <- NA

prices <- data$prices * data$universe
n <- ncol(prices)
nperiods <- nrow(prices)

frequency <- 'months'

# find period ends, can be 'weeks', 'months', 'quarters', 'years'
period.ends <- endpoints(prices, frequency)
period.ends <- period.ends[period.ends > 0]

models <- list()

commission <- list(cps = 0.01, fixed = 10.0, percentage = 0.0)
```

The benchmark portfolio for the QATAA strategy is naturally a static portfolio that allocates weights to all asset classes equally. It turns out that the later method generates quite sound and respectful performance. One of the references regarding the 1/N asset allocation rule is [a paper by L. Garlappi, V. DeMiguel and R. Uppal](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=676997).


#### 1/N Allocation

``` r
# 1/N Allocation

data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)
models$EqualWeight = bt.run.share(data, clean.signal=F, commission = commission, trade.summary=T, silent=T)
```

<br>
#### QATAA Strategy

``` r
# The Quantitative Approach To Tactical Asset Allocation Strategy 

# compute 10 month moving average
sma = bt.apply.matrix(prices, SMA, 200)

# go to cash if prices falls below 10 month moving average
go2cash = prices < sma
go2cash = ifna(go2cash, TRUE)

# equal weight target allocation
target.allocation = ntop(prices,n)

# If asset is above it's 10 month moving average it gets allocation
weight = iif(go2cash, 0, target.allocation)

# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight)

data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
models$QATAA = bt.run.share(data, clean.signal=F, commission = commission, trade.summary=TRUE, silent=TRUE)
```

<br>
#### QATAA Strategy With Bands

``` r
# check price cross over with +5%/-5% bands
signal = iif(cross.up(prices, sma * 1.05), 1, iif(cross.dn(prices, sma * 0.95), 0, NA))
signal = ifna(bt.apply.matrix(signal, ifna.prev),0)

# If asset is above it's 10 month moving average it gets 20% allocation
weight = iif(signal == 1, 20/100, 0)


# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight)


data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
models$QATAA_Bands = bt.run.share(data, clean.signal=F, commission = commission, trade.summary=T, silent=T)
```

<br>
#### Performance Reporting

``` r
# Performance

plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)
```
![]({{ site.baseurl }}/img/2018-06-03-QATAA_Bands_files/unnamed-chunk-7-1.png)

``` r
cat ("\nPerformance Stats: \n\n")
```

    ## 
    ## Performance Stats:

``` r
knitr::kable(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T), padding = 2)
```

|                   | EqualWeight       | QATAA             | QATAA\_Bands      |
|-------------------|:------------------|:------------------|:------------------|
| Period            | May1996 - Jun2018 | May1996 - Jun2018 | May1996 - Jun2018 |
| Cagr              | 6.86              | 8.11              | 8.84              |
| Sharpe            | 0.59              | 1.03              | 1.15              |
| DVR               | 0.55              | 1.01              | 1.13              |
| Volatility        | 12.57             | 7.9               | 7.63              |
| MaxDD             | -47.84            | -17.19            | -13.24            |
| AvgDD             | -1.55             | -1.21             | -1.11             |
| VaR               | -1.05             | -0.75             | -0.71             |
| CVaR              | -1.93             | -1.18             | -1.12             |
| Exposure          | 99.75             | 99.75             | 99.75             |
| Win.Percent       | 59.2              | 63.8              | 64.1              |
| Avg.Trade         | 0.1               | 0.2               | 0.2               |
| Avg.Win           | 0.7               | 0.6               | 0.6               |
| Avg.Loss          | -0.7              | -0.5              | -0.5              |
| Best.Trade        | 6.14              | 4.11              | 4.11              |
| Worst.Trade       | -6.35             | -6.35             | -3.13             |
| WinLoss.Ratio     | 1.03              | 1.08              | 1.15              |
| Avg.Len           | 20.87             | 20.91             | 20.88             |
| Num.Trades        | 1325              | 1109              | 1056              |
| Win.Percent.Day   | 53.9              | 54.5              | 55.1              |
| Best.Day          | 7.1               | 4.1               | 4.1               |
| Worst.Day         | -7.6              | -4.9              | -4.9              |
| Win.Percent.Month | 64.7              | 68                | 69.5              |
| Best.Month        | 10.4              | 5.3               | 5.2               |
| Worst.Month       | -19.6             | -8.3              | -6.3              |
| Win.Percent.Year  | 78.3              | 82.6              | 91.3              |
| Best.Year         | 26.5              | 25.2              | 23.7              |
| Worst.Year        | -27.6             | -3.4              | -4.1              |

``` r
for(m in names(models)) 
  {
  cat(paste0("\n\n", m, ' strategy:\n'))
  plotbt.transition.map(models[[m]]$weight, name=m)
  legend('topright', legend = m, bty = 'n')

  cat (paste0( "\n\n", m, " Strategy -" , " Monthly Performance Stats: \n"))
  cat(knitr::kable(plotbt.monthly.table(models[[m]]$equity, make.plot = F), padding = 2, format = "markdown"), sep = "\n")

  cat (paste0( "\n\n", m, " Strategy -" , " Latest Allocation: \n"))
  print(knitr::kable(to.percent(last(models[[m]]$weight))))
}
```
	##
    ## EqualWeight strategy:

![]({{ site.baseurl }}/img/2018-06-03-QATAA_Bands_files/unnamed-chunk-7-2.png)

	##
    ## EqualWeight Strategy - Monthly Performance Stats: 
	##
	
|      |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct    |Nov   |Dec   |Year   |MaxDD  |
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:------|:-----|:-----|:------|:------|
|1996  |NA    |NA    |NA    |NA    |NA    |1.0   |-2.1  |2.6   |2.7   |1.2    |4.8   |2.1   |12.8   |-3.5   |
|1997  |1.2   |-0.1  |-0.7  |0.8   |3.9   |2.1   |3.7   |-2.3  |5.0   |-1.9   |-0.2  |0.3   |12.1   |-5.1   |
|1998  |0.5   |0.9   |2.4   |-0.3  |-1.4  |0.1   |-2.6  |-8.5  |4.5   |1.6    |0.5   |1.2   |-1.7   |-14.4  |
|1999  |0.4   |-3.2  |4.3   |4.3   |-1.9  |3.0   |-0.7  |0.9   |0.0   |0.9    |2.8   |5.3   |16.8   |-5.6   |
|2000  |-1.2  |2.5   |2.2   |-1.4  |0.8   |3.3   |0.2   |3.2   |-1.3  |-2.2   |-0.8  |2.6   |8.0    |-5.4   |
|2001  |1.0   |-3.3  |-3.6  |3.6   |-0.2  |-0.4  |-0.8  |-0.6  |-5.7  |-0.1   |3.1   |0.3   |-6.8   |-13.9  |
|2002  |-1.2  |1.3   |4.5   |0.1   |0.3   |-0.7  |-4.2  |1.7   |-3.1  |0.8    |2.3   |1.0   |2.5    |-10.5  |
|2003  |-0.1  |1.1   |-1.4  |3.8   |5.3   |1.2   |1.1   |2.3   |1.7   |2.9    |1.7   |4.4   |26.5   |-3.5   |
|2004  |2.4   |3.1   |1.7   |-5.1  |2.5   |0.3   |-0.4  |2.2   |2.2   |2.4    |2.6   |1.8   |16.4   |-7.4   |
|2005  |-2.0  |2.9   |-0.6  |-0.5  |1.6   |2.0   |3.3   |1.1   |0.9   |-3.0   |2.4   |2.4   |11.0   |-4.5   |
|2006  |4.4   |-1.0  |2.4   |1.6   |-2.3  |0.7   |1.6   |1.3   |0.2   |2.9    |3.6   |-0.4  |15.8   |-7.3   |
|2007  |1.7   |0.4   |0.5   |1.8   |1.0   |-2.0  |-1.8  |1.6   |4.6   |3.9    |-3.2  |-0.3  |8.2    |-7.2   |
|2008  |-1.7  |1.2   |1.1   |4.1   |1.6   |-3.2  |-2.1  |-1.3  |-6.5  |-19.6  |-8.5  |5.7   |-27.6  |-44.0  |
|2009  |-9.4  |-9.4  |6.0   |10.4  |7.4   |-1.7  |6.3   |3.5   |3.4   |-0.7   |4.7   |1.4   |21.5   |-23.6  |
|2010  |-4.1  |2.9   |4.3   |2.6   |-6.3  |-2.3  |6.8   |-1.9  |6.6   |3.3    |-1.4  |5.2   |15.7   |-10.5  |
|2011  |1.9   |3.0   |0.2   |4.0   |-1.2  |-2.2  |0.9   |-3.2  |-8.8  |8.7    |-1.3  |0.4   |1.3    |-15.8  |
|2012  |4.5   |2.5   |0.8   |0.3   |-6.1  |3.5   |2.1   |2.1   |0.6   |-1.3   |1.0   |1.5   |11.6   |-8.3   |
|2013  |2.5   |-0.5  |1.6   |1.9   |-2.4  |-2.5  |2.8   |-2.2  |2.6   |2.5    |-0.9  |0.3   |5.7    |-8.2   |
|2014  |-1.0  |4.1   |0.1   |1.3   |1.3   |1.4   |-1.7  |1.7   |-4.3  |2.1    |-0.7  |-2.4  |1.7    |-6.9   |
|2015  |0.5   |1.9   |-1.3  |1.2   |-0.8  |-1.9  |-0.8  |-4.1  |-1.2  |4.0    |-1.7  |-1.9  |-6.2   |-11.8  |
|2016  |-3.2  |-0.4  |6.0   |1.9   |0.7   |2.7   |1.1   |-0.7  |0.8   |-2.3   |-0.4  |2.5   |8.8    |-7.3   |
|2017  |1.0   |1.7   |-0.6  |0.3   |0.5   |0.4   |2.1   |0.4   |0.9   |1.3    |1.3   |0.9   |10.9   |-2.8   |
|2018  |1.4   |-4.2  |0.9   |0.7   |1.6   |0.3   |NA    |NA    |NA    |NA     |NA    |NA    |0.7    |-7.1   |
|Avg   |0.0   |0.3   |1.4   |1.7   |0.3   |0.2   |0.7   |0.0   |0.3   |0.3    |0.5   |1.6   |7.2    |-10.2  |

       
    ## 
	## EqualWeight Strategy - Latest Allocation: 
    ## 
    ##              US.STOCKS   FOREIGN.STOCKS   US.10YR.GOV.BOND   REAL.ESTATE   COMMODITIES   CASH 
    ## -----------  ----------  ---------------  -----------------  ------------  ------------  -----
    ## 2018-06-01   20%         20%              20%                20%           20%           0%   
    ## 
    ## 
    ## QATAA strategy:

![]({{ site.baseurl }}/img/2018-06-03-QATAA_Bands_files/unnamed-chunk-7-3.png)

    ## 
    ## QATAA Strategy - Monthly Performance Stats: 
	##
	
|      |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD  |
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:------|
|1996  |NA    |NA    |NA    |NA    |NA    |1.3   |0.2   |-0.2  |1.7   |2.2   |1.7   |-0.9  |6.2   |-1.8   |
|1997  |0.2   |0.0   |-0.7  |0.8   |3.9   |2.1   |3.7   |-2.3  |5.0   |-1.9  |-0.2  |1.5   |12.5  |-5.1   |
|1998  |1.0   |2.0   |2.5   |0.3   |0.0   |1.1   |-0.1  |-4.7  |2.3   |-0.6  |1.4   |1.5   |6.6   |-7.0   |
|1999  |1.4   |-2.7  |2.0   |2.0   |-2.0  |1.5   |-0.7  |0.9   |0.0   |0.2   |3.1   |4.5   |10.5  |-3.6   |
|2000  |-1.3  |3.0   |1.7   |-1.4  |0.8   |3.3   |0.2   |3.2   |-1.3  |-1.3  |2.3   |2.1   |11.9  |-4.3   |
|2001  |1.0   |0.0   |-0.6  |-0.1  |0.9   |1.5   |1.4   |1.6   |0.2   |0.8   |-1.6  |-0.2  |5.1   |-3.3   |
|2002  |0.5   |1.1   |-0.2  |0.1   |0.8   |0.7   |-2.5  |2.1   |1.2   |-0.8  |-0.4  |3.3   |6.0   |-5.2   |
|2003  |1.5   |2.3   |-1.8  |0.7   |5.3   |1.2   |1.1   |2.3   |1.3   |2.9   |1.7   |4.4   |25.2  |-4.0   |
|2004  |2.4   |3.1   |1.7   |-5.1  |0.9   |0.2   |-0.5  |2.4   |1.2   |2.4   |2.6   |1.8   |13.7  |-7.1   |
|2005  |-2.0  |2.9   |-0.6  |-0.7  |1.6   |2.0   |3.3   |1.1   |0.9   |-3.1  |2.4   |2.3   |10.5  |-4.5   |
|2006  |4.4   |-0.9  |2.5   |1.7   |-2.3  |0.7   |1.5   |1.3   |0.2   |2.8   |2.5   |-0.4  |14.7  |-7.4   |
|2007  |1.7   |-0.4  |0.5   |1.8   |1.0   |-2.0  |-0.2  |0.6   |4.0   |3.7   |-0.9  |0.9   |10.9  |-5.6   |
|2008  |0.2   |2.5   |0.3   |0.5   |1.0   |-1.5  |-1.8  |-0.5  |-2.4  |-8.3  |4.7   |5.1   |-1.1  |-17.2  |
|2009  |-2.5  |-0.6  |1.5   |-0.2  |0.1   |0.0   |4.5   |3.5   |3.4   |-0.7  |4.7   |1.4   |15.7  |-4.9   |
|2010  |-4.4  |2.1   |4.3   |2.6   |-6.3  |-1.1  |2.6   |0.3   |0.8   |3.3   |-1.4  |5.2   |7.6   |-9.7   |
|2011  |1.9   |3.1   |0.2   |4.0   |-1.4  |-2.2  |0.9   |-3.3  |-4.4  |-0.2  |-0.7  |1.3   |-1.4  |-13.5  |
|2012  |2.7   |0.3   |0.8   |0.3   |-6.2  |1.8   |1.3   |0.5   |0.6   |-1.3  |0.6   |1.5   |2.7   |-7.7   |
|2013  |2.5   |-0.7  |1.5   |2.8   |-2.4  |-2.1  |1.5   |-2.8  |1.6   |2.0   |-0.6  |0.3   |3.5   |-8.2   |
|2014  |-0.9  |2.3   |0.1   |1.3   |1.3   |1.4   |-1.7  |2.2   |-3.0  |3.1   |1.4   |0.3   |7.9   |-3.1   |
|2015  |2.6   |-0.7  |0.1   |-1.4  |-0.2  |-1.7  |0.8   |-2.9  |0.9   |-0.2  |-0.3  |-0.4  |-3.4  |-7.9   |
|2016  |0.2   |0.7   |2.6   |-0.3  |0.7   |2.7   |1.1   |-0.7  |0.8   |-2.4  |-0.3  |1.6   |7.1   |-4.9   |
|2017  |1.0   |1.1   |-0.6  |0.3   |0.9   |0.6   |1.4   |0.5   |0.9   |1.3   |1.3   |0.9   |10.3  |-2.8   |
|2018  |1.6   |-2.9  |0.2   |0.5   |0.9   |0.0   |NA    |NA    |NA    |NA    |NA    |NA    |0.3   |-5.5   |
|Avg   |0.7   |0.8   |0.8   |0.5   |0.0   |0.5   |0.8   |0.2   |0.7   |0.2   |1.1   |1.7   |8.0   |-6.3   |
       
       
    ## 
	## QATAA Strategy - Latest Allocation: 
    ## 
    ##              US.STOCKS   FOREIGN.STOCKS   US.10YR.GOV.BOND   REAL.ESTATE   COMMODITIES   CASH 
    ## -----------  ----------  ---------------  -----------------  ------------  ------------  -----
    ## 2018-06-01   20%         0%               0%                 0%            20%           60%  
    ## 
    ## 
    ## QATAA_Bands strategy:

![]({{ site.baseurl }}/img/2018-06-03-QATAA_Bands_files/unnamed-chunk-7-4.png)

    ## 
    ## QATAA_Bands Strategy - Monthly Performance Stats: 
	##
	
|      |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD  |
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:------|
|1996  |NA    |NA    |NA    |NA    |NA    |1.3   |0.2   |-0.2  |1.7   |2.2   |1.7   |-0.9  |6.2   |-1.8   |
|1997  |0.2   |0.1   |-0.5  |1.1   |3.5   |1.4   |3.7   |-2.3  |3.4   |-1.1  |-0.5  |0.0   |9.2   |-4.1   |
|1998  |1.5   |2.4   |2.1   |1.1   |0.0   |1.1   |-0.1  |-4.7  |2.3   |-0.6  |0.3   |1.6   |6.8   |-7.0   |
|1999  |1.3   |-2.7  |2.0   |2.0   |-1.2  |1.6   |-0.7  |0.9   |0.0   |0.8   |3.1   |4.5   |12.1  |-3.7   |
|2000  |-1.4  |3.0   |1.8   |-2.8  |0.8   |3.3   |0.2   |3.2   |-1.3  |-2.2  |2.3   |2.1   |9.2   |-5.4   |
|2001  |1.0   |0.0   |-0.6  |-0.1  |0.9   |1.5   |1.4   |1.6   |0.2   |0.9   |-0.1  |-0.2  |6.6   |-2.8   |
|2002  |0.5   |1.1   |-0.1  |1.6   |0.8   |0.7   |-2.5  |2.4   |2.3   |-0.8  |-0.4  |3.3   |9.1   |-5.2   |
|2003  |1.5   |2.3   |-1.8  |0.1   |4.2   |1.2   |1.1   |2.3   |1.7   |2.9   |1.7   |4.4   |23.7  |-4.0   |
|2004  |2.4   |3.1   |1.7   |-5.1  |2.4   |-0.2  |-0.4  |2.2   |2.2   |2.4   |2.6   |1.8   |15.9  |-7.4   |
|2005  |-2.0  |2.9   |-0.6  |-0.5  |1.6   |2.0   |3.3   |1.1   |0.9   |-3.0  |2.4   |2.4   |11.0  |-4.5   |
|2006  |4.4   |-1.0  |2.4   |1.6   |-2.3  |0.7   |1.6   |1.3   |0.2   |2.8   |2.5   |0.0   |15.0  |-7.3   |
|2007  |2.2   |-0.3  |0.3   |1.8   |1.0   |-2.0  |0.0   |0.6   |4.0   |3.7   |-0.9  |0.9   |11.6  |-5.3   |
|2008  |-1.3  |2.5   |0.3   |0.5   |0.5   |0.2   |-1.8  |-0.5  |-2.4  |-2.6  |4.7   |5.1   |4.8   |-11.6  |
|2009  |-2.5  |-0.6  |1.5   |-0.2  |0.1   |-0.2  |4.3   |3.5   |3.4   |-0.7  |4.7   |1.4   |15.3  |-4.9   |
|2010  |-4.1  |2.9   |4.3   |2.6   |-6.3  |-1.1  |2.6   |1.3   |0.8   |1.7   |-1.4  |5.2   |8.1   |-9.7   |
|2011  |1.9   |3.0   |0.2   |4.0   |-1.2  |-2.2  |0.9   |-3.3  |-2.1  |-0.2  |0.0   |1.3   |2.1   |-13.2  |
|2012  |0.6   |0.4   |1.0   |0.8   |-3.7  |1.8   |1.3   |0.5   |0.1   |-1.3  |1.0   |1.5   |3.9   |-5.1   |
|2013  |2.5   |-0.5  |1.6   |1.9   |-2.4  |-2.3  |2.2   |-2.8  |2.9   |2.0   |0.4   |0.3   |5.7   |-8.3   |
|2014  |-0.9  |2.4   |0.0   |1.2   |1.7   |1.1   |-0.8  |2.1   |-2.9  |2.8   |1.4   |0.3   |8.4   |-4.0   |
|2015  |2.2   |-0.6  |0.5   |-1.4  |-0.2  |-2.4  |0.9   |-2.9  |0.9   |-0.1  |-0.4  |-0.4  |-4.1  |-8.0   |
|2016  |1.6   |1.0   |0.7   |-0.3  |0.4   |3.3   |1.1   |-0.7  |0.8   |-2.3  |-0.4  |1.6   |6.7   |-5.2   |
|2017  |1.1   |1.2   |-0.1  |0.4   |0.8   |0.3   |1.4   |0.8   |0.5   |0.8   |0.8   |1.4   |9.7   |-2.0   |
|2018  |2.2   |-2.9  |0.2   |0.5   |0.9   |0.2   |NA    |NA    |NA    |NA    |NA    |NA    |1.1   |-5.5   |
|Avg   |0.7   |0.9   |0.8   |0.5   |0.1   |0.5   |0.9   |0.3   |0.9   |0.4   |1.2   |1.7   |8.6   |-5.9   |
       
       
    ## 
	## QATAA_Bands Strategy - Latest Allocation: 
    ## 
    ##              US.STOCKS   FOREIGN.STOCKS   US.10YR.GOV.BOND   REAL.ESTATE   COMMODITIES   CASH 
    ## -----------  ----------  ---------------  -----------------  ------------  ------------  -----
    ## 2018-06-01   20%         20%              0%                 0%            20%           40%
