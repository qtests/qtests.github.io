### Systematic Investor

The Systematic Investor blog has contributed many interesting ideas since the first post in 2011 (<https://systematicinvestor.wordpress.com/2011/10/03/hello-world/>). During the last year there was no new analysis published, but the R code of the Systematic Investor Toolbox (SIT) is still available on github. It is easy to download and to test the SIT. The details are briefly summarised on the About page, <http://systematicinvestor.github.io/about/>.

Probably the easiest way to install the SIT is to download the code directly from the Internet.

``` r
# Cleaning
rm(list = ls(all=TRUE))


# Download SIT library
library(curl)
con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
source(con)
close(con)
```

The package contains a `plota.test` function that could be used to test the toolbox. The function produces few image files with graphs that could be viewed with the standard `browseURL`. If the images popped up – the SIT was installed and works fine.

``` r
# Test that the package is installed well
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

plota.test()
for (fName in list.files(pattern="*.png")) browseURL(fName)
```

For example, the first image looks like

![]({{ site.baseurl }}/images/plot1.png)

One of the biggest worries for systematic investors is a high quality data. The SIT has a function `make.data.proxy` that creates a nice time series collection from available data samples on the Internet. Most of the data is collected automatically, however one needs to download Thomson Reuters / CoreCommodity CRB Index (<http://www.corecommodityllc.com/CoreIndexes.aspx>) data file manually. The full list of time series collected is in the table below.

| Time Series | Start Date | End Date   |
|:------------|:-----------|:-----------|
| AGG         | 1986-12-11 | 2017-12-29 |
| BIL         | 1954-01-04 | 2017-12-29 |
| BWX         | 1992-01-07 | 2017-12-29 |
| COM         | 1994-01-03 | 2017-12-29 |
| CORP.FI     | 1980-01-02 | 2017-12-29 |
| DBC         | 1994-01-03 | 2017-12-29 |
| EAFE        | 1996-04-29 | 2017-12-29 |
| EEM         | 1994-05-04 | 2017-12-29 |
| EFA         | 1996-04-29 | 2017-12-29 |
| EMB         | 1994-12-30 | 2017-12-29 |
| EMER.EQ     | 1994-05-04 | 2017-12-29 |
| EMER.FI     | 1994-12-30 | 2017-12-29 |
| EUROPE.EQ   | 1986-09-30 | 2017-12-29 |
| EWJ         | 1992-09-15 | 2017-12-29 |
| GLD         | 1968-04-01 | 2017-12-29 |
| GOLD        | 1968-04-01 | 2017-12-29 |
| GSG         | 1994-01-03 | 2017-12-29 |
| HYG         | 1980-01-02 | 2017-12-29 |
| ICF         | 1996-05-13 | 2017-12-29 |
| IEF         | 1991-10-28 | 2017-12-29 |
| IEV         | 1986-09-30 | 2017-12-29 |
| INTL.BOND   | 1992-01-07 | 2017-12-29 |
| IWM         | 1980-01-02 | 2017-12-29 |
| JAPAN.EQ    | 1992-09-15 | 2017-12-29 |
| LONG.TR     | 1986-05-19 | 2017-12-29 |
| LQD         | 1980-01-02 | 2017-12-29 |
| MID.TR      | 1991-10-28 | 2017-12-29 |
| QQQ         | 1985-10-01 | 2017-12-29 |
| RE          | 1996-05-13 | 2017-12-29 |
| RE.US       | 1996-05-13 | 2017-12-29 |
| REXUS       | 1996-05-13 | 2017-12-29 |
| RWX         | 1996-05-13 | 2017-12-29 |
| SHY         | 1962-01-02 | 2017-12-29 |
| TECH.EQ     | 1985-10-01 | 2017-12-29 |
| TIP         | 1991-05-20 | 2017-12-29 |
| TIPS        | 1991-05-20 | 2017-12-29 |
| TLT         | 1986-05-19 | 2017-12-29 |
| US.BOND     | 1986-12-11 | 2017-12-29 |
| US.CASH     | 1954-01-04 | 2017-12-29 |
| US.EQ       | 1980-01-02 | 2017-12-29 |
| US.HY       | 1980-01-02 | 2017-12-29 |
| US.MID      | 1998-05-21 | 2017-12-29 |
| US.SMCAP    | 1980-01-02 | 2017-12-29 |
| VB          | 1980-01-02 | 2017-12-29 |
| VO          | 1998-05-21 | 2017-12-29 |
| VTI         | 1980-01-02 | 2017-12-29 |

Since the package is a bit dated the function requires minor corrections.

    make.data.proxy <- function() 
    {
      load.packages('quantmod')
      raw.data = env()
      filename = 'data/TR_CC-CRB.xlt'
      # TRJ_CRB file was downloaded from the 
      # http://www.corecommodityllc.com/CoreIndexes.aspx
      # select TR/CC-CRB Index-Total Return and click "See Chart"
      # on Chart page click "Download to Spreadsheet" link
      # copy TR_CC-CRB, downloaded file, to data folder
      if(file.exists(filename)) {
        temp = extract.table.from.webpage( join(readLines(filename)), 'EODValue' )
        temp = join( apply(temp, 1, join, ','), '\n' )
        raw.data$CRB = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
      }
      filename = 'data/TB3M.Rdata'
      if(!file.exists(filename)) {
        TB3M = quantmod::getSymbols('DTB3', src='FRED', auto.assign = FALSE)
        save(TB3M, file=filename)
      }
      load(file=filename)
      TB3M[] = ifna.prev(TB3M)
      raw.data$TB3M = make.stock.xts(processTBill(TB3M, timetomaturity = 1/4, 261))
      filename = 'data/TB3Y.Rdata'
      if(!file.exists(filename)) {
        TB3Y = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)
        save(TB3Y, file=filename)
      }
      load(file=filename)
      TB3Y[] = ifna.prev(TB3Y)
      raw.data$TB3Y = make.stock.xts(processTBill(TB3Y, timetomaturity = 3, 261))
      filename = 'data/TB10Y.Rdata'
      if(!file.exists(filename)) {
        TB10Y = quantmod::getSymbols('DGS10', src='FRED', auto.assign = FALSE)
        save(TB10Y, file=filename)
      }
      load(file=filename)
      TB10Y[] = ifna.prev(TB10Y)
      raw.data$TB10Y = make.stock.xts(processTBill(TB10Y, timetomaturity = 10, 261))
      filename = 'data/TB20Y.Rdata'
      if(!file.exists(filename)) {
        TB20Y = quantmod::getSymbols('GS20', src='FRED', auto.assign = FALSE)
        save(TB20Y, file=filename)
      }
      load(file=filename)
      TB20Y[] = ifna.prev(TB20Y)
      raw.data$TB20Y = make.stock.xts(processTBill(TB20Y, timetomaturity = 20, 12))
      filename = 'data/GOLD.Rdata'
      if(!file.exists(filename)) {
        GOLD = bundes.bank.data.gold()
        save(GOLD, file=filename)
      }
      load(file=filename)
      raw.data$GOLD = make.stock.xts(GOLD)
      filename = 'data/NAREIT.xls'
      if(!file.exists(filename)) {
        url = 'http://returns.reit.com/returns/MonthlyHistoricalReturns.xls'
        download.file(url, filename,  mode = 'wb')
      }
      load.packages('readxl')
      temp = read_excel(filename, sheet='Index Data', skip=7)
      i.nna <- !is.na(temp$Date)
      NAREIT = make.xts(temp$Index[i.nna], as.Date(temp$Date[i.nna]))
      raw.data$NAREIT = make.stock.xts(NAREIT)
      tickers = '
      COM = DBC;GSG + CRB
      RExUS = [RWX] + VNQ + VGSIX
      RE = [RWX] + VNQ + VGSIX
      RE.US = [ICF] + VGSIX
      EMER.EQ = [EEM] + VEIEX
      EMER.FI = [EMB] + PREMX
      GOLD = [GLD] + GOLD,
      US.CASH = [BIL] + TB3M,
      SHY + TB3Y,
      US.HY = [HYG] + VWEHX
      US.BOND = [AGG] + VBMFX
      INTL.BOND = [BWX] + BEGBX
      JAPAN.EQ = [EWJ] + FJPNX
      EUROPE.EQ = [IEV] + FIEUX
      US.SMCAP = IWM;VB + NAESX
      TECH.EQ = [QQQ] + ^NDX
      US.EQ = [VTI] + VTSMX + VFINX
      US.MID = [VO] + VIMSX
      EAFE = [EFA] + VDVIX + VGTSX
      MID.TR = [IEF] + VFITX
      CORP.FI = [LQD] + VWESX
      TIPS = [TIP] + VIPSX + LSGSX
      LONG.TR = [TLT] + VUSTX
      '
      # EAFE = [EFA] + VDMIX + VGTSX
      
      data.proxy = env()
      getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data.proxy, raw.data = raw.data, auto.assign = T)
      data.proxy.raw = raw.data
      save(data.proxy.raw, file='data/data.proxy.raw.Rdata',compress='gzip')
      save(data.proxy, file='data/data.proxy.Rdata',compress='gzip')
    }
