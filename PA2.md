# Weather events: effects on population health and economic consequences 

Author: jfs<br />
Date: 09 Jan 2016

## Synopsis
Weather can cause many issues. The most relevant ones are the effects on the
population that suffers the wrath of the elements. People can drown as a
consequence of a flood, or can be hit to death by some object flying as a
consequence of strong winds. Besides, the economic damage can also be high.
Roads and towns destroyed by floods, houses destroyed by tornadoes, etc.

## Data Processing

### Downloading the data
The raw data are provided by the instructors of the *Reproducible Research*
course on Coursera, and can be downloaded from the course web site:

- [Storm Data](https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2).

Some documentation is also available:

- National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata/peer2_doc/pd01016005curr.pdf)
- National Climatic Data center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata/peer2_doc/NCDC%20Storm%20Events-FAQ%20Page.pdf)

### Loading the data
Note: I assume that the file containing the raw data is in the working directory.

We can read the data directly from the bz2 file. But the data file is rather
large (47MB compressed, 535MB uncompressed!!!), so it may take a while to
load...

A purely technical note here: sometimes while loading the data, my machine gets
stuck, so first I decompress the file, and then I load the data. Anyway, the
loading time is rather long. To avoid decompressing and/or loading the data each
time I run knitr to create the HTML document, I check whether they already exist
on disk and/or in memory.

In order to have only the data needed for the analysis, I only read the columns
I need. Namely: `BGN_DATE`, `END_DATE`, `EVTYPE`, `FATALITIES`, `INJURIES`,
`PROPDMG`, `PROPDMGEXP`, `CROPDMG`, and `CROPDMGEXP`.

```r
DATAFILE <- "./StormData.csv"
COMPDATAFILE <- paste0("./StormData.csv", ".bz2")

## I actually downloaded the file by hand anyway
if (!file.exists(COMPDATAFILE)) {
  fileurl <- "http://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
  download.file(url = fileurl, destfile = COMPDATAFILE)
}

## Names of the columns we want
wanted <- c("BGN_DATE", "END_DATE", "EVTYPE", "FATALITIES", "INJURIES",
            "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
## Names of all the columns existing in the data set
header <- names(read.csv(COMPDATAFILE, nrows = 1))
## Wanted columns marked as "character", unwanted columns marked as NULL
columns <- ifelse(header %in% wanted, "character", "NULL")
## We know some of these columns are numeric, and we want them as numbers
asnum <- match(c("FATALITIES", "INJURIES", "PROPDMG", "CROPDMG"), header)
columns[asnum] <- "numeric"

POWERFULMACHINE = TRUE  ## TRUE: reads in one step, FALSE: reads in two steps
if (POWERFULMACHINE) {
  stormdata <- read.csv(COMPDATAFILE, colClasses = columns)
} else {
  library(R.utils)  ## for bunzip2()
  # I don't want to decompress the file each time I run knitr
  if (!file.exists(DATAFILE)) {
    bunzip2(COMPDATAFILE, overwrite = FALSE, remove = FALSE)
  }
  # I don't want to load the data each time I run knitr
  if (!exists("stormdata")) {
    stormdata <- read.csv(DATAFILE, colClasses = columns)
  }
}
```

### Cleaning the data
A bit of cleaning in the `EVTYPE` variable is necessary, for there are kind of
duplicated names like **Thunderstorm wind** and **Thunderstorm winds** and also
**tstm wind**. The problem here is that this should be done almost by hand, by
looking at the `EVTYPE` and seeing which of them are the same.

In the *Storm Data Documentation* mentioned at the beginning, on page 6, we can
see a list of all the event names. For some reason, the data file contains names
that don't appear in the documentation. So we must guess/deduce which ones can
be *translated* into the *as-correct-as-possible* names.

```r
library(R.utils)  ## for trim() and capitalize()

## Work with all lowercase, for ease of handling
stormdata$EVTYPE <- tolower(trim(stormdata$EVTYPE))

## Normalize to one space between words 
stormdata$EVTYPE <- gsub("  +", " ", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("\\\\", "/", stormdata$EVTYPE)

## Ignore punctuation signs at end of text
stormdata$EVTYPE <- sub("[[:punct:]]+$", "", stormdata$EVTYPE)

## Try some kind of fix for misspellings and the like
stormdata$EVTYPE <- sub("avalance", "avalanche", stormdata$EVTYPE)

## Whatever 'starts with blizzard' is 'blizzard'
stormdata$EVTYPE <- sub("^blizzard.*", "blizzard", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^blowing snow.*", "blowing snow", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("clouds", "cloud", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("cstl", "coastal", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^coastal.*flood$", "coastal flood", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("^drought.*", "drought", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("erosin", "erosion", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("fires", "fire", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("flood(ing?|s)", "flood", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("fldg?", "flood", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("funnels", "funnel", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("hvy", "heavy", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("storms", "storm", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("tides", "tide", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("tornadoe?s", "tornado", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("torndao", "tornado", stormdata$EVTYPE)
## Whatever starts with 'tornado' is 'tornado'
stormdata$EVTYPE <- sub("^tornado.*", "tornado", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("temperatures", "temperature", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("tstmw", "thunderstorm wind", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("tstm", "thunderstorm", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("th?u\\w+m", "thunderstorm", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("thunderstorm(w(ind)?)?", "thunderstorm wind", stormdata$EVTYPE)
## Whatever starts with 'thunderstorm wind' is 'thunderstorm wind'
stormdata$EVTYPE <- sub("^thunderstorm wind.*", "thunderstorm wind", stormdata$EVTYPE)

## Whatever starts with 'tropical storm' is 'tropical storm'
stormdata$EVTYPE <- sub("^tropical storm.*", "tropical storm", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("wayter", "water", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("water ?spouts?", "waterspout", stormdata$EVTYPE)
## Whatever starts with 'waterspout' is 'waterspout'
stormdata$EVTYPE <- sub("^waterspout.*", "waterspout", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("wi?nd", "wind", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("wind?s", "wind", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("wind +wind", "wind", stormdata$EVTYPE)

stormdata$EVTYPE <- sub("wintry", "wintery", stormdata$EVTYPE)
## Many more should need to be modified...


## Try and normalize event names as those specified in the document
stormdata$EVTYPE <- sub("^winter weather.*", "winter weather", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^winter storm.*", "winter storm", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("wild|forest", "wildfire", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^volcanic ash.*", "volcanic ash", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^storm surge.*", "storm surge/tide", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^sleet.*", "sleet", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^rip current.*", "rip current", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("lake effect", "lake-effect", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^.*(hurricane[^-]|typhoon).*$", "hurricane (typhoon)", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^high wind.*", "high wind", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^high surf.*", "high surf", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^heavy snow.*", "heavy snow", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^heavy rain.*", "heavy rain", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^hail.*", "hail", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^funnel cloud.*", "funnel cloud", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^(frost|freeze)$", "frost/freeze", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^flood.*", "flood", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^flash flood.*", "flash flood", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^extreme cold.*", "extreme cold/wind chill", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^extreme wind chill.*", "extreme cold/wind chill", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("excessive heat", "excessive heat", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("dust ?storm", "dust storm", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("dust devil", "dust devil", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("^cold.*wind chill.*", "cold/wind chill", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("coastal flood", "coastal flood", stormdata$EVTYPE)

## Capitalization just for cosmetic purposes
stormdata$EVTYPE <- capitalize(stormdata$EVTYPE)
```

### Extracting the useful data
First of all, for the analysis we need, I just keep the rows where `INJURIES`
and `FATALITIES` are not zero on one hand. On the other hand, I keep the rows
where `PROPDMG` and `CROPDMG` are not zero.

The documentation only describes clearly three possible values for the variables
`PROPDMGEXP` and `CROPDMGEXP`: `K` (thousands of dollars), `M` (millions of
dollars), and `B` (billions of dollars). Actually, the data also contains other
values:

```r
unique(stormdata$PROPDMGEXP)
```

```
##  [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-"
## [18] "1" "8"
```

```r
unique(stormdata$CROPDMGEXP)
```

```
## [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
```

I assume that:

- Only values `K`, `M` and `B` are relevant.
- An empty string stands for dollars alone, i.e. 5 "" are $5.
- Lowercase letters mean the same as their uppercase counterpart.
- All other values are equivalent to the empty string.


```r
stormdata1 <- stormdata[stormdata$FATALITIES > 0 | stormdata$INJURIES > 0,
                        c("BGN_DATE", "END_DATE",
                          "EVTYPE", "FATALITIES", "INJURIES")]

stormdata2 <- stormdata[stormdata$PROPDMG > 0 | stormdata$CROPDMG > 0,
                        c("BGN_DATE", "END_DATE",
                          "EVTYPE", "PROPDMG", "PROPDMGEXP",
                                    "CROPDMG", "CROPDMGEXP")]

stormdata2$PROPDMGEXP <- toupper(stormdata2$PROPDMGEXP)
stormdata2$CROPDMGEXP <- toupper(stormdata2$CROPDMGEXP)


## Convert dates, used later for the main titles in the plots
convdate <- function(val) {
  newval <- sub(" 0:00:00", "", val)
  newval <- as.Date(newval, format = "%m/%d/%Y")
}

stormdata1$BGN_DATE <- convdate(stormdata1$BGN_DATE)
stormdata1$END_DATE <- convdate(stormdata1$END_DATE)

stormdata2$BGN_DATE <- convdate(stormdata2$BGN_DATE)
stormdata2$END_DATE <- convdate(stormdata2$END_DATE)
```

For `PROPDMG` and `CROPDMG`, their companion variables `PROPDMGEXP` and
`CROPDMGEXP` indicate whether the value is in dollars, thousands of dollars
(K), millions of dollars (M) or billions of dollars (B). We can convert and
replace the values to work only in dollars:


```r
convert <- function(val, units) {
  # Example: val=5, units="M" => 5 * 10^(2*3)
  mult <- c("K", "M", "B")
  val * 10^(match(units, mult, nomatch = 0) * 3)
}

stormdata2$PROPDMG <- convert(stormdata2$PROPDMG, stormdata2$PROPDMGEXP)
stormdata2$CROPDMG <- convert(stormdata2$CROPDMG, stormdata2$CROPDMGEXP)
```


## Results

### 1. Which type events are most harmful with respect to population health?
The only information available referring to human health are two variables named
`FATALITIES` and `INJURIES`.

What does *most harmful* mean? What criterion should be used to determine that
something is more harmful than something else? Numbers are cold, so in order to
make the plot somewhat readable, I ignore the cases where the number of
fatality/injury is less than the average number.


```r
library(ggplot2)
library(reshape2)  ## for melt()

## Prepare the main title for the plot
firstyear <- min(na.omit(stormdata1$BGN_DATE))
firstyear <- format(firstyear, "%Y")
lastyear <- max(na.omit(stormdata1$END_DATE))
lastyear <- format(lastyear, "%Y")
maintitle <- paste0("Total number of human casualties by type of weather event",
                    "\n",
                    "between ", firstyear, " and ", lastyear)

casualt <- aggregate(x = list(total = stormdata1$FATALITIES + stormdata1$INJURIES,
                              totfat = stormdata1$FATALITIES,
                              totinj = stormdata1$INJURIES),
                     by = list(event = stormdata1$EVTYPE),
                     FUN = sum)

## Ignore cases where total fatalities and injuries are less than the average
## number
avgcasualt <- mean(casualt$total)
casualt <- casualt[casualt$total >= avgcasualt, ]

## Prepare for use in facets
casualt <- melt(casualt, id = "event")

## Rename labels for facet_wrap()
labeller_human <- function(variable, value) {
  labnames <- c(totfat = "1. Total Fatalities",
                totinj = "2. Total Injuries",
                total = "3. Total Fatalities+Injuries")
  labnames[as.character(value)]
}

## Create label column for facet_wrap()
casualt$label <- factor(labeller_human('variable', casualt$variable))

ggplot(casualt, aes(x = reorder(event, -value), y = value)) +
       geom_bar(stat = "identity") +
       facet_wrap( ~ label, ncol = 3) +
       xlab("Weather event") + ylab("Fatalities and Injuries") +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
       ggtitle(maintitle)
```

<img src="figure/effectsonhealth-1.png" title="plot of chunk effectsonhealth" alt="plot of chunk effectsonhealth" style="display: block; margin: auto;" />

Looking at the plot, there is no question, the most harmful weather event for
the population by far is the tornado. Actually, this is the event that causes
the most fatalities and the most injuries:


```r
fatalities <- aggregate(FATALITIES ~ EVTYPE, data = stormdata1, FUN = sum)
injuries <- aggregate(INJURIES ~ EVTYPE, data = stormdata1, FUN = sum)

fatalities[which.max(fatalities$FATALITIES), ]
```

```
##      EVTYPE FATALITIES
## 119 Tornado       5658
```

```r
injuries[which.max(injuries$INJURIES), ]
```

```
##      EVTYPE INJURIES
## 119 Tornado    91364
```

Other highly dangerous events are thunderstorm wind, excessive heat, flood and
lightning.

### 2. Which types of events have the greatest economic consequences?
The only information available referring to economic damages are two variables
named `PROPDMG` and `CROPDMG`.


```r
library(ggplot2)
library(reshape2)  ## for melt()

## Prepare the main title for the plot
firstyear <- min(na.omit(stormdata1$BGN_DATE))
firstyear <- format(firstyear, "%Y")
lastyear <- max(na.omit(stormdata1$END_DATE))
lastyear <- format(lastyear, "%Y")
maintitle <- paste0("Total economic damage by type of weather event",
                    "\n",
                    "between ", firstyear, " and ", lastyear)

eco <- aggregate(x = list(total = stormdata2$PROPDMG + stormdata2$CROPDMG,
                          totprop = stormdata2$PROPDMG,
                          totcrop = stormdata2$CROPDMG),
                 by = list(event = stormdata2$EVTYPE),
                 FUN = sum)

## Ignore cases where total damage are less than the average number
avgeco <- mean(eco$total)
eco <- eco[eco$total >= avgeco, ]

## Prepare for use in facets
eco <- melt(eco, id = "event")

## Rename labels for facet_wrap()
labeller_eco <- function(variable, value) {
  labnames <- c(totprop = "1. Total Properties Damage",
                totcrop = "2. Total Crop damage",
                total = "3. Total Properties+Crop Damage")
  labnames[as.character(value)]
}

## Create label column for facet_wrap()
eco$label <- factor(labeller_eco('variable', eco$variable))

ggplot(eco, aes(x = reorder(event, -value), y = value / 1e9)) +
       geom_bar(stat = "identity") +
       facet_wrap( ~ label, ncol = 3) +
       xlab("Weather event") +
       ylab("Economic Damage (Billions of USD)") +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
       ggtitle(maintitle)
```

<img src="figure/effectsoneconomy-1.png" title="plot of chunk effectsoneconomy" alt="plot of chunk effectsoneconomy" style="display: block; margin: auto;" />

The greatest economic consequences are caused by floods, followed by
hurricanes, tornadoes and storm surges, especially when it comes to properties
damage. For crop damage, we can see that the main cause is drought.

