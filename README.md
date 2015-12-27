# Public Health and Economic Consequences of Severe Weather Events, 1950-2011

## SYNOPSIS

---

## ASSUMPTIONS

---

## RESULTS

---

## DATA PROCESSING

Libraries required include `tm`, `stringdist`, `stringr`, `ggplot2`, `gridExtra`.
Local system must also have a pdf processor installed.  I used `xpdf` on mac.

```r
library(tm)         # PDF processing
```

```
## Loading required package: NLP
```

```r
library(stringdist) # Approximate string matching
library(stringr)    # Consistent regex
library(ggplot2)    # Plots
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```
## 
## Attaching package: 'ggplot2'
## 
## The following object is masked from 'package:NLP':
## 
##     annotate
```

```r
library(gridExtra)  # Layout of plots
```

### SOURCING DATA

First we download and extract the NOAA storm data.

```r
# Check for data dir
if (!file.exists("data")) {
  dir.create("data")
}

# Check for data source file, download if needed.
if (!file.exists("data/noaa_storm_data.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="data/noaa_storm_data.bz2", method="curl")
}

noaa <- read.csv('data/noaa_storm_data.bz2',
                 header = TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
dim(noaa)
```

```
## [1] 902297     37
```

Also let's grab the documentation from http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf.
Linux/Mac package 'xpdf' is required to extract the 48 categories. (credit to 
https://class.coursera.org/repdata-035/forum/thread?thread_id=35 for the idea).

```r
if (!file.exists("data/pd01016005curr.pdf")) {
  download.file("http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf", destfile="data/pd01016005curr.pdf", method="curl")
}

# Read the PDF using `tm`'s `readPDF`.  This requires a pdf engine, I used
# xpdf on a mac.
pdf <- readPDF(control = list(c(text = "-layout")))
pdf <- pdf(elem=list(uri="data/pd01016005curr.pdf"),language="en")
```

### PROCESSING CATEGORY DATA

We extract the 48 'official' categories from the table of contents in the source document.
These are matched by their format: 7.(1 or two digit number), space, text, space, event designator in parens.  Then we strip the leading/trailing metainformation.

```r
# Extract the 'Event Types' from the table of contents
toc <- paste(pdf$content[78:91], collapse = ' ')

# Then extract the 48 'official' category names.
# We match them based on their format: 7.(1 or two digit number), 
# space, text, space, event designator in parens.
categories <- str_extract_all(toc, "7\\.[:digit:]+\\.? ([^\\)]*) \\([CMZ]\\)")[[1]]

# Then we strip the leading/trailing metainformation
categories <- str_replace(categories, "^7\\.[:digit:]+\\.? ", "")
categories <- str_replace(categories, "[:space:]+\\([CMZ]\\)$", "")

print(categories)
```

```
##  [1] "Astronomical Low Tide"    "Avalanche"               
##  [3] "Blizzard"                 "Coastal Flood"           
##  [5] "Cold/Wind Chill"          "Debris Flow"             
##  [7] "Dense Fog"                "Dense Smoke"             
##  [9] "Drought"                  "Dust Devil"              
## [11] "Dust Storm"               "Excessive Heat"          
## [13] "Extreme Cold/Wind Chill"  "Flash Flood"             
## [15] "Flood"                    "Freezing Fog"            
## [17] "Frost/Freeze"             "Funnel Cloud"            
## [19] "Hail"                     "Heat"                    
## [21] "Heavy Rain"               "Heavy Snow"              
## [23] "High Surf"                "High Wind"               
## [25] "Hurricane/Typhoon"        "Ice Storm"               
## [27] "Lakeshore Flood"          "Lake-Effect Snow"        
## [29] "Lightning"                "Marine Hail"             
## [31] "Marine High Wind"         "Marine Strong Wind"      
## [33] "Marine Thunderstorm Wind" "Rip Current"             
## [35] "Seiche"                   "Sleet"                   
## [37] "Storm Tide"               "Strong Wind"             
## [39] "Thunderstorm Wind"        "Tornado"                 
## [41] "Tropical Depression"      "Tropical Storm"          
## [43] "Tsunami"                  "Volcanic Ash"            
## [45] "Waterspout"               "Wildfire"                
## [47] "Winter Storm"             "Winter Weather"
```

### PROCESSING STORM EVENT DATA
We'll first separate the data into groups by the collection year.
We need to coerce a date.  For our purposes, we'll determine the event's year
by the beginning date of the event.

Then we'll assign a 'collection category' corresponding to the information from here:
http://www.ncdc.noaa.gov/stormevents/details.jsp.  The categories are

- `tornado`: Tornado only
- `ttws1`: Tornado, Thunderstorm Wind and Hail 1 (manually keyed)
- `ttws2`: Tornado, Thunderstorm Wind and Hail 2 (text files)
- `all`: All event types


```r
noaa$event_date <- as.Date(strptime(noaa$BGN_DATE, "%m/%d/%Y %H:%M:%S"))
noaa$event_year <- strftime(noaa$event_date, "%Y")
noaa$collection_category <- apply(noaa, 1, function(row){
  y <- as.numeric(row['event_year'])
  if (y < 1955) { 'tornado_only' }
  else if (y >= 1955 & y < 1993) { 'ttws1' }
  else if (y >= 1992 & y < 1996) { 'ttws2' }
  else if (y >= 1996) { 'all' }
})

table(noaa$collection_category)
```

```
## 
##          all tornado_only        ttws1        ttws2 
##       653530         1865       185694        61208
```

Now let's work on shrinking our working dataset a little bit.

We start by removing the columns we don't care about from the dataset.

```r
dim(noaa)
```

```
## [1] 902297     40
```

```r
object.size(noaa)
```

```
## 511052328 bytes
```

```r
noaa <- subset(noaa, select = c('event_year', 
                                'collection_category', 
                                'PROPDMG', 'PROPDMGEXP',
                                'CROPDMG', 'CROPDMGEXP',
                                'FATALITIES', 'INJURIES',
                                'EVTYPE'))
dim(noaa)
```

```
## [1] 902297      9
```

```r
object.size(noaa)
```

```
## 65033992 bytes
```

We're going to disregard the tornado-only data, since it's not likely to be very
useful in a comparison (there being nothing in those years' data to compare to).
We'll retain the post-1955 data.

```r
noaa <- noaa[noaa$collection_category != 'tornado_only', ]
dim(noaa)
```

```
## [1] 900432      9
```

```r
object.size(noaa)
```

```
## 68501136 bytes
```

We also don't care about entries with no 'population' or 'economic' cost,
so in this case, if the record doesn't have values for any of 'PROPDMG',
'CROPDMG', 'FATALITIES', 'INJURIES', we'll let it go.

We'll store this one off to a separate variable, because relative frequency
of impactful events may be interesting (even if not all of those events cause
damage).  We don't need or want to normalize the categories for occurrences that
are ONLY for nonzero economic/personal cost observations.

```r
noaa_lim <- noaa[rowSums(noaa[ , c('PROPDMG', 'CROPDMG', 'FATALITIES', 'INJURIES')]) > 0, ]
dim(noaa_lim)
```

```
## [1] 253046      9
```

```r
object.size(noaa_lim)
```

```
## 19267496 bytes
```

There are WAY TOO MANY event types listed for the remaining data.

```r
length(unique(noaa_lim$EVTYPE))
```

```
## [1] 488
```

We try to match some of them up to the 48 categories.

```r
# exact match, not very good.
table(noaa_lim$EVTYPE %in% categories)
```

```
## 
##  FALSE   TRUE 
## 253027     19
```

```r
# case normalized string match.  better, but still not very good.
table(tolower(noaa_lim$EVTYPE) %in% tolower(categories))
```

```
## 
##  FALSE   TRUE 
##  81736 171310
```

H/T https://class.coursera.org/repdata-035/forum/thread?thread_id=8#post-284 for the idea
to use amatch to correct for typographical errors.  Assuming a max typo/whitespace difference of 3 characters, we see the following

```r
table(amatch(tolower(noaa_lim$EVTYPE), tolower(categories), maxDist = 3, nomatch = -1))
```

```
## 
##    -1     1     2     3     4     5     7     8     9    10    11    12 
## 67898     2   269   253   229    90    74     1   266    95   103   698 
##    13    14    15    16    17    18    19    20    21    22    23    24 
##   111 21285 10388     7   118    13 26138   215  1134  1377   146  6182 
##    25    26    27    28    29    30    31    32    33    34    35    36 
##    72   709     5   198 13296     2    19    46    33   641    33     1 
##    38    39    40    41    42    43    44    45    46    47    48 
##  3424 55801 38373    35   416    14     2    54   864  1509   407
```

We partially populate the table, so we're working with fewer records going forward.

```r
noaa_lim$category <- categories[amatch(tolower(noaa_lim$EVTYPE), tolower(categories), maxDist = 3)]
```

There are still a bunch of unmatched entries


```r
head(sort(table(noaa_lim[is.na(noaa_lim$category), ]$EVTYPE), decreasing=TRUE), 20)
```

```
## 
##            TSTM WIND URBAN/SML STREAM FLD       TSTM WIND/HAIL 
##                63234                  702                  441 
##     WILD/FOREST FIRE    FLOOD/FLASH FLOOD         EXTREME COLD 
##                  388                  279                  197 
##            LANDSLIDE          STORM SURGE          URBAN FLOOD 
##                  193                  177                  139 
##   WINTER WEATHER/MIX            HURRICANE           LIGHT SNOW 
##                  139                  129                  119 
##     MARINE TSTM WIND          RIVER FLOOD                 WIND 
##                  109                  107                   83 
##       URBAN FLOODING       DRY MICROBURST                 SNOW 
##                   80                   78                   52 
## HEAVY SURF/HIGH SURF     STORM SURGE/TIDE 
##                   50                   47
```

but we can clean some/most of these up.
thunderstorm/tstm wind is the VAST majority of these, and some of the others
are relatively easy to match.  We'll match all the ones that occur with some
frequency, and some of the ones that are easy to determine.  We avoid matching things
that combine categories, or are nondeterminant (like SNOW, it could possibly fit in several 
of the specified categories)

The process used to match these is as follows:

-  Any `EVTYPE` with 'thunderstorm' or 'tstm', and without 'marine' = `Thunderstorm Wind`
-  Any `EVTYPE` with both 'flash' and 'flood' = `Flash Flood`
-  Any remaining `EVTYPE`s with 'flood' = `Flood`
-  Any `EVTYPE` with both 'wild' and 'fire' = `Wildfire`
-  Any `EVTYPE` with both 'ext' and 'cold' = `Extreme Cold/Wind Chill`
-  Any `EVTYPE` with 'landslide' = `Debris Flow`
-  Any `EVTYPE` with 'storm surge' = `Storm Tide`
-  Any `EVTYPE` with either 'hurricane' or 'typhoon' = `Hurricane/Typhoon`
-  Any `EVTYPE` with 'hail' = `Hail`
-  Any `EVTYPE` with either 'winter' or 'light snow' = `Winter Weather`


```r
# update thunderstorm wind to include anything non-marine with thunderstorm/tstm
# (other tstorm related issues would be coded as lightning or tornado damage)
noaa_lim$category <- ifelse( is.na(noaa_lim$category) & 
                         grepl("(tstm|thunderstorm)", tolower(noaa_lim$EVTYPE)) & 
                         !grepl("marine", tolower(noaa_lim$EVTYPE)), 
                         "Thunderstorm Wind", noaa_lim$category)

# Manually take on marine t-storm wind.
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("marine tstm wind", tolower(noaa_lim$EVTYPE)), "Marine Thunderstorm Wind", noaa_lim$category)

# if it matches both 'flash' and 'flood', it's a flash flood
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("flash", tolower(noaa_lim$EVTYPE)) & grepl("flood", tolower(noaa_lim$EVTYPE)), "Flash Flood", noaa_lim$category)

# if it matches 'flood' (or stream fld), it's a 'regular' flood
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("(flood|stream fld)", tolower(noaa_lim$EVTYPE)), "Flood", noaa_lim$category)

# wildfire
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("wild", tolower(noaa_lim$EVTYPE)) & grepl("fire", tolower(noaa_lim$EVTYPE)), "Wildfire", noaa_lim$category)

# extreme cold
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("ext", tolower(noaa_lim$EVTYPE)) & grepl("cold", tolower(noaa_lim$EVTYPE)), "Extreme Cold/Wind Chill", noaa_lim$category)

# Hurricanes / Tsunamis
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("(hurricane|typhoon)", tolower(noaa_lim$EVTYPE)), "Hurricane/Typhoon", noaa_lim$category)

# landslides = debris flow
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("landslide", tolower(noaa_lim$EVTYPE)), "Debris Flow", noaa_lim$category)

# storm surge = storm tide
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("storm surge", tolower(noaa_lim$EVTYPE)), "Storm Tide", noaa_lim$category)

# hail
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("hail", tolower(noaa_lim$EVTYPE)), "Hail", noaa_lim$category)

# probably safe to say 'winter' is winter weather.  We'll allow 'light snow' here too.
noaa_lim$category <- ifelse(is.na(noaa_lim$category) & grepl("(winter|light snow)", tolower(noaa_lim$EVTYPE)), "Winter Weather", noaa_lim$category)
```

This takes care of all the uncategorized event types with more than
100 observations (and nonzero economic/population cost)

```r
head(sort(table(noaa_lim[is.na(noaa_lim$category), ]$EVTYPE), decreasing=TRUE), 20)
```

```
## 
##                 WIND       DRY MICROBURST                 SNOW 
##                   83                   78                   52 
## HEAVY SURF/HIGH SURF        FREEZING RAIN            HEAT WAVE 
##                   50                   35                   34 
##                OTHER       EXCESSIVE SNOW            ICY ROADS 
##                   33                   25                   22 
##  LIGHT FREEZING RAIN          GUSTY WINDS   HEAVY SNOW SQUALLS 
##                   22                   21                   21 
##    EXTREME WINDCHILL                GLAZE                WINDS 
##                   19                   19                   18 
##         EXTREME HEAT               FREEZE          SNOW SQUALL 
##                   17                   16                   16 
##   HEAVY SNOW-SQUALLS  MIXED PRECIPITATION 
##                   15                   15
```

And leaves less than 0.4% of the observations we're interested in outside
of one of the 48 major categories.

```r
table(is.na(noaa_lim$category))['TRUE'] / nrow(noaa_lim) * 100
```

```
##      TRUE 
## 0.3924188
```

Let's apply the process to the non-limited dataset, so we'll have accurate totals for
occurrences of each of the types when we look at freqeuencies.


---

Since we're looking to determine the economic and human impacts of storm events, we should begin by determining how we'll measure each of these and preparing those data.

### Human Impact

For `most harmful with respect to population health`, we'll take a look at both fatalities and injuries.


```r
aggregate_fatalities <- aggregate(noaa$FATALITIES, by = list('event_type' = noaa$EVTYPE), sum)
aggregate_fatalities <- aggregate_fatalities[order(aggregate_fatalities$x, decreasing = TRUE), ]
aggregate_fatalities$event_type <- factor(aggregate_fatalities$event_type, levels = aggregate_fatalities[order(aggregate_fatalities$x, decreasing = TRUE), ]$event_type)

head(aggregate_fatalities, 100)
```

```
##                     event_type    x
## 834                    TORNADO 4744
## 130             EXCESSIVE HEAT 1903
## 153                FLASH FLOOD  978
## 275                       HEAT  937
## 464                  LIGHTNING  816
## 856                  TSTM WIND  504
## 170                      FLOOD  470
## 585                RIP CURRENT  368
## 359                  HIGH WIND  248
## 19                   AVALANCHE  224
## 972               WINTER STORM  206
## 586               RIP CURRENTS  204
## 278                  HEAT WAVE  172
## 140               EXTREME COLD  160
## 760          THUNDERSTORM WIND  133
## 310                 HEAVY SNOW  127
## 141    EXTREME COLD/WIND CHILL  125
## 676                STRONG WIND  103
## 30                    BLIZZARD  101
## 350                  HIGH SURF  101
## 290                 HEAVY RAIN   98
## 142               EXTREME HEAT   96
## 79             COLD/WIND CHILL   95
## 427                  ICE STORM   89
## 957                   WILDFIRE   75
## 411          HURRICANE/TYPHOON   64
## 786         THUNDERSTORM WINDS   64
## 188                        FOG   62
## 402                  HURRICANE   61
## 848             TROPICAL STORM   58
## 342       HEAVY SURF/HIGH SURF   42
## 442                  LANDSLIDE   38
## 66                        COLD   35
## 376                 HIGH WINDS   35
## 877                    TSUNAMI   33
## 978             WINTER WEATHER   33
## 888  UNSEASONABLY WARM AND DRY   29
## 919       URBAN/SML STREAM FLD   28
## 980         WINTER WEATHER/MIX   28
## 842 TORNADOES, TSTM WIND, HAIL   25
## 960                       WIND   23
## 117                 DUST STORM   22
## 164             FLASH FLOODING   19
## 89                   DENSE FOG   18
## 146          EXTREME WINDCHILL   17
## 177          FLOOD/FLASH FLOOD   17
## 580      RECORD/EXCESSIVE HEAT   17
## 244                       HAIL   15
## 72               COLD AND SNOW   14
## 161          FLASH FLOOD/FLOOD   14
## 488         MARINE STRONG WIND   14
## 670                STORM SURGE   13
## 955           WILD/FOREST FIRE   12
## 671           STORM SURGE/TIDE   11
## 886          UNSEASONABLY WARM   11
## 489   MARINE THUNDERSTORM WIND   10
## 976              WINTER STORMS   10
## 490           MARINE TSTM WIND    9
## 596                 ROUGH SEAS    8
## 851      TROPICAL STORM GORDON    8
## 201              FREEZING RAIN    7
## 222                      GLAZE    7
## 339                 HEAVY SURF    7
## 480            LOW TEMPERATURE    7
## 487              MARINE MISHAP    7
## 680               STRONG WINDS    7
## 185                   FLOODING    6
## 405             HURRICANE ERIN    6
## 417                        ICE    6
## 77                COLD WEATHER    5
## 165       FLASH FLOODING/FLOOD    5
## 280                 HEAT WAVES    5
## 348                  HIGH SEAS    5
## 435                  ICY ROADS    5
## 588    RIP CURRENTS/HEAVY SURF    5
## 629                       SNOW    5
## 873             TSTM WIND/HAIL    5
## 243                GUSTY WINDS    4
## 279          HEAT WAVE DROUGHT    4
## 373             HIGH WIND/SEAS    4
## 415       Hypothermia/Exposure    4
## 517                   Mudslide    4
## 544                  RAIN/SNOW    4
## 597                 ROUGH SURF    4
## 636               SNOW AND ICE    4
## 54               COASTAL FLOOD    3
## 60               COASTAL STORM    3
## 65                        Cold    3
## 76                   COLD WAVE    3
## 101             DRY MICROBURST    3
## 307                 HEAVY SEAS    3
## 340        Heavy surf and wind    3
## 349                  High Surf    3
## 356                 HIGH WATER    3
## 366         HIGH WIND AND SEAS    3
## 395            HIGH WINDS/SNOW    3
## 416       HYPOTHERMIA/EXPOSURE    3
## 936                 WATERSPOUT    3
## 943         WATERSPOUT/TORNADO    3
## 954                 WILD FIRES    3
```

```r
aggregate_injuries   <- aggregate(noaa$INJURIES,   by = list('event_type' = noaa$EVTYPE), sum)
aggregate_injuries <- aggregate_injuries[order(aggregate_injuries$x, decreasing = TRUE), ]
aggregate_injuries$event_type <- factor(aggregate_injuries$event_type, levels = aggregate_injuries[order(aggregate_injuries$x, decreasing = TRUE), ]$event_type)

head(aggregate_injuries, 100)
```

```
##                   event_type     x
## 834                  TORNADO 82402
## 856                TSTM WIND  6957
## 170                    FLOOD  6789
## 130           EXCESSIVE HEAT  6525
## 464                LIGHTNING  5230
## 275                     HEAT  2100
## 427                ICE STORM  1975
## 153              FLASH FLOOD  1777
## 760        THUNDERSTORM WIND  1488
## 244                     HAIL  1361
## 972             WINTER STORM  1321
## 411        HURRICANE/TYPHOON  1275
## 359                HIGH WIND  1137
## 310               HEAVY SNOW  1021
## 957                 WILDFIRE   911
## 786       THUNDERSTORM WINDS   908
## 30                  BLIZZARD   805
## 188                      FOG   734
## 955         WILD/FOREST FIRE   545
## 117               DUST STORM   440
## 978           WINTER WEATHER   398
## 89                 DENSE FOG   342
## 848           TROPICAL STORM   340
## 278                HEAT WAVE   309
## 376               HIGH WINDS   302
## 586             RIP CURRENTS   297
## 676              STRONG WIND   280
## 290               HEAVY RAIN   251
## 585              RIP CURRENT   232
## 140             EXTREME COLD   231
## 222                    GLAZE   216
## 19                 AVALANCHE   170
## 142             EXTREME HEAT   155
## 350                HIGH SURF   152
## 954               WILD FIRES   150
## 417                      ICE   137
## 877                  TSUNAMI   129
## 873           TSTM WIND/HAIL    95
## 960                     WIND    86
## 919     URBAN/SML STREAM FLD    79
## 984               WINTRY MIX    77
## 980       WINTER WEATHER/MIX    72
## 277                Heat Wave    70
## 979       WINTER WEATHER MIX    68
## 442                LANDSLIDE    52
## 557              RECORD HEAT    50
## 66                      COLD    48
## 342     HEAVY SURF/HIGH SURF    48
## 402                HURRICANE    46
## 851    TROPICAL STORM GORDON    43
## 115               DUST DEVIL    42
## 943       WATERSPOUT/TORNADO    42
## 339               HEAVY SURF    40
## 670              STORM SURGE    38
## 656          SNOW/HIGH WINDS    36
## 645              SNOW SQUALL    35
## 435                ICY ROADS    31
## 629                     SNOW    29
## 936               WATERSPOUT    29
## 101           DRY MICROBURST    28
## 821            THUNDERSTORMW    27
## 489 MARINE THUNDERSTORM WIND    26
## 501             MIXED PRECIP    26
## 29                 BLACK ICE    24
## 141  EXTREME COLD/WIND CHILL    24
## 201            FREEZING RAIN    23
## 488       MARINE STRONG WIND    22
## 134       EXCESSIVE RAINFALL    21
## 680             STRONG WINDS    21
## 366       HIGH WIND AND SEAS    20
## 886        UNSEASONABLY WARM    17
## 976            WINTER STORMS    17
## 838               TORNADO F2    16
## 177        FLOOD/FLASH FLOOD    15
## 195         FREEZING DRIZZLE    15
## 224          GLAZE/ICE STORM    15
## 279        HEAT WAVE DROUGHT    15
## 973  WINTER STORM HIGH WINDS    15
## 44              BLOWING SNOW    13
## 79           COLD/WIND CHILL    12
## 753             THUNDERSTORM    12
## 331           HEAVY SNOW/ICE    10
## 617               SMALL HAIL    10
## 754      THUNDERSTORM  WINDS    10
## 164           FLASH FLOODING     8
## 243              GUSTY WINDS     8
## 348                HIGH SEAS     8
## 490         MARINE TSTM WIND     8
## 526   NON-SEVERE WIND DAMAGE     7
## 395          HIGH WINDS/SNOW     6
## 58  COASTAL FLOODING/EROSION     5
## 146        EXTREME WINDCHILL     5
## 487            MARINE MISHAP     5
## 596               ROUGH SEAS     5
## 671         STORM SURGE/TIDE     5
## 879                  TYPHOON     5
## 95                   DROUGHT     4
## 305              HEAVY RAINS     4
## 349                High Surf     4
## 392          HIGH WINDS/COLD     4
```

```r
top100_fatalities <- ggplot(head(aggregate_fatalities, 20), aes(x = event_type, y = x, fill = x)) + geom_bar(stat = 'identity') + scale_fill_gradient2(mid='lightgrey', high='darkred')
top100_injuries <- ggplot(head(aggregate_injuries, 20), aes(x = event_type, y = x, fill = x)) + geom_bar(stat = 'identity') + scale_fill_gradient2(mid='lightgrey', high='darkred')

grid.arrange(top100_fatalities, top100_injuries, ncol=1)
```

![](README_files/figure-html/unnamed-chunk-17-1.png) 


### Economic Impact

For economic impact, we'll combine property (`PROPDMG`) and crop (`CROPDMG`) damage into a total dollar value per incident.

In the specification document (https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf, section 2.7) it is indicated that the numeric values of damage estimates are rounded to 3 significant digits, then abbreviated by a magnitude indicator

> Alphabetical characters used to signify magnitude include “K” for thousands, “M” for millions, and “B” for billions.

This magnitude indicator is in the `PROPDMGEXP` and `CROPDMGEXP` variables.  Some of the magnitude indicators don't follow the alpha guidelines, and so we don't know how to normalize them:


```r
# Total nonzero property damage event counts
nonzero_prop_counts <- table(noaa$PROPDMG > 0)
print(nonzero_prop_counts)
```

```
## 
##  FALSE   TRUE 
## 662794 237638
```

```r
# Table of event counts with nonzero property damage and invalid magnitude indicator
invalid_prop_magnitude_counts <- addmargins(table(noaa[noaa$PROPDMG > 0 & !grepl('[kKmMbB ]', noaa$PROPDMGEXP), ]$PROPDMGEXP))
print(invalid_prop_magnitude_counts)
```

```
## 
##       -   +   0   2   3   4   5   6   7   h   H Sum 
##  76   1   5 209   1   1   4  18   3   2   1   6 327
```

```r
# Percent of events with nonzero property damage that have an invalid magnitude indicator
(invalid_prop_magnitude_counts['Sum'] / nonzero_prop_counts['TRUE']) * 100
```

```
##       Sum 
## 0.1376043
```

```r
# Total nonzero crop damage event counts
nonzero_crop_counts <- table(noaa$CROPDMG > 0)
print(nonzero_crop_counts)
```

```
## 
##  FALSE   TRUE 
## 878333  22099
```

```r
# Table of event counts with nonzero crop damage and invalid magnitude indicator
invalid_crop_magnitude_counts <- addmargins(table(noaa[noaa$CROPDMG > 0 & !grepl('[kKmMbB ]', noaa$CROPDMGEXP), ]$CROPDMGEXP))
print(invalid_crop_magnitude_counts)
```

```
## 
##       0 Sum 
##   3  12  15
```

```r
# Percent of events with nonzero crop damage that have an invalid magnitude indicator
(invalid_crop_magnitude_counts['Sum'] / nonzero_crop_counts['TRUE']) * 100
```

```
##        Sum 
## 0.06787637
```

Since for both variables these comprise a proportionally very small number of incidents (both less than 0.15 percent), we'll simply exclude these incidents from our value calculations.

To normalize the values, we'll decompress and combine the values for property and crop damage, and combine them into a single total dollar value for each incident.


```r
# Populate a column with total economic cost per incident
noaa$economic_cost <- apply(noaa, 1, function(row){
  pval <- as.numeric(row['PROPDMG'])
  if (pval == 0 | !grepl('[kKmMbB ]', row['PROPDMGEXP'])){
    pval <- 0
  } else if (grepl('^[kK]$', row['PROPDMGEXP'])) {
    pval <- pval * 1000
  } else if (grepl('^[mM]$', row['PROPDMGEXP'])) {
    pval <- pval * 1000000
  } else if (grepl('^[bB]$', row['PROPDMGEXP'])) {
    pval <- pval * 1000000000
  }

  cval <- as.numeric(row['CROPDMG'])
  if (cval == 0 | !grepl('[kKmMbB ]', row['CROPDMGEXP'])){
    cval <- 0
  } else if (grepl('^[kK]$', row['CROPDMGEXP'])) {
    cval <- cval * 1000
  } else if (grepl('^[mM]$', row['CROPDMGEXP'])) {
    cval <- cval * 1000000
  } else if (grepl('^[bB]$', row['CROPDMGEXP'])) {
    cval <- cval * 1000000000
  }

  pval + cval
})
```

Now we can aggregate economic cost by event type and compare.
After aggregating, we order the output to be highest aggregate cost to lowest.
We also convert the event type to a factor and force-reorder the levels in the
same arrangement (for plotting purposes).


```r
aggregate_cost <- aggregate(noaa$economic_cost, by = list('event_type' = noaa$EVTYPE), sum)
aggregate_cost <- aggregate_cost[order(aggregate_cost$x, decreasing = TRUE), ]
aggregate_cost$event_type <- factor(aggregate_cost$event_type, levels = aggregate_cost[order(aggregate_cost$x, decreasing = TRUE), ]$event_type)
head(aggregate_cost, 25)
```

```
##                     event_type            x
## 170                      FLOOD 150319678250
## 411          HURRICANE/TYPHOON  71913712800
## 834                    TORNADO  56476113690
## 670                STORM SURGE  43323541000
## 244                       HAIL  18758221170
## 153                FLASH FLOOD  17562128610
## 95                     DROUGHT  15018672000
## 402                  HURRICANE  14610229010
## 590                RIVER FLOOD  10148404500
## 427                  ICE STORM   8967041310
## 848             TROPICAL STORM   8382236550
## 972               WINTER STORM   6715441250
## 359                  HIGH WIND   5908617560
## 957                   WILDFIRE   5060586800
## 856                  TSTM WIND   5038935790
## 671           STORM SURGE/TIDE   4642038000
## 760          THUNDERSTORM WIND   3897964190
## 408             HURRICANE OPAL   3191846000
## 955           WILD/FOREST FIRE   3108626330
## 299  HEAVY RAIN/SEVERE WEATHER   2500000000
## 786         THUNDERSTORM WINDS   1926607550
## 842 TORNADOES, TSTM WIND, HAIL   1602500000
## 290                 HEAVY RAIN   1427647890
## 140               EXTREME COLD   1360710400
## 604        SEVERE THUNDERSTORM   1205560000
```

Let's take a look at the event types and see if there's a clear cutoff

```r
top_25 <- ggplot(head(aggregate_cost, 25), aes(x = event_type, y = x, fill = x)) + geom_bar(stat = 'identity') + scale_fill_gradient2(mid='lightgrey', high='darkred')

print(top_25)
```

![](README_files/figure-html/unnamed-chunk-21-1.png) 
