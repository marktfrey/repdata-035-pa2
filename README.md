# Public Health and Economic Consequences of Severe Weather Events, 1950-2011

## SYNOPSIS

---


## DATA PROCESSING

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

noaa <- read.csv('data/noaa_storm_data.bz2', header = TRUE, stringsAsFactors = FALSE)
dim(noaa)
```

```
## [1] 902297     37
```

Since we're looking to determine the economic and human impacts of storm events, we should begin by determining how we'll measure each of these and preparing those data.

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
## 663123 239174
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
## 0.1367205
```

```r
# Total nonzero crop damage event counts
nonzero_crop_counts <- table(noaa$CROPDMG > 0)
print(nonzero_crop_counts)
```

```
## 
##  FALSE   TRUE 
## 880198  22099
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

Since for both variables these comprise a relatively small number of incidents, we'll simply exclude these incidents from our value calculations.

To normalize the values, we'll decompress and combine the values for property and crop damage, and combine them into a single total dollar value for each incident.

---


## RESULTS
