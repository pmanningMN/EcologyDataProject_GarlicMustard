GMTransectCompile
================
Paul Manning
12/25/2018

Note: X,Y, and START all must be on the same line of the imported csv file, and the previous (X,Y) coordinates' "END" must be one row above these 3

When importing the data to R, be sure that all the columns are set to integer or doubles, often Aphids would turn into character and mess everything up, so double check that

NEED TO IMPORT ZOO for indexing the coordinates

``` r
#uncomment if not in library

#install.packages('dplyr')
#library(rmarkdown)
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
GMdata <- read.csv(file="GMdata.csv", header=TRUE, sep=",")
save(GMdata, file="GMdata.RData")
load(file="GMdata.RData")

GMdataraw <- read.csv(file="GMdataraw.csv", header=TRUE, sep=",")
save(GMdataraw, file="GMdataraw.RData")
load(file="GMdataraw.RData")
```

The following sets a unique ID for each line of GMData so that the overlap function in the next chunk can be run. It appends these values to the GMdata dataframe.

``` r
uniqueraw=(1/GMdata$X)+(1/GMdata$Y)
uniquesimple=dense_rank(uniqueraw)
Newunique=na.locf(uniquesimple)
GMdata$UniqueID=Newunique
```

Now the overlap values can be calculated for each individual transect coordinate. These values are then added to the overlap list variable. These are done in order so the first list item of overlap will match the first list item for Unique.ID.

This function calculates overlap by checking for the following 9 conditions (for all non NA values):

1.  If any stem start is greater than ros start AND the stem end is less than the ros end THEN the overlap value to add to total overlap will be equal to (stem end - stem start)
2.  If any ros start is greater than stem start AND the ros end is less than the stem end THEN the overlap value to add to total overlap will be equal to (ros end - ros start)
3.  If any ros start is less than the stem start AND the stem start is less than the ros end AND the ros end is less than the stem end THEN the overlap value to add to total overlap will be equal to (ros end - stem start)
4.  If any stem start is less than the ros start AND the ros start is less than the stem end AND the stem end is less than the ros end THEN the overlap value to add to total overlap will be equal to (stem end - ros start)
5.  If the stem start is equal to the ros start AND the stem end is less than the ros end THEN the overlap value to add to total overlap will be equal to (stem end - stem start)
6.  If the ros start is equal to the stem start AND the ros end is less than the stem end THEN the overlap value to add to total overlap will be equal to (ros end - ros start)
7.  If the ros start is less than the stem start AND the ros end is equal to the stem end THEN the overlap value to add to total overlap will be equal to (ros end - stem start)
8.  If the stem start is less than the ros start AND the stem end is equal to the ros end THEN the overlap value to add to total overlap will be equal to (stem end - ros start)
9.  If the stem start is equal to the ros start AND the stem end is equal to the ros end THEN the overlap value to add to total overlap will be equal to (stem end - ros start)

It then loops these checks for each uniqueID value and returns a list of the overlap values.

``` r
overlap=rep(0,322)
ilist = c(1:322)

OverlapFun=function(GMdata){
  for (i in ilist) {
    yy.sub=GMdata

    UniqueID=yy.sub$UniqueID   #For each loop, these steps make a subset of data that is at the Unique.ID values.
    STEM_START=yy.sub$STEM_START
    STEM_END=yy.sub$STEM_END
    ROS_START=yy.sub$ROS_START
    ROS_END=yy.sub$ROS_END
    
    indices=which(UniqueID==i)   #This identifies the indicies or row numbers that we are subetting from GMdata. 
  
    for (j in indices[1]:indices[length(indices)]) {
        
        for (k in indices[1]:indices[length(indices)]) {
            
            if( is.na( (STEM_START[j]+ROS_START[k]) ) == FALSE ) overlap[i] = overlap[i]+
                    (STEM_START[j]>ROS_START[k])*(STEM_END[j]<ROS_END[k])*(STEM_END[j]-STEM_START[j])+
                    (ROS_START[k]>STEM_START[j])*(ROS_END[k]<STEM_END[j])*(ROS_END[k]-ROS_START[k])+
                    (ROS_START[k]<STEM_START[j])*(STEM_START[j]<ROS_END[k])*(ROS_END[k]<STEM_END[j])*(ROS_END[k]-STEM_START[j])+
                    (STEM_START[j]<ROS_START[k])*(ROS_START[k]<STEM_END[j])*(STEM_END[j]<ROS_END[k])*(STEM_END[j]-ROS_START[k])+
                    (STEM_START[j]==ROS_START[k])*(STEM_END[j]<ROS_END[k])*(STEM_END[j]-STEM_START[j])+
                    (ROS_START[k]==STEM_START[j])*(ROS_END[k]<STEM_END[j])*(ROS_END[k]-ROS_START[k])+
                    (ROS_START[k]<STEM_START[j])*(ROS_END[k]==STEM_END[j])*(ROS_END[k]-STEM_START[j])+
                    (STEM_START[j]<ROS_START[k])*(STEM_END[j]==ROS_END[k])*(STEM_END[j]-ROS_START[k])+
                    (STEM_START[j]==ROS_START[k])*(STEM_END[j]==ROS_END[k])*(STEM_END[j]-ROS_START[k])  }  
    }}
  i = i + 1
  return(overlap)
  }

overlap=OverlapFun(GMdata)   #Calls the overlap function
print(overlap)
```

    ##   [1] 0.57 0.01 0.00 0.06 0.04 0.00 0.17 0.00 0.00 0.00 0.00 0.00 0.10 0.04
    ##  [15] 0.00 0.15 0.76 0.00 1.18 0.00 0.00 0.55 0.38 0.00 0.12 0.00 0.00 0.00
    ##  [29] 0.78 0.80 0.00 0.24 0.85 0.00 0.00 0.02 0.00 0.00 0.00 0.00 0.00 0.02
    ##  [43] 0.08 0.96 1.36 0.00 0.49 0.00 0.67 0.00 0.28 0.00 0.21 0.00 0.00 0.00
    ##  [57] 0.52 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.94 0.00 0.37 0.03 0.24
    ##  [71] 0.00 0.04 0.21 0.00 0.00 0.00 0.25 0.00 0.00 0.00 0.00 0.06 0.00 0.10
    ##  [85] 0.00 0.46 0.06 0.06 0.10 0.00 0.05 0.04 0.02 0.00 0.00 0.02 0.00 0.00
    ##  [99] 0.00 0.00 0.18 0.00 1.46 0.56 0.00 0.00 0.00 0.00 0.03 0.00 0.14 0.38
    ## [113] 0.00 0.00 0.00 0.00 0.00 0.54 0.00 0.17 0.00 0.00 1.22 0.09 0.00 0.07
    ## [127] 0.34 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.39 0.37
    ## [141] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.11 0.01
    ## [155] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.41 0.00 0.00 0.00 0.19
    ## [169] 0.03 0.00 0.80 0.22 0.12 0.08 0.00 0.00 0.00 0.00 0.04 0.06 0.00 0.00
    ## [183] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.06 0.01
    ## [197] 0.61 0.72 0.00 0.15 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
    ## [211] 0.00 0.00 0.00 0.32 0.21 0.04 0.00 0.29 0.07 0.00 0.00 0.00 0.21 0.61
    ## [225] 0.35 0.20 0.54 0.05 0.42 0.00 0.00 0.00 0.00 0.00 0.00 0.41 0.00 0.22
    ## [239] 0.00 0.00 0.00 0.00 0.59 0.16 0.00 1.04 2.04 0.10 1.24 1.04 0.14 0.43
    ## [253] 0.00 0.00 1.22 0.00 0.00 0.12 0.00 0.00 0.08 0.00 0.00 0.00 0.00 0.02
    ## [267] 0.00 0.00 0.25 0.07 0.00 0.00 0.83 0.07 0.22 0.66 0.12 0.54 0.00 0.00
    ## [281] 0.00 0.08 0.00 0.00 0.04 0.00 0.00 0.00 0.08 0.10 0.25 0.00 0.05 0.02
    ## [295] 0.35 0.08 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.06
    ## [309] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.02 0.00 0.00 0.00 0.07 0.00 0.00

Build an empty dataframe in order to compile the data on each transect
======================================================================

``` r
IndivTransectdf = data.frame(matrix(vector(), 0, 13, dimnames=list(c(), c("X", "Y", "Unique ID", "Percent GM","Percent Rosettes", "Percent Stems","Num Stems","Num Aphids","Num Miners","Num Other","Total Possible Length", "Overlap Length", "Percent Overlap"))))
```

Now the SummaryOneCoordinate takes 1 transect line (ex. 810,170) and finds the total percent cover of: general Garlic Mustard(GM), rosettes, stems and Percent overlap on that line.

This also calculates the total count of: stems, aphid, miner, other & the total possible length measured in the square. It does this by adding the sum of the counts for the transect.

``` r
SummaryOneCoordinate=function(UniqueID2Check,GMdata, overlap){
    #Takes the name of the coordinate you want to find calculated data for and returns a dataframe of the results of the compilation calculations.
y.sub=subset(GMdata, UniqueID2Check == UniqueID);
NumRowInSubset=nrow(y.sub); #finds number of rows in subset
X = mean(y.sub$X)
Y = mean(y.sub$Y)
UniqueIDpoint= mean(y.sub$UniqueID)

StartDistance=as.numeric(y.sub[1,"START"]); #Find Start Tape value
EndDistance=as.numeric(y.sub[NumRowInSubset, "END"]) #Find End Tape value

TOTMeasureLength= EndDistance-StartDistance; #The absolute value of the measurement
TOTMISCLength= sum(y.sub$MISClength)         #The sum of all MISC lengths along that measurement
TOTPossibleLength=TOTMeasureLength-TOTMISCLength #The total possible length that GM can inhabit is the length of the tape minus any misc. gaps.

#finds total length by finding the sum of all the GM lengths
TOTGMLength = sum(y.sub$GMlength);  
TOTROSLength = sum(y.sub$ROSlength);
TOTSTEMLength = sum(y.sub$STEMlength);
OverlapLen = overlap[UniqueID2Check]

#defines percentages by dividing Total occupied length by possible length times 100
PerGM=as.numeric((TOTGMLength/(TOTPossibleLength)*100));
PerROS=as.numeric((TOTROSLength/(TOTPossibleLength)*100));
PerSTEM=as.numeric((TOTSTEMLength/(TOTPossibleLength)*100));
PerOverlap = OverlapLen/TOTPossibleLength
        
#these define the total number of counts, taking sum of the subset.
TOTNumAphid=sum(y.sub$NUM_APHID);
TOTNumStem=sum(y.sub$NUM_STEMS);
TOTNumMiner=sum(y.sub$NUM_MINER);
TOTNumOther=sum(y.sub$NUM_OTHER);
       
        output =data.frame("X"=X, "Y"=Y, "Unique ID" = UniqueIDpoint, "Percent GM"=PerGM, "Percent Rosettes"=PerROS, "Percent Stems"=PerSTEM, "Total Num Stems"=TOTNumStem, "Total Num Aphids"=TOTNumAphid, "Total Num Miners"=TOTNumMiner, "Total Num Other"=TOTNumOther,"Total Possible Length"=TOTPossibleLength, "Overlap Length" = OverlapLen, "Percent Overlap" = PerOverlap)
return(output)} #returns a dataframe of the summary data
```

This function calls the SummaryOneCoordinate fucntion at each unique transect coordinate and returns a dataframe of all the summary statistics per transect as IndivTransectdf and as the file: GMoutputIndivTransects.csv

``` r
AtEachTransect = function(GMdata,overlap){
  for (i in ilist){
    output = SummaryOneCoordinate(i, GMdata, overlap);
    IndivTransectdf=rbind.data.frame(IndivTransectdf,output);
  }
  return(IndivTransectdf)
}

IndivTransectdf = AtEachTransect(GMdata,overlap)
head(IndivTransectdf, n=20)
```

    ##      X   Y Unique.ID Percent.GM Percent.Rosettes Percent.Stems
    ## 1  836 202         1  37.091837        29.540816     8.9285714
    ## 2  835 202         2  12.182741         4.720812     7.1065990
    ## 3  834 202         3   7.046632         5.544041     1.5025907
    ## 4  836 200         4  14.827586        14.581281     0.5418719
    ## 5  835 200         5  27.944800        22.129128     5.8156727
    ## 6  834 200         6   6.882793         6.384040     0.4987531
    ## 7  833 200         7  11.710398         9.475219     2.5267250
    ## 8  832 200         8   0.000000         0.000000     0.0000000
    ## 9  831 200         9         NA               NA            NA
    ## 10 830 200        10   0.000000         0.000000     0.0000000
    ## 11 829 200        11   0.000000         0.000000     0.0000000
    ## 12 828 200        12  17.583603        14.455232     1.9956850
    ## 13 827 200        13  39.010417        34.843750     3.6979167
    ## 14 826 200        14  26.141732        23.569554     1.4173228
    ## 15 836 198        15   9.508197         9.508197     0.0000000
    ## 16 835 198        16  15.320042        11.647429     2.6232949
    ## 17 834 198        17  31.243523        27.668394     8.0310881
    ## 18 833 198        18   2.206573         1.877934     0.3286385
    ## 19 832 198        19  20.343980        16.805897     8.5995086
    ## 20 831 198        20   6.339468         5.061350     1.2781186
    ##    Total.Num.Stems Total.Num.Aphids Total.Num.Miners Total.Num.Other
    ## 1               19                0               10              18
    ## 2               16                0                5              16
    ## 3                3                0                3               2
    ## 4                2                0                0               2
    ## 5               12                0                2              10
    ## 6                2                0                2               2
    ## 7                7                0                2               7
    ## 8                0                0                0               0
    ## 9                0                0                0               0
    ## 10               0                0                0               0
    ## 11               0                0                0               0
    ## 12               4                0                0               2
    ## 13              11                0                3              10
    ## 14               4                0                1               4
    ## 15               0                0                0               0
    ## 16               8                1                3               8
    ## 17              14                0                5              14
    ## 18               1                0                0               1
    ## 19              23                0                8              22
    ## 20               3                1                1               3
    ##    Total.Possible.Length Overlap.Length Percent.Overlap
    ## 1                  19.60           0.57    0.0290816327
    ## 2                  19.70           0.01    0.0005076142
    ## 3                  19.30           0.00    0.0000000000
    ## 4                  20.30           0.06    0.0029556650
    ## 5                  20.29           0.04    0.0019714145
    ## 6                  20.05           0.00    0.0000000000
    ## 7                  20.58           0.17    0.0082604470
    ## 8                  19.45           0.00    0.0000000000
    ## 9                     NA           0.00              NA
    ## 10                 19.76           0.00    0.0000000000
    ## 11                 17.24           0.00    0.0000000000
    ## 12                 18.54           0.00    0.0000000000
    ## 13                 19.20           0.10    0.0052083333
    ## 14                 19.05           0.04    0.0020997375
    ## 15                 18.30           0.00    0.0000000000
    ## 16                 19.06           0.15    0.0078698846
    ## 17                 19.30           0.76    0.0393782383
    ## 18                 21.30           0.00    0.0000000000
    ## 19                 20.35           1.18    0.0579852580
    ## 20                 19.56           0.00    0.0000000000

``` r
write.csv(IndivTransectdf, file = "GMoutputIndivTransects.csv")
```

Now we want to find the spatial representation of the data in terms of a square of area. This data will be easier to visualize spatially than individual transects lines. In this case each square will equal 3 adjacent transects. (ex. 810,170 & 811,170 & 812,170)

First I build an empty dataframe to put the data onto.

``` r
outputavgbase = data.frame(matrix(vector(), 0, 10, dimnames=list(c(), c("X", "Y", "Average Percent GM","Average Percent Rosettes", "Average Percent Stems", "Average Percent Overlap", "Num Stems","Num Aphids","Num Miners","Num Other"))));
```

Avg cell builds a dataframe of 3 adjacent transects and runs calculations (sums and averages) to return a result of the avergae value across this square. The X,Y coordinate returned for the square is equal to the coordinate of the furthest west transect at the square. So a square marked 810, 170 will contain transects (810,170 & 811,170 & 812,170)

``` r
avgcell= function(j,k,dataframe){
#this function returns a dataframe of 1 cell with the values of 3 transects, taking the most southwest corner rebar as the input and returning a dataframe with that coordinate as the coordinate of the whole
startj=j;
startk=k;
      for(i in 1:3){
#This combined 3 transects into one dataframe called avg1 to do calculations on
           output=subset(dataframe, X==j & Y == k);
                 outputavgbase=rbind.data.frame(outputavgbase,output);
                 j=j+1}    
           avg1=outputavgbase;

#finding averages in the cell by making each column a list and averaging the list.
meanGM =  mean(ts(avg1[,c('Percent.GM')]));
meanROS= mean(ts(avg1[,c('Percent.Rosettes')]));
meanSTEM=mean(ts(avg1[,c('Percent.Stems')]));
meanOverlap = mean(ts(avg1[,c('Percent.Overlap')]));
NumSTEM=sum(ts(avg1[,c('Total.Num.Stems')]));
NumAphids= sum(ts(avg1[,c('Total.Num.Aphids')]));
NumMiners=sum(ts(avg1[,c('Total.Num.Miners')]));
NumOther = sum(ts(avg1[,c('Total.Num.Other')]));

#buildnewdataframeforcell
celloutput =data.frame("X"=startj, "Y"=startk, "Average Percent GM"=meanGM, "Average Percent Rosettes"=meanROS, "Average Percent Stems"=meanSTEM, "Average Percent Overlap" = meanOverlap , "Num Stems"=NumSTEM, "Num Aphids"=NumAphids, "Num Miners"=NumMiners, "Num Other"=NumOther);
return(celloutput)}
```

The following function calls the avg cell function on the coordinates on one north-south column to find all the summary data for one square of transects.

``` r
#This goes through a column of coordinates starting at the southernmost point, the if conditions are needed to adjust for when the southernmost point gets more southern at 826,828,and 830. 
#Need to count the number of j+1 to get a true idea of how many cells should be made, for example at 818,there are points in the south that aren’t made into an average cell.
columnavg=function(j,k,data){
b=as.integer(j)+1;
occur = table(unlist(IndivTransectdf));
nu=occur[as.character(b)]
numbertimes=as.integer(nu);
a=numbertimes;
if(j==826){k=174}
if(j==828){k=174}
if(j==830){k=174}
for(i in 1:a){
out=avgcell(j,k,data);
k=k+2;
outputavgbase=rbind.data.frame(outputavgbase,out)};
return(outputavgbase)}
```

The squaretot function runs the above columnavg function on adjacent columns to yield a rectangle of data. For example we would call the column data on the X=810 column and the X=812 column

``` r
#For the squaretot you have to tell it  how many times to do the N-S loop, as long as the map doesn’t change this works
squaretot=function(j,k,numberoftimes,data)
{for (i in 1:numberoftimes) 
{outcol=columnavg(j,k,data);
j=j+2;
outputavgbase=rbind.data.frame(outputavgbase,outcol)};
return(outputavgbase)}
```

Our transect data is not a perfect square so we must run multiple squaretot functions to cover all the study area.

``` r
#All of the above output dataframes
AllCellAverages=function(IndivTransectdf){
dfaa=squaretot(810,170,4,IndivTransectdf)
dfbb=squaretot(818,174,1,IndivTransectdf)
dfcc=squaretot(820,176,7,IndivTransectdf)
dfdd=squaretot(834,178,1,IndivTransectdf)
GMAverageOutputdf=rbind.data.frame(dfaa,dfbb,dfcc,dfdd)
return(GMAverageOutputdf)}
```

Now a total GMAverageOutputdf can be built with a call to AllCell Averages and yield dataframe= GMAverageOutputdf and file "GMoutputAverageCellsTransects.csv"

``` r
GMAverageOutputdf = AllCellAverages(IndivTransectdf)
head(GMAverageOutputdf, n=20)
```

    ##      X   Y Average.Percent.GM Average.Percent.Rosettes
    ## 1  810 170           7.413470                 6.753147
    ## 2  810 172           7.297053                 5.746866
    ## 3  810 174          11.777852                11.619253
    ## 4  810 176          13.558999                12.089607
    ## 5  810 178          20.760920                18.437936
    ## 6  810 180          16.522596                15.459203
    ## 7  810 182          12.505192                11.206914
    ## 8  810 184          15.740782                14.717793
    ## 9  812 170          12.939649                12.361015
    ## 10 812 172          11.031522                 7.733929
    ## 11 812 174          14.472198                14.421591
    ## 12 812 176          27.321191                27.035986
    ## 13 812 178          22.531502                21.847079
    ## 14 812 180          23.905840                23.463362
    ## 15 812 182          12.292240                 9.359720
    ## 16 812 184          11.363429                10.582447
    ## 17 812 186          25.380518                24.544727
    ## 18 814 170          18.563825                16.530397
    ## 19 814 172          22.260405                21.725426
    ## 20 814 174          22.062017                22.062017
    ##    Average.Percent.Stems Average.Percent.Overlap Num.Stems Num.Aphids
    ## 1             2.38862785            0.0010852713         7          0
    ## 2             1.22881506            0.0000000000         7          0
    ## 3             0.14172955            0.0000000000         2          0
    ## 4             1.21864005            0.0006686727         7          0
    ## 5             1.95640959            0.0014109347         9          0
    ## 6             1.06339291            0.0000000000         7          0
    ## 7             0.95473874            0.0000000000         5          0
    ## 8             0.82792840            0.0014988082        10          0
    ## 9             0.54117481            0.0010852713         5          0
    ## 10            3.09183184            0.0000000000        29          0
    ## 11            0.03373819            0.0000000000         1          0
    ## 12            0.28666877            0.0014406627         2          0
    ## 13            0.75919155            0.0019047619         5          0
    ## 14            0.44247788            0.0000000000         2          0
    ## 15            1.77033063            0.0000000000        13          0
    ## 16            0.71718598            0.0000000000         5          0
    ## 17            0.99967224            0.0018026876        13          0
    ## 18            0.96346926            0.0003292181         7          0
    ## 19            0.43674499            0.0010752688         5          0
    ## 20            0.00000000            0.0000000000         0          0
    ##    Num.Miners Num.Other
    ## 1           3         4
    ## 2           3         7
    ## 3           0         2
    ## 4           1         7
    ## 5           2         7
    ## 6           4         5
    ## 7           4         4
    ## 8           3        10
    ## 9           1         3
    ## 10          2        13
    ## 11          0         1
    ## 12          2         0
    ## 13          5         7
    ## 14          2         1
    ## 15          6        11
    ## 16          4         4
    ## 17          0        13
    ## 18          5         5
    ## 19          2         1
    ## 20          0         0

``` r
write.csv(GMAverageOutputdf, file= "GMoutputAverageCellsTransects.csv")
```
