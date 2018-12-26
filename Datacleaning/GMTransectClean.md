GMTransectClean
================
Paul Manning
12/23/2018

Project overview: At the Ordway Field Station in Inver Grove Heights Minnesota, the invasive plant garlic mustard is studied. Study of the plant takes place yearly and involves taking 322 transects of the forest floor to produce data. These transects are approximately 20 meters long and measurements of individual patches are taken within each transect. For example a patch of garlic mustard may be measured 0.51-1.73 meters, and a patch of gooseberry at 1.68-2.41m. Furthermore, along the path obstacles that prevent any growth are noted and numbers of invidiual cases are noted as well (ex. number of Garlic mustard plants with aphid/pest damage).

This Rmarkdown presents a solution to clean the data that is gathered in spreadsheet form in the field. An example of the raw data can be seen on my github.

Before you can clean the data, you must combine the 3-5 individual datatables generated in the field as one master dataframe. This can be done by reading each individual table as a laballed dataframe and combining them with rbind. uncomment and add the dataframe names into this. If you run into errors make sure the column names, counts and types are all the same. Location doesnt matter.

ALso make sure that the GMdataraw has columns labelled by type correctly, commonly the NUM\_APHID category does not autoset at an integer type.

``` r
#install.packages('tidyverse')
#install.packages("zoo")
#install.packages("knitr")
#install.packages("dplyr")
#library(dplyr)
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
#GMdataraw = rbind(dataframe1, dataframe2, dataframe3)
GMdataraw <- read.csv(file="GMdataraw.csv", header=TRUE, sep=",")


save(GMdataraw, file="GMdataraw.RData")
load(file="GMdataraw.RData")
```

Each of the 322 transects taken of the forest floor can be located from it's southern-most end (transects run North-South), by GPS coordinates shorted to the last 3 digits.

The biggest error in past transect data has been transects that appear to be entirely missing on first glance. This is due to error in measurement orginially or mislablling on transects.

To solve this problem I build two lists that contains the ideal count of the transects at each given line (ex. at E-W latitude of 810 there should be 8 transects, and etc.). I did this for the E-W measurement of the transects and the N-S meaurements, so this can be compared to, to ensure that each has the correct amount at each coordinate.

``` r
Xfreq = data.frame("freq" = c(8, 8, 9, 9, 11, 11, 12, 12 ,13 ,11 ,11 ,10 ,11 ,11 ,12 ,12 ,14 ,14 ,14 ,14 ,14 ,14 , 14, 13, 14, 13, 13))

Yfreq= data.frame("freq" = c(9,9,18,25,27,27,27,27,25,23,23,21,19,15,13,11,3))
```

Now a function loops over each transect in order and prints a message if any transect counts do not match the above datasets.

This will identify the following errors: 1) too many or too few transects present in data (will print "This data should have 322 coordinates, but actually has X") 2) if any transect value along (X,Y) is not expected,( will print "X value is not expected check Y column.) 3) checks each line (X and Y) for too few or many transects at that line compared to the above lists (prints which line has the incorrect amount of transects)

``` r
CorrectNumTransects= function(data,lookAtCol,ReferenceDf,increment,lowval,highval){
#This function takes the raw data, specifically the isolated column (chosen through LookAtCol) and compares that list with one of the Xfreq/Yfreq lists. It returns a printed messages if either of these is incorrect. Coordinates increase in a +1 increment for X coordinates and +2 for Y coordinates so this is specified. It also makes sure that all coordinates points are between 810-836 and prints an error if there is a problem there.
  SelectedColumn=data[,c(lookAtCol)]     #Selects only target column
  ReferenceDfLength=nrow(ReferenceDf)    #Counts length of reference dataframe
  sGMsub = SelectedColumn[complete.cases(SelectedColumn), ]  #represents target column as booleans, should reveal a list with 322 TRUE to show presence of value.
  irow=nrow(sGMsub);
  if (any(sGMsub<lowval)|any(sGMsub>highval)) {print(paste0(sGMsub, ' is a value not in the range of ', lowval, ' to ', highval, ' double check ', lookAtCol ,' column'))};        #if any value in the target column is not between low val and high val an error message will print
  if (irow != 322) {print(paste0('This data should have 322 coordinates, but actually has ',irow))}
  xline=lowval 
  listorder=1
  for(i in 1:ReferenceDfLength){
    Whatis= sum(xline==sGMsub) #taking the sum of the number of times a number shows up in the raw data allows us to tell how many coordinates use that X/Y coordinate.
  ShouldBe= ReferenceDf[listorder,1]
  if (Whatis !=ShouldBe) {print(paste0('at ', xline, 
' the amount of coordinates in this line is incorrect. There should be ',ShouldBe, ' coordinated but this line has ',Whatis ))}
  xline= xline+ increment
  listorder= listorder + 1}
}

#Below are calls to the above function

#CorrectNumTransects(GMdataraw, 'X', Xfreq, 1, 810, 836)
#CorrectNumTransects(GMdataraw, 'Y', Yfreq, 2, 170, 202)
```

Now we quickly doublcheck that there are 322 instances of a value in the X column and in the Y column. We can also check that there are 322 instances of a transect tapemeasure START value and an END value.

``` r
varsub=GMdataraw[,c('Y')]
irow = sum(!is.na(varsub))
if (irow != 322){print(paste0('incorrect amount of ',var))}
```

``` r
Checkvar= function(GMdataraw,var){
#Takes raw data and a variable and make sure each only occurs as many times as the number of coordiantes(322)
varsub=GMdataraw[,c(var)]
irow = sum(!is.na(varsub))
if (irow != 322){print(paste0('incorrect amount of ',var))}
if (irow == 322){print(paste0('correct amount of ',var))}}

Checkvar(GMdataraw,'X');
```

    ## [1] "correct amount of X"

``` r
Checkvar(GMdataraw,'Y');
```

    ## [1] "correct amount of Y"

``` r
Checkvar(GMdataraw,'START');
```

    ## [1] "correct amount of START"

``` r
Checkvar(GMdataraw,'END')
```

    ## [1] "correct amount of END"

The next most major and common error is when the entered data has a end measurement shorter than the start measurement, where if it is not caught provides that transect with a negative value for the patch measurements.

This makes sure that: 1) The GM\_START is shorter than the GM\_END 2) The ROS\_START (Rosette start) is shorter than the ROS\_END 3) the STEM\_START is shorter than the STEM\_END 4) the MISC\_START is shorter than the MISC\_END

``` r
#The following Makes sure that the end point is at a longer distance than the start point

listchecks= function(GMdataraw){
#Makes a Boolean (TRUE FALSE) List checking whether the END is greater than the START for most distance variables. Then each list is checked to print on what lines there is a false answer to the listcheck
listcheck1 = GMdataraw$GM_END > GMdataraw$GM_START
listcheck2 = GMdataraw$ROS_END > GMdataraw$ROS_START
listcheck3 = GMdataraw$STEM_END > GMdataraw$STEM_START
listcheck4 = GMdataraw$MISC_END > GMdataraw$MISC_START


if (FALSE %in% listcheck1){
loca= which(listcheck1 == FALSE);
print(paste0('GM_end is smaller number than start at ',loca));
}

if (FALSE %in% listcheck2){
loca = which(listcheck2 == FALSE);
print(paste0('ROS_end is smaller number than start at ',loca));
}

if (FALSE %in% listcheck3){
loca=which(listcheck3 == FALSE);
print(paste0('STEM_end is smaller number than start at ',loca));
}

if (FALSE %in% listcheck4){
loca=which(listcheck4 == FALSE);
print(paste0('MISC_end is smaller number than start at ',loca))
}

print('All GM and MISC measured have an end that longer than the start')}


#Calls the above functions appropriately for X and Y
listchecks(GMdataraw)
```

    ## [1] "All GM and MISC measured have an end that longer than the start"

Once these above tests returns nothing the data is ready to be indexed and analyzed, all the necessary pre data corrections are done, without this the following functions will not work.

Now the data must be prepared which means that: 1) each row should have an X and Y coordinate to show what transect that data belongs to. 2) The measurements that are lengths are calculated by calculating (end - start). resulting in : ROS\_LENGTH, STEM\_LENGTH, GM\_LENGTH, MISC\_LENGTH 3) relevant values that were NA are now changed to 0: including, NUM\_APHID, NUM\_STEMS, NUM\_OTHER, NUM\_MINER, and the above lengths

``` r
initialize= function(GMdataraw){
#This will index each coordinate and make new columns for total lengths per segment

NewX=na.locf(ts(GMdataraw[,c('X')]))  #fills NAs with the last non-NA, making subsetting of coordinates possible.
GMdataraw[,c('X')]=NewX #Now replaces the X column with the non-NA list

NewY=na.locf(ts(GMdataraw[,c('Y')]))  #fills NAs with the last non-NA, making subsetting of coordinates possible.
GMdataraw[,c('Y')]=NewY #Now replaces the X column with the non-NA list

    #round all measurements to two decimal points to prevent floating point errors.
GMdataraw$START=round(GMdataraw$START, digits = 2)
GMdataraw$END=round(GMdataraw$END, digits = 2)
GMdataraw$GM_START=round(GMdataraw$GM_START, digits = 2)
GMdataraw$GM_END=round(GMdataraw$GM_END, digits = 2)
GMdataraw$ROS_START=round(GMdataraw$ROS_START, digits = 2)
GMdataraw$ROS_END=round(GMdataraw$ROS_END, digits = 2)
GMdataraw$STEM_START=round(GMdataraw$STEM_START, digits = 2)
GMdataraw$STEM_END=round(GMdataraw$STEM_END, digits = 2)


#add GMlength,STEMlength,ROSlength value to chart

GMdataraw$GMlength=GMdataraw$GM_END-GMdataraw$GM_START
GMdataraw$STEMlength=GMdataraw$STEM_END-GMdataraw$STEM_START
    GMdataraw$ROSlength=GMdataraw$ROS_END-GMdataraw$ROS_START
GMdataraw$MISClength=GMdataraw$MISC_END-GMdataraw$MISC_START

#change NAs to 0 for all columns except the original measurements
GMdataraw$NUM_APHID[is.na(GMdataraw$NUM_APHID)] = 0
GMdataraw$NUM_STEMS[is.na(GMdataraw$NUM_STEMS)] = 0
GMdataraw$NUM_OTHER[is.na(GMdataraw$NUM_OTHER)] = 0
GMdataraw$NUM_MINER[is.na(GMdataraw$NUM_MINER)] = 0
GMdataraw$STEMlength[is.na(GMdataraw$STEMlength)] = 0
GMdataraw$ROSlength[is.na(GMdataraw$ROSlength)] = 0
GMdataraw$GMlength[is.na(GMdataraw$GMlength)] = 0
GMdataraw$MISClength[is.na(GMdataraw$MISClength)] = 0

GMdata=GMdataraw
print('GM data is now indexed')
return(GMdata)}


#Calls the above function
GMdata=initialize(GMdataraw)
```

    ## [1] "GM data is now indexed"

The data name GMdata is what has been indexed and initalized, GMdataraw is what is pre index and initalized.

Now we can ccheck to see that the GM measurements are in order and spaced appropriately. This means that we will check that 1) On a transect line, a later patch comes at a higher measurement than an earlier patch. 2) The distance between the patches is our standardized 0.20 m

``` r
checkTwentycmApart=function(j,k,GMdata,s,e) {
#Takes each subset of coordinates (j=X and k=Y) and what the name of the start column and end column you want to look at are. Returns error messages if there are any flaws or ends the function
yy.sub=subset(GMdata, X==j & Y == k);
ENDrow=nrow(yy.sub) #finds length of subset
START=as.double(yy.sub[1,'START']) #finds value for start
END=as.double(yy.sub[ENDrow,'END']) #finds value for end
count=1
yGMsub=yy.sub[,c(s,e)]
yGMsub = yGMsub[complete.cases(yGMsub), ] #deletes all NA rows
irow=nrow(yGMsub)
if (irow<0.2) {return()} #If there are no values to compare the function ends here.
aa=as.double(yGMsub[count,s]) #Takes the first X_START measurement and rounds it
a=round(aa, digits = 2)
bb=as.double(yGMsub[count,e])#Takes the first X_END measurement and rounds it
b=round(bb, digits = 2)

if (a < START){print(paste0('First measurement ',a,' is less than Tape Start', START))}
if (b > END){print('Last measurement ',b,' is greater than Tape END',END)}
if (irow>1) {
xrow=irow-1 #if length of start-end pairs is 7 we do the following comparison 6 times.
for (i in 1:xrow){
b=as.double(yGMsub[count,e]) #Takes first end in list
count=count+1;
a=as.double(yGMsub[count,s]) #Takes the start after then end in list
diff=a-b #checks difference
Inputdiff = round(diff, digits = 2) #rounds to 2 decimal places
if (Inputdiff < 0.20){ print(paste0('difference between ',s,' and ',e,' is less than .2 at ',j,' ',k,' ',a,' ',b))}
}}}

#Calls distance check function for each thing to check
checkAllApart= function(j,k,GMdata){
checkTwentycmApart(j,k,GMdata,'GM_START','GM_END')
checkTwentycmApart(j,k,GMdata,'ROS_START','ROS_END')
checkTwentycmApart(j,k,GMdata,'STEM_START','STEM_END')
}
#Test functions
#checkTwentycmApart(810,170,GMdata,'STEM_START','STEM_END')
#checkAllApart(825,196,GMdata)

CheckSquare=function(j,k,GMdata,length){
#checks each coordinate in order looping through each row and then movings  column over, how many coordinates are checked at each is determined by the number of coordiantes in the raw data that’s not indexed. j=X, k=Y, and length is the number of X coordinates that is the length of the square being checked (ex. On the 170 line 810-818, there are 9 coordinates.
entryj=j
entryk=k
basek=k
kyy.sub=subset(GMdataraw, Y==k)
krow= nrow(kyy.sub)
for (i in 1:length){
k=basek
yyy.sub=subset(GMdataraw, X==j)
irow= nrow(yyy.sub)
if(entryj== 826 & entryk==174){irow=1} #5 coordinates are lower than the rest so there needs to be an exception for these. (824, 176 was the last corner of the sqaure)
for (i in 1:irow){
    print(paste0('checking ', j,k))
checkAllApart(j,k,GMdata);
k=k+2}
j=j+1
}}

CheckSquaresGM=function(GMdata){
CheckSquare(810,170,GMdata,9)
CheckSquare(819,174,GMdata,2)
CheckSquare(821,176,GMdata,16)
CheckSquare(826,174,GMdata,7)}

#Calls to the checks above.
CheckSquaresGM(GMdata)
```

    ## [1] "checking 810170"
    ## [1] "checking 810172"
    ## [1] "checking 810174"
    ## [1] "checking 810176"
    ## [1] "checking 810178"
    ## [1] "checking 810180"
    ## [1] "checking 810182"
    ## [1] "checking 810184"
    ## [1] "checking 811170"
    ## [1] "checking 811172"
    ## [1] "checking 811174"
    ## [1] "checking 811176"
    ## [1] "checking 811178"
    ## [1] "checking 811180"
    ## [1] "checking 811182"
    ## [1] "checking 811184"
    ## [1] "checking 812170"
    ## [1] "checking 812172"
    ## [1] "checking 812174"
    ## [1] "checking 812176"
    ## [1] "checking 812178"
    ## [1] "checking 812180"
    ## [1] "checking 812182"
    ## [1] "checking 812184"
    ## [1] "checking 812186"
    ## [1] "checking 813170"
    ## [1] "checking 813172"
    ## [1] "checking 813174"
    ## [1] "checking 813176"
    ## [1] "checking 813178"
    ## [1] "checking 813180"
    ## [1] "checking 813182"
    ## [1] "checking 813184"
    ## [1] "checking 813186"
    ## [1] "checking 814170"
    ## [1] "checking 814172"
    ## [1] "checking 814174"
    ## [1] "checking 814176"
    ## [1] "checking 814178"
    ## [1] "checking 814180"
    ## [1] "checking 814182"
    ## [1] "checking 814184"
    ## [1] "checking 814186"
    ## [1] "checking 814188"
    ## [1] "checking 814190"
    ## [1] "checking 815170"
    ## [1] "checking 815172"
    ## [1] "checking 815174"
    ## [1] "checking 815176"
    ## [1] "checking 815178"
    ## [1] "checking 815180"
    ## [1] "checking 815182"
    ## [1] "checking 815184"
    ## [1] "checking 815186"
    ## [1] "checking 815188"
    ## [1] "checking 815190"
    ## [1] "checking 816170"
    ## [1] "checking 816172"
    ## [1] "checking 816174"
    ## [1] "checking 816176"
    ## [1] "difference between GM_START and GM_END is less than .2 at 816 176 28.2 28.01"
    ## [1] "checking 816178"
    ## [1] "checking 816180"
    ## [1] "checking 816182"
    ## [1] "checking 816184"
    ## [1] "checking 816186"
    ## [1] "checking 816188"
    ## [1] "checking 816190"
    ## [1] "checking 816192"
    ## [1] "checking 817170"
    ## [1] "checking 817172"
    ## [1] "checking 817174"
    ## [1] "checking 817176"
    ## [1] "checking 817178"
    ## [1] "checking 817180"
    ## [1] "checking 817182"
    ## [1] "checking 817184"
    ## [1] "checking 817186"
    ## [1] "checking 817188"
    ## [1] "checking 817190"
    ## [1] "checking 817192"
    ## [1] "checking 818170"
    ## [1] "checking 818172"
    ## [1] "checking 818174"
    ## [1] "checking 818176"
    ## [1] "checking 818178"
    ## [1] "checking 818180"
    ## [1] "checking 818182"
    ## [1] "checking 818184"
    ## [1] "checking 818186"
    ## [1] "checking 818188"
    ## [1] "checking 818190"
    ## [1] "checking 818192"
    ## [1] "checking 818194"
    ## [1] "checking 819174"
    ## [1] "checking 819176"
    ## [1] "checking 819178"
    ## [1] "checking 819180"
    ## [1] "checking 819182"
    ## [1] "checking 819184"
    ## [1] "checking 819186"
    ## [1] "checking 819188"
    ## [1] "checking 819190"
    ## [1] "checking 819192"
    ## [1] "checking 819194"
    ## [1] "checking 820174"
    ## [1] "checking 820176"
    ## [1] "checking 820178"
    ## [1] "difference between GM_START and GM_END is less than .2 at 820 178 55.1 54.93"
    ## [1] "checking 820180"
    ## [1] "checking 820182"
    ## [1] "checking 820184"
    ## [1] "checking 820186"
    ## [1] "checking 820188"
    ## [1] "checking 820190"
    ## [1] "checking 820192"
    ## [1] "checking 820194"
    ## [1] "checking 821176"
    ## [1] "checking 821178"
    ## [1] "checking 821180"
    ## [1] "checking 821182"
    ## [1] "checking 821184"
    ## [1] "checking 821186"
    ## [1] "checking 821188"
    ## [1] "checking 821190"
    ## [1] "checking 821192"
    ## [1] "checking 821194"
    ## [1] "checking 822176"
    ## [1] "checking 822178"
    ## [1] "checking 822180"
    ## [1] "checking 822182"
    ## [1] "checking 822184"
    ## [1] "checking 822186"
    ## [1] "checking 822188"
    ## [1] "checking 822190"
    ## [1] "checking 822192"
    ## [1] "checking 822194"
    ## [1] "checking 822196"
    ## [1] "checking 823176"
    ## [1] "checking 823178"
    ## [1] "checking 823180"
    ## [1] "checking 823182"
    ## [1] "difference between GM_START and GM_END is less than .2 at 823 182 74.95 82.92"
    ## [1] "checking 823184"
    ## [1] "checking 823186"
    ## [1] "checking 823188"
    ## [1] "checking 823190"
    ## [1] "checking 823192"
    ## [1] "checking 823194"
    ## [1] "difference between GM_START and GM_END is less than .2 at 823 194 21.36 21.23"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 823 194 21.36 21.23"
    ## [1] "checking 823196"
    ## [1] "checking 824176"
    ## [1] "checking 824178"
    ## [1] "difference between GM_START and GM_END is less than .2 at 824 178 36.05 36.25"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 824 178 36.01 35.83"
    ## [1] "checking 824180"
    ## [1] "First measurement 41.27 is less than Tape Start41.32"
    ## [1] "First measurement 41.27 is less than Tape Start41.32"
    ## [1] "checking 824182"
    ## [1] "checking 824184"
    ## [1] "checking 824186"
    ## [1] "checking 824188"
    ## [1] "checking 824190"
    ## [1] "checking 824192"
    ## [1] "checking 824194"
    ## [1] "checking 824196"
    ## [1] "checking 824198"
    ## [1] "checking 825176"
    ## [1] "checking 825178"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 825 178 28.34 29.01"
    ## [1] "checking 825180"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 825 180 47.75 47.57"
    ## [1] "checking 825182"
    ## [1] "checking 825184"
    ## [1] "checking 825186"
    ## [1] "difference between GM_START and GM_END is less than .2 at 825 186 22.5 22.36"
    ## [1] "difference between GM_START and GM_END is less than .2 at 825 186 22.98 22.79"
    ## [1] "difference between GM_START and GM_END is less than .2 at 825 186 27.99 28.47"
    ## [1] "difference between GM_START and GM_END is less than .2 at 825 186 29.58 29.41"
    ## [1] "difference between GM_START and GM_END is less than .2 at 825 186 35.02 34.83"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 825 186 27.99 28.47"
    ## [1] "checking 825188"
    ## [1] "checking 825190"
    ## [1] "checking 825192"
    ## [1] "difference between GM_START and GM_END is less than .2 at 825 192 2.33 2.14"
    ## [1] "checking 825194"
    ## [1] "checking 825196"
    ## [1] "checking 825198"
    ## [1] "checking 826176"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 826 176 36.55 36.43"
    ## [1] "checking 826178"
    ## [1] "difference between GM_START and GM_END is less than .2 at 826 178 44.37 44.22"
    ## [1] "checking 826180"
    ## [1] "checking 826182"
    ## [1] "checking 826184"
    ## [1] "checking 826186"
    ## [1] "checking 826188"
    ## [1] "checking 826190"
    ## [1] "checking 826192"
    ## [1] "checking 826194"
    ## [1] "difference between GM_START and GM_END is less than .2 at 826 194 50.2 50.85"
    ## [1] "checking 826196"
    ## [1] "checking 826198"
    ## [1] "difference between GM_START and GM_END is less than .2 at 826 198 18.44 18.25"
    ## [1] "checking 826200"
    ## [1] "checking 826202"
    ## [1] "checking 827176"
    ## [1] "checking 827178"
    ## [1] "checking 827180"
    ## [1] "checking 827182"
    ## [1] "checking 827184"
    ## [1] "checking 827186"
    ## [1] "checking 827188"
    ## [1] "checking 827190"
    ## [1] "checking 827192"
    ## [1] "checking 827194"
    ## [1] "checking 827196"
    ## [1] "checking 827198"
    ## [1] "checking 827200"
    ## [1] "checking 827202"
    ## [1] "checking 828176"
    ## [1] "checking 828178"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 178 51.57 51.67"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 178 56.34 56.52"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 178 59.73 59.82"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 828 178 51.57 51.67"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 828 178 56.34 56.52"
    ## [1] "checking 828180"
    ## [1] "First measurement 60.06 is less than Tape Start61.7"
    ## [1] "First measurement 60.06 is less than Tape Start61.7"
    ## [1] "checking 828182"
    ## [1] "checking 828184"
    ## [1] "checking 828186"
    ## [1] "checking 828188"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 188 68.79 68.68"
    ## [1] "checking 828190"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 190 1.92 1.75"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 828 190 1.92 1.75"
    ## [1] "checking 828192"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 192 23.99 23.82"
    ## [1] "checking 828194"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 194 56.75 56.59"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 828 194 56.75 56.59"
    ## [1] "checking 828196"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 196 69.72 69.58"
    ## [1] "checking 828198"
    ## [1] "checking 828200"
    ## [1] "difference between GM_START and GM_END is less than .2 at 828 200 25.32 25.85"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 828 200 25.32 25.85"
    ## [1] "checking 828202"
    ## [1] "checking 829176"
    ## [1] "checking 829178"
    ## [1] "checking 829180"
    ## [1] "difference between GM_START and GM_END is less than .2 at 829 180 62.4 62.5"
    ## [1] "checking 829182"
    ## [1] "checking 829184"
    ## [1] "checking 829186"
    ## [1] "checking 829188"
    ## [1] "difference between GM_START and GM_END is less than .2 at 829 188 66.72 73"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 829 188 66.72 73"
    ## [1] "checking 829190"
    ## [1] "checking 829192"
    ## [1] "checking 829194"
    ## [1] "checking 829196"
    ## [1] "checking 829198"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 829 198 12.31 12.12"
    ## [1] "checking 829200"
    ## [1] "checking 829202"
    ## [1] "checking 830176"
    ## [1] "checking 830178"
    ## [1] "checking 830180"
    ## [1] "checking 830182"
    ## [1] "checking 830184"
    ## [1] "checking 830186"
    ## [1] "checking 830188"
    ## [1] "difference between GM_START and GM_END is less than .2 at 830 188 82.02 81.84"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 830 188 82.02 81.84"
    ## [1] "checking 830190"
    ## [1] "checking 830192"
    ## [1] "checking 830194"
    ## [1] "difference between GM_START and GM_END is less than .2 at 830 194 48.9 48.73"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 830 194 48.9 48.73"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 830 194 56.82 56.63"
    ## [1] "checking 830196"
    ## [1] "checking 830198"
    ## [1] "checking 830200"
    ## [1] "checking 830202"
    ## [1] "checking 831176"
    ## [1] "checking 831178"
    ## [1] "difference between GM_START and GM_END is less than .2 at 831 178 53.26 53.08"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 831 178 53.26 53.08"
    ## [1] "checking 831180"
    ## [1] "difference between GM_START and GM_END is less than .2 at 831 180 77.53 77.34"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 831 180 77.53 77.34"
    ## [1] "checking 831182"
    ## [1] "checking 831184"
    ## [1] "checking 831186"
    ## [1] "checking 831188"
    ## [1] "difference between GM_START and GM_END is less than .2 at 831 188 76.96 76.81"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 831 188 76.96 76.81"
    ## [1] "checking 831190"
    ## [1] "checking 831192"
    ## [1] "checking 831194"
    ## [1] "checking 831196"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 831 196 81.19 81.62"
    ## [1] "checking 831198"
    ## [1] "checking 831200"
    ## [1] "checking 831202"
    ## [1] "checking 832176"
    ## [1] "checking 832178"
    ## [1] "checking 832180"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 832 180 73.93 75.5"
    ## [1] "checking 832182"
    ## [1] "difference between GM_START and GM_END is less than .2 at 832 182 10.09 9.91"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 832 182 10.09 9.91"
    ## [1] "checking 832184"
    ## [1] "checking 832186"
    ## [1] "checking 832188"
    ## [1] "checking 832190"
    ## [1] "checking 832192"
    ## [1] "checking 832194"
    ## [1] "checking 832196"
    ## [1] "checking 832198"
    ## [1] "checking 832200"
    ## [1] "checking 832202"
    ## [1] "checking 833176"
    ## [1] "checking 833178"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 833 178 20.56 20.42"
    ## [1] "checking 833180"
    ## [1] "checking 833182"
    ## [1] "checking 833184"
    ## [1] "difference between GM_START and GM_END is less than .2 at 833 184 4.3 4.14"
    ## [1] "checking 833186"
    ## [1] "checking 833188"
    ## [1] "checking 833190"
    ## [1] "difference between GM_START and GM_END is less than .2 at 833 190 2.41 2.24"
    ## [1] "checking 833192"
    ## [1] "checking 833194"
    ## [1] "checking 833196"
    ## [1] "checking 833198"
    ## [1] "checking 833200"
    ## [1] "checking 834176"
    ## [1] "checking 834178"
    ## [1] "checking 834180"
    ## [1] "checking 834182"
    ## [1] "checking 834184"
    ## [1] "checking 834186"
    ## [1] "checking 834188"
    ## [1] "checking 834190"
    ## [1] "checking 834192"
    ## [1] "checking 834194"
    ## [1] "checking 834196"
    ## [1] "difference between GM_START and GM_END is less than .2 at 834 196 73.47 73.3"
    ## [1] "checking 834198"
    ## [1] "difference between GM_START and GM_END is less than .2 at 834 198 4.25 4.1"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 834 198 6.34 6.17"
    ## [1] "checking 834200"
    ## [1] "checking 834202"
    ## [1] "checking 835176"
    ## [1] "checking 835178"
    ## [1] "checking 835180"
    ## [1] "checking 835182"
    ## [1] "checking 835184"
    ## [1] "checking 835186"
    ## [1] "checking 835188"
    ## [1] "checking 835190"
    ## [1] "checking 835192"
    ## [1] "difference between GM_START and GM_END is less than .2 at 835 192 45.09 44.92"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 835 192 45.09 44.92"
    ## [1] "checking 835194"
    ## [1] "difference between GM_START and GM_END is less than .2 at 835 194 71.03 71.14"
    ## [1] "checking 835196"
    ## [1] "checking 835198"
    ## [1] "checking 835200"
    ## [1] "checking 836176"
    ## [1] "checking 836178"
    ## [1] "checking 836180"
    ## [1] "checking 836182"
    ## [1] "checking 836184"
    ## [1] "difference between GM_START and GM_END is less than .2 at 836 184 63.17 63.06"
    ## [1] "difference between GM_START and GM_END is less than .2 at 836 184 79.14 79.22"
    ## [1] "checking 836186"
    ## [1] "checking 836188"
    ## [1] "difference between GM_START and GM_END is less than .2 at 836 188 8.49 8.32"
    ## [1] "difference between GM_START and GM_END is less than .2 at 836 188 9.02 8.83"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 836 188 8.49 8.32"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 836 188 9.02 8.83"
    ## [1] "checking 836190"
    ## [1] "checking 836192"
    ## [1] "checking 836194"
    ## [1] "checking 836196"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 836 196 61.91 62.42"
    ## [1] "checking 836198"
    ## [1] "checking 836200"
    ## [1] "checking 826174"
    ## [1] "checking 827174"
    ## [1] "difference between ROS_START and ROS_END is less than .2 at 827 174 8 11.95"
    ## [1] "checking 828174"
    ## [1] "checking 829174"
    ## [1] "checking 830174"
    ## [1] "difference between GM_START and GM_END is less than .2 at 830 174 15.78 15.66"
    ## [1] "checking 831174"
    ## [1] "difference between GM_START and GM_END is less than .2 at 831 174 17.11 16.93"
    ## [1] "difference between STEM_START and STEM_END is less than .2 at 831 174 17.11 16.93"
    ## [1] "checking 832174"
    
Now you after you clean the data for the above noted mistakes, you only need to export the data as GMdata.csv (change the title in the quotes if needed)
    
```r
write.csv(GMdata, "GMdata.csv")
```
