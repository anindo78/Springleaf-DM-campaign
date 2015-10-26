setwd("~/Kaggle/SpringLeaf DM Campaign")


# Read number of rows of data in R without reading entire table
testcon <- file("train.csv",open="r")
readsizeof <- 20000
nooflines <- 0
( while((linesread <- length(readLines(testcon,readsizeof))) > 0 ) 
  nooflines <- nooflines+linesread )
close(testcon)
nooflines
#145232, 1932


train2 <- read.csv("train.csv", header=T, nrows = 100)
head(train)

prop.table(table(train$target))
#   0    1 
# 0.82 0.18 

#source("listtoDT.R")

ls <- as.data.frame(lapply(train, class), nrow=length(train), ncol=2)
ls <- as.data.frame(t(ls))
ls$Var <- names(train)
ls <- ls[order(ls$V1),]

# write.csv(ls, file="ls.csv")
write.csv(ls, file="ls2.csv")

# integer	    1868
# factor  	  50
# numeric	    11


#drop logical values since all are NAs in this sample
train2 <- train2[!sapply(train2, is.logical)]
#1929

# separate non factor values
intonly <- !sapply(train2, is.factor)
int_train <- train2[intonly]
# 1879

# Looking for unique values
uni_train <- as.data.frame(unlist(lapply(int_train, function(x) length(unique(x)))))
colnames(uni_train) <- "count"

barplot(quantile(uni_train$count, probs=seq(0,1,0.05)))

#only count = 1
count1 <- subset(uni_train, count == 1)
int_train2 <- int_train[, !names(int_train) %in% rownames(count1)]
#1744


# identify columns with 9999s... w.r.t. regular length and replace by 0
c1 <- function(x) {
  max=max(nchar(x)) - min(nchar(x))
}


int2 <- as.data.frame(unlist(lapply(int_train2, c1)))
colnames(int2) <- "count"
quantile(int2$count, probs=seq(0,1,0.1))

int2$names <- rownames(int2)
int3 <- int2[int2$count >=5, ]

int_train3 <- int_train2[, names(int_train2) %in% int3$names]
#381

c2 <- function(x) {
  miss = ifelse(substr(as.character(x),1,2)=="99" | substr(as.character(x),1,3)== "-99" |
                  substr(as.character(x),1,2)== "-1", 0, x)
}

int_train3 <- as.data.frame(lapply(int_train3, c2))
int_train2 <- as.data.frame(cbind(int_train2[, !names(int_train2) %in% int3$names], int_train3))

# replace values e.g. 98, 99 etc. by 0

c4 <- function(x) {
  miss = ifelse(substr(as.character(x),1,2)=="99" | substr(as.character(x),1,3)== "-99" |
                substr(as.character(x),1,2)=="98" | substr(as.character(x),1,3)== "-98" 
                , 0, x)
}
int_train4 <- as.data.frame(lapply(int_train2, c4))

# % of missing values

# convert all miss into 0
int_train4[is.na(int_train4)] <- 0

# random check on unique values on int_train4
uni_train2 <- as.data.frame(unlist(lapply(int_train4, function(x) length(unique(x)))))
colnames(uni_train2) <- "count"
quantile(uni_train2$count, probs=seq(0,1,0.05))

count2 <- subset(uni_train2, count == 1)
int_train4 <- int_train4[, !names(int_train4) %in% rownames(count2)]
#1685


# if the column has 1,2,...discrete integers, keep as is. For others find missing val %

int5 <- as.data.frame(unlist(lapply(int_train4, c1)))
colnames(int5) <- "count"
quantile(int5$count, probs=seq(0,1,0.05))

int5$names <- rownames(int5)
int6 <- int5[int5$count >=4, ]

int_train5 <- int_train4[, names(int_train4) %in% int6$names]
#199

mv <- colSums(int_train5!= 0)/nrow(int_train5)
quantile(mv, probs=seq(0,1,0.1))
plot(quantile(mv, probs=seq(0,1,0.05)))

# Distribution of non missing values
# 0%    5%   10%   15%   20%   25%   30%   35%   40%   45%   50%   55%   60%   65%   70%   75%   80%   85%   90% 
# 0.010 0.010 0.020 0.040 0.076 0.115 0.174 0.230 0.250 0.290 0.360 0.438 0.490 0.580 0.630 0.700 0.740 0.770 0.820 
# 95%   100% 
# 0.940 0.990

# Removing columns with less than 10% populated
int_train6 <- int_train5[, colSums(int_train5!= 0)/nrow(int_train5) <= 0.1]
#46

int_train7 <- int_train4[, !names(int_train4) %in% names(int_train6)]
#1639

# Drop low and high variance. Top and bottom 5%
# Scale and normalize the columns

norm <- function(x) {
  x = var(   (x-min(x))/(max(x)-min(x))   )
}

int7 <- as.data.frame(sapply(int_train7, norm))
colnames(int7) <- "variance"
int7$names <- rownames(int7)


quantile(int7$variance, probs = seq(0,1,0.05), na.rm=T)
plot(quantile(int7$variance, probs = seq(0,1,0.05), na.rm=T))
plot(density(int7$variance))


int8 <- int7[(int7$variance < 0.010035323 | int7$variance > 0.121565572 | rownames(int7)=='target') | is.na(int7$variance)==T,  ]
quantile(int8$variance, probs = seq(0,1,0.05), na.rm=T)
plot(density(int8$variance))
#164

int_train8 <- int_train7[, !(names(int_train7) %in% int8$names)]
#1475

# Finding near zero variables before pre processing
nzv <- nearZeroVar(int_train8, saveMetrics= TRUE)
table(nzv$nzv)
# FALSE  TRUE 
# 1248   227

int_train9 <- int_train8[, !nzv$nzv]
# 1248

# correlation check with absolute thresh at 0.9
cor <- cor(int_train9)
summary(cor[upper.tri(cor)])
hcor <- findCorrelation(cor, cutoff = .90)
int_train10 <- int_train9[, -hcor]
#697


# bring back and read non integer values

nint <- sapply(train2, is.factor)
chr <- train2[nint]
#50

# separate out all datetime columns
locator <- apply(chr, 2, function(x) grepl(":", x))
index <- apply(locator, 2, any)
dat <- chr[, index]

dat1 <- as.data.frame(apply(dat, 2, function(x) format(as.Date(substr(x[!is.na(x)], 1, 7), '%d%b%y'), "%Y-%m-%d")))
colnames(dat1) <- colnames(dat)

par(mar=c(2,2,2,2),mfrow=c(4,4))
for (i in 1:ncol(dat1)) {plot(dat1[order(dat[,i]),i], main=colnames(dat1)[i], xlab="", ylab="")}

# post analyses, decide to drop ALL the date and time columns.
ndat <- chr[, !index]
ndat[ndat==-1] = NA
ndat[ndat=="[]"] = NA

# count missing values & remove with 70% missing
mv2 <- ndat[, colSums(is.na(ndat))/nrow(ndat)> 0.7]
ndat1 <- ndat[, !names(ndat) %in% names(mv2)]

# Missing value imputation with MODE
MODE <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (x in 1:ncol(ndat1)) {
  ndat1[is.na(ndat1[, x]), x] <- MODE(ndat1[, x], na.rm=T)
}

# drop factors with only one level
lev <- NULL
ndat2 = rep(" ", 100)
name <- " "
for (i in 1:ncol(ndat1)) {
  lev[i] <- length(levels(ndat1[,i]))
  if (lev[i]==1) {ndat2 <- ndat2} else {ndat2 <- as.data.frame(cbind(ndat2, ndat1[,i]))
  name <- cbind(name, names(ndat1[i]))
  }
}
colnames(ndat2) <- name
ndat2 <- ndat2[, -1]
# 14

fcols12 <- c(colnames(int_train9), colnames(ndat2))
write.csv(fcols12, file="fcols12.csv")
fcols12 <- as.list(fcols12)
# ndat2 and int_train10 have the names of columns needed to pull from larger dataset
# get column names and col classess from this list

fcols1 <- c(colnames(int_train10), colnames(ndat2))
#712
# repeat the same procedure as above with 
# fcols2 = 10000 rows, fcols3 = 30000 (sampled from middle)
# fcols4 = 50000(sampled from middle) to see if same 712 columns are selected
# see file repeatanalysis.R


length(intersect(fcols1,fcols2)) # common
#497
length(setdiff(fcols1,fcols2)) # in 1 not in 2
#214
length(setdiff(fcols2,fcols1)) # in 2 not in 1
#194

length(intersect(fcols2,fcols3)) # common
#639
length(setdiff(fcols2,fcols3)) 
#52
length(setdiff(fcols3,fcols2)) 
#38


length(intersect(fcols1,fcols3)) # common
#478
length(setdiff(fcols1,fcols3)) 
#233
length(setdiff(fcols3,fcols1)) 
#199


length(intersect(fcols2,fcols4)) # common
#640
length(setdiff(fcols2,fcols4)) 
#51
length(setdiff(fcols4,fcols2)) 
#36

length(intersect(fcols3,fcols4)) # common
#660
length(setdiff(fcols3, fcols4)) 
#17
length(setdiff(fcols4,fcols3)) 
#16

## More the records in sample, the less columns chosen
## Decide to go for fcols4
fcols4 <- rbind(names(train$target), fcols4)
write.csv(fcols4, file="fcols4.csv")
f4 <- read.csv("fcols4.csv", header=T)

f4 <- as.list(f4)
