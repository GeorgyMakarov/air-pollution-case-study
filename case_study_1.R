setwd("C:/Users/Георгий/Documents/GitHub/air-pollution-case-study")

# read the data for 1999
pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#", 
                  header = FALSE, sep = "|", na.strings = "")
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm0) <- make.names(cnames[[1]])

x0 <- pm0$Sample.Value
class(x0)
str(x0)

summary(x0)

# see the proportion of NA in x0
mean(is.na(x0)) ## missing values in this case do not impact significantly on
                ## country level of PM25

# read the data for 2012
pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#", 
                  header = FALSE, sep = "|", na.strings = "")
dim(pm1)

# the names are the same as in 1999
names(pm1) <- make.names(cnames[[1]])
head(pm1)

x1 <- pm1$Sample.Value
str(x1)
summary(x1)
summary(x0)

# see the proportion of NA in x1
mean(is.na(x1)) ## proportion of missing values in 2012 is lower than 1999

# look at boxplot comparison of 1999 and 2012
boxplot(x0, x1) ## the data is right skewed
                ## this can be fixed with the log10

boxplot(log10(x0), log10(x1)) ## the spread of data increased in 2012
                              ## return to this feature later

# find negative values in 2012
negative <- x1 < 0
str(negative)
sum(negative, na.rm = TRUE)
mean(negative, na.rm = TRUE) ## the proportion of negative values is
                             ## is 2% - this is relatively low

# look at the dates column
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, "month")
hist(dates[negative], "month")

# look at New York state subset
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
head(site0)

site0 <- paste(site0[, 1], site0[, 2], sep = ".")
site1 <- paste(site1[, 1], site1[,2], sep = ".")

str(site0)
str(site1)

# find sites intersection for 1999 and 2012

both <- intersect(site0, site1)
both

pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))

# subset only NY and both monitors

cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
head(cnt0)

# find number of observations in each data frame

sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

# create subsets for country 63 and site 2008

pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm0sub)
dim(pm1sub)

# create dataset with dates

dates1 <- pm1sub$Date
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)

dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm = T)) ## plots are hard to compare as they are
                                     ## at different range
                                     ## this can be solved by ranging both plots

# range both plots

rng <- range(x0sub, x1sub, na.rm = T)
par(mfrow = c(1, 2))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T)) ## the average is going down and the range is going down

# exploring change by state

# calculate mean by state

mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)
summary(mn1)

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
head(d0)
head(d1)

# merge 2 datasets in 1

mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)

# plot the dataframe

par(mfrow = c(1, 1))
with(mrg, plot(rep(1999, 52), mrg[, 2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 52), mrg[, 3]))
segments(rep(1999, 52), mrg[, 2], rep(2012, 52), mrg[, 3])
