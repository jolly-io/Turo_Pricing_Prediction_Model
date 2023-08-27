
#-----------------------------First Provision Required Packages and Libraries--------------------------
install.packages("e1071")
install.packages("comprehenr")
library(e1071)
library(comprehenr)

turo.df  <- readRDS("turo.data.5140")
turodata.df.dummy <- turo.df[turo.df$car.state == "nj",] # subset df for nj data 
# Clean blank data level by write csv and read it again
# (the data level of other state)
write.csv(turodata.df.dummy,"turodata_dummy.csv", row.names = FALSE)
turodata.df.nj <- read.csv("turodata_dummy.csv")



# Replace the car.year column with car.age
turodata.df.nj[,"car.age"] <- 2022-turodata.df.nj[,"car.year"]
turodata.df.nj <- turodata.df.nj[,(-grep("car.year",colnames(turodata.df.nj)))]

# Q3 construct 2 df for continuous and categorical data
con.turodata.df <- data.frame(row.names=c(1:nrow(turodata.df.nj)))
cat.turodata.df <- data.frame(row.names=c(1:nrow(turodata.df.nj)))

# All the continuous variables in turodata.df.nj are numeric and have at least 6 in value of
# length(unique(column data)) by observing the csv file. 
# Assign them to 2 different data frames for calculating their stats.
for (i in names(turodata.df.nj)){
  if (is.numeric(turodata.df.nj[,i])){
    if (length(unique(turodata.df.nj[,i]))>=6){
      con.turodata.df[,i] <- turodata.df.nj[,i]
    }
    else{
      cat.turodata.df[,i] <- turodata.df.nj[,i]
    }
  }
  else{
    cat.turodata.df[,i] <- turodata.df.nj[,i]
  }
}

# Stat for continuous variable
for (i in names(con.turodata.df)){
  print(paste("============================================"))
  print(paste("Stat for continuous variable ",i))
  print(paste("Minimum of ",i," :", min(con.turodata.df[,i], na.rm = TRUE)))
  print(paste("First quartile of ",i," :", quantile(con.turodata.df[,i], 0.25, na.rm = TRUE)))
  print(paste("Median of ",i," :", median(con.turodata.df[,i], na.rm = TRUE))) 
  print(paste("Third quartile of ",i," :", quantile(con.turodata.df[,i], 0.75, na.rm = TRUE)))
  print(paste("Maximum of ",i," :", max(con.turodata.df[,i], na.rm = TRUE)))
  print(paste("Mean of ",i," :", mean(con.turodata.df[,i], na.rm = TRUE)))
  print(paste("Standard deviation of ",i," :", sd(con.turodata.df[,i], na.rm = TRUE)))
  print(paste("Skewness as summary statistics of ",i," :", skewness(con.turodata.df[,i], type = 2, na.rm = TRUE)))
  hist(con.turodata.df[,i], breaks = "FD", xlab = paste("Number of ",i), 
       main = paste("Histogram of ",i))
}

# Frequency distribution for categorical variable
for (i in names(cat.turodata.df)){
  print(paste("============================================"))
  print(paste("Below is the frequency distribution of ",i))
  selected.column <- cat.turodata.df[,i]
  selected.column.freq <- data.frame(table(selected.column))
  names(selected.column.freq)[1] <- "Type"
  names(selected.column.freq)[2] <- "Frequency"
  selected.column.freq <- selected.column.freq[order(-selected.column.freq$Frequency),]
  print(selected.column.freq)
  print(paste("Below is the relative frequency distribution of ",i))
  selected.column.freq$Rel.Freq<-selected.column.freq$Frequency/sum(selected.column.freq$Frequency)
  selected.column.freq$Percent.Freq<-selected.column.freq$Rel.Freq*100
  print(selected.column.freq[,-2])
  sum.cat<-length(unique(cat.turodata.df[,i]))# Sum of category in a column
  print(paste("Below is the bar plot of ",i))
  # Display minimum of (sum of category or 8) bars in the bar chart
  barplot(selected.column.freq$Frequency[1:min(8,sum.cat)],
          names.arg=selected.column.freq$Type[1:min(8,sum.cat)],
          xlab = "Categorical value", ylab = "Frequency", 
          main = paste("Bar Chart of ",i))
}


# Q4 Identify outlier
outlier.row.num <- c()
for (i in names(con.turodata.df)){
  lower.bound <- quantile(con.turodata.df[,i],p=0.25,na.rm=TRUE)-
    1.5*IQR(con.turodata.df[,i],na.rm=TRUE)
  upper.bound <- quantile(con.turodata.df[,i],p=0.75,na.rm=TRUE)+
    1.5*IQR(con.turodata.df[,i],na.rm=TRUE)
  out.total <- 0 #prepare to calculate total of outlier
  for (rownum in (1:nrow(con.turodata.df))){
    if (!is.na(con.turodata.df[rownum,i])){
      if (con.turodata.df[rownum,i]<lower.bound|con.turodata.df[rownum,i]>upper.bound){
        out.total<- out.total+1
      }
    }
  }
  # If more than 5% of data in a column are outliers, too many data will be 
  # removed. Also, the kurtosis and skewness may be too extreme for using IQR
  # to remove, therefore we will not remove outliers of the column that have more 
  # than 5% of data are outliers
  dontremove<-FALSE 
  if (out.total>0.05*nrow(con.turodata.df)){
    dontremove<-TRUE 
  }
  for (j in (1:nrow(con.turodata.df))){
    if (!is.na(con.turodata.df[j,i])&dontremove!=TRUE){
      if (con.turodata.df[j,i]<lower.bound|con.turodata.df[j,i]>upper.bound&
          (upper.bound!=0)){
        if (!j%in%outlier.row.num){
          outlier.row.num<-append(outlier.row.num,j)
        }
      }
    }
  }
}
# the vector of row num that have outlier(will be removed with missing value)

print(paste("=============================="))
print(paste("The row that have outlier are as below"))
print(outlier.row.num)

# Q5 Remove missing value (since all NA values are suitable to remove, they will
# all be removed)
missing.row.num <- c()
for (i in names(turodata.df.nj)){
  for (j in (1:nrow(turodata.df.nj))){
    if (is.na(turodata.df.nj[j,i])){
      if (!j%in%missing.row.num){
        missing.row.num<-append(missing.row.num,j)
      }
    }   
  }
}
print(paste("=============================="))
print(paste("The row that have missing values are as below"))
print(missing.row.num) # Rows that have missing data

# Find out the row that need to be removed(have missing data and outlier)
remove.row <- c()
remove.row <- append(remove.row,outlier.row.num)
for (item in missing.row.num){
  if (!item%in%remove.row){
    remove.row <- append(remove.row,item)
  }
}
print(paste("=============================="))
print(paste("The row that need to be removed are as below"))
print(remove.row)
turodata.nj.clean <- turodata.df.nj[-remove.row,]

# Q6 Regression
options(scipen = 999)
car.make.levels.order <- c("bmw", "aston-martin", "mercedes-benz", "bentley", "rolls-royce", "nissan", "toyota", "honda", "tesla","porsche")
turodata.nj.clean$car.make<- factor(turodata.nj.clean$car.make,car.make.levels.order)
#=======
turo.lm <- lm(car.trip.price ~ car.make + car.age  + car.model + car.extra.num + 
                car.displayed.user.review.num.past.6m + car.extra.mile.fee  + car.photo.num +  car.extra.pet.fee + car.city  , data = turodata.nj.clean )
turo.lm.summary <- summary(turo.lm)
print(turo.lm.summary)