# ------ First, look at COV and population sizes ------


# ---- Race ----

# The population that describes themselves as ‘Two or more races’ represent 3% of the total population. Because of the small population size, we omitted this variable.
i <- "B02001"
data <- matrix(nrow = 0, ncol = 10)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}
sum(data[,"B02001_008"])/sum(data[,"B02001_001"])



# ---- Sex ----

# In 97% of tracts, the ratio of men to women is no greater than 3:2 and no less than 2:3.
i <- "B01001"
data <- matrix(nrow = 0, ncol = 48)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data <- rbind(data, thisdata)
}

sum((data[,"B01001_002"] >= .4 & data[,"B01001_002"] <= .6), na.rm = T)/sum(!is.na(data[,"B01001_002"]))



# ---- Transportation to work ----
i <- "B08301"
data <- matrix(nrow = 0, ncol = 21)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}
# This variable represents 4% of the population.
sum(data[,"B08301_021"])/sum(data[,"B08301_001"])

# Only represents 1% of the population.
sum(data[,"B08301_020"])/sum(data[,"B08301_001"])

# ? Represents 5% of the population.
sum(data[,"B08301_010"])/sum(data[,"B08301_001"])

i <- "B08301"
data <- matrix(nrow = 0, ncol = 20)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data <- rbind(data, thisdata)
}

# This variable represents 4% of the population. However, the cov of this variable across tracts is very low at 0.03. Due to low cov, ths variable was excluded.
var(data[,"B08301_021"], na.rm = T)/mean(data[,"B08301_021"], na.rm = T)

# ? Represents 5% of the population. COV is 0.25.
var(data[,"B08301_010"], na.rm = T)/mean(data[,"B08301_010"], na.rm = T)



# ---- Degree ----
i <- "B15012"
data <- matrix(nrow = 0, ncol = 16)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}
sci <- rowSums(data[,2:9])/data[,1]
bus <- data[,10]/data[,1]
edu <- data[,11]/data[,1]
art <- rowSums(data[,12:16])/data[,1]

# Reported degree had a low cov (Science/Engineering = .03, Business = .04, Education = .06, Arts/Humanities/Other = 0.4)
var(sci, na.rm = T)/mean(sci, na.rm = T)
var(bus, na.rm = T)/mean(bus, na.rm = T)
var(edu, na.rm = T)/mean(edu, na.rm = T)
var(art, na.rm = T)/mean(art, na.rm = T)

sum(data[,2:9])/sum(data[,1])
sum(data[,10])/sum(data[,1])
sum(data[,11])/sum(data[,1])
sum(data[,12:16])/sum(data[,1])

# ---- Education ----
i <- "B15003"
data <- matrix(nrow = 0, ncol = 25)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}

# 30% of the population has a bachelor degree or higher
(sum(data[,"B15003_022"])+sum(data[,"B15003_023"])+sum(data[,"B15003_024"])+sum(data[,"B15003_025"]))/sum(data[,"B15003_001"])
# 11% of the population has a masters degree or higher
(sum(data[,"B15003_023"])+sum(data[,"B15003_024"])+sum(data[,"B15003_025"]))/sum(data[,"B15003_001"])
# 19% of the population has a bachelor's degree
(sum(data[,"B15003_022"]))/sum(data[,"B15003_001"])
# 8% of the population has a master's degree
(sum(data[,"B15003_023"]))/sum(data[,"B15003_001"])
# 2% of the population has a professional degree
(sum(data[,"B15003_024"]))/sum(data[,"B15003_001"])
# 1% of the population has a doctoral degree
(sum(data[,"B15003_025"]))/sum(data[,"B15003_001"])

i <- "B15003"
data <- matrix(nrow = 0, ncol = 24)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data <- rbind(data, thisdata)
}

college <- (data[,"B15003_022"] + data[,"B15003_023"] + data[,"B15003_024"] + data[,"B15003_025"])

var(data[,"B15003_023"], na.rm = T)/mean(data[,"B15003_023"], na.rm = T)

# The cov of individuals with a bachelors degree or higher is 0.12. 
var(college, na.rm = T)/mean(college, na.rm = T)

hist(college)



# ---- PAI & SNAP ----
i <- "B19057"
data_B19057 <- matrix(nrow = 0, ncol = 3)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data_B19057 <- rbind(data_B19057, thisdata)
}

# 3% of the population receives PAI
sum(data_B19057[,2])/sum(data_B19057[,1])

# SNAP
i <- "B19058"
data_B19058 <- matrix(nrow = 0, ncol = 3)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data_B19058 <- rbind(data_B19058, thisdata)
}

# 13% of the population receives SNAP or PAI
sum(data_B19058[,2])/sum(data_B19058[,1])

# PAI
i <- "B19057"
data_B19057 <- matrix(nrow = 0, ncol = 2)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data_B19057 <- rbind(data_B19057, thisdata)
}

# COV of PAI is .038
var(data_B19057[,"B19057_002"], na.rm = T)/mean(data_B19057[,"B19057_002"], na.rm = T)

# SNAP
i <- "B19058"
data_B19058 <- matrix(nrow = 0, ncol = 2)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data_B19058 <- rbind(data_B19058, thisdata)
}
# COV of PAI or SNAP is .105
var(data_B19058[,"B19058_002"], na.rm = T)/mean(data_B19058[,"B19058_002"], na.rm = T)





# ---- Veteran status ----
i <- "B21001"
data <- matrix(nrow = 0, ncol = 38)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data <- rbind(data, thisdata)
}
# COV of veteran status is 0.025
var(data[,"B21001_002"], na.rm = T)/mean(data[,"B21001_002"], na.rm = T)

i <- "B21001"
data <- matrix(nrow = 0, ncol = 39)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}
# 8% of the population are veterans
sum(data[,"B21001_002"])/sum(data[,"B21001_001"])




# ---- Median year house built ----
i <- "B25035"
data <- matrix(nrow = 0, ncol = 1)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}

hist(data[,"B25035_001"])

mean(data[,"B25035_001"], na.rm = T)
var(data[,"B25035_001"], na.rm = T)/mean(data[,"B25035_001"], na.rm = T)




# ---- Heating Fuel ----
i <- "B25040"
data <- matrix(nrow = 0, ncol = 10)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}

(sum(data[,"B25040_002"])+sum(data[,"B25040_004"]))/sum(data[,"B25040_001"])

data <- matrix(nrow = 0, ncol = 9)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data <- rbind(data, thisdata)
}

main.fuel <- data[,"B25040_002"] + data[,"B25040_004"]
var(main.fuel, na.rm = T)/mean(main.fuel, na.rm = T)



# ---- Kitchen ----
i <- "B25051"
data <- matrix(nrow = 0, ncol = 3)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  data <- rbind(data, thisdata)
}

# 97% of American families have complete kitchens.
sum(data[,"B25051_002"])/sum(data[,"B25051_001"])

i <- "B25051"
data <- matrix(nrow = 0, ncol = 2)
for(k in 1:length(state.codes)){
  # Start the matrix by setting it equal to the first table.
  print(state.codes[k])
  thisdata <- readUseful(state.codes[k], endyear, i)
  thisdata <- thisdata@estimate
  thisdata <- sapply(1:dim(thisdata)[1], function(x){thisdata[x,2:dim(thisdata)[2]]/thisdata[x,1]})
  thisdata <- t(thisdata)
  data <- rbind(data, thisdata)
}

# the cov for complete kitchens is 0.002
var(data[,"B25051_002"], na.rm = T)/mean(data[,"B25051_002"], na.rm = T)



# ------ Now, a look at communalities ------

# veteran: 0.2535519
.2/183*232

# single dads: 0.2393443
.2/183*219

# other means (of getting to work): 0.1169399
.2/183*107

# two or more races: 0.1497268
.2/183*137
