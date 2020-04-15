# Compares income of T1K subjects to the average income of the tract in which they live.

setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")
source("R/functions.R")
require('stringr')
require('leaflet')

endyear <- readline("Endyear: ")
nfactors <- readline("Number of Factors: ")
state <- readline("State Code: ")

if (!file.exists(paste0("output/", endyear, "/useful_ACS_originalData.csv"))){
  stop("\nThere is no data available for this endyear.\nPlease run 'download_ACS_data.R' for this endyear")
}

original <- read.csv(paste0("output/", endyear, "/useful_ACS_originalData.csv"), row.names = 1)
# If the state code is only a single digit, it is padded.
rownames(original) <- str_pad(rownames(original), 11, "left", pad="0")

original <- original[which(substr(rownames(original), 1, 2) == state),]
# Define the number of tracts
n_tracts <- dim(original)[1]

# Total population ----
condensed <- original['Total']
condensed['Total..Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars.'] <- original['Total..Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars.']

Household.Income <- c("Less.than..10.000", "X.10.000.to..14.999", "X.15.000.to..19.999", 
                      "X.20.000.to..24.999", "X.25.000.to..29.999", "X.30.000.to..34.999", 
                      "X.35.000.to..39.999", "X.40.000.to..44.999", "X.45.000.to..49.999",
                      "X.50.000.to..59.999", "X.60.000.to..74.999", "X.75.000.to..99.999", 
                      "X.100.000.to..124.999", "X.125.000.to..149.999", "X.150.000.to..199.999", 
                      "X.200.000.or.more")
N <- length(Household.Income)
n <- original[Household.Income]
x <- c(4999.5,12499.5,17499.5,22499.5,27499.5,32499.5,37499.5,42499.5,
       47499.5,54999.5,67499.5,87499.5,112499.5,137499.5,174999.5,200000)

mu <- matrix(nrow = n_tracts, ncol = dim(n)[2])
for(i in 1:dim(n)[2]){
  mu[,i] <- x[i] * n[,i]
}
mu <- rowSums(mu)
mu <- mu/original['Total..Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars.']

sigma_2 <- matrix(nrow = n_tracts, ncol = 1)
for(i in 1:n_tracts){
  sigma_2[i,] <- rowSums((x-mu[i,])^2 * n[i,])/(rowSums(n[i,])-1)
}

sigma <- sqrt(sigma_2)

ord <- order(mu)
data <- data.frame(avg = mu[ord,], sd = sigma[ord,], tract = row.names(mu)[ord], rank = seq(1, n_tracts, by = 1))

demo <- as.data.frame(read.csv(paste0("output/",endyear,"/", nfactors," factors/loc_and_rw_and_score.csv")))[,c("geoCode","id","event","Income")]
demo <- demo[which(!is.na(demo[, "Income"])),]
subi2traci <- merge(demo, data, by.x = 1, by.y = 3)

require(ggplot2)
p <- ggplot(data) + geom_path(aes(y=avg, x = rank, colour = "Average"))+
  geom_ribbon(aes(ymin=avg-sd, x = rank, ymax=avg+sd, fill = "1 sd"), alpha = 0.5, fill = "red") +
  geom_ribbon(aes(ymin=avg-2*sd, x = rank, ymax=avg+2*sd, fill = "2 sd"), alpha = 0.3, fill = "red") +
  scale_y_continuous(limits = c(-1e+05, 8e+05)) + 
  labs(x = "Tract Rank", y = "Income") + annotate("point", x=subi2traci[,"rank"], y=subi2traci[,"Income"], size = .5)
p
# In this figure, each black dot represents an individual subject's income. The red distribution represents the distribution of income within each tract. It is important to note here that even though the subjects do not typically have an average income, they fall into the error margin.


png(paste0("plots/", endyear, "/sd_tractIncome_OK(200K).png"), width = 960*1.25, height = 480*1.25)
p
dev.off()

n <- as.matrix(n)
n <- cbind (rownames(n),n)
data <- as.matrix(data)
data2 <- merge(n, data, by.x = 1, by.y = 3)
ord2 <- order(data2["rank"])
n <- n[,-1]
r <- mapply(n, FUN=as.numeric)
r <- matrix(data=r, ncol=16, nrow=n_tracts)

mycolors <- grDevices::rainbow(n_tracts)
pal <- colorNumeric(palette = "Spectral", domain = c(1,n_tracts), reverse = T)
j = 1
par(mfrow = c(1,10), mar = c(4,0,4,0)+0.1)
# for(i in ord2){
#   barplot(r[i,], space=0, axisnames = F, ylim = c(16,0), col = pal(j), horiz = T)
#   j = j + 1
#  #text(seq(1.5, 2*16.5,by=2), par("usr")[3]-0.25, srt = 60, adj= 1, xpd = TRUE, labels = colnames(n), cex=0.65)
# }

for(i in ord2){
  barplot(r[1,], space=0, width = c(10,5,5,5,5,5,5,5,5,10,15,25,25,25,50,50), axisnames = F, ylim = c(250,0), col = pal(j), horiz = T)
  j = j + 1
  #text(seq(1.5, 2*16.5,by=2), par("usr")[3]-0.25, srt = 60, adj= 1, xpd = TRUE, labels = colnames(n), cex=0.65)
}

s <- matrix(ncol = 5, nrow = n_tracts)
s[,1] <- sapply(1:n_tracts, function(x){sum(r[x,c(1:9)])})
s[,2] <- sapply(1:n_tracts, function(x){sum(r[x,c(10:12)])})
s[,3] <- sapply(1:n_tracts, function(x){sum(r[x,c(13:14)])})
s[,4] <- r[,15]
s[,5] <- r[,16]

s <- matrix(ncol = 40, nrow = n_tracts)
s[,1:2] <- r[,1]/2
s[,3:10] <- r[,2:9]
s[,11:12] <- r[,10]/2
s[,13:15] <- r[,11]/3
s[,16:20] <- r[,12]/5
s[,21:25] <- r[,13]/5
s[,26:30] <- r[,14]/5
s[,31:40] <- r[,15]/10

mu <- as.matrix(mu)

j = 1
png(paste0("plots/2015/income_dist_", state, ".png"), height = n_tracts*5, 500)
par(mfrow = c(n_tracts,1), mar = c(0,0,0,0), bg = "black", xaxs = "i")
for(i in ord2){
  barplot(s[i,], space=0, width = 5, axisnames = F, xlim = c(0,200), col = pal(j), horiz = F, border = NA)
  lines(x = c(mu[i]/1000, mu[i]/1000), y = c(0, max(s[i,])), lwd = 5, col = "red")
  j = j + 1
  #text(seq(1.5, 2*16.5,by=2), par("usr")[3]-0.25, srt = 60, adj= 1, xpd = TRUE, labels = colnames(n), cex=0.65)
}
dev.off()

j = 1
par(mfrow = c(10,1), mar = c(0,4,0,1)+0.1)
for(i in ord2){
  barplot(r[i,], space=0, axisnames = F, xlim = c(0,16), col = pal(j), horiz = F)
  j = j + 1
  #text(seq(1.5, 2*16.5,by=2), par("usr")[3]-0.25, srt = 60, adj= 1, xpd = TRUE, labels = colnames(n), cex=0.65)
}

