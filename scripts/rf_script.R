# Setup
library(randomForest)
data <- read.csv('./raw_data/train.csv')
test_data <- read.csv('./raw_data/test.csv')
summary(data)
str(data)

# Pre-process train data
data$northness <- cos(pi * data$Aspect / 180)
data$eastness <- sin(pi * data$Aspect / 180)
data$Cover_Type <- factor(data$Cover_Type)
x <- data[, -c(1, 3, 56)] # "Id", "Aspect" and "Cover_Type)
y <- data$Cover_Type

# Pre-process test data
test_data$northness <- cos(pi * test_data$Aspect / 180)
test_data$eastness <- sin(pi * test_data$Aspect / 180)
test_x <- test_data[, -c(1, 3)] # "Id" and "Aspect")

# Initial exploration -----------------------------------------------------
# Plot histograms
var_names <- names(x)
pdf("./outputs/hist_plots.pdf", width = 10, height = 40)
par(mfrow=c(14,4))
for(i in seq(x[1,])){
  hist(x[[i]], main = var_names[i])
}
dev.off()

# Plot boxplots
var_names <- names(x)
pdf("./outputs/box_plots.pdf", width = 10, height = 40)
par(mfrow=c(14,4))
for(i in seq(x[1,])){
  boxplot(x[[i]]~y, main = var_names[i])
}
dev.off()

# Note: soil_type7, soil_type15 have no representation in the train dataset
# but have 105 and 3 (respectivly) observations in the test dataset

# Draft model - Random forest V1-------------------------------------------
mod_rf <- randomForest(x=x, y=y, importance = TRUE, ntree = 500)

sub <- data.frame('Id' = test_data$Id,
                  'Cover_Type' = predict(mod_rf, test_x))
write.csv(sub, file = './outputs/submission_rf_v1.csv', row.names = F)
