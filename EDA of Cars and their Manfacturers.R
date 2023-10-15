vehicles <- read.csv("C:/Users/apurv/Downloads/Vehicle.csv")
head(vehicles)
summary(vehicles)



dim(vehicles)



# Display column names
names(vehicles)

# Display first 3 rows
head(vehicles, 3)

# Display last 6 rows
tail(vehicles, 6)



# Calculate average Kms_Driven for each Car_Name
aggregate(Kms_Driven ~ Car_Name, data = vehicles, FUN = mean)

# Calculate average Selling_Price for each Year
aggregate(Selling_Price ~ Year, data = vehicles, FUN = mean)

# Display unique combinations of Car_Name, Fuel_Type, Seller_Type, and Transmission
unique(vehicles[, c("Car_Name", "Fuel_Type", "Seller_Type", "Transmission")])



# Count occurrences of each combination of Car_Name, Fuel_Type, Seller_Type, and Transmission
counts <- table(vehicles$Car_Name, vehicles$Fuel_Type, vehicles$Seller_Type, vehicles$Transmission)

# Convert table to data frame
counts_df <- as.data.frame(counts)

# Rename columns
colnames(counts_df) <- c("Car_Name", "Fuel_Type", "Seller_Type", "Transmission", "Count")

# Display data frame in ascending order of Count
counts_df[order(counts_df$Count), ]

# Display data frame in descending order of Count
counts_df[order(-counts_df$Count), ]



# Check for missing values
any(is.na(vehicles))



# Count missing values in each column
missing_values <- colSums(is.na(vehicles))

# Display columns that contain missing values and their counts
missing_values[missing_values > 0]



# Find indices of missing values
missing_indices <- which(is.na(vehicles$Column1))

# Find most common value in column
most_common_value <- names(which.max(table(vehicles$Column1)))

# Replace missing values with most common value
vehicles$Column1[missing_indices] <- most_common_value

# Check if missing values were replaced successfully
any(is.na(vehicles$Column1))



# Check for duplicate rows
any(duplicated(vehicles))

# Remove duplicate rows
vehicles <- vehicles[!duplicated(vehicles), ]

# Check again for duplicate rows
any(duplicated(vehicles))





# Convert Fuel_Type column
vehicles$Fuel_Type <- as.numeric(factor(vehicles$Fuel_Type, levels = c("Petrol", "Diesel", "CNG"), labels = c(0, 1, 2)))

# Convert Seller_Type column
vehicles$Seller_Type <- as.numeric(factor(vehicles$Seller_Type, levels = c("Dealer", "Individual"), labels = c(0, 1)))

# Convert Transmission column
vehicles$Transmission <- as.numeric(factor(vehicles$Transmission, levels = c("Manual", "Automatic"), labels = c(0, 1)))

# Display converted columns
vehicles[, c("Fuel_Type", "Seller_Type", "Transmission")]





# Add Age column
vehicles$Age <- 2023 - vehicles$Year

# Display data frame with new Age column
vehicles



# Create new data frame with selected columns
new_vehicles <- vehicles[, c("Car_Name", "Selling_Price", "Present_Price", "Kms_Driven")]

# Display new data frame
new_vehicles



# Set seed for reproducibility
set.seed(1)

# Shuffle rows of data frame
shuffled_Vehicle <- vehicles[sample(nrow(vehicles)), ]

# Display shuffled data frame
shuffled_Vehicle










# Create scatter plot
plot(vehicles$Present_Price, vehicles$Selling_Price,
     col = ifelse(vehicles$Transmission == 0, "red", "blue"),
     pch = 2,
     xlab = "Present Price",
     ylab = "Selling Price",
     main = "Selling Price vs Present Price")

# Add legend
legend("topleft", legend = c("Manual", "Automatic"), col = c("red", "blue"), pch = 2)




# Create box plot
boxplot(Selling_Price ~ Transmission + Fuel_Type, data = vehicles,
        xlab = "Transmission and Fuel Type",
        ylab = "Selling Price",
        main = "Selling Price vs Transmission and Fuel Type")





# Perform k-means clustering with k = 4
clusters <- kmeans(vehicles[, c("Selling_Price", "Kms_Driven")], centers = 4)

# Create scatter plot
plot(vehicles$Kms_Driven, vehicles$Selling_Price,
     col = clusters$cluster,
     xlab = "Kms Driven",
     ylab = "Selling Price",
     main = "Selling Price vs Kms Driven")

# Add legend
legend("topleft", legend = paste("Cluster", 1:4), col = 1:4, pch = 1)




# Compute distance matrix
d <- dist(vehicles[, c("Selling_Price", "Present_Price")])

# Perform hierarchical clustering
hc <- hclust(d)

# Cut tree into 3 clusters
clusters <- cutree(hc, k = 3)

# Create scatter plot
plot(vehicles$Present_Price, vehicles$Selling_Price,
     col = clusters,
     xlab = "Present Price",
     ylab = "Selling Price",
     main = "Selling Price vs Present Price")

# Add legend
legend("topleft", legend = paste("Cluster", 1:3), col = 1:3, pch = 1)




#the age column was already added to the vehicles dataset refer the output of the question 13
# Create bar plots for specified fields
par(mfrow=c(3,2))
barplot(table(vehicles$Age), main="Age", xlab="Age", ylab="Frequency", col="blue")
barplot(table(vehicles$Year), main="Year", xlab="Year", ylab="Frequency", col="red")
barplot(table(vehicles$Transmission), main="Transmission", xlab="Transmission", ylab="Frequency", col="green")
barplot(table(vehicles$Seller_Type), main="Seller Type", xlab="Seller Type", ylab="Frequency", col="orange")
barplot(table(vehicles$Fuel_Type), main="Fuel Type", xlab="Fuel Type", ylab="Frequency", col="purple")
barplot(table(vehicles$Owner), main="Owner", xlab="Owner", ylab="Frequency", col="brown")





# Convert categorical variables to numeric
vehicles$Fuel_Type <- as.numeric(vehicles$Fuel_Type)
vehicles$Seller_Type <- as.numeric(vehicles$Seller_Type)
vehicles$Transmission <- as.numeric(vehicles$Transmission)

# Create correlation matrix
corr_matrix <- cor(vehicles)

# Plot correlation matrix
library(corrplot)
corrplot(corr_matrix, method="circle")


#to use corrplot i have to install corrplot

install.packages("corrplot")

# Identify non-numeric columns
non_numeric_cols <- sapply(vehicles, class) != "numeric"

# Remove non-numeric columns
vehicles_numeric <- vehicles[, !non_numeric_cols]

# Create correlation matrix
corr_matrix <- cor(vehicles_numeric)

# Plot correlation matrix
library(corrplot)
corrplot(corr_matrix, method="circle")


head(vehicles)



  
# Create scatter plot of Selling_Price vs Kms_Driven
plot(vehicles$Kms_Driven, vehicles$Selling_Price, xlab="Kms Driven", ylab="Selling Price", main="Selling Price vs Kms Driven")

# Perform DBSCAN clustering
library(dbscan)
data <- cbind(vehicles$Kms_Driven, vehicles$Selling_Price)
clusters <- dbscan(data, eps=5000, minPts=5)$cluster

# Color-code points based on cluster
colors <- c("red", "green", "blue")
plot(vehicles$Kms_Driven, vehicles$Selling_Price, col=colors[clusters], xlab="Kms Driven", ylab="Selling Price", main="Selling Price vs Kms Driven")

# Add legend to plot
legend("topleft", legend=c("Cluster 1", "Cluster 2", "Cluster 3"), fill=colors)


