#Download nycflights13 package and plot a scatter chart 
#with x axis as arrival delay and y axis as departure delay.

# installed.packages() #U can Install Any Package using this
install.packages("nycflights13") #ctrl+ enter <- run
library(nycflights13)
nycflights13::flights
data("flights")

#-------------------------------------------------Knowing Your Dataset----------------------------------------------#
# View the first few rows of the dataset
head(flights)

# Check the data types of each column
str(flights)

# Summary of the dataset
summary(flights)

#---------describing the Dataset
# install.packages("psych")
library(psych)
describe(flights) 
??describe()

# Count missing and NA values in each column
install.packages("dplyr")
library(dplyr)

??summarise_all
missing_values <- flights %>% 
  summarise_all(~sum(is.na(.)))

missing_values

#Q1<- 8255 missing values in dep time and in dep_dekay why?? are these value for same flights?

#-----------------------------------Missing Value Treatment-------------------------------------#
# Calculate the mean of dep_delay and arr_delay columns
mean_dep_delay <- mean(flights$dep_delay, na.rm = TRUE)

mean_arr_delay <- mean(flights$arr_delay, na.rm = TRUE)
mean_arr_delay

# Replace missing values with the mean for dep_delay and arr_delay
flights <- flights %>%
  mutate(dep_delay = ifelse(is.na(dep_delay), mean_dep_delay, dep_delay),
         arr_delay = ifelse(is.na(arr_delay), mean_arr_delay, arr_delay))

??mutate
check_missing <- flights %>%
  summarise_all(~sum(is.na(.)))

missing_values
check_missing

#----------------------------------------Duplicate Values Treatment----------------------------------------
# Check for duplicate rows in the flights dataset
duplicates <- flights[duplicated(flights), ]
duplicates

??duplicated
# Display the duplicate rows, if any
if (nrow(duplicates) > 0) {
  cat("Duplicate rows found:\n")
  head(duplicates)
  cat("Total duplicate rows:", nrow(duplicates))
} else {
  cat("No duplicate rows found in the dataset.")
}

#Q<- imput arr_time and dep_time with median?

#--------------------How to retrive the shape of the dataset------------------------
# Find the shape of the flights dataset
dataset_shape <- dim(flights)
dataset_shape

# Print the shape
cat("Number of rows:", dataset_shape[1], "\n")
cat("Number of columns:", dataset_shape[2], "\n")


#--------------------------------------Created a shape function to quickly check the rows and columns------------------
shape<-function(dataset) {
  shape <- dim(dataset)
  cat("Number of rows:", shape[1], "\n")
  cat("Number of columns:", shape[2], "\n")
}

shape(flights)

#-------------------------------------comprehensive function to perform EDA-----------------------------------------

analyze_dataset_interactive <- function(dataset) {
  # 1) Print the shape of the dataset
  get_dataset_shape <- function(data) {
    shape <- dim(data)
    cat("Number of rows:", shape[1], "\n")
    cat("Number of columns:", shape[2], "\n")
  }
  print("Shape of the Dataset")
  get_dataset_shape(dataset)
  
  # 3) Print the str of the dataset
  print_structure <- function(data) {
    str(data)
  }
  print("")
  print("Structure of the Dataset")
  print_structure(dataset)
  
  # 5) Print the number of missing values in each column in a transpose view
  print_missing_values <- function(data) {
    missing_values <- data %>%
      summarise_all(~sum(is.na(.)))
  }
  print("")
  print("Missing Values in the Dataset")
  print_missing_values(dataset)
  
  # 7) Print the shape of the dataset after replacing missing values
  print("Chechking the shape of the Dataset")
  get_dataset_shape(dataset)
  
  # 8) Check for duplicates in the dataset
  
  duplicates <- dataset[duplicated(dataset), ]
  if (nrow(duplicates) > 0) {
    cat("Duplicate rows found:\n")
    head(duplicates)
    cat("Total duplicate rows:", nrow(duplicates), "\n")
  } else {
    cat("No duplicate rows found in the dataset.\n")
  }
  
  # 9) Removal of duplicate values
  dataset <- dataset[!duplicated(dataset), ]
  
  # 10) Print the shape of the dataset after removing duplicates
  cat("Shape of the dataset after removing duplicates:\n")
  get_dataset_shape(dataset)
  
  return(dataset)
}

# Call the function with the 'flights' dataset and user input
flights <- analyze_dataset_interactive(flights)


#---------------------------------------------Visualisation Begins---------------------------------
# The First Plot
?plot()
a <- c(10,13,14,10,12,17)
b <- c(15,17,19,13,13,19)
plot(a,b) #The first Plot


attach(flights)
#--------------------------------------------------------------------------------------------
#                                   SCATTER PLOT
#--------------------------------------------------------------------------------------------

attach(nycflights13::flights) #Attching files is important to access every column by not reffering to the Database Again and again
plot(arr_delay,dep_delay)

#aAdding header to the plot
plot(arr_delay,dep_delay, main="Arrival vs Departure Delay")

#Adding x and y axis labels
plot(arr_delay,dep_delay, main="Arrival vs Departure Delay", 
     xlab="Arrival Delay", ylab="Departure Delay")

#Plotting a line on top of the plot
plot(arr_delay,dep_delay, main="Arrival vs Departure Delay", 
     xlab="Arrival Delay", ylab="Departure Delay")
abline(0,1)
abline(0,0)

#Inference ----------
# 1. It is a linear relationship Betwwen Arrivale delay and Dept. Delay
# 2. As arrival Time is increasing the Departing Time is Also Increasing
# 3. when Arrival time is - 50 to 150 minutes delayed , then their is very good chance
# that the flight is not been delayed in departure.

# Customization in the scatter Chart

#Changing Plot Character (pch=) 1. Circle 2. Triangle 3. 
#Plus 4. Cross 5. Diamond, 6. Reverese triangle 7. Box and crossed
plot(arr_delay,dep_delay, main="Arrival vs Departure Delay", 
     xlab="Arrival Delay", ylab="Departure Delay", pch=7)

#Giving shapes as per choice
plot(arr_delay,dep_delay, main="Arrival vs Departure Delay", 
     xlab="Arrival Delay", ylab="Departure Delay", pch="r")

#Filtering data = IF want a graph of specific airline
#After using plot command if you want a graph of another airline as well then you
# can use points () funtion to show.
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"])
points(arr_delay[carrier=="AA"],dep_delay[carrier=="AA"], pch=3)

#colors 
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], col="Red")
points(arr_delay[carrier=="AA"],dep_delay[carrier=="AA"], col="green")

#This is somewhat little messy we cannot clearly see whats happeing behind these 
#green dots 
# partition the plot frame in 2 parts using Partition - Multi Frame Rows
#par(mfrow= c(,))

par(mfrow=c(1,2))# running this command will divide
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], main="UA") 
plot(arr_delay[carrier=="AA"],dep_delay[carrier=="AA"], main="AA")

#Equal scale for x and y axis.for this use xlim= c(), ylim- c()
par(mfrow=c(1,2))
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], main="UA", 
     xlim=c(0,1000), ylim=c(0,1000))
plot(arr_delay[carrier=="AA"],dep_delay[carrier=="AA"], main="AA",
     xlim=c(0,1000), ylim=c(0,1000))
#this is giving us a clear picture of whats happening inside UA airline delayed 
# maximum delayed in the UA airlines is 500 minutes in arrival which is very 
# loww as compare to American airline as some flights of AA delayed upto more
# than thousand minutes

#Customizing
#Restoring the frame for a single plot
par(mfrow=c(1,1))
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], main="UA")

#Adding text and lines
# Text we can plot to types of text one is the margin text and the other is the
# normal text which is plotted inside the graphn and margin text plotted 
# outside the graph
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], 
     main="Arrival vs Departure Time")


text(x=100, y=400, label="United Airways") #Plotted in the middle , the adj= 0.5 for middle 
text(x=400, y=0, adj=1, label="Ecorishi") # adj use to move text left and right
#ADj = 1 means right and adj= 0 means left

mtext(text="UA = United Airways", side=3) #side = 3(top), 2(y axis), 1(x axis)
mtext(text="in minutes", side=2, adj=1)
mtext(text="in minutes", side=1, adj=1)
abline(h=250)
abline(v=250)# Creting horizontal and vertical line

abline(h=0)# At a scale of 0,0
abline(v=0)

#Changing the size and color of points using ‘cex’ and ‘col’ argument
# cex is to sixe and col is to color

## Normal size point
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], 
     main="Arrival vs Departure Time")

## Half size point
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], 
     main="Arrival vs Departure Time", cex=0.5) #cex is smaller mean smaller size

# Color to red
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], 
     main="Arrival vs Departure Time", cex=0.5, col="red")

# add green points on top of the UA plot for AA
points(arr_delay[carrier=="AA"],dep_delay[carrier=="AA"], 
       cex=0.5, col="green" )
#Change the color and font of labels
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], 
     main="Arrival vs Departure Time", cex=0.5, col="red", 
     col.main="red", font.main=3) #col.maim font. main to edit main title

#Change the font and color of text as well
plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], 
     main="Arrival vs Departure Time", cex=0.5, col="red", 
     col.main="red", font.main=3, col.lab="green", font.lab=4)#color to x and y axis
text(100,400, "United Airways", col="green", font=2)

text(100,300, "United Airways", col="red", font=2, cex=2)

#Pairs of Scatter plots <- this is interesting
pairs(~ arr_delay + dep_delay)

pairs(~ distance + dep_delay + arr_delay + air_time) #getting relation between diff column

#----------------------------------------------------------------------------------------------
#                                         Histograms
#----------------------------------------------------------------------------------------------
# typeof(flights$distance)
# flights$distance <- as.numeric(flights$distance)
# attach(flights)
hist(flights$distance) # flights covering hor much distance
# flights are covering 1000 km on average

hist(distance, breaks = 10) # to have a broader view and making bars more wider

# Filter by airline
hist(distance[carrier == "UA"]) # filtering histogram for UA

hist(distance[carrier == "AA"]) # filtering histogram for AA

#--------------------------------------------------------------------------------------------------
#                           Box And Whisker Plot is same as Box plot
#---------------------------------------------------------------------------------------------------

boxplot(distance) # box plot of distance

##Filter by airline
boxplot(distance[carrier == "UA"])

#Two box plots side by side
par(mfrow=c(1,2))
boxplot(distance[carrier == "UA"], main="UA")
boxplot(distance[carrier == "AA"], main="AA") 

#Two box plots side by side with equal scale
par(mfrow=c(1,2))
boxplot(distance[carrier == "UA"], main="UA", ylim = c(0,5000))
boxplot(distance[carrier == "AA"], main="AA", ylim = c(0,5000))

#Two histograms and two box plots = 4 plots on a single plot
par(mfrow=c(2,2))
boxplot(distance[carrier == "UA"], main="UA", ylim = c(0,5000))
boxplot(distance[carrier == "AA"], main="AA", ylim = c(0,5000)) #codes for 2 box plot

hist(distance[carrier == "UA"], main="UA", 
     breaks = c(0, 1000, 2000, 3000, 4000, 5000),
     xlim = c(0,5000), ylim = c(0,30000))
hist(distance[carrier == "AA"], main="AA", 
     breaks = c(0, 1000, 2000, 3000, 4000, 5000),     
     xlim = c(0,5000), ylim = c(0,30000)) # codes for 2 histograms

#Box and Whisker plot with scatter plots.
par(mfrow=c(2,2))
boxplot(distance[carrier == "UA"], main="UA", ylim = c(0,5000))
boxplot(distance[carrier == "AA"], main="AA", ylim = c(0,5000)) #codes for 2 box plot

plot(arr_delay[carrier=="UA"],dep_delay[carrier=="UA"], main="UA", 
     xlim=c(0,1000), ylim=c(0,1000))
plot(arr_delay[carrier=="AA"],dep_delay[carrier=="AA"], main="AA",
     xlim=c(0,1000), ylim=c(0,1000)) # codes for scatter plot

#Box plots of distance by carrier
par(mfrow=c(1,1))
boxplot(distance ~ carrier) #that means we will be having all the boxplot plot respective to every flight
boxplot(distance ~ carrier)
#Insights what will be getting
#1 Ha is a flight that is flying to a much longer distance
#2 UA has a lot of variation so they fly low distances to high distance
#3 F9 and AS  have very limited no. of flights so they might be travelling to a specific place at 2500 km.
