
# Installing the important Packages
install.packages('datasets') # mt cars and air quality dataset
install.packages('skimr') #for Summary of the variable
install.packages("moments") #Skewness and Kurtosis
install.packages('stats') #For Correlation
install.packages('missMethods') #Impute Mean
install.packages('ggbiplot') # for biplot
install.packages("palmerpenguins") #Penguin dataset
install.packages('rattle') #Wine Dataset



# Loading all the important libraries installed above
library(datasets)
library(skimr)
library(moments)
library(stats)
library(missMethods)
library(ggbiplot)
library(palmerpenguins)
library(rattle)




#Dataset 1: airquality ---------------------------------------------------------

# Data Overview ----

?airquality #information on the datasset
names(airquality)
head(airquality)
View(airquality)



View(skim(airquality))

#Q2 Summary Statistics----
#Summary of the temperature variable.

summary(airquality$Temp) # Details of the variables.
View(skim(airquality$Temp)) # Gives additional information like standard deviation, missing values and distribution

#Q3 Distribution Visualization----
# Histogram of temperature variable
temp_hist <- hist(airquality$Temp, 
                  breaks = 'Sturges', 
                  col = 'lightgreen',
                  border = 'black', 
                  main = 'Histogram of Temperature',
                  xlab = 'Temperature')

#skewness
skewness(airquality$Temp)

#box plot of the temperature variable
temp_boxplot <- boxplot(airquality$Temp, 
                        horizontal = TRUE,
                        main = 'Box plot of Temperature',
                        col = 'lightgreen',
                        border = 'black',
                        xaxt = 'n',
                        xlab = 'Temperature')


#Annotating the boxplot with five important metrics 
labx <- fivenum(airquality$Temp) # function provides the min, 25th, 50th, 75th percentiles and the max values
axis(1, at = labx, labels = round(labx, 2))



#Q4 ---------
#Considering Month variable for the analysis
month_counts <- table(airquality$Month)

#Boxplot of the month variable
month_barplot <- barplot(table(airquality$Month),
                         col = 'lightgreen',
                         border = 'black',
                         main = 'Observations',
                         xlab = 'Month')
text(
  x = barplot(table(airquality$Month), plot = FALSE),
  y = month_counts,                       
  labels = month_counts,                  
  pos = 1,                                
  cex = 0.8,                              
  col = "black"                            
)



#Q5 ----
#Considering Wind and Temperature variable for the analysis
#Correlation between wind and temperature variabhle
cor(airquality$Wind, airquality$Temp)

#Q6 ----
#scatter plot of the temperature and the wind variable
plot(airquality$Wind, airquality$Temp, col = 'blue',
     pch = 20,
     ylab = 'Temperature(F)',
     xlab = 'Wind (mph)',
     main = 'Scatterplot')

l <- lm(airquality$Temp ~ airquality$Wind) #Trend line for the scatter plot
abline(l, col = "red", lwd = 2)


#Q7 ----
#Multiple linear regression predicting temperature using the wind and solar radiation variables
mlr <- lm(airquality$Temp ~ airquality$Wind + airquality$Solar.R)
summary(mlr)

#Q8 ----

#Plots of the residuals of the based on the multiple linear regression. Shows qq plot and the homoscedasticity of the residuals

par(mfrow = c(2, 2))
plot(mlr)
par(mfrow = c(1, 1))



#Q9 ----

#The airquality dataset currently has missing values. Imputing mean to perform the PCA
airquality1 <- impute_mean(airquality)

skim(airquality1) # Summary after imputing the mean

#Considering only the continuous variables for the PCA
airquality_new <- airquality1[,1:4]
head(airquality_new)


#Performing PCA:
pca_result <- prcomp(airquality_new, center = TRUE, scale = TRUE)
summary(pca_result)


#Screeplots

#Calulating the proportion of variance to output the screeplot
variance <- (pca_result$sdev)^2
prop_variance <- variance/sum(variance)
plot(prop_variance, type = 'b', xlab = 'Principal Components', ylab = 'Variance Proportion', main = 'Screeplot')

# 3 Principal components can be considered covering 90% variance

#Q10 ----

# Biplot of the PCA result
ggbiplot(pca_result,
         obs.scale = 1,
         var.scale = 1,
         varname.size = 6,
         main = "Biplot",
         circle = TRUE)+
  ggtitle('Biplot', )+
  theme_minimal()
  






#Dataset 2: mtcars ---------------------------------------------------------

#Q1 ----

?mtcars #information on the dataset
names(mtcars)
head(mtcars)
View(mtcars)

#Summary of mt cars variables
View(skim(mtcars))

#Q2 ----
#Considering the qsec numeric variable for the analysis
#summary of the qsec variables
summary(mtcars$qsec)
View(skim(mtcars$qsec))

#Q3 ----
#histogram of the qsec variable
qsec_hist <- hist(mtcars$qsec, 
                  breaks = 'Sturges', 
                  col = 'lightgreen',
                  border = 'black', 
                  main = 'Histogram of 1/4 mile time (qsec)',
                  xlab = 'Time (secs)')

#boxplot of the qsec variables
qsec_boxplot <- boxplot(mtcars$qsec, 
                        horizontal = TRUE,
                        main = 'BoxPlot of 1/4 mile time (qsec)',
                        col = 'lightgreen',
                        border = 'black',
                        xaxt = 'n')

#Skewness of the qsec variable
skewness(mtcars$qsec)


labx <- fivenum(mtcars$qsec) # function provides the min, 25th, 50th, 75th percentiles and the max values
axis(1, at = labx, labels = round(labx, 2))
#As pet the boxplot, one outlier is present

#Q4 ---------
#Considering Gear variable for the analysis
#Barplot of the gear variable
gear_barplot <- barplot(table(mtcars$gear),
                         col = 'lightgreen',
                         border = 'black',
                         main = 'Barplot of Gear',
                        xlab = 'Gear')

text(
  x = barplot(table(mtcars$gear), plot = FALSE),
  y = table(mtcars$gear),                       
  labels = table(mtcars$gear),                  
  pos = 1,                                
  cex = 1,                              
  col = "black"                            
)

#Q5 ----
#Considering miles per gallon (mpg) and Horsepower (hp) for the analysis
#Correlation between mpg and hp
cor(mtcars$mpg, mtcars$hp)

#Q6 ----
#Scatter plot between hp and mpg
plot(mtcars$hp, mtcars$mpg,
     col = 'blue', pch = 20, 
     xlab = 'Horsepower', 
     ylab = 'Miles Per Gallon', 
     main = 'Scatterplot')

#Adding the trend line
l1 <- lm(mtcars$mpg ~ mtcars$hp)
abline(l1, col = "red", lwd = 2)



#Q7 ----
#Multiple linear regression predicting mpg from displacement and horsepower
mlr1 <- lm(mtcars$mpg ~ mtcars$disp + mtcars$hp)
summary(mlr1)



#Q8 ----
#Plots of the residuals of the based on the multiple linear regression. Shows qq plot and the homoscedasticity of the residuals

par(mfrow = c(2, 2))
plot(mlr1)
par(mfrow = c(1, 1))

#Q9 ----
#Performing PCA:


#Considering only the continuous variables for the PCA
mtcars_pca <- mtcars[,c(1,3:7)]
mtcars_pca

pca_result1<- prcomp(mtcars_pca, center = TRUE, scale = TRUE)
summary(pca_result1)

#Calculating the proportion of the variance
variance1 <- (pca_result1$sdev)^2
prop_variance1 <- variance1/sum(variance1)
plot(prop_variance1, type = 'b', ylab = 'Variance Proportion', xlab = 'Pricipal Components', main = 'Screeplot')

# 3 Principal components can be considered covering 90% variance


#Q10 ----

#Plotting the biplot based on the results of the PCA
ggbiplot(pca_result1, obs.scale = 0.5, var.scale = 1, varname.size = 5,
         circle = TRUE)+
  theme_minimal()



#Dataset 3: penguins dataset ---------------------------------------------------------

help(package = "palmerpenguins")  #Information about the package

#using the penguins dataset for the analysis.

#Q1 ----

?penguins #information on the dataset
names(penguins)
head(penguins)
View(head(penguins, n = 15))

#Summary of the varaibles in the penguins dataset
View(skim(penguins))

#Null values are present in the dataset. Finding the rows with null values
Nullvalues <- penguins[!complete.cases(penguins), ] 
print(Nullvalues) 
# In two cases, all the attribute values are missing, remaining 9 has only gender missing.
# Removing the two cases where all the attributes are missing. Other data are kept as Gender is not being used for the analysis.


#Only using the non null instances of the penguin dataset
penguins_new<- penguins[complete.cases(penguins$flipper_length_mm), ]
print(penguins_new)
View(head(penguins_new, n = 15))


#Q2 ----
#Considering the body_mass_g numeric variable for the analysis
#Sumamry of the body mass variable
summary(penguins_new$body_mass_g)
View(skim(penguins_new$body_mass_g))


#Q3 ----
#Histogram of body mass
body_mass_hist <- hist(penguins_new$body_mass_g, 
                  breaks = 'Sturges', 
                  col = 'lightgreen',
                  border = 'black', 
                  main = 'Histogram of Body Mass',
                  xlab = 'Body Mass')

#Boxplot of the body nass variable
body_mass_boxplot <- boxplot(penguins_new$body_mass_g, 
                        horizontal = TRUE,
                        main = 'Boxplot of Body Mass',
                        col = 'lightgreen',
                        border = 'black',
                        xaxt = 'n')


labx <- fivenum(penguins_new$body_mass_g) # function provides the min, 25th, 50th, 75th percentiles and the max values
axis(1, at = labx, labels = round(labx, 2))

#Skewness of the body mass variable
skewness(penguins_new$body_mass_g)

#Q4 ---------
#Considering Species variable for the analysis

#Barplot of the species variable
species_barplot <- barplot(table(penguins_new$species),
                         col = 'lightgreen',
                         border = 'black',
                         main = 'Barplot of Penguin Species'
                         )

text(
  x = barplot(table(penguins_new$species), plot = FALSE),
  y = table(penguins_new$species),                       
  labels = table(penguins_new$species),                  
  pos = 1,                                
  cex = 0.8,                              
  col = "black"                            
)

#Q5 ----
#Considering Body Mass and Flipper Length variable for the analysis
#Correlation between the two variables
cor(penguins_new$flipper_length_mm, penguins_new$body_mass_g) #Strong correlation of 0.87


#Q6 ----

#Scatterplot between the flipper length and the body mass varaibles
plot(penguins_new$flipper_length_mm, 
     penguins_new$body_mass_g, 
     col = 'blue', 
     pch = 20, 
     main = 'Scatterplot', 
     xlab = 'Flipper length (mm)', 
     ylab = 'Body mass (g)')

# Trend line for the scatter plot
l <- lm(penguins_new$body_mass_g ~ penguins_new$flipper_length_mm)
abline(l, col = "red", lwd = 2,)


#Q7 ----
#Multiple linear regression predicting body mass from flipper length, bill length and the bill depth
mlr2 <- lm(penguins_new$body_mass_g ~ penguins_new$flipper_length_mm + penguins_new$bill_length_mm + penguins_new$bill_depth_mm)
summary(mlr2)

#Q8 ----
#Plotting the results of the multiple linear regression
par(mfrow = c(2, 2))
plot(mlr2)
par(mfrow = c(1, 1))

#Q9 ----

#Considering only the continuous variables for the pca
penguins_new_pca <- penguins_new[,c(3:6)]
head(penguins_new_pca)

#Performing PCA:
pca_result2 <- prcomp(penguins_new_pca, center = TRUE, scale = TRUE)
summary(pca_result2)

#Calculating the proportion of the variance for the scree plot
variance2 <- (pca_result2$sdev)^2
prop_variance2 <- variance2/sum(variance2)
plot(prop_variance2, type = 'b', main ='Scree plot', xlab = 'Principal Components', ylab = 'Variance Proportion' )


sum(prop_variance2[1:2]) #Total variance captured by the first three principal components
# 2 Principal components can be considered covering 88% variance

#Q10 ----

#Biplot of the PCA result
ggbiplot(pca_result2,
         obs.scale = 0.6,
         var.scale = 1,
         varname.size = 4,
         circle = TRUE)+
  theme_minimal()




# Dataset 4  Wine dataset from the Rattle package-------------------

#Information on the package
help(package = 'rattle')

#Q1 ----

?wine #information on the dataset
names(wine)
head(wine)
View(head(wine, n = 15))

#Summary of the wine dataset
View(skim(wine)) # No missing values in the dataset. One categorical attribute is present


#Q2 ----
#Considering the color numeric variable for the analysis. The variable depicts the intensity of color of the wine
summary(wine$Color)
View(skim(wine$Color))

#Q3 ----
#Histogram of the color variable
wine_color_hist <- hist(wine$Color, 
                       breaks = 'Sturges', 
                       col = 'lightgreen',
                       border = 'black', 
                       main = 'Histogram of Wine Color',
                       xlab = 'Color Intensity')

#Boxplot of the color variables
body_mass_boxplot <- boxplot(wine$Color, 
                             horizontal = TRUE,
                             main = 'Boxplot of Wine Color Intensity',
                             col = 'lightgreen',
                             border = 'black',
                             xaxt = 'n',
                             xlab= 'Color Intensity')


labx <- fivenum(wine$Color) # function provides the min, 25th, 50th, 75th percentiles and the max values
axis(1, at = labx, labels = round(labx, 2))

#Skewness of the color variable
skewness(wine$Color)

#Q4 ---------
#Considering Species variable for the analysis

#Barplot of the wine type variable
species_barplot <- barplot(table(wine$Type),
                           col = 'lightgreen',
                           border = 'black',
                           main = 'Barplot of Wine Type',
                           xlab = 'Type'
)

text(
  x = barplot(table(wine$Type), plot = FALSE),
  y = table(wine$Type),                       
  labels = table(wine$Type),                  
  pos = 1,                                
  cex = 1,                              
  col = "black"                            
)

#Q5 ----
#Considering Colour and Alcohol variable for the analysis
#Correlation between the color and the alcohol level of the wine
cor(wine$Color, wine$Alcohol)



#Q6 ----
#Scatter plot between the color and the alcohol variable
plot(wine$Color, 
     wine$Alcohol, 
     col = 'blue', 
     pch = 20, 
     xlab = 'Color', 
     ylab = 'Alcohol', 
     main = 'Scatterplot')

#Fitting the trend line to the dataset
l <- lm(wine$Alcohol ~ wine$Color)
abline(l, col = "red", lwd = 2)


#Q7 ----
#Multiple Linear regression predicting the alcohol level from the color, alcalinity and the dilution level of the wine
mlr4 <- lm(wine$Alcohol ~ wine$Color + wine$Alcalinity + wine$Dilution)
summary(mlr4)

#Q8 ----
#Plotting the results of the multiple Linear regression
par(mfrow = c(2, 2))
plot(mlr4)
par(mfrow = c(1, 1))

#Q9 ----
#Considering only the continuous variables for the PCA
wine_pca <- wine[2:ncol(wine)]


#Performing PCA:

pca_result3 <- prcomp(wine_pca, center = TRUE, scale = TRUE)
summary(pca_result3)


#Calculating the proportion of the variance for the screeplot
variance3 <- (pca_result3$sdev)^2
prop_variance3 <- variance3/sum(variance3)

plot(prop_variance3, type = 'b', xlab = 'Principal Components', ylab = 'Variance Proportion', main = 'Screeplot' ) 
# 7 Principal components can be considered covering 90% variance

sum(prop_variance3[1:7])#Proportion of variance captured from the first 7 PCs



#Q10 ----
#Visualization of the biplot based on the result of the PCA 
ggbiplot(pca_result3, obs.scale = 0.6, var.scale = 1, circle = TRUE,
         varname.size = 4)+
  theme_minimal()

#-------------------- END ------------------------------ 

