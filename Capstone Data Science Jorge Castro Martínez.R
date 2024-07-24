# First part: Exploratory data analysis -EDA

## The complete database is considered for all 193 countries

#Loading packages
install.packages("readxl")
installed.packages("faraway")
install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("corrplot")
install.packages("psych")
install.packages("lmtest")



#Loading libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(haven)
library(faraway)
library(readr)
library(dplyr)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(psych)
library(janitor)
library(car)
library (lmtest)
library(tseries)

#Read the csv file
setwd("C:/Users/jorge.castro/Desktop/PROYECTOS/Life EXp")

datas <- read.table("Life Expectancy Data.csv", header= TRUE,sep="," )
datas

#Analysis of dataframe
str(datas)
head(datas,n=5)
tail(datas,5)
ncol(datas)
nrow(datas)
names(datas)

#Variable transformation
dato<-datas


#Categorical variables
datac <-dato %>% mutate(across(contains(c("Country","Status")),~as.factor(.x)))

#Numerics variables
datan <-dato %>% mutate(across(contains(c("Year","Life.expectancy","Adult.Mortality","infant.deaths","Alcohol","percentage.expenditure","Diphtheria","HIV.AIDS","Measles","GDP","Population","thinness..1.19.years","thinness.5.9.years","Income.composition.of.resources","Schooling","Total.expenditure","Polio","BMI","Hepatitis.B")),~as.numeric(.x)))

data <-mutate(datac,datan)
data

#Data cleaning and transformation

View(data)

anyNA(data)

sum(is.na(data))

#Transform to numeric
Life.expectancy <-as.numeric(unlist(data[,-1]))
Adult.Mortality <-as.numeric(unlist(data[,-2]))
infant.deaths <-as.numeric(unlist(data[,-3]))
Alcohol <-as.numeric(unlist(data[,-4]))
percentage.expenditure <-as.numeric(unlist(data[,-5]))
Hepatitis.B <-as.numeric(unlist(data[,-6]))
Measles <-as.numeric(unlist(data[,-7]))
BMI <-as.numeric(unlist(data[,-8]))
under.five.deaths <-as.numeric(unlist(data[,-9]))
Polio <-as.numeric(unlist(data[,-10]))
Total.expenditure <-as.numeric(unlist(data[,-11]))
Diphtheria <-as.numeric(unlist(data[,-12]))
HIV.AIDS <-as.numeric(unlist(data[,-13]))
GDP <-as.numeric(unlist(data[,-14]))
Population <-as.numeric(unlist(data[,-15]))
thinness..1.19.years <-as.numeric(unlist(data[,-16]))
thinness.5.9.years <-as.numeric(unlist(data[,-17]))
Income.composition.of.resources <-as.numeric(unlist(data[,-18]))
Schooling <-as.numeric(unlist(data[,-19]))
Status<-as.factor(unlist(data[,-20]))


# Class type identification
class(data$Life.expectancy)

class(data$Hepatitis.B)

View(data)


#Mean value imputation

data$Life.expectancy[is.na(data$Life.expectancy)] <-mean(data$Life.expectancy, na.rm=T) ##cambiar por la media de expectactiva de vida
data$Adult.Mortality[is.na(data$Adult.Mortality)] <-mean(data$Adult.Mortality, na.rm=T) ##cambiar por la media de Adult.Mortality
data$infant.deaths[is.na(data$infant.deaths)] <-mean(data$infant.deaths, na.rm=T) ##cambiar por la media de infant.deaths
data$Alcohol[is.na(data$Alcohol)] <-mean(data$Alcohol, na.rm=T) ##cambiar por la media de Alcohol
data$percentage.expenditure[is.na(data$percentage.expenditure)] <-mean(data$percentage.expenditure, na.rm=T) ##cambiar por la media de percentage.expenditure
data$Hepatitis.B[is.na(data$Hepatitis.B)] <-mean(data$Hepatitis.B, na.rm=T) ##cambiar por la media de Hepatitis.B
data$Measles[is.na(data$Measles)] <-mean(data$Measles, na.rm=T) ##cambiar por la media de Measles
data$BMI[is.na(data$BMI)] <-mean(data$BMI , na.rm=T) ##cambiar por la media de BMI 
data$under.five.deaths[is.na(data$under.five.deaths)] <-mean(data$under.five.deaths, na.rm=T) ##cambiar por la media de under.five.deaths 
data$Polio[is.na(data$Polio)] <-mean(data$Polio, na.rm=T) ##cambiar por la media de Polio
data$Total.expenditure[is.na(data$Total.expenditure)] <-mean(data$Total.expenditure, na.rm=T) ##cambiar por la media de Total.expenditure
data$Diphtheria[is.na(data$Diphtheria)] <-mean(data$Diphtheria, na.rm=T) ##cambiar por la media de Diphtheria
data$HIV.AIDS[is.na(data$HIV.AIDS)] <-mean(data$HIV.AIDS, na.rm=T) ##cambiar por la media de HIV.AIDS
data$GDP[is.na(data$GDP)] <-mean(data$GDP, na.rm=T) ##cambiar por la media de GDP
data$Population[is.na(data$Population)] <-mean(data$Population, na.rm=T) ##cambiar por la media de Population
data$thinness..1.19.years[is.na(data$thinness..1.19.years)] <-mean(data$thinness..1.19.years, na.rm=T) ##cambiar por la media de thinness..1.19.years
data$thinness.5.9.years[is.na(data$thinness.5.9.years)] <-mean(data$thinness.5.9.years, na.rm=T) ##cambiar por thinness.5.9.years
data$Income.composition.of.resources[is.na(data$Income.composition.of.resources)] <-mean(data$Income.composition.of.resources, na.rm=T) ##cambiar por Income.composition.of.resources
data$Schooling[is.na(data$Schooling)] <-mean(data$Schooling, na.rm=T) ##cambiar por Schooling

#Review of null and na. Also, it is possible to obtain values missing with data <- na.omit(data) or drop_na()

sum(is.na(data$Life.expectancy))
sum(is.null(data$Life.expectancy))#no hay

sum(is.na(data))


#DESCRIPTION OF VARIABLES 
#The first part constitutes the univariate and bivariate exploitative analysis to know the independent variables and apply learned codes.
##(assessing the concentration of data, identifying anomalies, outliers, relationships and others)

datas %>% glimpse

##Univariate analysis


#Status-categorical variable


Stated <-unique(data$Country)
Stated



Statuss <-factor(sample(c("Developed", "Developing"),
                        size=length(data$Status),replace = TRUE))
Statuss
Tipodepais<-data$Status
table(Tipodepais)
sum(Tipodepais=="Developed")

Statust<-prop.table(table(Tipodepais))
Statust


ggplot(data)+
  aes(x=Status,fill="Stated")+
  geom_bar()+
  ggtitle("Status")


boxplot(Life.expectancy ~ Status, data = data)


#Country

length(data$Country[data$Country == "Costa Rica"])#number of times CR appears


CostaRicacountryy<-  data %>%
  filter(Country=="Costa Rica",
         Year==2014)#information about a specific variable
CostaRicacountryy

View(CostaRicacountryy)
summary(CostaRicacountryy)

data  %>%
  filter(Country == "Costa Rica") %>%
  ggplot(aes(Year, Life.expectancy,col = Life.expectancy)) +
  geom_point(size = 2)+
  theme(legend.position = "none")


#Comparison CR vs 2 countries
data %>%
  filter(Year == 2014 & Country %in% c("Costa Rica", "Canada", "Chile")) %>%
  select(Life.expectancy, Country, Year)


#Life Expectancy

summary(data$Life.expectancy)

Life_Adult <-data %>% select(Adult.Mortality, Country) %>% filter(Year==2015 & Country=="Costa Rica")
Life_Adult

Life_Adult_world <-data %>% select(Adult.Mortality, Country) %>% filter(Year==2015)
Life_Adult_world


data %>%
  group_by(data$Country) %>%
  summarize(min_size = min(Life.expectancy, na.rm = TRUE))

#Life.expectancy

ggplot(data)+
  aes(x=, y=Life.expectancy)+
  geom_boxplot()+
  labs(title = "Life Expectancy", y = "Cantidad") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Distribution by age
data %>% ggplot((aes(x=Life.expectancy)))+
  geom_density(aes(x=Life.expectancy,binwidth = 1, color = "black"))+
  ggtitle("Life.expectancy")


boxplot(data$Life.expectancy)
hist(data$Life.expectancy, main="Histograma expectatitva de vida",ylab="Frecuencia")


#Years of life expectancy

mean(data$Life.expectancy)
min(data$Life.expectancy)
max(data$Life.expectancy)


data %>% select(Life.expectancy, Country) %>% filter(Life.expectancy <=50)
match(50,data$Life.expectancy)
data %>% select(Life.expectancy, Country) %>% filter(Life.expectancy >=80)
data %>% select(Life.expectancy, Country, Year) %>% filter(Country=="Costa Rica")

ggplot(data)+
  aes(x=Status,y=Life.expectancy,fill="") +
  geom_point()+
  ggtitle("Status")


#GDP

summary(data$GDP)
sd((data$GDP))

data %>% ggplot((aes(x=GDP)))+
  geom_histogram(color="Blue",fill="green",bins=40)+
  labs(title = "GDP", y = "Cantidad")


#HIV.AIDS

data  %>% ggplot(aes(Year,HIV.AIDS)) + geom_jitter(width = 0.1, alpha = 0.2)

plot(data$HIV.AIDS)


summary(data$HIV.AIDS)
sd((data$HIV.AIDS))

data %>%
  group_by(data$Year) %>%
  summarize(max = max(HIV.AIDS, na.rm = TRUE))



#Alcohol
data  %>% ggplot(aes(Year,Alcohol)) + geom_jitter(width = 0.1, alpha = 0.2)

summary(data$Alcohol)
sd((data$Alcohol))



#Bivariate analysis

#Life.expectancy vs Adult.Mortality

LifevsAdulMort<-ggplot(data)+aes(x=Adult.Mortality, y=Life.expectancy, color=Total.expenditure,fill="Life.expectancy")+
  geom_point(alpha=0.25)+
  ggtitle("LifevsAdulMort")
LifevsAdulMort

#Life.expectancy vs Status

LifevStatuws<-ggplot(data)+ geom_point (aes(x=Status, y=Life.expectancy,fill="Life.expectancy")) +
  ggtitle("LifevStatuws")
LifevStatuws


#Life.expectancy vs Year

Lifeyear2<-ggplot(data)+aes(x=Year, y=Life.expectancy,fill="Life.expectancy")+
  geom_boxplot(alpha=0.25)+
  ggtitle("Lifeyear")
Lifeyear2




#thinness..1.19.years vs thinness.5.9.years

thinness1.19vs..5..9<-ggplot(data,color="red",size="cty")+
  aes(x=thinness..1.19.years, thinness.5.9.years)+
  geom_point()

thinness1.19vs..5..9

boxplot(Life.expectancy~thinness..1.19.years, data = data)

boxplot(Life.expectancy~ thinness.5.9.years, data = data)

# Second part: Application of machine learning

## Principal Components Analysis is applied as an unsupervised technique and multiple and logistic regression as a supervised analysis.
## OECD countries are prioritized for carrying out this second part.

## Create the List of OECD Member Countries

ocde_countries <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czech Republic","Denmark", 
                    "Estonia","Finland", "France", "Germany", "Greece", "Hungary", 
                    "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", 
                    "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
                    "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
                    "Switzerland", "Turkey", "United Kingdom", "United States","Costa Rica")

length(ocde_countries)

#Use the list to filter the data before performing PCA
# Filter data to include only OECD countries

data_ocde <- data %>% filter(Country %in% ocde_countries)
length(data_ocde)

# Show the first rows of the filtered dataframe
head(data_ocde)
str(data_ocde)
colSums(is.na(data_ocde))

#Add numerical variables by country (filtered)
# Average numerical variables by country for OECD countries
data_agg_ocde <- data_ocde %>%
  group_by(Country) %>%
  summarize(across(where(is.numeric), mean))

## Delete the Year and HIV/AIDS column

data_agg_ocde2 <-data_agg_ocde[,-c(2,15)]  
data_agg_ocde2


# Show the first rows of the added dataframe
data_agg_ocde

# Check for missing values
sum(is.na(data_agg_ocde))


# Ensure relevant variables are in the correct format
str(data_agg_ocde)

#View(data_agg_ocde)


## Save the countries variable 

paises <- data_agg_ocde2$Country
paises

# Convert 'Country' column to row names

rownames(data_agg_ocde2) <- data_agg_ocde2$Country


# Remove the 'Country' column as it is now in the row names
data_agg_ocde2 <- data_agg_ocde2[, -which(names(data_agg_ocde2) == "Country")]
data_agg_ocde2

#View(data_agg_ocde2)

## Convert Country column to rows
#the country becomes an index in order to reference the variables to a specific observation
#defining its correlative value, based on the average of the data

data_agg_ocde3 <- data.frame(data_agg_ocde2)
rownames(data_agg_ocde3) <- paises
data_agg_ocde3

## Principal Component Analysis (PCA)

# Scale the data (it is one of the steps of PCA)
#Ensures that each attribute has the same level of contribution, so that one variable does not dominate the others

# Normalize data
data_normalized <- scale(data_agg_ocde3)


# Perform PCA on the normalized data
data.pca <- princomp(data_normalized, cor = TRUE, scores = TRUE)

## Print the standard deviations of each component and its correlation

data.pca

## PCA Summary, function to display a summary of the data.pca object
#PCA is a dimensionality reduction and machine learning method used to simplify a large data set into a smaller set while still maintaining significant patterns and trends.
#describe a data set in terms of new uncorrelated variables

summary(data.pca)

## Scree plot, PCA tries to put maximum possible information in the first component, then maximum remaining information in the second and so on, until having something like shown in the scree plot below

screeplot(data.pca, type = "lines")
grid()

##Scree Plot 
#use the fviz_eig() function from the R package factoextra to display the eigenvalues of a #principal component analysis (PCA)
#

fviz_eig(data.pca, addlabels = TRUE)

#The resulting graph can help determine the number of principal components to retain for later analysis.

##Another method to select components through cumulative variances and set a cumulative variance value

# Explained variance
pc.var <- data.pca$sdev^2

# Proportion of variation explained
pc.pvar <- pc.var / sum(pc.var)

# cumulative ratio
#shows the cumulative 90% of data variability, better representing the data
#to decide by size or shape of the data
plot(cumsum(pc.pvar),xlab = "Componente Principal", ylab = "Proporción acumulada de varianza explicada",xlim = c(1,10), ylim = c(0.3,1), type = 'b')
grid()

abline(h = 0.9, col = "blue")

## Attributes or variables of data.pca

attributes(data.pca)

## Interpretation of loadings

data.pca$loadings

#The loadings represent the correlation between the original variables and the principal components. Loadings tell you how much each original variable contributes to a specific principal component. High loadings (positive or negative) indicate that the original variable has a strong relationship with the principal component. 
#If a variable has a high loading in the first principal component, it means that this variable is important to define that component.
#The higher the loading, the better the representation of the correlation between the original data and the CPA. The value depends on the researcher


#######################################################################ojo aqui se aborda para 2 componentes
## Visualization of the components of interest

data.pca$loadings[ , 1:2]

## Visualization of the loadings and scores of the first two components
#It is possible to visualize the similarities and dissimilarities of the samples, and also shows the impact of each attribute on each of the main components.

biplot(data.pca, col = c("gray","steelblue"), cex = c(0.6, 0.9))
grid()

#Higher life expectancy, GDP,Percentage expenditure in countries such as Canada, Germany, Switzerland, France
#Inversely predominate Measles infections and infant deaths in countries like Mexico, Colombia, Costa Rica
#Diseases such as Polio, Dipheria, Alcohol are related to the countries Belgium, Finland, Slovenia and Portugal.
#Adult mortality, thinness..1.19.years, thinness..519.years in aging countries such as Poland, Lithuania, Hungary, Latvia, Estonia



#The first component explains the greatest variability of the data and the others with little significance accumulate proportion
#includes information on the proportion of variance explained by each principal component, the # loadings (i.e., the weights assigned to each variable in each principal component), and other relevant # statistics.

## visualization of the first 2 components
data.pca$loadings[, 1:2]
sort(data.pca$loadings[, 1:2])

#In this case we can select the first two main components, since they explain 90% of the total variability. The main decreases occur in the first two, then the decrease is practically the same for the other components - equal size
# The first component is a linear combination with the original variables centered, distinguishes between the scenario of populations with good income composition, good life expectancy
#high GDP, percentage of spending, schooling, level of alcohol in relation to a scenario of deaths of children under five years of age, thinness.5.9.years and 1 to 19 years, child deaths and adult mortality and hepatitis B.
# The second component distinguishes between the scenario of populations with muscle mass, good life expectancy, infant deaths, GDP, percentage of expenditure, good income composition
#as opposed to adult mortality,alcohol,diphtheria,infant death,Polio


##############################################################################################

## PCA Scores
#a higher value means a greater relationship with the axis

head(data.pca$scores)


#The scores are the values of the new variables (principal components) for each observation in the original data set. In other words, they are the coordinates of the observations in the principal component space. The scores tell you how the original observations are projected in the principal component space. If the observations have similar scores on the first two components, that means that those observations are similar in terms of the variations captured by those components.


## PCA Scores of the first two components

head(data.pca$scores[ ,1:2])

## Viewing PCA Scores

biplot(data.pca, col = c("steelblue","White"), cex = c(0.8, 0.01))
grid()

## Visualization of PCA Scores with GGPLOT

scores <- data.frame(data.pca$scores)
ggplot(data = scores, aes(x = Comp.1, y = Comp.2, label = rownames(scores))) +
  geom_text(size = 4, col = "steelblue")


## Loading graph

# Graph of the variables
#Correlation circle in range from 0 to 1
fviz_pca_var(data.pca, col.var = "black")


#maintain similar distances or distribution in the quadrants

#shows the impact of each attribute on each of the main components.

#1.All the variables that are grouped together are positively correlated with each other.
#2.The greater the distance between the variable and the origin, the better represented that variable will be.
#3.Negatively correlated variables are shown on opposite sides of the biplot origin

##Score graph


# Individuals Chart
fviz_pca_ind(data.pca, col.ind = "blue")


##Biplot of loading and scores

# Individuals Chart
fviz_pca_biplot(data.pca, col.var = "darkgrey",col.ind = "green")





## Contribution of each variable
#The goal of the third visualization is to determine the extent to which each variable is represented in a given component. This quality of representation is called Cos2, it corresponds to the #squared cosine and is computed with the function fviz_cos2.
fviz_cos2(data.pca, choice = "var", axes = 1:2)

#A low value means that the variable is not perfectly represented by that component. but considering more variables can enrich the analysis

## Contribution of each individual
#a stronger value better representation, but considering more countries can enrich the analysis

fviz_cos2(data.pca, choice = "ind", axes = 1:2)




##Biplot of individuals


# Biplot of individuals in components 1 and 2
#a stronger value better representation,
fviz_pca_ind(data.pca,
             axes = c(1, 2),          # Especifica las dos primeras componentes
             col.ind = "cos2",        # Color según los valores de cos2: calidad de representación
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,            # Evita que se solape el texto de las etiquetas
             label = "ind",           # Agrega etiquetas a los individuos
             habillage = "none",      # No agrupa los individuos por ningún factor
             pointshape = 21,         # Forma del punto (círculo)
             pointsize = 2.5)         # Tamaño del punto

##Biplot of variables

# Biplot of the variables in components 1 and 2
#a stronger value better representation,

fviz_pca_var(data.pca,
             axes = c(1, 2),          # Especifica las dos primeras componentes
             col.var = "cos2",        # Color según los valores de cos2: calidad de representación
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,            # Evita que se solape el texto de las etiquetas
             label = "var",           # Agrega etiquetas a los individuos
             habillage = "none",      # No agrupa los individuos por ningún factor
             pointshape = 21,         # Forma del punto (círculo)
             pointsize = 0.8)         # Tamaño del punto



## Square cosines of the variables
#The analysis by cosines is clearer than by loadings to determine the variables by components

# Cos2 of variables7
#a stronger value better representation,add up to 1
variables_cos2 <- data.pca$loadings[, 1:2]^2
print("Cos2 de las variables:")
print(variables_cos2)

## Square cosines of individuals

# Calculate the scores of the individuals (coordinates) and then square and normalize

individuos_cos2 <- data.pca$scores[, 1:2]^2 / rowSums(data.pca$scores^2)
print("Cos2 de los individuos (Primera y Segunda Componente):")
print(individuos_cos2[, 1:2])


# Regression

## Regression Multiple

#Valuing 7 and 2 components at the regression level
## Regression model
#y=Bo+B1X1+B2X2+...+BpXp+E
#y=XB+E (vector matricial),Regression shows n, n nuples reg multiple (xi,yi) define most relevant variables
#Intercept, beta for each variable, p value,  income p is less than 0.005 but almost 0..
# p value other than 0,

# Extract the first 7 main components
pca_components <- data.pca$scores[, 1:7]

# Convert to data.frame and add 'Life.expectancy' variable
pca_df <- as.data.frame(pca_components)

pca_df$Life.expectancy <- data_agg_ocde3$Life.expectancy  

# Fit the multiple linear regression model
model <- lm(Life.expectancy ~ ., data = pca_df)

# Model Summary
summary(model)


#Residuals: These values indicate the distribution of the model's residuals, which should ideally be centered around zero.

#Coefficients:
#(Intercept): The intercept is 79.255082 with an extremely small p-value (< 2e-16), indicating that it is significantly different from zero.
#Comp.1: Estimated coefficient of 1.036428 with a p-value of 1.71e-10 (***), indicating that this component is highly significant.
#Comp.2: Estimated coefficient of 1.106976 with a p-value of 2.18e-07 (***), also highly significant.


#Residual standard error: 0.9161, indicating that the standard deviation of the residuals is relatively small.
#Multiple R-squared: 0.9547, meaning the model explains 95.47% of the variability in Life.expectancy.
#Adjusted R-squared: 0.9336, adjusted for the number of predictors, still indicating a good fit.
#F-statistic: 45.19 with a p-value of 5.829e-09, indicating that the model as a whole is highly significant.


# Extract the first 2 main components
pca_components2 <- data.pca$scores[, 1:2]

# Convert to data.frame and add 'Life.expectancy' variable
pca_df2 <- as.data.frame(pca_components2)

pca_df2$Life.expectancy <- data_agg_ocde3$Life.expectancy  

# Fit the multiple linear regression model
model2 <- lm(Life.expectancy ~ ., data = pca_df2)

# Model Summary
summary(model2)


# Assuming data_new is your new observations
# data_new must be in the same format as data_normalized_dep

# Normalize the new data (the same way as the original data)
#data_new_normalized <- scale(data_new, center = TRUE, scale = TRUE)

# Apply the PCA transformation to the new data
#pca_new_scores <- predict(data.pca, newdata = data_new_normalized)

# Extract the first 2 principal components from the new data
#pca_new_components2 <- pca_new_scores[, 1:2]

# Convert to data.frame
#pca_new_df2 <- as.data.frame(pca_new_components2)
#colnames(pca_new_df2) <- c("PC1", "PC2")

# Use the fitted model to make predictions
#predicted_life_expectancy <- predict(model2, newdata = pca_new_df2)

# Show predictions
#print(predicted_life_expectancy)

#Test

#Heteroscedasticity Test
bptest(model2)

#Multicollinearity Test
#VIF Test (Variance Inflation Factor)

vif(model2)
#The VIF test value is less than 5. Therefore, there is no multicollinearity.

#Autocorrelation Test

dwtest(model2)
bgtest(model2)
#2 to 4 indicate negative autocorrelation
#best value, most meaningful, best effect
#p value greater than 0.05 (depends on our confidence level) and r squared is 

#Regression shows n, n nuples reg multiple (xi,yi) define most relevant variables
#y=alfa+betax+epsilon lineal simple}

#Ordinary least squares methods-minimize the errors in the line and return estimates of the parameters
#valuation of atypicals-analyze them, there are assumptions of linearity, homoesthecity, normality of the residuals and independence, multicollinearity (correlation=they explain the same thing)

## Logistic regression

# Average numerical variables by country for OECD countries
data_agg_ocde_logistic <- data_ocde %>%
  group_by(Country, Status) %>%
  summarize(across(where(is.numeric), mean))

data_agg_ocde_logistic

## Delete the Year and HIV/AIDS column

data_agg_ocde_logistic <-data_agg_ocde_logistic[,-c(3,16)]  
data_agg_ocde_logistic

## Convert Country column to rows 

data_agg_ocde_logistic_df <- data.frame(data_agg_ocde_logistic)
rownames(data_agg_ocde_logistic_df) <- data_agg_ocde_logistic$Country
data_agg_ocde_logistic_df <-data_agg_ocde_logistic_df[,-c(1)]  
data_agg_ocde_logistic_df

# Load necessary libraries
library(dplyr)
library(caret)
library(lattice)

# Convert Status variable to a factor
data_agg_ocde_logistic_df$Status <- factor(data_agg_ocde_logistic_df$Status, levels = c("Developing", "Developed"))



str(data_agg_ocde_logistic_df)


# Split the data into training and test sets (70% training, 30% testing)
set.seed(123)
trainIndex <- createDataPartition(data_agg_ocde_logistic_df$Status, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dfTrain <- data_agg_ocde_logistic_df[trainIndex,]
dfTest <- data_agg_ocde_logistic_df[-trainIndex,]

# Fit the logistic regression model
logistic_model <- glm(Status ~ Measles + BMI + Polio + Alcohol + Life.expectancy, 
                      data = dfTrain, 
                      family = binomial)


# Model Summary
summary(logistic_model)

# Make predictions on the test set
predictions <- predict(logistic_model, newdata = dfTest, type = "response")

# Convert probabilities into classes
predicted_classes <- ifelse(predictions > 0.5, "Developed", "Developing")

# Convert predictions and actual labels to factors with the same levels
predicted_classes <- factor(predicted_classes, levels = c("Developing", "Developed"))
dfTest$Status <- factor(dfTest$Status, levels = c("Developing", "Developed"))

# Create a confusion table
confusionMatrix(predicted_classes, dfTest$Status)

dfTrain

dfTest

predictions


predicted_classes


#Conclusions
#Life expectancy in the study period reveals results in two segments for the population and variables under study from the comprehensive perspective, which explain 90% of the variability of the data and with the highest level of significance.

#Determining different variables or factors in common for groups of countries such as the following:
#Greater life expectancy, GDP, percentage of spending related to countries such as Canada, Germany, Switzerland, France.

#Inversely, measles infections and child deaths predominate in countries such as Mexico, Colombia, Costa Rica.

#Diseases like Polio, Diferia, Alcohol are related to the countries Belgium, Finland, Slovenia and Portugal.

#Adult mortality, thinness...1.19.years, thinness...519.years in countries where their population ages such as Poland, Lithuania, Hungary, Latvia, Estonia

#Although the study period is limited to 5 years and responds to previous years, some of these factors may currently prevail or have been overcome by the countries involved due to development from public and private actions carried out recently or changes in the environmental condition, political or others factors.

#Now, to counteract the prevailing and future challenges on this issue of longevity among the studied countries or others, it is necessary to detect opportunities and strategies in public policies in order to overcome multifactorial poverty with social investment on issues such as health, employment, housing, education and social protection, which improve the income and living conditions of the population.


# Bibliographic reference

#Giuseppe Ciaburro.Regression Analysis with R.2018.(https://www.google.co.cr/books/edition/Regression_Analysis_with_R/nyZKDwAAQBAJ?hl=es&gbpv=0)

#Databricks.Machine Learning Models. 2024.(https://www.databricks.com/glossary/machine-learning-models)


#IBM.What is principal component analysis (PCA).(https://www.ibm.com/topics/principal-component-analysis)

#Geeksforgeeks.Principal Component Analysis(PCA).2023.(https://www.geeksforgeeks.org/principal-component-analysis-pca/)
