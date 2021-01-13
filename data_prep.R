

###################################################
### Preparation of the Environment ####


# Clear environment
remove(list = ls())

# Create list with needed libraries
pkgs <- c("readr", "fastDummies","dplyr")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


###################################################
### Function Definition ####

#' Title Fast creation of normalized variables
#' Quickly create normalized columns from numeric type columns in the input data. This function is useful for statistical analysis when you want normalized columns rather than the actual columns.
#'
#' @param .data An object with the data set you want to make normalized columns from.
#' @param norm_values Dataframe of column names, means, and standard deviations that is used to create corresponding normalized variables from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) with same number of rows an dcolumns as the inputted data, only with normalized columns for the variables indicated in the norm_values argument.
#' @export
#'
#' @examples
norm_cols <- function (.data, norm_values = NULL) {
  for (i in 1:nrow(norm_values)  ) {
    .data[[norm_values$name[i]]] <- (.data[[norm_values$name[i]]] - norm_values$mean[i]) / norm_values$sd[i]
  }
  return (.data)
}


#' Title Creation of a Dataframe including the Information to Standardize Variables
#' This function is meant to be used in combination with the function norm_cols
#'
#' @param .data A data set including the variables you want to get the means and standard deviations from.
#' @param select_columns A vector with a list of variable names for which you want to get the means and standard deviations from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) including the names, means, and standard deviations of the variables included in the select_columns argument.
#' @export
#'
#' @examples
get.norm_values <- function (.data, select_columns = NULL) {
  result <- NULL
  for (col_name in select_columns) {
    mean <- mean(.data[[col_name]], na.rm = TRUE)
    sd <- sd(.data[[col_name]], na.rm = TRUE)
    result <- rbind (result, c(mean, sd))
  }
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  result <- data.frame (select_columns, result, stringsAsFactors = FALSE)
  names(result) <- c("name", "mean", "sd")
  return (result)
}



###################################################
### Data Import ####

# Reading the data file
umsatzdaten <- read_csv("https://raw.githubusercontent.com/christophson3/Data-science-Team-8/main/umsatzdaten.csv", 
                        col_types = cols(X1 = col_skip()))

full_dataset <- read_csv("https://raw.githubusercontent.com/christophson3/Data-science-Team-8/main/full_dataset.csv", 
                         col_types = cols(X1 = col_skip()))


newdata <- (filter(full_dataset, Datum == "2019-06-05"))
newdata['Wochentag'] <- "Mittwoch"
newdata <-rbind(newdata, newdata[rep(1, 5), ])

data <- filter(umsatzdaten, Datum == "2019-06-04")


for (i in (1:6)) {
  
  newdata$Warengruppe[[i]] <- i
  newdata$Vortagsumsatz[[i]] <- data$Umsatz[i]
}
newdata$Vortagsumsatz[[6]] <- 0
###################################################
### Data Preparation ####
umsatzdaten <- rbind(umsatzdaten,newdata)

# Recoding of the variables into one-hot encoded (dummy) variables
dummy_list <- c("Wertung", "Warengruppe", "KielerWoche")
umsatzdaten_dummy <- dummy_cols(umsatzdaten, dummy_list)

# Definition of lists for each one-hot encoded variable (just to make the handling easier)
wertung_dummies <- c('Wertung_1', 'Wertung_2', 'Wertung_3', 'Wertung_4')
kiwo_dummies <- c('KielerWoche_0','KielerWoche_1')
gruppen_dummies <- c('Warengruppe_1','Warengruppe_2','Warengruppe_3','Warengruppe_4','Warengruppe_5','Warengruppe_6')

# Standardization of all variables (features and label)
norm_list <- c("Umsatz","Windgeschwindigkeit","Wettercode","Vortagsumsatz","Bewoelkung","Temperatur",kiwo_dummies, wertung_dummies, gruppen_dummies) # list of all relevant variables
norm_values_list <- get.norm_values(umsatzdaten_dummy, norm_list)    # Calculation of the means and standard deviations
umsatzdaten_norm = norm_cols(umsatzdaten_dummy, norm_values_list)   # Standardization of the variables


###################################################
### Selection of the Feature Variables and the Label Variable ####

# Selection of the features (the independent variables used to predict the dependent)
features <- c('Windgeschwindigkeit','Wettercode', 'Vortagsumsatz', 'Bewoelkung','Temperatur', wertung_dummies,kiwo_dummies ,gruppen_dummies)
# Selection of the label (the dependent variable)
label <- c('Umsatz')

newdata <- filter(umsatzdaten_norm, Datum == "2019-06-05")
umsatzdaten_norm <- filter(umsatzdaten_norm, Datum != "2019-06-05")
umsatzdaten <- filter(umsatzdaten, Datum!= "2019-06-05")

###################################################
### Selection of Training and Validation data ####

# Setting the random counter to a fixed value, so the random initialization stays the same (the random split is always the same)
set.seed(1)
# Generating the random indices for the training data set
train_ind <- sample(seq_len(nrow(umsatzdaten_norm)), size = floor(0.66 * nrow(umsatzdaten_norm)))

# Splitting the data into training and validation data and selecting the feature variables as a separate data frame
train_dataset <- umsatzdaten_norm[train_ind, features]
test_dataset  <- umsatzdaten_norm[-train_ind, features]
newdata_dataset <- newdata[,features]
# Splitting the data into training and validation data and selecting the label variable as a separate vector
train_labels <- umsatzdaten_norm[train_ind, label]
test_labels <- umsatzdaten_norm[-train_ind, label]



