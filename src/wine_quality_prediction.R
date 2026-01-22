# ================================
# Wine Quality Prediction
# Author: Durga Prasad Narsing
# ================================

# Load libraries
library(randomForest)
library(tcltk)
library(openxlsx)
library(caret)

# -------------------------------
# Load Dataset
# -------------------------------
data <- read.xlsx("data/WineQT_dataset.xlsx")

# Target and features
y <- data$quality
X <- data[, !names(data) %in% "quality"]

set.seed(8)

# Train-test split
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test  <- y[-trainIndex]

# -------------------------------
# Train Random Forest Model
# -------------------------------
RF_clf <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 200,
  importance = TRUE
)

# Model Evaluation
pred_test <- predict(RF_clf, X_test)
accuracy <- mean(pred_test == y_test)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

# -------------------------------
# GUI Prediction Function
# -------------------------------
showQuality <- function() {

  new_data <- data.frame(
    fixed_acidity = as.numeric(tkget(e1)),
    volatile_acidity = as.numeric(tkget(e2)),
    citric_acid = as.numeric(tkget(e3)),
    residual_sugar = as.numeric(tkget(e4)),
    chlorides = as.numeric(tkget(e5)),
    free_sulfur_dioxide = as.numeric(tkget(e6)),
    total_sulfur_dioxide = as.numeric(tkget(e7)),
    density = as.numeric(tkget(e8)),
    pH = as.numeric(tkget(e9)),
    sulphates = as.numeric(tkget(e10)),
    alcohol = as.numeric(tkget(e11))
  )

  result <- predict(RF_clf, new_data)
  tkinsert(quality, 0, as.character(result))
}

# -------------------------------
# GUI Layout
# -------------------------------
win <- tktoplevel()
tkwm.title(win, "Wine Quality Prediction System")

labels <- c(
  "Fixed Acidity", "Volatile Acidity", "Citric Acid", "Residual Sugar",
  "Chlorides", "Free Sulfur Dioxide", "Total Sulfur Dioxide",
  "Density", "pH", "Sulphates", "Alcohol"
)

entries <- list()

for (i in seq_along(labels)) {
  tkgrid(tklabel(win, text = labels[i]), padx = 5, pady = 3)
  entries[[i]] <- tkentry(win)
  tkgrid(entries[[i]], padx = 5, pady = 3)
}

e1 <- entries[[1]]; e2 <- entries[[2]]; e3 <- entries[[3]]
e4 <- entries[[4]]; e5 <- entries[[5]]; e6 <- entries[[6]]
e7 <- entries[[7]]; e8 <- entries[[8]]; e9 <- entries[[9]]
e10 <- entries[[10]]; e11 <- entries[[11]]

tkgrid(tklabel(win, text = "Predicted Quality"), padx = 5, pady = 5)
quality <- tkentry(win)
tkgrid(quality, padx = 5, pady = 5)

tkgrid(
  tkbutton(win, text = "Find Quality", command = showQuality, width = 15),
  padx = 5, pady = 10
)

tkgrid(
  tkbutton(win, text = "Quit", command = function() tkdestroy(win), width = 15),
  padx = 5, pady = 10
)
