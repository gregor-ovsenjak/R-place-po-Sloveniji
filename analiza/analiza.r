library("caret")
library("recipes")
library("ggplot")


place_po_sektorjih %>% View
place_po_sektorjih %>% filter(IZOBRAZBA != "Izobrazba - Skupaj") %>%
  ggplot(aes(x=POVPRECNA_PLACA, fill=factor(IZOBRAZBA))) + 
  geom_histogram(binwidth=100)


inTrain <- createDataPartition(
  y = place_po_sektorjih$POVPRECNA_PLACA,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training <- place_po_sektorjih[ inTrain,]
testing  <- place_po_sektorjih[-inTrain,]


preProcessValues <-preProcess(training,method = c("BoxCox","center","scale"))
trainTransformed <- predict(preProcessValues, training)
trainTransformed
