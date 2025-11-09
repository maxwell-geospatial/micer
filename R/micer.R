#' mice
#'
#' Calculate map image classification efficacy (MICE) and other metrics using columns/vectors of reference and predicted classes
#'
#' For multiclass classification, returns a list object with the following items:
#' $Mappings = class names;
#' $confusionMatrix = confusion matrix where columns represent the reference data and rows represent the classification result;
#' $referenceCounts = count of samples in each reference class;
#' $predictionCounts = count of predictions in each class;
#' $overallAccuracy = overall accuracy;
#' $MICE = map image classification efficacy;
#' $usersAccuracies = class-level user's accuracies (1 - commission error);
#' $CTBICEs = classification-total-based image classification efficacies (adjusted user's accuracies);
#' $producersAccuracies = class-level producer's accuracies (1 - omission error);
#' $RTBICEs = reference-total-based image classification efficacies (adjusted producer's accuracies);
#' $F1Scores = class-level harmonic mean of user's and producer's accuracies;
#' $F1Efficacies = F1-score efficacies;
#' $macroPA = class-aggregated, macro-averaged producer's accuracy;
#' $macroRTBICE = class-aggregated, macro-averaged reference-total-based image classification efficacy;
#' $macroUA = class-aggregated, macro-averaged user's accuracy;
#' $macroCTBICE = class-aggregated, macro-averaged classification-total-based image classification efficacy;
#' $macroF1 = class-aggregated, macro-averaged F1-score;
#' $macroF1Efficacy = class-aggregated, macro-averaged F1 efficacy;
#'
#' For binary classification, returns a list object with the following items:
#' $Mappings = class names;
#' $confusionMatrix = confusion matrix where columns represent the reference data and rows represent the classification result;
#' $referenceCounts = count of samples in each reference class;
#' $predictionCounts = count of predictions in each class;
#' $postiveCase = name or mapping for the positive case;
#' $overallAccuracy = overall accuracy;
#' $MICE = map image classification efficacy;
#' $Precision = precision (1 - commission error relative to positive case);
#' $precisionEfficacy = precision efficacy;
#' $NPV = negative predictive value (1 - commission error relative to negative case);
#' $npvEfficacy = negative predictive value efficacy;
#' $Recall = recall (1 - omission error relative to positive case);
#' $recallEfficacy = recall efficacy;
#' $specificity = specificity (1 - omission error relative to negative case);
#' $specificityEfficacy = specificity efficacy;
#' $f1Score = harmonic mean of precision and recall;
#' $f1Efficacy = F1-score efficacy;
#'
#' @param reference column/vector of reference labels as factor data type.
#' @param prediction column/vector of predicted labels as factor data type.
#' @param mappings names of classes (if not provided, factor levels are used).
#' @param multiclass TRUE or FALSE. If TRUE, treats classification as multiclass. If FALSE, treats classification as binary. Default is TRUE.
#' @param positiveIndex index for positive case for binary classification. Ignored for multiclass classification. Default is 1 or first factor level.
#' @returns multiclass or binary assessment metrics in a list object. See details for description of generated metrics.
#' @examples
#' #Multiclass example
#' data(mcData)
#' mice(mcData$ref,
#' mcData$pred,
#' mappings=c("Barren", "Forest", "Impervious", "Low
#' Vegetation", "Mixed Dev", "Water"),
#' multiclass=TRUE)
#'
#' #Binary example
#' data(biData)
#' mice(biData$ref,
#' biData$pred,
#' mappings = c("Mined", "Not Mined"),
#' multiclass=FALSE,
#' positiveIndex=1)
#' @export
#' @importFrom stats median quantile setNames t.test
mice <- function(reference, #Factor of correct/reference labels
                 prediction,#Factor of predicted labels
                 mappings=levels(as.factor(reference)), #Names of classes (if not provided, will use factor levels)
                 multiclass = TRUE, #TRUE for multiclass, FALSE for binary
                 positiveIndex = 1 #Index for the positive case (only used for binary classification
){

  positiveIndex <- as.numeric(positiveIndex) #Make sure index is numeric

  ctab <- table(prediction, reference) #generate contingency table

  colnames(ctab) <- mappings #Apply class names to columns
  rownames(ctab) <- mappings #Apply class names to rows
  dimnames(ctab) <- setNames(dimnames(ctab),c("Predicted", "Reference")) #Label row axis as "predicted" and column axis as "reference"

  refCnts <- colSums(ctab) #Get column total counts
  names(refCnts) <- mappings #Name column counts
  predCnts <- rowSums(ctab) #Get row total counts
  names(predCnts) <- mappings #Name row counts

  oa <- sum(diag(ctab))/sum(ctab) #Calculate overall accuracy
  sumCols <- colSums(ctab)
  sumRows <- rowSums(ctab)

  sumColsN <- (sumCols/sum(sumCols))+.00001 #calculate nj/n
  sumColsN2 <- sumColsN*sumColsN #square nj/n

  oa0 <- sum(sumColsN2) #calculate random model correction
  mice <- (oa-oa0)/(1-oa0) #Calculate MICE


  ua <- diag(ctab)/(sumRows +.00001) #Calculate all class user's accuracies (i.e., precisions)
  pa <- diag(ctab)/(sumCols +.00001) #Calculate all class producer's accuracies (i.e., recalls)
  f1 <- (2*ua*pa)/(ua+pa) #Calculate all class F1-scores ((2*Precision*Recall)/(precision+recall))

  rtbice <- (pa - sumColsN)/(1- sumColsN) #Calculate all reference-total-based image classification efficacies
  ctbice <- (ua - sumColsN)/(1- sumColsN) #Calculate all classification-total-based image classification efficacies
  f1Efficacy <- (2*rtbice*ctbice)/(rtbice+ctbice) #Calculate F1-scores with efficacy-based correction

  if(multiclass==TRUE){
    #Add names to all vectors
    names(ua) <- mappings
    names(pa) <- mappings
    names(rtbice) <- mappings
    names(ctbice) <- mappings
    names(f1) <- mappings
    names(f1Efficacy) <- mappings

    #Macro-average all UAs/precisions, PAs/recalls, and F1-scores for class-aggregated metrics
    macroUA <- mean(ua)
    macroPA <- mean(pa)
    macroF1 <- (2*macroUA*macroPA)/(macroUA+macroPA)

    #Macro-average all efficiacy-based measusures for class-aggregated metrics
    macroRTBICE <- mean(rtbice)
    macroCTBICE <- mean(ctbice)
    macrof1Efficacy <- (2*macroRTBICE*macroCTBICE)/(macroRTBICE+macroCTBICE)

    #Return list object for multiclass classification
    return(list(Mappings = mappings,
                confusionMatrix = ctab,
                referenceCounts = refCnts,
                predictionCounts = predCnts,
                overallAccuracy = oa,
                MICE = mice,
                usersAccuracies = ua,
                CTBICEs = ctbice,
                producersAccuracies = pa,
                RTBICEs = rtbice,
                f1Scores = f1,
                f1Efficacies = f1Efficacy,
                macroPA = macroPA,
                macroRTBUCE = macroRTBICE,
                macroUA = macroUA,
                macroCTBICE = macroCTBICE,
                macroF1 = macroF1,
                macroF1Efficacy = macrof1Efficacy
    )
    )
  }else{

    #Return list object for binary classification
    negativeIndex = 3 - positiveIndex
    return(list(Mappings = mappings,
                confusionMatrix = ctab,
                referenceCounts = refCnts,
                predictionCounts = predCnts,
                positiveCase = mappings[positiveIndex],
                overallAccuracy = oa,
                mice = mice,
                Precision = unname(ua)[positiveIndex],
                precisionEfficacy = unname(ctbice)[positiveIndex],
                NPV = unname(ua)[negativeIndex],
                npvEfficacy = unname(ctbice)[negativeIndex],
                Recall = unname(pa)[positiveIndex],
                recallEfficacy = unname(rtbice)[positiveIndex],
                Specificity = unname(pa)[negativeIndex],
                specificityEfficicacy = unname(rtbice)[negativeIndex],
                f1Score = unname(f1)[positiveIndex],
                f1ScoreEfficacy = unname(f1Efficacy)[positiveIndex]
    )
    )

  }
}


#' miceCM
#'
#' Calculate map image classification efficacy (MICE) and other metrics using confusion matrix
#'
#' For multiclass classification, returns a list object with the following items:
#' $Mappings = class names;
#' $confusionMatrix = confusion matrix where columns represent the reference data and rows represent the classification result;
#' $referenceCounts = count of samples in each reference class;
#' $predictionCounts = count of predictions in each class;
#' $overallAccuracy = overall accuracy;
#' $MICE = map image classification efficacy;
#' $usersAccuracies = class-level user's accuracies (1 - commission error);
#' $CTBICEs = classification-total-based image classification efficacies (adjusted user's accuracies);
#' $producersAccuracies = class-level producer's accuracies (1 - omission error);
#' $RTBICEs = reference-total-based image classification efficacies (adjusted producer's accuracies);
#' $F1Scores = class-level harmonic mean of user's and producer's accuracies;
#' $F1Efficacies = F1-score efficacies;
#' $macroPA = class-aggregated, macro-averaged producer's accuracy;
#' $macroRTBICE = class-aggregated, macro-averaged reference-total-based image classification efficacy;
#' $macroUA = class-aggregated, macro-averaged user's accuracy;
#' $macroCTBICE = class-aggregated, macro-averaged classification-total-based image classification efficacy;
#' $macroF1 = class-aggregated, macro-averaged F1-score;
#' $macroF1Efficacy = class-aggregated, macro-averaged F1 efficacy;
#'
#' For binary classification, returns a list object with the following items:
#' $Mappings = class names;
#' $confusionMatrix = confusion matrix where columns represent the reference data and rows represent the classification result;
#' $referenceCounts = count of samples in each reference class;
#' $predictionCounts = count of predictions in each class;
#' $postiveCase = name or mapping for the positive case;
#' $overallAccuracy = overall accuracy;
#' $MICE = map image classification efficacy;
#' $Precision = precision (1 - commission error relative to positive case);
#' $precisionEfficacy = precision efficacy;
#' $NPV = negative predictive value (1 - commission error relative to negative case);
#' $npvEfficacy = negative predictive value efficacy;
#' $Recall = recall (1 - omission error relative to positive case);
#' $recallEfficacy = recall efficacy;
#' $specificity = specificity (1 - omission error relative to negative case);
#' $specificityEfficacy = specificity efficacy;
#' $f1Score = harmonic mean of precision and recall;
#' $f1Efficacy = F1-score efficacy;
#'
#' @param cm confusion matrix as table object where rows define predictions and columns define reference labels.
#' @param mappings names of classes (if not provided, factor levels are used).
#' @param multiclass TRUE or FALSE. If TRUE, treats classification as multiclass. If FALSE, treats classification as binary. Default is TRUE.
#' @param positiveIndex index for positive case for binary classification. Ignored for multiclass classification. Default is 1 or first factor level.
#' @returns multiclass or binary assessment metrics in a list object. See details for description of generated metrics.
#' @examples
#' #Multiclass example
#' data(mcData)
#' cmMC <- table(mcData$pred, mcData$ref)
#' miceCM(cmMC,
#' mappings=c("Barren", "Forest", "Impervious", "Low Vegetation", "Mixed Dev", "Water"),
#' multiclass=TRUE)
#'
#' #Binary example
#' data(biData)
#' cmB <- table(biData$pred, biData$ref)
#' miceMCResult <- miceCM(cmB,
#' mappings=c("Mined", "Not Mined"),
#' multiclass=FALSE,
#' positiveIndex=1)
#' print(miceMCResult)
#' @export
#' @importFrom stats median quantile setNames t.test
miceCM <- function(cm,#Factor of predicted labels
                   mappings=levels(as.factor(row.names(cm))), #Names of classes (if not provided, will use factor levels)
                   multiclass = TRUE, #TRUE for multiclass, FALSE for binary
                   positiveIndex = 1 #Index for the positive case (only used for binary classification
){

  positiveIndex <- as.numeric(positiveIndex) #Make sure index is numeric

  ctab <- cm #generate contingency table

  colnames(ctab) <- mappings #Apply class names to columns
  rownames(ctab) <- mappings #Apply class names to rows
  dimnames(ctab) <- setNames(dimnames(ctab),c("Predicted", "Reference")) #Label row axis as "predicted" and column axis as "reference"

  refCnts <- colSums(ctab) #Get column total counts
  names(refCnts) <- mappings #Name column counts
  predCnts <- rowSums(ctab) #Get row total counts
  names(predCnts) <- mappings #Name row counts

  oa <- sum(diag(ctab))/sum(ctab) #Calcualte overall accuracy
  sumCols <- colSums(ctab)
  sumRows <- rowSums(ctab)

  sumColsN <- (sumCols/sum(sumCols))+.00001 #calculate nj/n
  sumColsN2 <- sumColsN*sumColsN #square nj/n

  oa0 <- sum(sumColsN2) #calculate random model correction
  mice <- (oa-oa0)/(1-oa0) #Calculate MICE


  ua <- diag(ctab)/(sumRows +.00001) #Calculate all class user's accuracies (i.e., precisions)
  pa <- diag(ctab)/(sumCols +.00001) #Calculate all class producer's accuracies (i.e., recalls)
  f1 <- (2*ua*pa)/(ua+pa) #Calculate all class F1-scores ((2*Precision*Recall)/(precision+recall))

  rtbice <- (pa - sumColsN)/(1- sumColsN) #Calculate all reference-total-based image classification efficacies
  ctbice <- (ua - sumColsN)/(1- sumColsN) #Calcualte all classification-total-based image classification efficacies
  f1Efficacy <- (2*rtbice*ctbice)/(rtbice+ctbice) #Calculate F1-scores with efficacy-based correction

  if(multiclass==TRUE){
    #Add names to all vectors
    names(ua) <- mappings
    names(pa) <- mappings
    names(rtbice) <- mappings
    names(ctbice) <- mappings
    names(f1) <- mappings
    names(f1Efficacy) <- mappings

    #Macro-average all UAs/precisions, PAs/recalls, and F1-scores for class-aggregated metrics
    macroUA <- mean(ua)
    macroPA <- mean(pa)
    macroF1 <- (2*macroUA*macroPA)/(macroUA+macroPA)

    #Macro-average all efficiacy-based measusures for class-aggregated metrics
    macroRTBICE <- mean(rtbice)
    macroCTBICE <- mean(ctbice)
    macrof1Efficacy <- (2*macroRTBICE*macroCTBICE)/(macroRTBICE+macroCTBICE)

    #Return list object for multiclass classification
    return(list(Mappings = mappings,
                confusionMatrix = ctab,
                referenceCounts = refCnts,
                predictionCounts = predCnts,
                overallAccuracy = oa,
                MICE = mice,
                usersAccuracies = ua,
                CTBICEs = ctbice,
                producersAccuracies = pa,
                RTBICEs = rtbice,
                f1Scores = f1,
                f1Efficacies = f1Efficacy,
                macroPA = macroPA,
                macroRTBUCE = macroRTBICE,
                macroUA = macroUA,
                macroCTBICE = macroCTBICE,
                macroF1 = macroF1,
                macroF1Efficacy = macrof1Efficacy
    )
    )
  }else{

    #Return list object for binary classification
    negativeIndex = 3 - positiveIndex
    return(list(Mappings = mappings,
                confusionMatrix = ctab,
                referenceCounts = refCnts,
                predictionCounts = predCnts,
                positiveCase = mappings[positiveIndex],
                overallAccuracy = oa,
                mice = mice,
                Precision = unname(ua)[positiveIndex],
                precisionEfficacy = unname(ctbice)[positiveIndex],
                NPV = unname(ua)[negativeIndex],
                npvEfficacy = unname(ctbice)[negativeIndex],
                Recall = unname(pa)[positiveIndex],
                recallEfficacy = unname(rtbice)[positiveIndex],
                Specificity = unname(pa)[negativeIndex],
                specificityEfficicacy = unname(rtbice)[negativeIndex],
                f1Score = unname(f1)[positiveIndex],
                f1ScoreEfficacy = unname(f1Efficacy)[positiveIndex]
    )
    )

  }
}

#' miceCI
#'
#' Calculate confidence intervals (CIs) for MICE and associated metrics using bootstrap sampling and the percentile method.
#'
#' Confidence intervals are estimated for overall accuracy, MICE, and all class-aggregated, macro-averaged metrics produced by mice() or miceCM().
#' Returns metric name, mean metric value, median metric value, lower confidence interval bounds (low.ci), and upper confidence interval bounds
#' (upper.ci) as a dataframe object.
#'
#' @param reps number of bootstrap replicates to use. Default is 200.
#' @param lowPercentile lower percentile for confidence interval. Default is 0.025 for a 95% CI.
#' @param highPercentile upper percentile for confidence interval. Default is 0.975 for a 95% CI.
#' @param reference column of reference labels as factor data type.
#' @param prediction column of predicted labels as factor data type.
#' @param mappings names of classes (if not provided, factor levels are used).
#' @param multiclass TRUE or FALSE. If TRUE, treats classification as multiclass. If FALSE, treats classification as binary. Default is TRUE.
#' @param positiveIndex index for positive case for binary classification. Ignored for multiclass classification. Default is 1 or first factor level
#' @returns dataframe object of metric name and estimated mean value, median value, and lower and upper CIs.
#' @examples
#' #Multiclass example
#' data(mcData)
#' ciResultsMC <- miceCI(rep=100,
#' mcData$ref,
#' mcData$pred,
#' lowPercentile=0.025,
#' highPercentile=0.975,
#' mappings=c("Barren", "Forest", "Impervious", "Low Vegetation", "Mixed Dev", "Water"),
#' multiclass=TRUE)
#'
#' print(ciResultsMC)
#'
#' #Binary example
#' data(biData)
#' ciResultsBi <- miceCI(rep=100,
#' biData$ref,
#' biData$pred,
#' lowPercentile=0.025,
#' highPercentile=0.975,
#' mappings = c("Mined", "Not Mined"),
#' multiclass=FALSE,
#' positiveIndex=1)
#'
#' print(ciResultsBi)
#' @export
#' @importFrom stats median quantile setNames t.test
miceCI <- function(reps=200,
                   lowPercentile,
                   highPercentile,
                   reference, #Factor of correct/reference labels
                   prediction,#Factor of predicted labels
                   mappings=levels(as.factor(reference)), #Names of classes (if not provided, will use factor levels)
                   multiclass = TRUE, #TRUE for multiclass, FALSE for binary
                   positiveIndex = 1 #Index for the positive case (only used for binary classification
){

  if(multiclass==TRUE){
    resultDF <- data.frame(
      overallAccuracy = as.numeric(),
      MICE = as.numeric(),
      macroPA = as.numeric(),
      macroRTBUCE = as.numeric(),
      macroUA = as.numeric(),
      macroCTBICE = as.numeric(),
      macroF1 = as.numeric(),
      macroF1Efficacy = as.numeric())
  }else{
    resultDF <- data.frame(
      overallAccuracy = as.numeric(),
      MICE = as.numeric(),
      Precision = as.numeric(),
      precisionEfficacy = as.numeric(),
      NPV = as.numeric(),
      npvEfficacy = as.numeric(),
      Recall = as.numeric(),
      recallEfficacy = as.numeric(),
      Specificity = as.numeric(),
      specificityEfficicacy = as.numeric(),
      f1Score = as.numeric(),
      f1ScoreEfficacy = as.numeric())
  }

  inData <- data.frame(ref=reference, pred=prediction)
  positiveIndex <- as.numeric(positiveIndex)

  repLst <- list()

  i <- 1
  while(i <= reps){

    subData <- dplyr::sample_n(inData, nrow(inData), replace=TRUE)

    ctab <- table(subData$pred, subData$ref) #generate contingency table

    colnames(ctab) <- mappings #Apply class names to columns
    rownames(ctab) <- mappings #Apply class names to rows
    dimnames(ctab) <- setNames(dimnames(ctab),c("Predicted", "Reference")) #Label row axis as "predicted" and column axis as "reference"

    refCnts <- colSums(ctab) #Get column total counts
    names(refCnts) <- mappings #Name column counts
    predCnts <- rowSums(ctab) #Get row total counts
    names(predCnts) <- mappings #Name row counts

    oa <- sum(diag(ctab))/sum(ctab) #Calcualte overall accuracy
    sumCols <- colSums(ctab)
    sumRows <- rowSums(ctab)

    sumColsN <- (sumCols/sum(sumCols))+.00001 #calculate nj/n
    sumColsN2 <- sumColsN*sumColsN #square nj/n

    oa0 <- sum(sumColsN2) #calculate random model correction
    mice <- (oa-oa0)/(1-oa0) #Calculate MICE


    ua <- diag(ctab)/(sumRows +.00001) #Calculate all class user's accuracies (i.e., previsions)
    pa <- diag(ctab)/(sumCols +.00001) #Calculate all class producer's accuracies (i.e., recalls)
    f1 <- (2*ua*pa)/(ua+pa) #Calculate all class F1-scores ((2*Precision*Recall)/(precision+recall))

    rtbice <- (pa - sumColsN)/(1- sumColsN) #Calculate all reference-total-based image classification efficacies
    ctbice <- (ua - sumColsN)/(1- sumColsN) #Calculate all classification-total-based image classification efficacies
    f1Efficacy <- (2*rtbice*ctbice)/(rtbice+ctbice) #Calculate F1-scores with efficacy-based correction

    if(multiclass==TRUE){
      #Add names to all vectors
      names(ua) <- mappings
      names(pa) <- mappings
      names(rtbice) <- mappings
      names(ctbice) <- mappings
      names(f1) <- mappings
      names(f1Efficacy) <- mappings

      #Macro-average all UAs/precisions, PAs/recalls, and F1-scores for class-aggregated metrics
      macroUA <- mean(ua)
      macroPA <- mean(pa)
      macroF1 <- (2*macroUA*macroPA)/(macroUA+macroPA)

      #Macro-average all efficiacy-based measusures for class-aggregated metrics
      macroRTBICE <- mean(rtbice)
      macroCTBICE <- mean(ctbice)
      macrof1Efficacy <- (2*macroRTBICE*macroCTBICE)/(macroRTBICE+macroCTBICE)

      #Return list object for multiclass classification
      bootResult <- data.frame(
        overallAccuracy = oa,
        MICE = mice,
        macroPA = macroPA,
        macroRTBUCE = macroRTBICE,
        macroUA = macroUA,
        macroCTBICE = macroCTBICE,
        macroF1 = macroF1,
        macroF1Efficacy = macrof1Efficacy
      )
    }else{

      #Return list object for binary classification
      negativeIndex = 2 - positiveIndex
      bootResult <- data.frame(
        overallAccuracy = oa,
        MICE = mice,
        Precision = unname(ua)[positiveIndex],
        precisionEfficacy = unname(ctbice)[positiveIndex],
        NPV = unname(ua)[negativeIndex],
        npvEfficacy = unname(ctbice)[negativeIndex],
        Recall = unname(pa)[positiveIndex],
        recallEfficacy = unname(rtbice)[positiveIndex],
        Specificity = unname(pa)[negativeIndex],
        specificityEfficicacy = unname(rtbice)[negativeIndex],
        f1Score = unname(f1)[positiveIndex],
        f1ScoreEfficacy = unname(f1Efficacy)[positiveIndex]
      )

    }

    resultDF <- dplyr::bind_rows(resultDF, bootResult)
    i <- i+1
  }

  calculate_stats <- function(column) {
    mean_val <- mean(column)
    median_val <- median(column)
    quantiles <- quantile(column, probs = c(lowPercentile, highPercentile))

    return(c(mean = mean_val, median = median_val, ciLow = quantiles[1], ciHigh = quantiles[2]))
  }

  # Apply the function to each row and convert the result to a data frame
  resultStats <- t(apply(resultDF, 2, calculate_stats))

  # Convert the matrix back to a data frame and add row names
  resultStats <- as.data.frame(resultStats)
  resultStats$metric <- rownames(resultStats)
  row.names(resultStats) <- NULL
  resultStats <- resultStats[, c(ncol(resultStats), 1:(ncol(resultStats)-1))]
  names(resultStats) <- c("metric", "mean", "median", "low.ci", "high.ci")

  return(resultStats)
}

#' miceCompare
#'
#' Statistically compare two models using a paired t-test and bootstrap samples of the assessment results
#'
#' @param ref column of reference labels as factor data type.
#' @param result1 column of predicted labels as factor data type (first result to compare).
#' @param result2 column of predicted labels as factor data type (second result to compare).
#' @param reps number of bootstrap replicates to use. Default is 200.
#' @returns paired t-test results including t-statistic, degrees of freedom, p-value, 95% confidence interval, and mean difference
#' @examples
#' data(compareData)
#' compareResult <- miceCompare(ref=compareData$ref,
#' result1=compareData$rfPred,
#' result2=compareData$dtPred,
#' reps=100)
#' print(compareResult)
#' @export
#' @importFrom stats median quantile setNames t.test
miceCompare <- function(ref, result1, result2, reps){
  #Compare two models using bootstrapping and paired t-test
  #https://www.tmwr.org/compare

  inData <- data.frame(ref=ref, result1=result1, result2=result2)

  resultsDF <- data.frame(mice1 = numeric(),
                          mice2 = numeric())

  i <- 1
  while(i <= reps){

    subData <- dplyr::sample_n(inData, nrow(inData), replace=TRUE)

    ctab1 <- table(subData$result1, subData$ref)


    oa1 <- sum(diag(ctab1))/sum(ctab1)
    sumCols1 <- colSums(ctab1)
    sumRows1 <- rowSums(ctab1)

    sumColsN1 <- (sumCols1/sum(sumCols1))+.00001
    sumColsN21 <- sumColsN1*sumColsN1

    oa01 <- sum(sumColsN21)
    mice1 <- (oa1-oa01)/(1-oa01)


    ctab2 <- table(subData$result2, subData$ref)


    oa2 <- sum(diag(ctab2))/sum(ctab2)
    sumCols2 <- colSums(ctab2)
    sumRows2 <- rowSums(ctab2)

    sumColsN2 <- (sumCols2/sum(sumCols2))+.00001
    sumColsN22 <- sumColsN2*sumColsN2

    oa02 <- sum(sumColsN22)
    mice2 <- (oa2-oa02)/(1-oa02)

    bootResults <- data.frame(mice1 = mice1,
                              mice2 = mice2)

    resultsDF <- dplyr::bind_rows(resultsDF, bootResults)

    i <- i+1

  }

  return(t.test(resultsDF$mice1, resultsDF$mice2, paired=TRUE))
}
