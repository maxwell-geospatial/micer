#' @name biData
#'
#' @description Example binary classification dataset. "Mine" is the positive case and
#' "Not Mine" is the background class. There are 178 samples from the "Mine" class and
#' 4,822 samples from the "Not Mine" class. Counts are relative to reference labels.
#' Class proportions are based on landscape proportions. There are a total of 5,000 samples.
#'
#' @docType data
#'
#' @title Example binary classification dataset
#'
#' @format
#' \describe{
#'   \item{ref}{reference label}
#'   \item{pred}{predicted label}
#' }
#'
#' @references Maxwell, A.E., Bester, M.S., Guillen, L.A., Ramezan, C.A., Carpinello, D.J., Fan, Y.,
#' Hartley, F.M., Maynard, S.M. and Pyron, J.L., 2020. Semantic segmentation deep learning for extracting
#' surface mine extents from historic topographic maps. Remote Sensing, 12(24), p.4145.
#'
#' @keywords datasets
NULL


#' @name mcData
#'
#' @description Example multiclass classification dataset with the following classes (counts relative to reference labels):
#' "Barren" (n=163), "Forest" (n=20,807), "Impervious" (n=426), "Low Vegetation" (n=3,182), "Mixed Dev" (n=520),
#' and "Water" (n=200). There are a total of 25,298 samples.
#'
#' @title Example multiclass classification dataset
#'
#' @docType data
#'
#' @format
#' \describe{
#'   \item{ref}{reference label}
#'   \item{pred}{predicted label}
#' }
#'
#' @references Maxwell, A.E., Strager, M.P., Warner, T.A., Ramezan, C.A., Morgan, A.N. and Pauley, C.E.,
#' 2019. Large-area, high spatial resolution land cover mapping using random forests, GEOBIA, and NAIP
#' orthophotography: Findings and recommendations. Remote Sensing, 11(12), p.1409.
#'
#' @keywords datasets
NULL


#' @name compareData
#'
#' @description Example multiclass classification dataset with the following wetland-related classes:
#' "PFO", "PEM", "RLP", and "Not". PFO = Palustrine Forested; PEM = Palustrine Emergent;
#' RLP = River, Lake, Pond; Not = Not Wetland. There are 600 examples from each class relative to the
#' reference labels.
#'
#' @docType data
#'
#' @title Data for multiclass classification comparison
#'
#' @format
#' \describe{
#'   \item{ref}{correct label}
#'   \item{rfRred}{random forest prediction}
#'   \item{dfRred}{single decision tree prediction}
#' }
#'
#' @references These data are unpublished
#'
#' @keywords datasets
NULL
