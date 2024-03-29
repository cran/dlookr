#' Heart Failure Data
#'
#' @description 
#' A dataset containing the ages and other attributes of almost 300 cases. 
#' 
#' @details 
#' Heart failure is a common event caused by Cardiovascular diseasess and this dataset contains 12 features that can be used to predict mortality by heart failure.
#' 
#' @format A data frame with 299 rows and 13 variables. The variables are as follows:
#' \describe{
#'   \item{age}{patient's age.}
#'   \item{anaemia}{decrease of red blood cells or hemoglobin (boolean), Yes, No.}
#'   \item{cpk_enzyme}{level of the CPK(creatinine phosphokinase) enzyme in the blood (mcg/L).}
#'   \item{diabetes}{if the patient has diabetes (boolean), Yes, No.}
#'   \item{ejection_fraction}{percentage of blood leaving the heart at each contraction (percentage).}
#'   \item{hblood_pressure}{high_blood_pressure. if the patient has hypertension (boolean), Yes, No.}
#'   \item{platelets}{platelets in the blood (kiloplatelets/mL).}
#'   \item{creatinine}{level of serum creatinine in the blood (mg/dL).}
#'   \item{sodium}{level of serum sodium in the blood (mEq/L).}
#'   \item{sex}{patient's sex (binary), Male, Female.}
#'   \item{smoking}{if the patient smokes or not (boolean), Yes, No.}
#'   \item{time}{follow-up period (days).}
#'   \item{death_event}{if the patient deceased during the follow-up period (boolean), Yes, No.}
#' }
#' @docType data
#' @keywords datasets
#' @name heartfailure
#' @usage data(heartfailure)
#' @references {
#' Davide Chicco, Giuseppe Jurman: Machine learning can predict survival of patients with heart failure from serum creatinine and ejection fraction alone. BMC Medical Informatics and Decision Making 20, 16 (2020). 
#' <https://doi.org/10.1186/s12911-020-1023-5>
#' }
#' @source {
#' "Heart Failure Prediction" in Kaggle <https://www.kaggle.com/andrewmvd/heart-failure-clinical-data>, License : CC BY 4.0
#' }
NULL
