% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bridge_prism.R
\name{model_run}
\alias{model_run}
\title{Run QRISK3 Model}
\usage{
model_run(model_input = NULL)
}
\arguments{
\item{model_input}{A list/json object with "data", which is a dataset containing patients' data,
along with the names of each columns in the "data" assigned for
patid, gender, age,
atrial_fibrillation, atypical_antipsy,
regular_steroid_tablets, erectile_disfunction,
migraine, rheumatoid_arthritis,
chronic_kidney_disease, severe_mental_illness,
systemic_lupus_erythematosis,
blood_pressure_treatment, diabetes1,
diabetes2, weight="weight", height,
ethiniciy, heart_attack_relative,
cholesterol_HDL_ratio, systolic_blood_pressure,
std_systolic_blood_pressure, smoke, townsend)}
}
\value{
Return a dataset with three columns: patient identifier, caculated QRISK3 score,
caculated QRISK3 score with only 1 digit
}
\description{
This function calls the QRISK3 model.
If no model_input is specified, it will use the defaults
}
