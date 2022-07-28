#' @title Run QRISK3 Model
#' @description This function calls the QRISK3 model.
#' If no model_input is specified, it will use the defaults
#' @param model_input A list/json object with "data", which is a dataset containing patients' data,
#' along with the names of each columns in the "data" assigned for
#' patid, gender, age,
#' atrial_fibrillation, atypical_antipsy,
#' regular_steroid_tablets, erectile_disfunction,
#' migraine, rheumatoid_arthritis,
#' chronic_kidney_disease, severe_mental_illness,
#' systemic_lupus_erythematosis,
#' blood_pressure_treatment, diabetes1,
#' diabetes2, weight="weight", height,
#' ethiniciy, heart_attack_relative,
#' cholesterol_HDL_ratio, systolic_blood_pressure,
#' std_systolic_blood_pressure, smoke, townsend)
#' @return Return a dataset with three columns: patient identifier, caculated QRISK3 score,
#' caculated QRISK3 score with only 1 digit
model_run <- function(model_input)
{
  #Template is taken from qrisk example (?QRISK3_2017). These are exactly the columns on the input data frame
  #WE dont give a damn about the string values. We want data frame matches the name of these variables.

  init_var_names <- names(c(patid="ID", gender="gender", age="age",
                      atrial_fibrillation="b_AF", atypical_antipsy="b_atypicalantipsy",
                      regular_steroid_tablets="b_corticosteroids", erectile_disfunction="b_impotence2",
                      migraine="b_migraine", rheumatoid_arthritis="b_ra",
                      chronic_kidney_disease="b_renal", severe_mental_illness="b_semi",
                      systemic_lupus_erythematosis="b_sle",
                      blood_pressure_treatment="b_treatedhyp", diabetes1="b_type1",
                      diabetes2="b_type2", weight="weight", height="height",
                      ethiniciy="ethrisk", heart_attack_relative="fh_cvd",
                      cholesterol_HDL_ratio="rati", systolic_blood_pressure="sbp",
                      std_systolic_blood_pressure="sbps5", smoke="smoke_cat", townsend="town"))


  #loop over the dataset and update init_var_names to only include variables that are supplied.
  args <- list()
  df_names <- colnames(model_input)

  for (var_name in init_var_names)
  {
    if(!is.na(match(var_name,df_names)))
    {
      args[var_name] <- var_name
    }
  }

  #Pass 2: now make sure there are no columns in the df that is not used. This might be an error that needs to be noticed.
  for(df_name in df_names)
  {
    if(is.na(match(df_name,args)))
    {
      stop(paste("Error: there are unused variables in the input: encountered"), df_name)
    }
  }

  args$data <- model_input
  do.call(QRISK3_2017, args)
}






prism_get_default_input <- function()
{
  df <- QRISK3_2019_test

  vars <- c(patid="ID", gender="gender", age="age",
                 atrial_fibrillation="b_AF", atypical_antipsy="b_atypicalantipsy",
                 regular_steroid_tablets="b_corticosteroids", erectile_disfunction="b_impotence2",
                 migraine="b_migraine", rheumatoid_arthritis="b_ra",
                 chronic_kidney_disease="b_renal", severe_mental_illness="b_semi",
                 systemic_lupus_erythematosis="b_sle",
                 blood_pressure_treatment="b_treatedhyp", diabetes1="b_type1",
                 diabetes2="b_type2", weight="weight", height="height",
                 ethiniciy="ethrisk", heart_attack_relative="fh_cvd",
                 cholesterol_HDL_ratio="rati", systolic_blood_pressure="sbp",
                 std_systolic_blood_pressure="sbps5", smoke="smoke_cat", townsend="town")

  df_names <- colnames(df)
  var_names <- names(vars)
  columns_to_delete <- c()
  for(i in 1:length(df_names))
  {
    matched_index <- match(df_names[i],vars)
    if(!is.na(matched_index))
    {
      colnames(df)[i] <- var_names[matched_index]
    }
    else
    {
      columns_to_delete <- c(columns_to_delete,i)
    }
  }

  if(length(columns_to_delete>0)) df <- df[,-columns_to_delete]

  df
}
















#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}



#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()

  nms<-names(lst)

  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }

  return(out)
}
