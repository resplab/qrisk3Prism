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

model_run<-function(model_input = NULL)
{
  do.call(QRISK3_2017,model_input)
}




prism_get_default_input <- function() {
  QRISK3_2019_test
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
