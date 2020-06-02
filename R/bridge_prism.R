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

  input<-unflatten_list(model_input)
  results <- QRISK3_2017(data       = model_input$data,
                         patid       = model_input$patid,
                         gender       = model_input$gender,
                         age       = model_input$age,
                         atrial_fibrillation       = model_input$atrial_fibrillation,
                         atypical_antipsy       = model_input$atypical_antipsy,
                         regular_steroid_tablets       = model_input$regular_steroid_tablets,
                         erectile_disfunction       = model_input$erectile_disfunction,
                         migraine       = model_input$migraine,
                         rheumatoid_arthritis       = model_input$rheumatoid_arthritis,
                         chronic_kidney_disease       = model_input$chronic_kidney_disease,
                         severe_mental_illness       = model_input$severe_mental_illness,
                         systemic_lupus_erythematosis       = model_input$systemic_lupus_erythematosis,
                         blood_pressure_treatment       = model_input$blood_pressure_treatment,
                         diabetes1       = model_input$diabetes1,
                         diabetes2       = model_input$diabetes2,
                         weight       = model_input$weight,
                         height       = model_input$height,
                         ethiniciy       = model_input$ethiniciy,
                         heart_attack_relative       = model_input$heart_attack_relative,
                         cholesterol_HDL_ratio       = model_input$cholesterol_HDL_ratio,
                         systolic_blood_pressure       = model_input$systolic_blood_pressure,
                         std_systolic_blood_pressure       = model_input$std_systolic_blood_pressure,
                         smoke       = model_input$smoke,
                         townsend       = model_input$townsend)


  return(as.list(results))
}

get_default_input <- function() {

  model_input <- list(data       = QRISK3_2019_test,
                      patid="ID", gender="gender", age="age",
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
  return((flatten_list(model_input)))
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
