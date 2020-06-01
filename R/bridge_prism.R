model_run<-function(model_input = NULL)
{

  input<-unflatten_list(model_input)
  # replace the function below with main model function takes model inputs and returns the output.
  # for example, for bode package we will have:
  # results <- bode              (FEV1                   =model_input$FEV1,
  #                               mMRC                   =model_input$mMRC,
  #                               BMI                    =model_input$BMI,
  #                               walk                   =model_input$walk)
  #
  # for cfmortality package, we will have:
  # results <- predictcfmortality(age                    =model_input$age,
  #                               male                   =model_input$male,
  #                               fvc                    =model_input$fvc,
  #                               fev1                   =model_input$fev1,
  #                               fev1LastYear           =model_input$fev1LastYear,
  #                               bcepacia               =model_input$bcepacia,
  #                               underweight            =model_input$underweight,
  #                               nHosp                  =model_input$nHosp,
  #                               pancreaticInsufficient =model_input$pancreaticInsufficient,
  #                               CFRelatedDiabetes      =model_input$CFRelatedDiabetes,
  #                               ageAtDiagnosis         =model_input$ageAtDiagnosis        )

  return(as.list(results))
}


get_default_input <- function() {
  # replace the function below with default model inputs for the new Prism model.
  # for example, for bode package we will have:
  # model_input <- list(FEV1                   = 40,
  #                     mMRC                   = 3,
  #                     BMI                    = 22,
  #                     walk                   = 100)
  #
  # for cfmortality package, we will have:
  # model_input <- list(age                    = 16,
  #                     male                   = 0,
  #                     fvc                    = 66.7,
  #                     fev1                   = 47.4,
  #                     fev1LastYear           = 80.5,
  #                     bcepacia               = 0,
  #                     underweight            = 0,
  #                     nHosp                  = 0,
  #                     pancreaticInsufficient = 1,
  #                     CFRelatedDiabetes      = 0,
  #                     ageAtDiagnosis         = 0.9)
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
