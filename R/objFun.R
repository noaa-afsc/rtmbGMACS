#' The objective function
#' 
#' This function calculates the objective function value from a list of 
#' input objects and a list of parameter values.
#' 
#' @param input - data: list of non-AD objects
#' @param params - list of parameters 
#' 
#' @export
#' 
objFun<-function(input,params){
  #--overload functions
  "[<-" <- ADoverload("[<-");
  "c" <- ADoverload("c");
  "diag<-" <- ADoverload("diag<-");

  #--initialize the objective function
  objfun <- numeric(1);

  #--get the model dimensions
  dimsPop = input$dims$tblDims_Pop; #--nominal columns x,m,p,z
  dimsFlt = input$dims$tblDims_Flt; #--columns f,y,s,fm_type
  dimsYrS = input$dims$tblDims_YrS; #--columns y,s

  #--get the model transitions scaffold tables
  ##--table structure: "from_" columns, "to_columns", from_id, to_id, transition indicator
  ##----transition indicator values: 0: no transition; 1: forward transition; -1: 1-sum(1's)
  tblScaff_Mlt = input$scaffolds$tblMlt; #--"scaffold" matrix for molting process
  tblScaff_Grw = input$scaffolds$tblGrw; #--"scaffold" matrix for growth process
  tblScaff_Mat = input$scaffolds$tblMat; #--"scaffold" matrix for maturity process

  #--dummy calculation for objective function
  objfun <- objfun + (params$dummy)^2;

  return(objfun);
}