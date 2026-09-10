#' @title Advance population component one season step
#' 
#' @description This function will advance the population component one seasonal step
#' 
#' @param n_i - abundance vector by population class
#' @param p - list of population processes to apply
#' @param doInstProcesses1st - flag to do instantaneous processes (i.e., growth) first (TRUE/FALSE)
#' @param doGrowth - flag to evaluate growth (TRUE/FALSE)
#' @param ssbBeforeGrowth - flag to do spawning stock abundance calculation before growth (TRUE) or after (FALSE)
#' 
#' @return list including population abundance at end of time step (see Details)
#' 
#' @details The returned list has elements
#' 
#'   * n_f - final population abundance vector
#'   * n_ss - spawning stock abundance
#'   * c_fn - total catch abundance by fleet (not necessarily mortality)
#'   * rm_n - retained catch mortality (abundance) by fleet
#'   * dm_fn - discard catch mortality (abundance) by fleet
#' 
#' @md
#' @export
#' 
advance_pop_one_step<-function(n_i,p,doInstProcesses1st,doGrowth,ssbBeforeGrowth){
  #--evaluate instantantaneous processes before mortality
  if (doInstProcesses1st){
    if (doGrowth) {
      if (ssbBeforeGrowth)  n_ss = p$Tmat_msz %*% n_i;#--calc spawning stock numbers
      n_m = p$Tgr_msz %*% n_i;
      if (!ssbBeforeGrowth) n_ss = p$Tmat_msz %*% n_m;#--calc spawning stock numbers
    }
  } else n_m = n_i;

  #--calculate and apply fishery capture rates, fishery mortality rates, and total mortality
  ##--NOTE: all arrays here are 1-d, so no matrix multiplication needed
  Ft_n = 0.0*n_i; #--total fishing mortality "rate"
  Z_n  = 0.0*n_i; #--total mortality "rate"
  Cr_fn = array(0,dim=c(p$n_fsh_flts,length(n_i)));#--fleet-specific capture rates
  Fm_fn = array(0,dim=c(p$n_fsh_flts,length(n_i)));#--fleet-specific mortality rate
  for (f in p$n_fsh_flts){
    Cr_fn[f] = p$f_fn[f] * p$fsh_sel_fn[f];                                   #--fleet-specific capture rates
    Fm_fn[f] = Cr_fn[f]*(p$fsh_ret_fn[f] + p$fsh_hm[f]*(1.0-p$fsh_ret_fn[f]));#--fleet-specific mortality rate
    Ft_n    = Ft_n + Fm_fn[f];
  }
  Z_n = p$M_n * p$dt + Ft_n;  #--total mortality rate

  c_fn = array(0,dim=c(p$n_fsh_flts,length(n_i))); #--fleet-specific capture abundance
  rm_fn = array(0,dim=c(p$n_fsh_flts,length(n_i)));#--fleet-specific retained mortality abundance
  dm_fn = array(0,dim=c(p$n_fsh_flts,length(n_i)));#--fleet-specific discard mortality abundance
  for (f in p$fsh_flts){
    c_fn[f] = (Cr_fn[f]/Z_n) * n_m * exp(-Z_n);         #--total fleet-specific catch abundance (not necessarily mortality!)
    rm_fn[f] = c_fn[f]*p$fsh_ret_fn[f];        ;        #--fleet-specific retained mortality portion of catch (abundance)
    dm_fn[f] = c_fn[f]*(1-p$fsh_ret_fn[f])*p$fsh_hm[f]; #--fleet-specific discard mortality associated with catch (discards are c_fn[f]-rm_fn[f]) (abundance)
  }

  n_m = (1.0-exp(-Z)) * n_m; #--numbers after mortality

  #--evaluate instantantaneous processes before mortality
  if (doInstProcesses1st){
    if (doGrowth) {
      if (ssbBeforeGrowth)  n_ss = p$Tmat_msz %*% n_m;#--calc spawning stock numbers
      n_m = p$Tgr_msz %*% n_m;
      if (!ssbBeforeGrowth) n_ss = p$Tmat_msz %*% n_m;#--calc spawning stock numbers
    }
  }
  return(list(n_f=n_m,      #--final abundance
              n_ss=n_ss,    #--spawning stock abundance
              c_fn=c_fn,    #--total catch abundance by fleet (not necessarily mortality)
              rm_n=rm_n,    #--retained catch mortality (abundance) by fleet
              dm_fn=dm_fn)  #--discard catch mortality (abundance) by fleet
        );
}
