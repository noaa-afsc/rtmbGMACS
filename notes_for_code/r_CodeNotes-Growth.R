#--create transition matrix structure for molting
##--for Tanner crab:
##--n_imm_ns <-    p_molt(imm,ns,z)  * n_imm_ns +    p_molt(imm,os,z)  * n_imm_os
##--n_imm_os <- (1-p_molt(imm,ns,z)) * n_imm_ns + (1-p_molt(imm,os,z)) * n_imm_os
##--n_mat_ns <- n_mat_ns
##--n_mat_os <- n_mat_os
##
##--for BBRKC:
##--n_ns <-    p_molt(ns,z)  * n_ns +    p_molt(os,z)  * n_os
##--n_os <- (1-p_molt(ns,z)) * n_ns + (1-p_molt(os,z)) * n_os
##
#--create transition matrix structure for growth
##--for Tanner crab:
##--n_imm_ns <- p_grw(imm,ns,z)  * n_imm_ns
##--n_imm_os <- n_imm_os
##--n_mat_ns <- n_mat_ns
##--n_mat_os <- n_mat_os
##
##--for BBRKC:
##--n_ns <- p_grw(ns,z)  * n_ns
##--n_os <- n_ns
##
#--create transition matrix structure for maturity (after molting or after growth)
##--for Tanner crab:
##--n_imm_ns <- (1-p_mat(imm,ns,z)) * n_imm_ns
##--n_imm_os <- n_imm_os
##--n_mat_ns <- p_mat(imm,ns,z)) * n_imm_ns
##--n_mat_os <- n_mat_ns + n_mat_os
##
##--for BBRKC:
##--n_ns <- n_ns
##--n_os <- n_ns
##
