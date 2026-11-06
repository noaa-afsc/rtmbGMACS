#--state transition matrix examples
dirPrj = here::here();
source(file.path(dirPrj,"R/Functions-Text.R"));
source(file.path(dirPrj,"R/Functions-Matrices.R"));

##--define model dimensions
sex_states = c("m","f");      #--sexes
mat_states = c("imm","mat");  #--maturity state
pma_states = c("ns","os");    #--"post molt age" states (otherwise known as shell condition)
size_bins  = seq(27.5,52.5,5) #--size bins
##--define model frame
dfrMF = tidyr::expand_grid(x=sex_states,m=mat_states,p=pma_states,z=size_bins) |> 
        dplyr::mutate(id=dplyr::row_number());
cat(writeToStr_Dataframe(dfrMF))

##--create state transitions associated with molting for Chionoecetes spp. (mature crab don't molt)
###--immature, new shell crab may molt (imm,ns->imm,ns) or not (imm,ns->imm,os)
###--immature, old shell crab may molt (imm,os->imm,ns) or not (imm,os->imm,os)
###--mature, new shell crab don't change state (can't molt) (mat,ns->mat,ns)
###--mature, old shell crab don't change state (can't molt) (mat,os->mat,os)
###--create complete state transitions map dataframe for molt probabilities
dfrSTF = tidyr::expand_grid(m=mat_states,p=pma_states) |> 
         dplyr::mutate(sid=dplyr::row_number());
cat(writeToStr_Dataframe(dfrSTF))
##--create defined state transitions map dataframe for Chionoecetes spp. 
###--immature crab may molt, mature crab don't
dfrSTs = tibble::tribble(~from,~to,~val,
                            1,   1,  1,   # T(1,1) = p_molt(ns)              <= imm,ns can molt, stays imm,ns
                            1,   2, -1,   # T(1,2) = 1-Sum_{j ne 2}{T[2,j]}  <= imm,ns doesn't molt, becomes imm,os
                         #  1,   3,  0,   # T(1,3) = 0                       <= imm,ns can't become mat,ns
                         #  1,   4,  0,   # T(1,4) = 0                       <= imm,ns can't become mat,os
                            2,   1,  1,   # T(2,1) = p_molt(os)              <= imm,os can molt, becomes imm,ns
                            2,   2, -1);  # T(2,2) = 1-Sum_{j ne 2}{T[2,j]}  <= imm,os doesn't molt, stays imm,os
                         #  2,   3,  0,   # T(2,3) = 0                       <= imm,os can't become mat,ns
                         #  2,   4,  0,   # T(2,4) = 0                       <= imm,os can't become mat,os
                         #  3,   1,  0,   # T(3,1) = 0                       <= mat,ns can't become imm,ns
                         #  3,   2,  0,   # T(3,2) = 0                       <= mat,ns can't become imm,os
                         #  3,   3,  0,   # T(3,3) = 1                       <= mat,ns stays mat,ns
                         #  3,   4,  0,   # T(3,4) = 0                       <= mat,ns can't become mat,os
                         #  4,   1,  0,   # T(4,1) = 0                       <= mat,os can't become imm,ns
                         #  4,   2,  0,   # T(4,2) = 0                       <= mat,os can't become imm,os
                         #  4,   3,  0,   # T(4,3) = 0                       <= mat,os can't become mat,ns
                         #  4,   4,  0,   # T(4,4) = 1                       <= mat,os stays mat,os
 cat(writeToStr_Dataframe(dfrSTs))
 lst = createStateTransitionMatrixMap(dfrSTF,dfrSTs);
 cat(writeToStr_Dataframe(lst$dfrT))
 cat(writeToStr_Matrix(lst$mT))

##--create state transitions associated with maturation for Chionoecetes spp.
###--immature, new shell crab may mature (imm,ns->mat,ns) or stay immature (imm,ns->imm,ns)
###--immature, old shell crab can not mature (didn't molt) (imm,os->imm,os)
###--mature, new shell crab become old shell (didn't molt) (mat,ns->mat,os)
###--mature, old shell crab stay old shell (didn't molt) (mat,os->mat,os)
###--create complete state transitions map dataframe for maturity probabilities
dfrSTF = tidyr::expand_grid(m=mat_states,p=pma_states) |> 
         dplyr::mutate(sid=dplyr::row_number());
cat(writeToStr_Dataframe(dfrSTF))
##--create defined state transitions map dataframe
dfrSTs = tibble::tribble(~from,~to,~val,
                            1,   1, -1,   # T(1,1) = 1-Sum_{j ne 1}{T[1,j]} <= imm,ns stays imm,ns w/ prob 1-p_mature
                         #  1,   2,  0,   # T(1,2) = 0                      <= imm,ns can't become imm,os
                            1,   3,  1,   # T(1,3) = p_mature               <= imm,ns becomes mat,ns w/ prob p_mature
                         #  1,   4,  0,   # T(1,4) = 0                      <= imm,ns can't become mat,os
                         #  2,   1,  0,   # T(2,1) = 0                      <= imm,os can't become imm,ns
                         #  2,   2,  1,   # T(2,2) = 1                      <= imm,os stays imm,os
                         #  2,   3,  0,   # T(2,3) = 0                      <= imm,os can't become mat,ns
                         #  2,   4,  0,   # T(2,4) = 0                      <= imm,os can't become mat,os
                         #  3,   1,  0,   # T(3,1) = 0                      <= mat,ns can't become imm,ns
                         #  3,   2,  0,   # T(3,2) = 0                      <= mat,ns can't become imm,os
                         #  3,   3,  0,   # T(3,3) = 0                      <= mat,ns can't stay mat,ns
                            3,   4,  1);  # T(3,4) = 1                      <= mat,ns must become mat,os
                         #  4,   1,  0,   # T(4,1) = 0                      <= mat,os can't become imm,ns
                         #  4,   2,  0,   # T(4,2) = 0                      <= mat,os can't become imm,os
                         #  4,   3,  0,   # T(4,3) = 0                      <= mat,os can't become mat,ns
                         #  4,   4,  1,   # T(4,4) = 1                      <= mat,os stays mat,os
cat(writeToStr_Dataframe(dfrSTs))
lst = createStateTransitionMatrixMap(dfrSTF,dfrSTs);
cat(writeToStr_Dataframe(lst$dfrT))
cat(writeToStr_Matrix(lst$mT))
##
## Add a third shell condition and create a molt 
pma_states = c("ns","os1","os2");    #--"post molt age" states
##--create (new) model frame
dfrMF = tidyr::expand_grid(x=sex_states,m=mat_states,p=pma_states,z=size_bins) |> 
        dplyr::mutate(id=dplyr::row_number());
##--create state transitions associated with molting based Chionoecetes spp. (mature crab don't molt)
###--imm, ns crab  may molt (imm,ns ->imm,ns) or not (imm,ns ->imm,os1)
###--imm, os1 crab may molt (imm,os1->imm,ns) or not (imm,os1->imm,os2)
###--imm, os2 crab may molt (imm,os2->imm,ns) or not (imm,os2->imm,os2)
###--mature crab don't molt (can't change state) (mat,.->mat,.)
###--create complete state transitions map dataframe for molt probabilities
dfrSTF = tidyr::expand_grid(m=mat_states,p=pma_states) |> 
         dplyr::mutate(sid=dplyr::row_number());
cat(writeToStr_Dataframe(dfrSTF))
##--create defined state transitions map dataframe for Chionoecetes spp. 
###--immature crab may molt, mature crab don't
dfrSTs = tibble::tribble(~from,~to,~val,
                            1,   1,  1,   # T(1,1) = p_molt(ns)              <= imm,ns can molt, stays imm,ns
                            1,   2, -1,   # T(1,2) = 1-Sum_{j ne 2}{T[1,j]}  <= imm,ns doesn't molt, becomes imm,os1
                            2,   1,  1,   # T(2,1) = p_molt(os)              <= imm,os1 can molt, becomes imm,ns
                            2,   3, -1,   # T(2,3) = 1-Sum_{j ne 3}{T[2,j]}  <= imm,os1 doesn't molt, becomes imm,os2
                            3,   1,  1,   # T(3,1) = p_molt(os)              <= imm,os2 can molt, becomes imm,ns
                            3,   3, -1);  # T(3,3) = 1-Sum_{j ne 3}{T[3,j]}  <= imm,os2 doesn't molt, stays imm,os2
 cat(writeToStr_Dataframe(dfrSTs))
 lst = createStateTransitionMatrixMap(dfrSTF,dfrSTs);
 cat(writeToStr_Dataframe(lst$dfrT))
 cat(writeToStr_Matrix(lst$mT))
 dfrExpMF = dplyr::left_join(dfrMF,lst$dfrT,by=c("m","p"));
 cat(writeToStr_Dataframe(dfrExpMF)) 
 
