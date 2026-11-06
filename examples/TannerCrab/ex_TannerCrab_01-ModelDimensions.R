#--Tanner crab model setup

##--population dimensions
#| label: tbl-PopDimsTannerCrab
  x = c("male","female");         x = factor(x,levels=x);
  m = c("immature","mature");     m = factor(m,levels=m);
  c = c("new shell","old shell"); c = factor(c,levels=c);
  zc = seq(from=25,to=185,by=5);                    #--size bin cutpoints
  zb = (zc[2:length(zc)] + zc[1:(length(zc)-1)])/2; #--size bin centers
  mc = dplyr::bind_rows(list(m=m[1],c=c[1]),list(m=m[2],c=c)); # create dataframe with shell condition nested inside maturity
  tblPopDims = tidyr::expand_grid(x,mc,zb) |>                  #--create expanded dataframe
               dplyr::rename(z=zb) |>                          #--rename 'zb' column to canonical 'z'
               dplyr::mutate(rid=dplyr::row_number(),.before=1);         #--add row id column
  cap = "Tanner crab population dimensions dataframe. Only classes with size bins < 100 mm CW are shown."
  nc = ncol(tblPopDims);
  kableExtra::kbl(tblPopDims |> dplyr::filter(z<100),col.names=c("row_id",dimsPop),booktabs=TRUE,caption=cap);
  
##--fleets
  tblFlts = tibble::tribble(
              ~fleet, ~description,
              "TCF",  "directed fishery",
              "SCF",  "snow crab fishery",
              "RKF",  "BBRKC fishery",
              "GFA",  "Groundfish fisheries",
              "NMFS", "NMFS EBS trawl survey",
              "BSFSF","Bering Sea Research Foundation trawl survey"
            );
  cap = "Tanner crab fleet dimensions dataframe. "
  kableExtra::kbl(tblFlts,booktabs=TRUE,caption=cap);

##--model time frame
  y = 1948:2025;
  s = 1:7
  tblYS = tidyr::expand_grid(year=y,season=s);
  cap = "Tanner crab model time frame dimensions dataframe. "
  kableExtra::kbl(tblYS,booktabs=TRUE,caption=cap);
 
 tblModDims = tidyr::expand_grid(tblYS,tblFlts |> dplyr::select(f),tblPopDims);
 