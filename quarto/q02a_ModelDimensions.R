## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-StandardPopDimNames
#| tbl-cap: 'Names for "standard" population dimension variables. In addition to these, "y", "s", and "f" are reserved as year, season, and fleet variables, respectively.'
#| echo: false
  tbl = tibble::tribble(~variable,~description,
                        "x","sex",
                        "m","maturity state",
                        "c","shell condition",
                        "z","size (CL or CW, as appropriate)",
                        "r","region or area",
                        "p","post-molt age",
                        "a","age"
                       );
  kableExtra::kbl(tbl,booktabs=TRUE);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsEx1
  dimsPop = c("sex","maturity","size")
  x = c("male","female");
  m = c("immature","mature")
  z = seq(from=45,to=60,by=5);
  dfrPopDims = tidyr::expand_grid(x,m,z) |> dplyr::mutate(rid=dplyr::row_number());
  cap = "Population dimensions dataframe with sex, maturity state, and size classes defined."
  kableExtra::kbl(dfrPopDims,col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsEx2
  dfrPopDims = dfrPopDims |> dplyr::filter(!((x=="female") & (z>50))) |> dplyr::mutate(rid=dplyr::row_number());
  cap = "Population dimensions dataframe with females limited to $ \\le 50$."
  kableExtra::kbl(dfrPopDims,col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsEx3
  dimsPop = c("region","sex","maturity","size");
  r = c("east","west");
  x = c("male","female");
  m = c("immature","mature");
  z = seq(from=45,to=60,by=5);
  xz = dplyr::bind_rows(list(x="male",z=z),list(x="female",z=z[z<=50])); # create dataframe with z's nested inside x's
  dfrPopDims = tidyr::expand_grid(r,xz,m) |>            #--create expanded dataframe
               dplyr::select(r,x,m,z) |>                #--reorder columns
               dplyr::arrange(r,x,m,z) |>               #--reorder rows
               dplyr::mutate(rid=dplyr::row_number());  #--add row id column
  cap = "Alternative approach to creating a population dimensions dataframe with females limited to $ \\le 50$ (with additional category 'region')."
  kableExtra::kbl(dfrPopDims,col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsEx4
  dimsPop = c("region","sex","maturity","size");
  r = c("east","west");
  x = factor(c("male","female"),levels=c("male","female"));
  m = c("immature","mature");
  z = seq(from=45,to=60,by=5);
  xz = dplyr::bind_rows(list(x=x[1],z=z),list(x=x[2],z=z[z<=50])); # create dataframe with z's nested inside x's
  dfrPopDims = tidyr::expand_grid(r,xz,m) |>            #--create expanded dataframe
               dplyr::select(r,x,m,z) |>                #--reorder columns
               dplyr::arrange(r,x,m,z) |>               #--reorder rows
               dplyr::mutate(rid=dplyr::row_number());  #--add row id column
  cap = "Alternative approach to creating a population dimensions dataframe with females limited to $ \\le 50$ (with additional category 'region')."
  kableExtra::kbl(dfrPopDims,col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopTimeDimsEx1
  dimsPop = c("year","season","sex","maturity","size");
  y = 2025:2026;
  s = 1:2;
  x = factor(c("male","female"),levels=c("male","female"));
  m = c("immature","mature");
  z = seq(from=45,to=60,by=5);
  xz = dplyr::bind_rows(list(x=x[1],z=z),list(x=x[2],z=z[z<=50])); # create dataframe with z's nested inside x's
  dfrDimsP = tidyr::expand_grid(xz,m) |>              #--create expanded dataframe
               dplyr::select(x,m,z) |>                #--reorder columns
               dplyr::arrange(x,m,z) |>               #--reorder rows
               dplyr::mutate(rid=dplyr::row_number());  #--add row id column
  dfrDimsTP = tidyr::expand_grid(y,s,dfrDimsP);
  cap = "Expanding a population dimensions dataframe by year and season."
  kableExtra::kbl(dfrDimsTP,col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsTannerCrab
  dimsPop = c("sex","maturity","shell condition","size");
  x = c("male","female");         x = factor(x,levels=x);
  m = c("immature","mature");     m = factor(m,levels=m);
  c = c("new shell","old shell"); c = factor(c,levels=c);
  zc = seq(from=25,to=185,by=5);                    #--size bin cutpoints
  zb = (zc[2:length(zc)] + zc[1:(length(zc)-1)])/2; #--size bin centers
  mc = dplyr::bind_rows(list(m=m[1],c=c[1]),list(m=m[2],c=c)); # create dataframe with shell condition nested inside maturity
  dfrDimsP = tidyr::expand_grid(x,mc,zb) |>             #--create expanded dataframe
               dplyr::rename(z=zb) |>                   #--rename 'zb' column to canonical 'z'
               dplyr::mutate(rid=dplyr::row_number());  #--add row id column
  cap = "Tanner crab population dimensions dataframe. Only classes with size bins < 100 mm CW are shown."
  kableExtra::kbl(dfrDimsP |> dplyr::filter(z<100),col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsBBRKC
  dimsPop = c("sex","maturity","shell condition","size");
  x = c("male","female"); x = factor(x,levels=x);
  m = c("undetermined");  m = factor(m,levels=m);
  c = c("undetermined");  c = factor(c,levels=c);
  zc = seq(from=65,to=180,by=5);                    #--size bin cutpoints
  zb = (zc[2:length(zc)] + zc[1:(length(zc)-1)])/2; #--size bin centers
  xz = dplyr::bind_rows(list(x=x[1],z=zb),list(x=x[2],z=zb[1:18])); # create dataframe with z's nested inside x's
  dfrDimsP = tidyr::expand_grid(xz,m,c) |>              #--create expanded dataframe
               dplyr::select(x,m,c,z) |>                #--reorder columns
               dplyr::arrange(x,m,c,z) |>               #--reorder rows
               dplyr::mutate(rid=dplyr::row_number());  #--add row id column
  cap = "BBRKC population dimensions dataframe. Only classes with size bins < 100 mm CW are shown."
  kableExtra::kbl(dfrDimsP |> dplyr::filter(z>100),col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsNSRKC
  dimsPop = c("sex","maturity","shell condition","size");
  x = c("male");         x = factor(x,levels=x);
  m = c("mature");       m = factor(m,levels=m);
  c = c("undetermined"); c = factor(c,levels=c);
  zc = c(63.5, 73.5, 83.5, 93.5, 103.5, 113.5, 123.5, 133.5 ,143.5);   #--size bin cutpoints
  zb = (zc[2:length(zc)] + zc[1:(length(zc)-1)])/2;                    #--size bin centers
  dfrDimsP = tidyr::expand_grid(x,m,c,zb) |>            #--create expanded dataframe
               dplyr::rename(z=zb) |>                   #--rename 'zb' column to canonical 'z'
               dplyr::mutate(rid=dplyr::row_number());  #--add row id column
  cap = "NSRKC population dimensions dataframe."
  kableExtra::kbl(dfrDimsP,col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);


## --------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-PopDimsEAG
  dimsPop = c("sex","maturity","shell condition","size");
  x = c("male");         x = factor(x,levels=x);
  m = c("mature");       m = factor(m,levels=m);
  c = c("undetermined"); c = factor(c,levels=c);
  zc = seq(105.5,185.5,5);                           #--size bin cutpoints
  zb = (zc[2:length(zc)] + zc[1:(length(zc)-1)])/2;  #--size bin centers
  dfrDimsP = tidyr::expand_grid(x,m,c,zb) |>            #--create expanded dataframe
               dplyr::rename(z=zb) |>                   #--rename 'zb' column to canonical 'z'
               dplyr::mutate(rid=dplyr::row_number());  #--add row id column
  cap = "EAG population dimensions dataframe."
  kableExtra::kbl(dfrDimsP,col.names=c(dimsPop,"row_id"),booktabs=TRUE,caption=cap);

