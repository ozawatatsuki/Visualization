fileFDM <- paste0("data/FDM_kyoeiken.gdx")
fileFAO <- paste0("data/HungerTool.gdx")

df_FDM <- rgdx.param(fileFDM,"PoU_value") |> 
  filter(eff!="Income") |> 
  mutate(U="FDM",
         PoU_value=PoU_value/1000000,
         Ref = if_else(eff == "Base", "Baseline", "400C")) |> 
  select(-eff) |> 
  rename(value=PoU_value)
df_FAO <- rgdx.param("data/HungerTool.gdx","Risk_agg") |> 
  filter(U=="fao_based",str_detect(S, "SSP2"),R_agg=="WLD") |> 
  rename(Ref=S,R=R_agg,Y=T) |> 
  mutate(Ref = if_else(Ref == "SSP2_BaU_NoCC_No", "Baseline", "400C"))|> 
  rename(value=Risk_agg) |> 
  select(-R) |> 
  filter(Y %in% df_FDM$Y)
df_comp <- rbind(df_FDM,df_FAO)