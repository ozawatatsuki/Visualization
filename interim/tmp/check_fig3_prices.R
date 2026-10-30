Sys.setlocale("LC_CTYPE", ".UTF-8")
library(dplyr)
library(tidyr)
g <- gamstransfer::Container$new()
g$read("data/FDM/ave修正後/Fooddem_PriceEffect.gdx", symbols="FPchange")
d <- g["FPchange"]$records |> as_tibble() |>
  transmute(scenario=as.character(Ref), item=as.character(var2), country=as.character(R), year=as.character(Y), value) |>
  filter(year=="2050", scenario %in% c("SSP2_BaU_NoCC_No", "SSP2_400C_2030CP_NoCC_No")) |>
  pivot_wider(names_from=scenario, values_from=value) |>
  mutate(change=100*(SSP2_400C_2030CP_NoCC_No/SSP2_BaU_NoCC_No-1))
print(d |> group_by(item) |> summarise(n=n(), baseline_min=min(SSP2_BaU_NoCC_No), baseline_max=max(SSP2_BaU_NoCC_No), change_min=min(change), change_max=max(change), spread=max(change)-min(change)), width=Inf)
print(d |> filter(country %in% c("IND","USA","NGA","BRA","RUS","WLD")), n=50, width=Inf)
g$read("data/FDM/ave修正後/Fooddem_PriceEffect.gdx", symbols="FPindex")
p <- g["FPindex"]$records |> as_tibble() |>
 transmute(scenario=as.character(Ref), item=as.character(var), region=as.character(R), year=as.character(Yall), value) |>
 filter(year=="2050", scenario %in% c("SSP2_BaU_NoCC_No","SSP2_400C_2030CP_NoCC_No"))
m <- read.table("data/FDM/ave修正後/region_iso_R17toR.map", col.names=c("region","country")) |>
 mutate(region=sub("\\.$","",region)) |> count(region)
pred <- p |> inner_join(m,by="region") |> group_by(scenario,item) |>
 summarise(predicted=sum(value*n),.groups="drop")
print(p |> filter(region=="IND") |> inner_join(pred,by=c("scenario","item")) |>
 transmute(scenario,item,actual=value,predicted,error=value-predicted),n=14)
