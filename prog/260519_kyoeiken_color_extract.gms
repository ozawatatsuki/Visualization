SET
Y /2020*2100/
R /WLD/
seg Income decile /1*1000, ALL/
var /
Cereals
Meats
Fish
Dairy
Oils_and_Fats
Fruits_and_Veg
Food_Other
total
/
Ref /
SSP2_BaU_NoCC_No
SSP2_400C_2025CP_NoCC_No
/
eff /
Base
Income
Price
"Income+Price"
/
;

PARAMETER
FoodDem_load_In(Ref,var,R,Y,seg)
FoodDem_load_InPr(Ref,var,R,Y,seg)
FoodDem_value(var,seg,eff)
FoodDem_base_add(var,seg,eff)
PoU_load_In(Ref,R,Y)
PoU_load_InPr(Ref,R,Y)
PoU_value(Y,eff)
PoU_base_add(Y,eff)
;

$if not set fdm_gdx $setglobal fdm_gdx C:/A_economy/Visualization/data/260402~1/Fooddem_PriceEffect_1000.gdx
$if not set out_gdx $setglobal out_gdx C:/A_economy/Visualization/output/kyoeiken/FDM_kyoeiken_260402.gdx

$gdxin "%fdm_gdx%"
$load FoodDem_load_In=FoodDemIn FoodDem_load_InPr=FoodDem_peffect PoU_load_In=PoU_In_Ragg_calc PoU_load_InPr=PoU_In_Pr_Ragg_calc

FoodDem_value(var,seg,"Base") = FoodDem_load_In("SSP2_BaU_NoCC_No",var,"WLD","2050",seg);
FoodDem_value(var,seg,"Income") = FoodDem_load_In("SSP2_400C_2025CP_NoCC_No",var,"WLD","2050",seg);
FoodDem_value(var,seg,"Income+Price") = FoodDem_load_InPr("SSP2_400C_2025CP_NoCC_No",var,"WLD","2050",seg);

FoodDem_base_add(var,seg,"Base") = FoodDem_value(var,seg,"Base");
FoodDem_base_add(var,seg,"Income") = FoodDem_value(var,seg,"Income") - FoodDem_value(var,seg,"Base");
FoodDem_base_add(var,seg,"Price") = FoodDem_value(var,seg,"Income+Price") - FoodDem_value(var,seg,"Income");

PoU_value(Y,"Base") = PoU_load_In("SSP2_BaU_NoCC_No","WLD",Y);
PoU_value(Y,"Income") = PoU_load_In("SSP2_400C_2025CP_NoCC_No","WLD",Y);
PoU_value(Y,"Income+Price") = PoU_load_InPr("SSP2_400C_2025CP_NoCC_No","WLD",Y);

PoU_base_add(Y,"Base") = PoU_value(Y,"Base");
PoU_base_add(Y,"Income") = PoU_value(Y,"Income") - PoU_value(Y,"Base");
PoU_base_add(Y,"Price") = PoU_value(Y,"Income+Price") - PoU_value(Y,"Income");

execute_unload "%out_gdx%"
FoodDem_base_add
PoU_value
PoU_base_add
;
