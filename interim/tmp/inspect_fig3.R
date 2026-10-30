Sys.setlocale("LC_CTYPE", ".UTF-8")
g <- gamstransfer::Container$new()
g$read("data/FDM/ave修正後/Fooddem_PriceEffect.gdx", symbols=c("FPchange", "priceEla", "Population_seg", "budgetinitial"))
for (s in c("FPchange", "priceEla", "Population_seg", "budgetinitial")) {
 cat("\n",s,"\n"); print(head(g[s]$records)); print(sapply(g[s]$records, function(x) length(unique(x))))
}
