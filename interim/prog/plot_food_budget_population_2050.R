# Country distributions from the two GDX files in data/FDM/最新版.
# Run from any directory. No packages are installed by this script.
Sys.setlocale('LC_CTYPE', '.UTF-8')
options(warn=1)
.libPaths(c('C:/Users/TatsukiOzawa/AppData/Local/R/win-library/4.4', .libPaths()))
suppressPackageStartupMessages({library(data.table);library(ggplot2)})
args <- commandArgs(FALSE)
script <- normalizePath(sub('^--file=', '', grep('^--file=', args, value=TRUE)[1]), winslash='/')
root <- dirname(dirname(script))
input <- file.path(root,'data','FDM','最新版')
food_out <- file.path(root,'output','food_pop'); budget_out <- file.path(root,'output','budget_pop')
dir.create(food_out,recursive=TRUE,showWarnings=FALSE);dir.create(budget_out,recursive=TRUE,showWarnings=FALSE)
year <- '2050'
refs <- c('SSP2_BaU_NoCC_No','SSP2_400C_2025CP_NoCC_No','SSP2_400C_2030CP_NoCC_No')
ref_labels <- c('BaU','400C: policy starts 2025','400C: policy starts 2030')
read_symbol <- function(file,sym) {
 cat('Read',sym,'\n');g<-gamstransfer::Container$new();g$read(file,symbols=sym)
 d<-as.data.table(g[sym]$records);for(n in names(d))if(is.factor(d[[n]]))set(d,j=n,value=as.character(d[[n]]))
 domain<-switch(sym,FoodDem_peffect_CR=c('Ref','var','R','Y','seg'),Population_seg=c('Ref','R','Y','seg'),FoodDemValid=c('Ref','R','Y','seg'),MDER=c('Ref','R','Y'),BudgetInitial='seg',FreqSegall_exp=c('Ref','R','Y','seg'),EV=c('Ref','R','seg','Y'))
 stopifnot(ncol(d)==length(domain)+1L);setnames(d,c(domain,'value'))
 if('Y'%in%names(d))d<-d[Y==year]
 if('Ref'%in%names(d))d<-d[Ref%in%refs]
 if('Seg'%in%names(d))setnames(d,'Seg','seg')
 if('seg2'%in%names(d))setnames(d,'seg2','seg')
 if('seg'%in%names(d))d<-d[seg!='ALL']
 d
}
foodfile<-file.path(input,'Fooddem_PriceEffect.gdx');analysisfile<-file.path(input,'AnalysisExpenditure.gdx')
cache<-file.path(food_out,'plot_data_2050.rds')
if(!('--render-only'%in%commandArgs(TRUE))) {
 food<-read_symbol(foodfile,'FoodDem_peffect_CR')[var=='total',.(Ref,R,Y,seg,food=value)]
 pop<-read_symbol(foodfile,'Population_seg');setnames(pop,'value','population')
 valid<-read_symbol(foodfile,'FoodDemValid');setnames(valid,'value','valid')
 md<-read_symbol(foodfile,'MDER');setnames(md,'value','MDER')
 budget<-read_symbol(foodfile,'BudgetInitial');setnames(budget,'value','BudgetInitial')
 freq<-read_symbol(analysisfile,'FreqSegall_exp');setnames(freq,'value','frequency')
 ev<-read_symbol(analysisfile,'EV');setnames(ev,'value','EV')
 keys<-c('Ref','R','Y','seg')
 stopifnot(!anyDuplicated(food[,..keys]),!anyDuplicated(pop[,..keys]),!anyDuplicated(freq[,..keys]),!anyDuplicated(ev[,..keys]))
 food<-merge(food,pop,by=keys,all.x=TRUE);food<-merge(food,valid,by=keys,all.x=TRUE);food[is.na(valid),valid:=0]
 food<-merge(food,md,by=c('Ref','R','Y'),all.x=TRUE)
 stopifnot(!anyNA(food$population),!anyNA(food$MDER),all(is.finite(food$food)),all(food$food>0))
 food<-food[population>0 & !grepl('^R17',R) & R!='WLD']
 income<-merge(freq,budget,by='seg',all.x=TRUE);income<-merge(income,ev,by=keys,all.x=TRUE)
 income[,ev_absent:=is.na(EV)]
 # GDX parameters are sparse: absent EV records represent zero. Record this explicitly.
 income[is.na(EV),EV:=0]
 income[,income:=BudgetInitial*(1+EV)]
 income<-income[frequency>0 & !grepl('^R17',R) & R!='WLD']
 stopifnot(!anyNA(income$BudgetInitial),all(is.finite(income$income)),all(income$income>0))
 fwrite(income[ev_absent==TRUE,.(Ref,R,Y,seg,EV)],file.path(budget_out,'absent_EV_treated_as_zero.csv'))
 fwrite(food,file.path(food_out,'food_population_2050.csv'))
 fwrite(income,file.path(budget_out,'budget_frequency_2050.csv'))
 coverage<-rbindlist(list(food[,.(kind='food',n_segments=.N,sum_y=sum(population),missing_EV=0L),by=.(R,Ref)],income[,.(kind='budget',n_segments=.N,sum_y=sum(frequency),missing_EV=sum(ev_absent)),by=.(R,Ref)]))
 fwrite(coverage,file.path(food_out,'coverage_both_plots.csv'))
 saveRDS(list(food=food,income=income),cache)
 cat('FOOD countries:',uniqueN(food$R),'BUDGET countries:',uniqueN(income$R),'\n')
 print(income[,.(n=.N,absent_EV=sum(ev_absent),min_income=min(income),max_income=max(income)),by=Ref])
 if('--prepare-only'%in%commandArgs(TRUE))quit(status=0)
} else {d<-readRDS(cache);food<-d$food;income<-d$income}
weighted_q <- function(x,w,p=.999){o<-order(x);x<-x[o];w<-w[o];x[which(cumsum(w)>=p*sum(w))[1]]}
colors<-c('#247BA0','#E59834','#8D5BB7')
theme_set(theme_minimal(base_size=11)+theme(legend.position='bottom',plot.title=element_text(size=17,face='bold'),plot.caption=element_text(size=9,hjust=0)))
theme_dist<-theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),strip.text=element_text(face='bold'),legend.position='bottom',plot.title=element_text(face='bold'),plot.caption=element_text(hjust=0,size=9),axis.text.x=element_text(size=9))
summary_rows<-list()
make_plot<-function(d,kind,country){
 d<-copy(d);food_kind<-kind=='food'
 d[,`:=`(x=if(food_kind)food else income,y=if(food_kind)population else frequency)]
 d[,scenario:=factor(Ref,levels=refs,labels=ref_labels)]
 cuts<-d[,.(q=weighted_q(x,y)),by=Ref]
 thresholds<-if(food_kind)unique(d$MDER) else c(3,4.2,8.3)*365
 xmax<-max(c(cuts$q,thresholds))*1.08
 d[,scenario_color:=scenario]
 # One bar per original income segment, centered at its actual x value. No binning.
 # A constant narrow width prevents the variable spacing from changing the meaning of heights.
 xs<-sort(unique(d$x[d$x<=xmax]));spacing<-diff(xs);spacing<-spacing[spacing>xmax*1e-8]
 width<-xmax/400
 overview<-d[,.(n=.N,total=sum(y),outside=sum(y[x>xmax]),outside_percent=100*sum(y[x>xmax])/sum(y)),by=.(R,Ref)]
 overview[,`:=`(kind=kind,linear_xmax=xmax)]
 summary_rows[[length(summary_rows)+1]]<<-overview
 xlabel<-if(food_kind)'Final food demand (kcal / person / day)' else 'Income = BudgetInitial x (1 + EV) (annual)'
 ylabel<-if(food_kind)'Population_seg (people / segment)' else 'FreqSegall_exp (raw value / segment)'
 base<-ggplot(d,aes(x=x,y=y,fill=scenario))+scale_fill_manual(values=colors,guide='none',drop=FALSE)+facet_wrap(~scenario,ncol=1,drop=FALSE)+scale_y_continuous(labels=scales::label_number(scale_cut=scales::cut_short_scale()))+labs(x=xlabel,y=ylabel)+theme_dist
 main<-base+geom_col(width=width,position='identity')+coord_cartesian(xlim=c(0,xmax),expand=FALSE)+scale_x_continuous(labels=scales::label_number(big.mark=','))+labs(title='Main distribution (linear x)')
 if(food_kind){
  lines<-unique(d[,.(scenario,MDER)])
  main<-main+geom_vline(data=lines,aes(xintercept=MDER),color='#C3333E',linetype='dashed',linewidth=.65)
  note<-paste0('Red dashed line: MDER = ',paste(format(sort(unique(d$MDER)),digits=7),collapse=', '),' kcal/person/day. ')
 } else {
  lines<-data.table(x=thresholds,label=factor(c('3.00/day = 1,095/year','4.20/day = 1,533/year','8.30/day = 3,029.5/year'),levels=c('3.00/day = 1,095/year','4.20/day = 1,533/year','8.30/day = 3,029.5/year')))
  add_lines<-function(p)p+geom_vline(data=lines,aes(xintercept=x,color=label),linetype='dashed',linewidth=.6,show.legend=TRUE)+scale_color_manual(values=c('#C3333E','#B66A10','#304C89'),name='Poverty thresholds')
  main<-add_lines(main);note<-'Thresholds: 3.00, 4.20, 8.30 per day x 365. Frequency values are not normalized. '
 }
 max_out<-max(overview$outside_percent)
 note<-paste0(note,'\nCountry-specific linear range covers >=99.9% of the plotted population/frequency in every scenario; outside <=',format(max_out,digits=3),'%.\nOne bar per income segment; bar HEIGHT is population/frequency, not a density. ALL is excluded.')
 if(food_kind&&country%in%c('ATG','FSM','GRD','KIR','SDN','SYC'))note<-paste0(note,'\nSource-data note: food price mapping is absent for this country; direct price response is not applied.')
 main+labs(title=paste0(country,' | 2050 | ',if(food_kind)'Final food demand and population' else 'Income and expenditure frequency'),subtitle='Country-specific x range (linear scale)',caption=note)+theme(plot.title=element_text(size=17,face='bold'))
}
for(kind in c('food','budget')){
 d<-if(kind=='food')food else income;out<-if(kind=='food')food_out else budget_out
 countries<-sort(unique(d$R));if('--preview'%in%commandArgs(TRUE))countries<-intersect(c('AFG','USA','SDN'),countries)
 pdf_path<-file.path(out,paste0(kind,'_pop_2050_all_countries.pdf'))
 grDevices::pdf(pdf_path,width=13,height=10.5,onefile=TRUE)
 for(i in seq_along(countries)){
  country<-countries[i];p<-make_plot(d[R==country],kind,country)
  print(p)
  ggsave(file.path(out,paste0(country,'_',kind,'_pop_2050.png')),p,width=13,height=10.5,dpi=150,device=ragg::agg_png,bg='white')
  if(i%%10==0||i==length(countries))cat(kind,i,'/',length(countries),'\n')
 }
 dev.off()
}
summ<-rbindlist(summary_rows);fwrite(summ,file.path(food_out,'plot_range_summary_both.csv'));fwrite(summ[kind=='budget'],file.path(budget_out,'plot_range_summary.csv'))
for(out in c(food_out,budget_out))writeLines(c('2050 country distributions; three source scenarios are shown in separate rows.','Source: interim/data/FDM/最新版/Fooddem_PriceEffect.gdx and AnalysisExpenditure.gdx.','Food: var=total; ALL excluded. Population_seg is used as stored.','Income: BudgetInitial*(1+EV); FreqSegall_exp is used as stored (not normalized).','GDX sparse missing EV records are interpreted as zero, with a CSV audit.','Country-specific linear x range up to the largest scenario-specific weighted 99.9th percentile, extended to include all thresholds.','No logarithmic axes. Tail records outside the plotting range remain in the CSV; their share is reported in the figure.','One bar per segment. Heights, not areas, represent population/frequency.','All-country PDF and one PNG per country. Input data and existing model code are not modified.'),file.path(out,'README.txt'))
cat('Finished.\n')




