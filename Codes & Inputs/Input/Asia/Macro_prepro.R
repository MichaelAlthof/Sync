rm(list = ls(all = TRUE))
# library(data.table)

wdir = "D:\\Files in Google Drive\\FRM\\Input\\Asia"
setwd(wdir)
macro_new=read.csv("Macro2.0China Credit YTM_20201030.csv",header = T)
macro_old=read.csv("20190102-20201030/Asia_Macro_20201030_1.0MA.csv",header = T)
colnames(macro_new)[1]="date"
colnames(macro_old)[1]="date"

macro_all=merge(macro_new,macro_old,by="date")

(Null_rows=which(rowSums(is.na(macro_all))!=0))

colnames(macro_all)
macro=macro_all[,c("date","CGB3M","FXI.US.EQUITY","VXFXI.INDEX")]
macro=cbind(macro,TED.SP=abs(macro_all[,"CGB3M"]-macro_all[,"SHIBOR3M"]),CN3M10YSLOPE=macro_all[,"CGB10Y"]-macro_all[,"CGB3M"],CREDIT.SP=macro_all[,"AAA10Y"]-macro_all[,"CGB10Y"])

macro_denull=macro[-Null_rows,]

write.csv(macro_denull,"20190102-20201030/Asia_Macro_20201030.csv",row.names = FALSE,quote = F)