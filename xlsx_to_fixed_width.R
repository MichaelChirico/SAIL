#Structured, Active, In-Class Learning
#  Randomized Trial
#University of Pennsylvania
#Fall, 2015; MATH 104
#Michael Chirico and Rebecca Maynard

#File to convert .xlsx file to preferred-format
#  fixed-width file

setwd("~/Desktop/research/SAIL_maynard_et_al")
library(xlsx)
library(data.table)

input<-setDT(read.xlsx2(
  "finalfilejuly28_output.xlsx",sheetIndex=1,header=F,
  colClasses="character",stringsAsFactors=F),
  keep.rownames=T)
gc()

#add spaces as necessary to troublesome columns
input[,rn:=sprintf("%06d",as.integer(rn))]
input[,X1:=sprintf("%04s",X1)]
input[,X2:=sprintf("%09s",X2)]
input[,X4:=sprintf("%02s",X4)]
input[,X6:=sprintf("%010s",X6)]
input[,X7:=sprintf("%02s",X7)]
input[,X8:=sprintf("%04d",as.integer(X8))]
input[,X9:=sprintf("%02s",X9)]
main<-input[,paste0(rn,X1,X2,X3,X4,X5,X6,X7,X8,X9)]
output<-c("=COLS> ----+----1----+----2----+----3----+----4----+-",
          "****** ***************************** Top of Data ****",
          main)
write.table(output,file="finalfilejuly28_output_fwf.txt",
            row.names=F,col.names=F,quote=F)
