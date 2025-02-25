library(ggplot2)
library(ggbiplot)
library(readxl)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
substrLeft <- function(x, n){
  substr(x, 1, n)
}

'%!in%'<-function(x,y)!('%in%'(x,y))

my_theme<-theme_bw() + theme(axis.line=element_line(color="black"),axis.ticks = element_line(color="black"),axis.text = element_text(color="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), text=element_text(size = 10,color="black"))
my_theme2<-theme_bw() + theme(axis.line=element_line(color="black"),axis.ticks = element_line(color="black"),axis.text = element_text(color="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), text=element_text(size = 10,color="black"),axis.text.x = element_text(angle=30,hjust = 1))

setwd("C:/Users/mksch/Dropbox/matt_data/crosson_lab_msu_schnizlein/students/MMG_434/MGI_434_WINTER2025/student_data/")


spreadsheet_vector <- list.files("./")

spreadsheet_list<-lapply(X = spreadsheet_vector, FUN =  read_excel)
# spreadsheet_list[[1]]
#  i<- 1

names(spreadsheet_list)<- spreadsheet_vector %>% gsub("\\.xlsx","",.)

for (i in 1:length(spreadsheet_list)){
  colnames(spreadsheet_list[[i]]) <- c("empty","well","od590","od750")
  spreadsheet_list[[i]] <- spreadsheet_list[[i]][24:nrow(spreadsheet_list[[i]]),c("well","od590","od750")]
  spreadsheet_list[[i]]$sampleID <- names(spreadsheet_list[i])
}

spreadsheet_df <- bind_rows(spreadsheet_list)

spreadsheet_df$groupID <- gsub("_.*","",spreadsheet_df$sampleID)
spreadsheet_df$time <- gsub(".*_","",spreadsheet_df$sampleID)

write.csv(x = spreadsheet_df, file = "combined_ecoplate_data_winter2025.csv",row.names = FALSE)





