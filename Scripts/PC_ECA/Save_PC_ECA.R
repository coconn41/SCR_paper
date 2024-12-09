
if(buff_unbuff=="Buffered"){
  write.csv(result_df,paste0(getwd(),"/Data/Output_data/PC_ECA/Buffered/Buffered_", x, "_.csv"))}
if(buff_unbuff=="Unbuffered"){
  write.csv(result_df,paste0(getwd(),"/Data/Output_data/PC_ECA/Unbuffered/Unbuffered_",x, "_.csv"))}