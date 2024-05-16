
if(buff_unbuff=="Buffered"){
  write.csv(result_df,paste0(getwd(),"/SCR_paper/Data/Output_data/Buffered/Buffered_", x, "_.csv"))}
if(buff_unbuff=="Unbuffered"){
  write.csv(result_df,paste0(getwd(),"/SCR_paper/Data/Output_data/Unbuffered/Unbuffered_",x, "_.csv"))}