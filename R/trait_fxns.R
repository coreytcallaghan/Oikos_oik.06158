read_trait_data_in<-function(){
  require(readr)
traits<-read_csv("Data/Raw trait data/Australian_Bird_Data_Version_1.csv")
         traits$binom<- paste(traits$`4_Genus_name_2`, traits$`5_Species_name_2`,sep="_")
         return(traits)
}


read_process_trait_data<-function(){
  traits<-read_trait_data_in()
  traits %>%
    mutate(body_mass=ifelse(`99_Body_mass_average_8`=="NAV",NA,as.numeric(`99_Body_mass_average_8`))) %>%
    mutate(clutch_size=ifelse(`178_Clutch_size_average_12`=="NAV",NA,as.numeric(`178_Clutch_size_average_12`))) %>%
    group_by(binom) %>%
    summarise(mean_body_size=mean(body_mass,na.rm=T),clutch_size=mean(clutch_size,na.rm=T)) -> ms
              
    ms<-data.frame(ms)
   row.names(ms)<-ms$binom
  return(ms)  
}
