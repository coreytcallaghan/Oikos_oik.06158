read_trait_data_in<-function(){
  require(readr)
traits<-read_csv("Data/Raw trait data/Australian_Bird_Data_Version_1.csv")
         traits$binom<- paste(traits$`4_Genus_name_2`, traits$`5_Species_name_2`,sep="_")
         return(traits)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


read_process_trait_data<-function(){
  traits<-read_trait_data_in()
    traits %>%
    mutate(body_mass=ifelse(`99_Body_mass_average_8`=="NAV",NA,as.numeric(`99_Body_mass_average_8`))) %>%
    mutate(clutch_size=ifelse(`178_Clutch_size_average_12`=="NAV",NA,as.numeric(`178_Clutch_size_average_12`))) %>%
    mutate(category=`29_Population_description_4`) %>%
    mutate(status=`49_Australian_status_July_2015_5`) %>%
    mutate(brain_size=ifelse(`113_Brain_mass_8`=="NAV", NA, as.numeric(`113_Brain_mass_8`))) %>%
    mutate(feeding_habitat_generalism=rowSums(.[115:145], na.rm=TRUE)) %>%
    mutate(breeding_habitat_generalism=rowSums(.[146:162], na.rm=TRUE)) %>%
    mutate(diet_generalism=rowSums(.[163:173], na.rm=TRUE)) %>%
    group_by(binom) %>%
    summarise(mean_body_size=mean(body_mass,na.rm=T), 
              clutch_size=mean(clutch_size,na.rm=T),
              category=Mode(category),
              status=Mode(status),
              brain_size=mean(brain_size, na.rm=T),
              feeding_habitat_generalism=mean(feeding_habitat_generalism),
              breeding_habitat_generalism=mean(breeding_habitat_generalism),
              diet_generalism=mean(diet_generalism)) -> ms
              
    ms<-data.frame(ms)
   row.names(ms)<-ms$binom
  return(ms)  
}
