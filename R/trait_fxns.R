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


read_process_trait_data <- function(){
  
  traits <- read_trait_data_in()
    traits %>%
    mutate(body_mass=ifelse(`99_Body_mass_average_8`=="NAV",NA,as.numeric(`99_Body_mass_average_8`))) %>%
    mutate(clutch_size=ifelse(`178_Clutch_size_average_12`=="NAV",NA,as.numeric(`178_Clutch_size_average_12`))) %>%
    mutate(category=`29_Population_description_4`) %>%
    mutate(status=`49_Australian_status_July_2015_5`) %>%
    mutate(brain_size=ifelse(`113_Brain_mass_8`=="NAV", NA, as.numeric(`113_Brain_mass_8`))) %>%
    mutate(diet_generalism=rowSums(.[163:173], na.rm=TRUE)) %>%
    group_by(binom) %>%
    summarise(mean_body_size=mean(body_mass,na.rm=T), 
              clutch_size=mean(clutch_size,na.rm=T),
              category=Mode(category),
              status=Mode(status),
              brain_size=mean(brain_size, na.rm=T)) -> ms
              
    
## calculate df for feeding habitat generalism
    feeding_habitat_generalism <- traits %>%
      dplyr::select(binom, 115:144) %>%
      replace(is.na(.), 0) %>%
      group_by(binom) %>%
      summarise_all(sum) %>%
      mutate(feeding_habitat_generalism = rowSums(.[2:31])) %>%
      dplyr::select(binom, feeding_habitat_generalism)
    
## calculate df for breeding habitat generalism
    breeding_habitat_generalism <- traits %>%
      dplyr::select(binom, 146:161) %>%
      replace(is.na(.), 0) %>%
      group_by(binom) %>%
      summarise_all(sum) %>%
      mutate(breeding_habitat_generalism = rowSums(.[2:17])) %>%
      dplyr::select(binom, breeding_habitat_generalism)

## Diet generalism
    diet_generalism <- traits %>%
      dplyr::select(binom, 163:173) %>%
      replace(is.na(.), 0) %>%
      group_by(binom) %>%
      summarise_all(sum) %>%
      mutate(diet_generalism = rowSums(.[2:12])) %>%
      dplyr::select(binom, diet_generalism)
    
## movement
    movement <- traits %>%
      dplyr::select(binom, 193:195) %>%
      replace(is.na(.), 0) %>%
      mutate_all(funs(str_replace(., "NAV", "0"))) %>%
      mutate(`193_National_movement_local_dispersal_13` = as.integer(as.character(`193_National_movement_local_dispersal_13`))) %>%
      mutate(`194_National_movement_Partial_migrant_13` = as.integer(as.character(`194_National_movement_Partial_migrant_13`))) %>%
      mutate(`195_National_movement_Total_migrant_13` = as.integer(as.character(`195_National_movement_Total_migrant_13`))) %>%
      group_by(binom) %>%
      summarise_all(sum) %>%
      mutate(`193_National_movement_local_dispersal_13` = 
               ifelse(`193_National_movement_local_dispersal_13` == 0, "", "dispersal")) %>%
      mutate(`194_National_movement_Partial_migrant_13` = 
               ifelse(`194_National_movement_Partial_migrant_13` == 0, "", "partial_migrant")) %>%
      mutate(`195_National_movement_Total_migrant_13` = 
               ifelse(`195_National_movement_Total_migrant_13` == 0, "", "total_migrant")) %>%
      unite("movement_class", 2:4, sep=" and ") %>%
      mutate(movement_class = gsub("dispersal and partial_migrant and ", "dispersal and partial_migrant", .$movement_class)) %>%
      mutate(movement_class = gsub("dispersal and  and ", "dispersal", .$movement_class)) %>%
      mutate(movement_class = gsub(" and  and total_migrant", "total_migrant", .$movement_class)) %>%
      mutate(movement_class = gsub(" and partial_migrant and total_migrant", "partial_migrant and total_migrant", .$movement_class)) %>%
      mutate(movement_class = gsub(" and partial_migrant and ", "partial_migrant", .$movement_class)) %>%
      mutate(movement_class = gsub(" and  and ", "none", .$movement_class))
    
    ms <- ms %>%
      inner_join(., feeding_habitat_generalism, by="binom") %>%
      inner_join(., breeding_habitat_generalism, by="binom") %>%
      inner_join(., diet_generalism, by="binom") %>%
      inner_join(., movement, by="binom")
    
    
    ms <- data.frame(ms)
    
   row.names(ms) <- ms$binom
   
  return(ms)  
}
