#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create Servive map (adapted)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 Housekeeping, setup, etc. ---------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(readxl)
library(readr)
library(leaflet)
library(ggmap)
library(ckanr)
library(phsopendata)
library(rvest)


gc()

# 1.2 Reference data ---- 


# 2. Loading location data ------------------------------------------------

## 2.1. GP Data ----

most_recent_practice_sizes = package_show(id = "gp-practice-contact-details-and-list-sizes", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

GP_Locations <- get_resource(most_recent_practice_sizes) %>%
  dplyr::select(practice_code=PracticeCode,practice_name=GPPracticeName,postcode=Postcode) %>%
  mutate(practice_code = as.character(practice_code))


## 2.2 . Care Home Data ----

link = "https://www.careinspectorate.com/index.php/publications-statistics/93-public/datastore"

most_recent_care_home_data_url = read_html(link) %>% 
  html_elements(css = ".button") %>%
  html_attr(name = "href") %>%
  na.omit() %>%
  as.character() %>%
  data.frame(full_url=.) %>%
  mutate(end_of_url=str_split(full_url,"/")) %>%
  mutate(end_of_url=unlist(lapply(end_of_url,last))) %>%
  mutate(date=as.Date(end_of_url,"MDSF_data_%d %B %Y.csv")) %>%
  na.omit() %>%
  filter(date == max(date)) %>%
  pull(full_url) %>%
  gsub(" ","%20",.)

Care_Service_Locations <- read_csv(paste0("https://www.careinspectorate.com",most_recent_care_home_data_url)) %>%
  dplyr::select(care_home_number=CSNumber,type=CareService,subtype=Subtype,postcode=Service_Postcode)

## 2.3. Hospital Info ----

# Determines types of hospitals
most_recent_hospital_info = package_show(id = "nhs-scotland-accident-emergency-sites", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

Hospital_Info <- get_resource(most_recent_hospital_info)  %>%
  dplyr::select(hosp_code=TreatmentLocationCode,hosp_name=TreatmentLocationName,type=CurrentDepartmentType,status=Status)


## 2.4. Hospital Location Info ----

# Determines location data of hospitals
most_recent_hospital_codes_info = package_show(id = "hospital-codes", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

Hospital_Locations <-   get_resource(most_recent_hospital_codes_info)  %>%
  dplyr::select(hosp_code=HospitalCode,postcode=Postcode)


## 2.5 Pharmacy locations ----

most_recent_dispenser_sites = package_show(id = "dispenser-location-contact-details", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

Pharmacy_Locations <- get_resource(most_recent_dispenser_sites) %>%
  dplyr::select(pharm_code = DispCode, pharm_name = DispLocationName, postcode = DispLocationPostcode) %>%
  mutate(pharm_code = as.character(pharm_code))

# 3. Combine Hospital Data ----

Hospital_Locations_Full <- Hospital_Info %>%
  left_join(Hospital_Locations,by=c("hosp_code")) %>% 
  # Converting Type codes into descriptive category
  mutate(type = case_when(type == "Type 1" ~ "Emergency Department",
                          type == "Type 2" ~ "Emergency Department",
                          type == "Type 3" ~ "Minor Injuries Unit/Other"))

# 4. Split Some Data Into Service Types ----

## 4.1. Care Services ----

Elder_Care_Services <- Care_Service_Locations %>%
  filter(type == "Care Home Service") %>%
  filter(!(subtype %in% c("Day Care of Children (under 3s)",
                          "Day Care of Children (over 3s)",
                          "Mainstream Residential School",
                          "Residential Special School",
                          "School Hostel",
                          "Children & Young People"))) %>%
  filter(subtype == "Older People")


Other_Care_Services <- Care_Service_Locations %>%
  filter(type == "Care Home Service") %>%
  filter(!(subtype %in% c("Day Care of Children (under 3s)",
                          "Day Care of Children (over 3s)",
                          "Mainstream Residential School",
                          "Residential Special School",
                          "School Hostel",
                          "Children & Young People"))) %>%
  filter(subtype != "Older People")


## 4.2. Hospitals ----

MIU_Locations <- Hospital_Locations_Full %>%
  filter(type == "Minor Injuries Unit/Other") 

ED_Locations <- Hospital_Locations_Full %>%
  filter(type == "Emergency Department") 

# 6. Prepare Data To Be Joined ----

GP_Locations <- GP_Locations %>%
  dplyr::select(code=practice_code,name=practice_name,postcode) %>%
  mutate(service_type="GP Practice") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

MIU_Locations <- MIU_Locations %>%
  dplyr::select(code=hosp_code,name= hosp_name,postcode) %>%
  mutate(service_type="Minor Injuries Unit") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

ED_Locations <- ED_Locations %>%
  dplyr::select(code=hosp_code,name= hosp_name,postcode) %>%
  mutate(service_type="Emergency Department") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

Elder_Care_Locations <- Elder_Care_Services %>%
  dplyr::select(code=care_home_number,name=care_home_number,postcode) %>%
  mutate(service_type="Care Home") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

Other_Care_Services <- Other_Care_Services %>%
  dplyr::select(code=care_home_number,name=care_home_number,postcode) %>%
  mutate(service_type="Care Home") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

Pharmacy_Locations <- Pharmacy_Locations %>% 
  dplyr::select(code=pharm_code,name=pharm_name,postcode) %>%
  mutate(service_type="Pharmacy") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")


# 7. Combine All Services Data ----

All_Services_Locations <- rbind(GP_Locations,
                                MIU_Locations,
                                ED_Locations,
                                Elder_Care_Locations,
                                Other_Care_Services,
                                Pharmacy_Locations)

# Tidying up
rm(Hospital_Locations_Full,
   Hospital_Info,
   Hospital_Locations,
   GP_Locations,
   MIU_Locations,
   ED_Locations,
   Care_Service_Locations,
   Elder_Care_Locations,
   Elder_Care_Services,
   Other_Care_Services,
   Pharmacy_Locations,
   Other_Services,
   Opticians_CnS,
   Dental_Services_CnS,
   Community_Hospitals,
   most_recent_practice_sizes,
   most_recent_dispenser_sites, 
   most_recent_hospital_codes_info,
   most_recent_care_home_data_url,
   most_recent_hospital_info,
   link)

# 8. Fix Postcode (Remove Space) ----

All_Services_Locations <- All_Services_Locations %>%
  mutate(postcode = gsub(" ", "",postcode))

# 9. Attach Postcode Lookup ----

# Via National Records of Scotland - Postcode Index file
# https://www.nrscotland.gov.uk/publications/scottish-postcode-directory-2025/ 
post_code_data <- "https://www.nrscotland.gov.uk/media/dwmdsj1k/spd_postcodeindex_cut_25_1_csv.zip"

temp <- tempfile() # Creating a temporary storage location for our zipped data
download.file(post_code_data,temp) # Assigning the data available in the URL above to the URL
Postcode_Lookup <- rbind(
  
  # Binding postcode CSV files together
  
  read_csv(unz(temp, "SmallUser.csv")) %>% # Reading in CSV postcode file 
    janitor::clean_names() %>% # Cleaning variable names
    select(postcode, datazone2011 = data_zone2011code, latitude, longitude), 
  
  read_csv(unz(temp, "LargeUser.csv")) %>% # Reading in CSV postcode file 
             janitor::clean_names() %>% # Cleaning variable names
             select(postcode, datazone2011 = data_zone2011code, latitude, longitude)
  
  ) %>%  # End of rbind() call 
  distinct() # Removing duplicate entries
  
unlink(temp)


DataZone_Lookup <- get_resource("d6e500c4-c1f2-4507-979a-e18855efd7a4") %>% 
  janitor::clean_names() %>% 
  dplyr::select(datazone2011 = data_zone, hscp_locality = sub_hscp_name, hscp, hb)


Large_Geography_Lookup <- get_resource("944765d7-d0d9-46a0-b377-abb3de51d08e") %>% 
  janitor::clean_names() %>% 
  dplyr::select(hscp, hscp_name, hb, hb_name)


# Joining data
DataZone_Lookup <- left_join(DataZone_Lookup, Large_Geography_Lookup)

Postcode_Lookup <-  Postcode_Lookup %>% 
  left_join(
    DataZone_Lookup,
    by = "datazone2011",
    relationship = "many-to-one"
  ) 

Postcode_Lookup <- Postcode_Lookup %>%
  mutate(postcode = gsub(" ", "", postcode)) %>%
  select(
    postcode, latitude, longitude, datazone2011,
    hscp_locality, hscp2019name = hscp_name, hscp2019 = hscp, hb2019name = hb_name, hb2019 = hb) 


All_Services_Locations <- All_Services_Locations %>%
  left_join(Postcode_Lookup,by="postcode")

test_james <- All_Services_Locations


# 10. Filter For Location Of Interest ----

All_Services_Locations <- All_Services_Locations %>%
  filter_for_location(location=location)

# 11. Filter For Services Of Interest ----

All_Services_Locations <- All_Services_Locations %>% 
  filter(!(service_type %in% services_to_exclude))



