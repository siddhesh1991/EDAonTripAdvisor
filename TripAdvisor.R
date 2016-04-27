  library(rjson)
  library(dplyr)
  library(XML)
  library(stringr)
  
  file_list = list.files("F:\\json")
  #Creating an empty data frame df1
  df1 <- data.frame(hotel_id = numeric(0),
                         hotel_nm = character(0),
                         price = character(0),
                         address = character(0),stringsAsFactors = FALSE)
  colnames(df1) = c("hotel_id","hotel_nm", "price","address")
  
  for (i in file_list)
  { 
    json_data <- fromJSON(paste(readLines(paste("F:\\json\\",i,sep = "")),collapse = ""))
    hotel_nm = unlist(json_data$HotelInfo['Name'])
    hotel_id = unlist(json_data$HotelInfo['HotelID'])
    price = unlist(json_data$HotelInfo['Price'])
    address = unlist(json_data$HotelInfo['Address'])
    if(is.null(hotel_nm)){
      hotel_nm = "Not Defined"
    }
    if(is.null(hotel_id)){
      hotel_id = "Not Defined"
    }
    if(is.null(price)){
      price = "Not Defined"
    }
    if(is.null(address)){
      address = "Not Defined"
    }
    df1[nrow(df1)+1,] <- c(hotel_id,hotel_nm,price,address)  
  }
  
  #Creating a function for address scrubbing.

  address_scrubbing <- function(address) {
    addr_html = htmlTreeParse(address, useInternal = TRUE)
    #Parsing the HTML tages using HTML parser.
    scrubbed_full_addr =  unlist(xpathApply(addr_html,'//address', xmlValue))
    #print(scrubbed_full_addr)
    a = scrubbed_full_addr
    return(a)
  }
  #Adding the scrubbed address column to the data frame
  df1$scrubbed_addr = lapply(df1$address,address_scrubbing)
  #Dropping the non-scrubbed address column.
  df1$address = NULL
  
  st_ctr = function(scrubbed_addr) {
    if(is.null(scrubbed_addr)){
      return("Not Defined")
    }
    dlm_addr = strsplit(scrubbed_addr,split = ",")
    st_or_ctr_vec = c(do.call("cbind", dlm_addr))
    print (st_or_ctr_vec)
    st_or_ctr_nm = st_or_ctr_vec[length(st_or_ctr_vec)]
    print (st_or_ctr_nm)
    
    return (st_or_ctr_nm)
  }
  
  df1$state = lapply(df1$scrubbed_addr,st_ctr)
  
  
  #Logic for checking if the state belongs to the US.
  #For this creating a static lookup which contains the abbreveations of all the states in USA. If
  #the first 2 terms are not there in the lookup then the country is not United states.
  
  df1$state = str_trim(df1$state)
  country_check = function(city_or_country) {
    check = unlist(strsplit(city_or_country,split = " "))
    if(is.element(check[1],state.abb)) {
      a = "United States"
      print (a)
    }else {
      a = city_or_country
    }
    return(a)
  }
  
  df1$country = lapply(df1$state,country_check)
  df1$city = NULL
  
  city_name1 = function(scrubbed_addr) {
    if(is.null(scrubbed_addr)){
      return("Not Defined")
      
      
    }
    scrubbed_city_name = strsplit(scrubbed_addr,split = ",")
    city_nm_vec = c(do.call("cbind", scrubbed_city_name))
    city_nm = city_nm_vec[length(city_nm_vec) - 1]
    print (city_nm)
    return (city_nm)
  }
  df1$city = lapply(df1$scrubbed_addr,city_name1)
  #Question 2. Obtaining distinct city and distinct cities in the United States.
  #Please see the variable distinct_cities and distinct_city_us for  Question 2.
  distinct_cities = unique(df1$city)
  #Distinct cities in the United States.
  cities_US  = df1 %>% filter(country == "United States")
  distinct_city_us = unique(cities_US$city)
  
  #Question 4. 
  my.df <- data.frame(lapply(df1, as.character), stringsAsFactors=FALSE)
  write.csv(my.df, file = "F:\\json\\Hotel_ques4.csv")
  
  #Question 3. Count number of hotels in each city and filter cities with more
  #than 30 hotels.
  count_df = data.frame(count= numeric(0),
                        city = numeric(0),stringsAsFactors = FALSE)
  
  count_df = my.df %>%  group_by(city) %>%  summarise(count = n())  %>% filter(count > 30)
  write.csv(count_df, file = "F:\\json\\Hotel_ques3.csv")
  
  