library(babynames)


nicole <- subset(babynames, name == "Nicole")
nicole <- nicole[order(nicole$n, decreasing = TRUE),]
nicole <- nicole[c(1:10),]

#### given name, what year was it most popular?
name_popularyear <- function(babynames, name_in_quotes) {
  # subset datasets by name
  desired_name <- subset(babynames, name == name_in_quotes)
  # re-order the dataset by most popular 
  desired_name <- desired_name[order(desired_name$n, decreasing = TRUE),]
  # get the top 10
  desired_name <- desired_name[c(1:10),]
  # create an empty dataframe
  newdata <- matrix(nrow = 1, ncol = 2)
  newdata <- data.frame(newdata)
  row.names(newdata) <- name_in_quotes
  colnames(newdata) <- c("Year", "#ofpeople")
  # for loop
  for (i in 1:nrow(desired_name)) {
    # just get the loop to go through so the function will work! 
    if (desired_name$n[i] > 0) {
      # Add year to dataframe
      newdata[i,1] <- desired_name$year[i]
      # Add the number of people 
      newdata[i,2] <- desired_name$n[i]
    }
  }
  return(newdata)
}

#### given year, what name was more popular?
year_popularname <- function(babynames, Year) {
  for (i in 1:nrow(desired_year)) {
    maxnumber <- max(desired_year$n)
    if (desired_year$n[i] == maxnumber) {
      print(Year)
      print("most popular name")
      print(desired_year$name[i])
      print("how many people")
      print(maxnumber)
    }
  }
}

#### try and make one where it returns a table by sex 
year_popname_bysex <- function(babynames, Year) {
  # Get dataframe with just the year
  desired_year <- subset(babynames, year == Year)
  # loop through for males and females
  for (i in 1:nrow(desired_year)) {
    # First, subset data by sex 
    female <- subset(desired_year, sex == "F")
    male <- subset(desired_year, sex == "M")
    # create empty matrix to return at the end 
    popularnames <- matrix(nrow = 2, ncol = 3)
    popularnames <- data.frame(popularnames)
    row.names(popularnames) <- c("Female", "Male")
    colnames(popularnames) <- c("most popular name", "year", "# of people")
    popularnames[,2] <- Year
    
    # starting with females 
    for (i in 1:nrow(female)) {
      maxnum <- max(female$n)
      if (female$n[i] == maxnum) {
        # Add the most popular girl name to dataframe
        popularnames[1,1] <- female$name[i]
        # Add the number of people with that name
        popularnames[1,3] <- maxnum
      }
    }
    # now males 
    for (i in 1:nrow(male)) {
      maxnum_m <- max(male$n)
      if (male$n[i] == maxnum_m) {
        # Add the most popular male name to dataframe
        popularnames[2,1] <- male$name[i]
        # Add the number of people with that name
        popularnames[2,3] <- maxnum_m
      }
    }
    return(popularnames)
  } 
}






