
preprocessData <- function(raw_data) {
  
  raw_data$Experience <- as.double(raw_data$`Total years of experience`)
  raw_data$`Total years of experience` <- NULL
  
  raw_data$Experience_In_Germany <- as.double(raw_data$`Years of experience in Germany`)
  raw_data$`Years of experience in Germany` <- NULL
  
  raw_data$Seniority <- raw_data$`Seniority level`
  raw_data$`Seniority level` <- NULL
  
  raw_data$Main_Technology <- raw_data$`Your main technology / programming language`
  raw_data$`Your main technology / programming language` <- NULL
  
  raw_data$Other_Technologies <- raw_data$`Other technologies/programming languages you use often`
  raw_data$`Other technologies/programming languages you use often` <- NULL
  
  raw_data$Bruto <- raw_data$`Yearly brutto salary (without bonus and stocks) in EUR`
  raw_data$`Yearly brutto salary (without bonus and stocks) in EUR` <- NULL
  
  raw_data$Bonus <- as.double(raw_data$`Yearly bonus + stocks in EUR`)
  raw_data$`Yearly bonus + stocks in EUR` <- NULL
  
  raw_data$Total <- as.numeric(raw_data$`Annual bonus+stocks one year ago. Only answer if staying in same country`)
  raw_data$`Annual bonus+stocks one year ago. Only answer if staying in same country` <- NULL
  
  raw_data$Total_Last_Year <- as.double(raw_data$`Annual brutto salary (without bonus and stocks) one year ago. Only answer if staying in the same country`)
  raw_data$`Annual brutto salary (without bonus and stocks) one year ago. Only answer if staying in the same country` <- NULL
  
  raw_data$Vacation <- as.numeric(raw_data$`Number of vacation days`)
  raw_data$`Number of vacation days` <- NULL
  
  raw_data$Employment <- raw_data$`Employment status`
  raw_data$`Employment status` <- NULL
  
  raw_data$Contract <- raw_data$`?ontract duration`
  raw_data$`?ontract duration` <- NULL
  
  raw_data$Main_Language <- raw_data$`Main language at work`
  raw_data$`Main language at work` <- NULL
  
  raw_data$Size <- raw_data$`Company size`
  raw_data$`Company size` <- NULL
  
  raw_data$Type <- raw_data$`Company type`
  raw_data$`Company type` <- NULL
  
  raw_data$Fired <- raw_data$`Have you lost your job due to the coronavirus outbreak?`
  raw_data$`Have you lost your job due to the coronavirus outbreak?` <- NULL
  
  raw_data$Shorter_Week <- raw_data$`Have you been forced to have a shorter working week (Kurzarbeit)? If yes, how many hours per week`
  raw_data$`Have you been forced to have a shorter working week (Kurzarbeit)? If yes, how many hours per week` <- NULL
  
  raw_data$Support <- as.numeric(raw_data$`Have you received additional monetary support from your employer due to Work From Home? If yes, how much in 2020 in EUR`)
  raw_data$`Have you received additional monetary support from your employer due to Work From Home? If yes, how much in 2020 in EUR` <- NULL
  
  return (raw_data)
}

preprocessFired <- function(column){
      as.logical(ifelse(column=="Yes",1,ifelse(column=="No", 0, NA)))
}

preprocessSize <- function(column){
  ifelse(column == "1000+", "Large", 
         ifelse(column == "101-1000", "Medium", 
                ifelse(column == "51-100", "Small", 
                       ifelse(column == "up to 10", "Micro", NA))))
}

preprocessOtherTechnologies <- function(column){
  strsplit(column, ",")
}