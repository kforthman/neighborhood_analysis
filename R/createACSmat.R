# Alters the original matrix to be more suited for the factor analysis.
# Renames variables, throws out certain variables, and alters some variables to be percentages.
# Additionaly, gets rid of all tracts with a population of zero.
createACSmat <- function(endyear = 2015){
  filename <- paste0("output/",endyear,"/useful_ACS_condensedData.csv")
  bool_q <-  -1
  if (file.exists(filename)){
    while(bool_q != 1){
      bool_q <- readline("Condensed dataset has already been created... Would you like to recreate it? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'.")}
    }
  }
  else{
  while(bool_q != 1){
    bool_q <- readline("Condensed dataset has not been created... Would you like create it? Y/N(1/0)  ")
    if(bool_q == 0){return()}
    else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
  }
  }
  
  # Read in the original matrix.
  original <- read.csv(paste0("output/", endyear, "/useful_ACS_originalData.csv"), row.names = 1)
  # If the state code is only a single digit, it is padded.
  rownames(original) <- str_pad(rownames(original), 11, "left", pad="0")
  # Define the number of tracts
  n <- dim(original)[1]
  
  # --- Now, begin adding variables to a new matrix and changing them as needed. --- #
  
  # Total population ----
  condensed <- original['Total']
  
  # Age and gender ----
  Males.age <- c("Male...Under.5.years","Male...5.to.9.years","Male...10.to.14.years","Male...15.to.17.years",
                 "Male...18.and.19.years","Male...20.years","Male...21.years","Male...22.to.24.years",
                 "Male...25.to.29.years","Male...30.to.34.years","Male...35.to.39.years","Male...40.to.44.years",
                 "Male...45.to.49.years","Male...50.to.54.years","Male...55.to.59.years","Male...60.and.61.years",
                 "Male...62.to.64.years","Male...65.and.66.years","Male...67.to.69.years","Male...70.to.74.years",
                 "Male...75.to.79.years","Male...80.to.84.years","Male...85.years.and.over")
  
  Females.age <- c("Female...Under.5.years","Female...5.to.9.years","Female...10.to.14.years",
                   "Female...15.to.17.years","Female...18.and.19.years","Female...20.years",
                   "Female...21.years","Female...22.to.24.years","Female...25.to.29.years",
                   "Female...30.to.34.years","Female...35.to.39.years","Female...40.to.44.years",
                   "Female...45.to.49.years","Female...50.to.54.years","Female...55.to.59.years",
                   "Female...60.and.61.years","Female...62.to.64.years","Female...65.and.66.years",
                   "Female...67.to.69.years","Female...70.to.74.years","Female...75.to.79.years",
                   "Female...80.to.84.years","Female...85.years.and.over")
  
  Age <- original[Males.age]+original[Females.age]
  for(i in 1:n){
    Age[i,] <- c(2.5,7,12,16,18.5,20,21,23,27,32,37,42,47,52,57,60.5,63,65.5,68,72,77,82,85) * Age[i,]
  }
  Age <- rowSums(Age)
  Age <- Age/original["Total..Sex.by.age"]
  condensed['Age'] <- Age
  
  # Race ----
  condensed['Total..Race'] <- original['Total...Race']
  
  condensed['White'] <- original['White.alone']/original['Total...Race']
  
  condensed['Black'] <- original['Black.or.African.American.alone']/original['Total...Race']
  
  condensed['Asian'] <- original['Asian.alone']/original['Total...Race']
  
  condensed['Some.other.race'] <- ((original['Some.other.race.alone'] + 
                                      original['Native.Hawaiian.and.Other.Pacific.Islander.alone'] +
                                      original['American.Indian.and.Alaska.Native.alone'])
                                   /original['Total...Race'])
  
  # Ethnicity ----
  condensed['Total..Hispanic.or.Latino'] <- original['Total...Hispanic.or.Latino.by.Specific.Origin']
  
  condensed['Hispanic.or.Latino'] <- original['Hispanic.or.Latino.']/original['Total...Hispanic.or.Latino.by.Specific.Origin']
  
  # Nativity, citizenship, and mobility ----
  condensed['Total...Nativity.and.Citizenship.Status'] <- original['Total...Nativity.and.Citizenship.Status.in.the.United.States']
  
  condensed['Not.US.citizen'] <- ((original['Not.a.U.S..citizen'] 
                                   /original['Total...Nativity.and.Citizenship.Status.in.the.United.States']))
  
  # Mobility ----
  
  condensed['Total...Geographical.Mobility.in.the.Past.Year'] <- original['Total...Geographical.Mobility.in.the.Past.Year.by.Citizenship.Status.for.Current.Residence.in.the.U.S.']
  
  condensed['Moved.in.past.year'] <- 1 - (original['Same.house.1.year.ago.']/original['Total...Geographical.Mobility.in.the.Past.Year.by.Citizenship.Status.for.Current.Residence.in.the.U.S.'])
  
  # Transportation to work ----
  condensed['Total..Means.of.Transportation.to.Work'] <- original['Total...Means.of.Transportation.to.Work']
  
  condensed['Drove'] <- ((original['Car..truck..or.van.']+
                            original['Motorcycle']) / 
                           original['Total...Means.of.Transportation.to.Work'])
  
  condensed['Bicycle.or.walked'] <- ((original['Bicycle'] + original['Walked']) / 
                                       original['Total...Means.of.Transportation.to.Work'])
  
  # Time left for work ----
  condensed['Total..Time.Leaving.Home.to.Go.to.Work'] <- original['Total...Time.Leaving.Home.to.Go.to.Work']
  
  Time.Leaving.Home.to.Go.to.Work <- c("X12.00.a.m..to.4.59.a.m.", "X5.00.a.m..to.5.29.a.m.", "X5.30.a.m..to.5.59.a.m.", "X6.00.a.m..to.6.29.a.m.", 
                                       "X6.30.a.m..to.6.59.a.m.", "X7.00.a.m..to.7.29.a.m.", "X7.30.a.m..to.7.59.a.m.", "X8.00.a.m..to.8.29.a.m.", 
                                       "X8.30.a.m..to.8.59.a.m.", "X9.00.a.m..to.9.59.a.m.", "X10.00.a.m..to.10.59.a.m.", "X11.00.a.m..to.11.59.a.m.", 
                                       "X12.00.p.m..to.3.59.p.m.", "X4.00.p.m..to.11.59.p.m.")
  Time.Leaving.Home.to.Go.to.Work <- original[Time.Leaving.Home.to.Go.to.Work]
  for(i in 1:n){
    Time.Leaving.Home.to.Go.to.Work[i,] <- c(0,5,5.5,6,6.5,7,7.5,8,8.5,9,10,11,12,16) * Time.Leaving.Home.to.Go.to.Work[i,]
  }
  Time.Leaving.Home.to.Go.to.Work <- rowSums(Time.Leaving.Home.to.Go.to.Work)
  Time.Leaving.Home.to.Go.to.Work <- Time.Leaving.Home.to.Go.to.Work/original['Total...Time.Leaving.Home.to.Go.to.Work']
  condensed['Time.leaving.home'] <- Time.Leaving.Home.to.Go.to.Work
  
  # Time spent traveling to work ----
  condensed['Total..Travel.Time.to.Work'] <- original['Total...Travel.Time.to.Work']
  
  Travel.Time.to.Work <- c("Less.than.5.minutes", "X5.to.9.minutes", "X10.to.14.minutes", 
                           "X15.to.19.minutes", "X20.to.24.minutes", "X25.to.29.minutes", 
                           "X30.to.34.minutes", "X35.to.39.minutes", "X40.to.44.minutes",
                           "X45.to.59.minutes", "X60.to.89.minutes", "X90.or.more.minutes")
  Travel.Time.to.Work <- original[Travel.Time.to.Work]
  for(i in 1:n){
    Travel.Time.to.Work[i,] <- c(3, 7, 12, 17, 22, 27, 32, 37, 42, 47, 74.5, 90) * Travel.Time.to.Work[i,]
  }
  Travel.Time.to.Work <- rowSums(Travel.Time.to.Work)
  Travel.Time.to.Work <- Travel.Time.to.Work/original['Total...Travel.Time.to.Work']
  condensed['Travel.time.to.work'] <- Travel.Time.to.Work
  
  # Household type ----
  condensed['Total..Household.Type'] <- original['Total...Household.Type..including.Living.Alone.']
  
  condensed['Single.moms'] <- original['Family.households...Other.family...Female.householder..no.husband.present']/original['Total...Household.Type..including.Living.Alone.']
  
  condensed['Lives.alone'] <- original['Nonfamily.households...Householder.living.alone']/original['Total...Household.Type..including.Living.Alone.']
  
  condensed['Nonfamily.household'] <- original['Nonfamily.households...Householder.not.living.alone']/original['Total...Household.Type..including.Living.Alone.']
  
  # Marital status ----
  condensed['Total..Marital.Status.for.the.Population.15.Years.and.over'] <- original['Total...Sex.by.Marital.Status.for.the.Population.15.Years.and.over']
  
  Males.marital.status <- c("Male...Never.married", 
                            "Male...Now.married...Married..spouse.present",
                            "Male...Now.married...Married..spouse.absent.", 
                            "Male...Widowed", "Male...Divorced")
  
  Females.marital.status <- c("Female...Never.married", 
                              "Female...Now.married...Married..spouse.present",
                              "Female...Now.married...Married..spouse.absent.", 
                              "Female...Widowed", "Female...Divorced")
  
  Males.marital.status <- original[Males.marital.status]
  Females.marital.status <- original[Females.marital.status]
  
  Marital.status <- Males.marital.status + Females.marital.status
  
  names(Marital.status) <- c("Never.married", 
                             "Married.spouse.present", 
                             "Married.spouse.absent", 
                             "Widowed", "Divorced")
  
  for(i in 1:ncol(Marital.status)){
    Marital.status[,i] <- Marital.status[,i]/original['Total...Sex.by.Marital.Status.for.the.Population.15.Years.and.over']
  }
  
  condensed[names(Marital.status)] <- Marital.status
  
  
  
  # Education Level ----
  condensed['Total...Educational.Attainment.for.the.Population.25.Years.and.Over'] <- original['Total...Educational.Attainment.for.the.Population.25.Years.and.Over']
  
  Educational.Attainment <- c("No.schooling.completed", "Nursery.school", "Kindergarten", "X1st.grade", "X2nd.grade",
                              "X3rd.grade", "X4th.grade", "X5th.grade", "X6th.grade", "X7th.grade",
                              "X8th.grade", "X9th.grade", "X10th.grade", "X11th.grade", "X12th.grade..no.diploma",
                              "GED.or.alternative.credential", "Regular.high.school.diploma", "Some.college..less.than.1.year",
                              "Some.college..1.or.more.years..no.degree", "Associate.s.degree",
                              "Bachelor.s.degree", "Master.s.degree", "Professional.school.degree", "Doctorate.degree")
  
  Educational.Attainment <- original[Educational.Attainment]
  
  for(i in 1:n){
    Educational.Attainment[i,] <- ((0 * sum(Educational.Attainment[i,1:15]) +
                                      1 * sum(Educational.Attainment[i,16:20]) +
                                      2 * sum(Educational.Attainment[i,21:24]))
                                   /original[i, 'Total...Educational.Attainment.for.the.Population.25.Years.and.Over'])
  }
  
  Educational.Attainment <- Educational.Attainment[,2]
  
  condensed['Education'] <- Educational.Attainment
  
  # Disability status ----
  condensed['Total...Sex.by.Age.by.Disability.Status'] <- original['Total...Sex.by.Age.by.Disability.Status']
  
  Male.withDisability <- c("Male...Under.5.years...With.a.disability",
                           "Male...5.to.17.years...With.a.disability",
                           "Male...18.to.34.years...With.a.disability",
                           "Male...35.to.64.years...With.a.disability",
                           "Male...65.to.74.years...With.a.disability",
                           "Male...75.years.and.over...With.a.disability")
  
  Female.withDisability <- c("Female...Under.5.years...With.a.disability",
                             "Female...5.to.17.years...With.a.disability",
                             "Female...18.to.34.years...With.a.disability",
                             "Female...35.to.64.years...With.a.disability",
                             "Female...65.to.74.years...With.a.disability",
                             "Female...75.years.and.over...With.a.disability")
  
  Male.withDisability <- original[Male.withDisability]
  Female.withDisability <- original[Female.withDisability]
  
  Male.withDisability <- rowSums(Male.withDisability)
  Female.withDisability <- rowSums(Female.withDisability)
  
  Disabled <- (Male.withDisability + Female.withDisability)/original['Total...Sex.by.Age.by.Disability.Status']
  
  condensed['Disabled'] <- Disabled
  
  # Income ----
  condensed['Total..Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars.'] <- original['Total..Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars.']
  
  Household.Income <- c("Less.than..10.000", "X.10.000.to..14.999", "X.15.000.to..19.999", 
                        "X.20.000.to..24.999", "X.25.000.to..29.999", "X.30.000.to..34.999", 
                        "X.35.000.to..39.999", "X.40.000.to..44.999", "X.45.000.to..49.999",
                        "X.50.000.to..59.999", "X.60.000.to..74.999", "X.75.000.to..99.999", 
                        "X.100.000.to..124.999", "X.125.000.to..149.999", "X.150.000.to..199.999", 
                        "X.200.000.or.more")
  
  Household.Income <- original[Household.Income]
  
  for(i in 1:n){
    Household.Income[i,] <- c(4999.5,12499.5,17499.5,22499.5,27499.5,32499.5,37499.5,42499.5,
                              47499.5,54999.5,67499.5,87499.5,112499.5,137499.5,174999.5,200000) * Household.Income[i,]
  }
  Household.Income <- rowSums(Household.Income)
  Household.Income <- Household.Income/original['Total..Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars.']
  condensed['Income'] <- Household.Income
  
  # Social security ----
  condensed['Total..Social.Security.Income.for.Households'] <- original['Total...Social.Security.Income.for.Households']
  
  condensed['w.Social.Security'] <- original['With.Social.Security.income']/original['Total...Social.Security.Income.for.Households']
  
  # Supplementary security income ----
  condensed['Total..Supplemental.Security.Income.for.Households'] <- original['Total...Supplemental.Security.Income..SSI..for.Households']
  
  condensed['w.SSI'] <- original['With.Supplemental.Security.Income..SSI.']/original['Total...Supplemental.Security.Income..SSI..for.Households']
  
  # Public assistance ----
  condensed['Total..Public.Assistance.Income.for.Households'] <- original['Total...Public.Assistance.Income.for.Households']
  
  condensed['w.PAI'] <- original['With.public.assistance.income']/original['Total...Public.Assistance.Income.for.Households']
  
  # Retirement income ----
  condensed['Total..Retirement.Income.for.Households'] <- original['Total...Retirement.Income.for.Households']
  
  condensed['w.Retirement.income'] <- original['With.retirement.income']/original['Total...Retirement.Income.for.Households']
  
  condensed['Retirement.income'] <- original['Per.capita.income.in.the.past.12.months..in.2015.Inflation.adjusted.dollars.']
  
  # Employment status ----
  condensed['Total...Employment.Status.for.the.Population.16.Years.and.Over'] <- original['Total...Employment.Status.for.the.Population.16.Years.and.Over']
  
  condensed['Unemployed'] <- original['In.labor.force...Civilian.labor.force...Unemployed']/original['Total...Employment.Status.for.the.Population.16.Years.and.Over']
  
  condensed['Not.in.labor.force'] <- original['Not.in.labor.force']/original['Total...Employment.Status.for.the.Population.16.Years.and.Over']
  # Occupancy status ----
  condensed['Total..Occupancy.Status'] <- original['Total...Occupancy.Status']
  
  condensed['Vacant'] <- original['Vacant']/original['Total...Occupancy.Status']
  
  # Tenure ----
  condensed['Total..Tenure'] <- original['Total...Tenure']
  
  condensed['Not.owner.occupied'] <- 1 - (original['Owner.occupied']/original['Total...Tenure'])
  
  # Number of rooms ----
  condensed['Median.number.of.rooms'] <- original['Median.number.of.rooms']
  
  # Units in structure, home type, and home age ----
  condensed['Total..Units.in.Structure'] <- original['Total...Units.in.Structure']
  
  Units.in.structure <- c("X1..detached", "X1..attached", "X2", "X3.or.4", "X5.to.9", "X10.to.19", "X20.to.49", "X50.or.more")
  Units.in.structure <- original[Units.in.structure]
  for(i in 1:n){
    Units.in.structure[i,] <- c(1, 1, 2, 3.5, 7, 14.5, 34.5, 50) * Units.in.structure[i,]
  }
  Units.in.structure <- rowSums(Units.in.structure)
  condensed['Units.in.structure'] <- Units.in.structure / (original['Total...Units.in.Structure']-
                                                             original['Mobile.home'] - original['Boat..RV..van..etc.'])
  
  
  condensed['Mobile.home'] <- (original['Mobile.home'] + original['Boat..RV..van..etc.']) / original['Total...Units.in.Structure']
  
  # Kitchen facilities ----
  condensed['Total..Kitchen.facilities'] <- original['Total...KITCHEN.FACILITIES.FOR.ALL.HOUSING.UNITS']
  
  condensed['Incomplete.kitchen'] <- 1 - (original['Complete.kitchen.facilities']/original['Total...KITCHEN.FACILITIES.FOR.ALL.HOUSING.UNITS'])
  
  # Rent cost ----
  condensed['Total..Contract.Rent'] <- original['Total..Contract.Rent']
  
  Rent <- c("With.cash.rent...Less.than..100", "With.cash.rent....100.to..149", "With.cash.rent....150.to..199", 
            "With.cash.rent....200.to..249", "With.cash.rent....250.to..299", "With.cash.rent....300.to..349",
            "With.cash.rent....350.to..399", "With.cash.rent....400.to..449", "With.cash.rent....450.to..499", 
            "With.cash.rent....500.to..549", "With.cash.rent....550.to..599", "With.cash.rent....600.to..649",
            "With.cash.rent....650.to..699", "With.cash.rent....700.to..749", "With.cash.rent....750.to..799", 
            "With.cash.rent....800.to..899", "With.cash.rent....900.to..999", "With.cash.rent....1.000.to..1.249",
            "With.cash.rent....1.250.to..1.499", "With.cash.rent....1.500.to..1.999", "With.cash.rent....2.000.to..2.499", 
            "With.cash.rent....2.500.to..2.999", "With.cash.rent....3.000.to..3.499", "With.cash.rent....3.500.or.more")
  Rent <- original[Rent]
  for(i in 1:n){
    Rent[i,] <- c(49.5,124.5,174.5,224.5,274.5,324.5,374.5,424.5,474.5,524.5,574.5,624.5,674.5,724.5,774.5,849.5,949.5,1124.5,
                  1374.5,1749.5,2249.5,2749.5,3249.5,3500) * Rent[i,]
  }
  Rent <- rowSums(Rent)
  Rent <- Rent/original['With.cash.rent.']
  condensed['Cash.rent.cost'] <- Rent
  
  # Home value ----
  condensed['Total..Value'] <- original['Total..Value']
  
  Home.value <- c("Less.than..10.000.1", "X.10.000.to..14.999.1", "X.15.000.to..19.999.1", "X.20.000.to..24.999.1", "X.25.000.to..29.999.1", "X.30.000.to..34.999.1", "X.35.000.to..39.999.1",
                  "X.40.000.to..49.999", "X.50.000.to..59.999.1", "X.60.000.to..69.999", "X.70.000.to..79.999", "X.80.000.to..89.999", "X.90.000.to..99.999", "X.100.000.to..124.999.1",
                  "X.125.000.to..149.999.1", "X.150.000.to..174.999", "X.175.000.to..199.999", "X.200.000.to..249.999", "X.250.000.to..299.999", "X.300.000.to..399.999", "X.400.000.to..499.999",
                  "X.500.000.to..749.999", "X.750.000.to..999.999", "X.1.000.000.to..1.499.999", "X.1.500.000.to..1.999.999", "X.2.000.000.or.more")
  Home.value <- original[Home.value]
  for(i in 1:n){
    Home.value[i,] <- c(4999.5,12499.5,17499.5,22499.5,27499.5,32499.5,37499.5,44999.5,54999.5,64999.5,
                        74999.5,84999.5,94999.5,112499.5,137499.5,162499.5,187499.5,224999.5,274999.5,
                        349999.5,449999.5,624999.5,874999.5,1250000,1750000,2000000) * Home.value[i,]
  }
  Home.value <- rowSums(Home.value)
  Home.value <- Home.value/original['Total..Value']
  condensed['Home.value'] <- Home.value
  
  # Morgage status ----
  condensed['Total..Mortgage.status'] <- original['Total...MORTGAGE.STATUS']
  
  condensed['Mortgage'] <- original['Housing.units.with.a.mortgage..contract.to.purchase..or.similar.debt.']/original['Total...MORTGAGE.STATUS']
  
  # ----
  
  # Remove all tracts with a total population of zero.
  condensed <- condensed[which(condensed[,"Total"] > 0),]
  # Get rid of vectors containing Totals
  condensed <- condensed[,!grepl("Total", names(condensed))]
  
  # Create the cleaned up matrix
  write.csv(condensed, filename)
}