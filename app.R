## Disaster Resiliency, Mapping the Role Disaster Aid Has On Communities.
## Emmet Tam
## University of Maryland, College Park
## Last updated 1/08/2021

## Includes code adapted from the following sources:
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R
#

# Please read the README.md file nearby for more details on the overall purpose
# of this application.

################################################################################

# When server restarts, update to most recent data



source("kneedle.R")
################################################################################

# Download the required package dependencies
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidycensus)) install.packages("tidycensus", repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages("tidycensus", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(usmap)) install.packages("usmap", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dbscan)) install.packages("dbscan", repos = "http://cran.us.r-project.org")
################################################################################

# set R settings for how it treats how many entities are printed as well as
# turning off conversion of numbers to scientific notation within R.
options(max.print=1000000)
options(scipen = 999)


################################################################################

# Begin uploading datasets; these are currently static, but API will be added in
# to grab more current data.

# upload the 'cleaned' FEMA Individual and Household Program dataset.
# adapted from source: https://www.fema.gov/openfema-data-page/individuals-and-households-program-valid-registrations
IHP <- fread("IHP.csv", header=TRUE)

# upload the 'cleaned' FEMA Hazard Mitigation Assistance Program dataset.
# adapted from source: https://www.fema.gov/openfema-data-page/hazard-mitigation-assistance-projects
HMA <- fread("HMA.csv", header=TRUE)

# upload the 'cleaned' NOAA Storm Events dataset. 
# adapted from source: https://www.ncdc.noaa.gov/stormevents/ftp.jsp
# NOAA <- fread("NOAA.csv",
#                 header=TRUE)

# upload the 'cleaned' FEMA Public Assistance dataset.
# adapted from source: https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v1
# PA <- fread("PA.csv", header=TRUE)

# upload the 'cleaned' Community Development Block Grant Disaster Recovery - HUD dataset.
# adapted from source: 
# CDBG.DR <- fread("CDBG-DR.csv", header=TRUE)

################################################################################

# grab data from the tidycensus package which contains data to map the state name
# and county name to its rightful "fips" code; store the mapping in fips_code for
# later use.
# preprocess_fips_data = function() {
#   # grabs data from tidycensus
#   data(fips_codes)
#   fips_codes$fip = paste(fips_codes$state_code, fips_codes$county_code, sep = "")
#   fips_codes$county = casefold(sub(" County.*", "", fips_codes$county))
#   fips_codes$state_name = casefold(fips_codes$state_name)
#   return(fips_codes)
# }
# fips_codes = preprocess_fips_data()

################################################################################

# Read in shapefile for creating polygons of US COUNTIES on top of leaflet.
# Read in shapefile for creating polygons of US STATES on top of leaflet.
# us.county.map <- readOGR("tl_2021_us_county/tl_2021_us_county.shp", layer = "tl_2021_us_county", stringsAsFactors = FALSE)
us.state.map <- readOGR("tl_2021_us_state/tl_2021_us_state.shp", layer = "tl_2021_us_state", stringsAsFactors = FALSE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
# Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
# us.county.map <- us.county.map[!us.county.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
#                                         "64", "68", "70", "74"),]
us.state.map <- us.state.map[!us.state.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                                          "64", "68", "70", "74"),]

# Make sure other outling islands are removed.
# us.county.map <- us.county.map[!us.county.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
#                                         "95", "79"),]
us.state.map <- us.state.map[!us.state.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

#################################################################################
#################################################################################
#################################################################################
#################################################################################


# ui component of a shiny application
ui <- bootstrapPage(

  navbarPage("FEMA - Individuals and Household Programs", theme = shinytheme("readable"),collapsible = TRUE,
    ############################################################################
    
    tabPanel(
      "Map",
      div(class="outer", tags$head(includeCSS("styles.css")),
  
        absolutePanel(
          leafletOutput(
            "map",
            height = '91vh',
            width = '100vw'
          ),
          top = 10
        ),
          
        absolutePanel(
          id = "controls", 
          class = "panel panel-default",
          draggable = TRUE, 
          top = 27, 
          left = 60, 
          width = 325, 
          fixed= FALSE, 
          height = 600, 
          cursor = "inherit", 
          selectInput(
            "mapdataset",
            label = "",
            choices = c("Individuals and Household Programs", "Hazard Mitigation Assistance", "NOAA Storm Events Dataset", "Public Assistance", "Community Block Grant Disaster Funds")
          ),
          tags$b(h3(textOutput("curr_state")), style="color:#045a8d", align = "center"),
          h3(htmlOutput("details"), align = "right"),
          
          uiOutput("mapdates"),

          absolutePanel(
            bottom = 20,
            actionButton("hidemap", "Toggle Layer")
          ),
          absolutePanel(
            bottom = 10,
            right = 10,
            h6(uiOutput("getCountyDetails"), align = "left")
          ),
          absolutePanel(id = "logo", 
                        class = "", 
                        bottom = 20, 
                        left = "3%", 
                        width = 225,
                        fixed=TRUE, 
                        draggable = FALSE, 
                        height = "auto",
                        tags$a(href='https://eng.umd.edu/',
                               tags$img(src='ajamesclark.png',height='40',width='225')))
        )
      )
    ),
    
    ############################################################################
    tabPanel(
      "Plots",
      tabsetPanel(
        tabPanel("DBSCAN",
                 absolutePanel(
                   draggable = FALSE, top = 625, left = "1%", width = "30%", fixed=FALSE, 
                   height = "100%", cursor = "inherit", 
                   span(tags$b(h6("Please select")), style="color:#045a8d"),
                   selectInput(
                     
                     "clusterdataset",
                     label = "",
                     choices = c("Individuals and Household Programs", "Hazard Mitigation Assistance", "NOAA Storm Events Dataset", "Public Assistance", "Community Block Grant Disaster Funds")
                     
                   ),
                   
                   sliderInput(
                     "knnvalue",
                     label = "knn-value",
                     min = 1, max = 100,
                     value = 4
                   ),
                   
                   sliderInput(
                     "eps",
                     label = "eps(epsilon neighborhood)",
                     min = 0.001, max = 0.5,
                     value = 0.025,
                     step = 0.001,
                   ),
                   # Assuming from best practice, default size is 4
                   sliderInput(
                     "minPts",
                     label = "minPts",
                     min = 1, max = 100,
                     value = 4
                   ),
                 ),
                 uiOutput("clusterselect"),
                 absolutePanel(
                   plotOutput("barchart"),
                   top = 150,
                   right = "1%",
                   height = "100%",
                   width = "49%"
                 ),
                 absolutePanel(
                   
                   plotOutput("knn"),
                   top = 150,
                   left = "1%",
                   height = "100%",
                   width = "49%"
                 )
                 
                 ),
        tabPanel("Linear Regression",
                 
 
                 )
      )
      
    ),
    
    ############################################################################
    tabPanel(
      "Datasets",
      selectInput(
        "dataset",
        label = "Dataset",
        choices = c("IHP", "HMA", "NOAA", "PA", "CDBG.DR"),
        selected = "IHP"
      ),
      numericInput("maxrows", "Rows to show", 25),
      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(), 
              "Adapted from timeline data published by ", 
              tags$a(href="https://www.fema.gov/openfema-data-page/individuals-and-households-program-valid-registrations-v1", 
              "FEMA's OpenFEMA program"),
      verbatimTextOutput("rawtable")
      
    ),
    
    ############################################################################
    navbarMenu("About",
      tabPanel(
        "About",
        verbatimTextOutput( outputId = "text"),
        absolutePanel(id = "logo", class = "", bottom = 20, left = 20, width = 225,
                      fixed=TRUE, draggable = FALSE, height = "auto",
                      tags$a(href='https://eng.umd.edu/',
                             tags$img(src='ajamesclark.png',height='40',width='225')))
        
      )
    )
    
  )
)


#################################################################################
#################################################################################
#################################################################################


# server component of shiny application
server <- function(input, output, session) {
  
  # Shiny functionality to see output of input variables. Use this to print 
  # input variables to the console for debugging.
  observeEvent(input$dates, {
    
  })
  
  values <- reactiveValues(currGroup = "Individual Household Program")
  
  observeEvent(input$hidemap, {
    if(input$hidemap[1]%%2 == 1) {
      leafletProxy("map", session) %>%
        hideGroup(values$currGroup)
    } else {
      leafletProxy("map", session) %>%
        showGroup(values$currGroup)
    }
  })
  
  output$getCountyDetails <- renderUI ({
    if(length(input$map_shape_click) != 0 && input$map_zoom > 5) {
      actionButton("getCountyInfo", "More Details")
    }
  })
  
  output$mapdates <-renderUI({
    if(input$mapdataset == "Individuals and Household Programs") {
      absolutePanel(
        top = 450,
        sliderInput("dates", 
                    "Date Range:",
                    min = 2002,
                    max = 2021,
                    value = c(2002,2021),
                    sep = "")
      )
    } else if(input$mapdataset == "Hazard Mitigation Assistance") {
      absolutePanel(
        top = 450,
        sliderInput("dates", 
                    "Date Range:",
                    min = 1990,
                    max = 2021,
                    value = c(1990,2021),
                    sep = "")
      )
    } else {
      
    }
    
  })
  
  output$linearselect <- renderUI({
    if(input$lineardataset == "Individuals and Household Programs") {
      absolutePanel(
        draggable = FALSE, top = 650, left = "2.5%", width = "25%", fixed=FALSE, 
        height = "100%", cursor = "inherit", 
        span(tags$b(h6("Please select")), style="color:#045a8d"),
        
        
        selectInput(
          "groupby",
          label = "Group By",
          choices = c("Disaster-County", "State", "County", "Incident Type", "Disaster Year", "Disaster Number"),
          selected = "Disaster-County"
        ),
        
        selectInput(
          "xvariable",
          label = "X Variable",
          choices = c("Primary Residence",	"Home Owners Insurance",
                      "Flood Insurance",	"IHP Referral",	"IHP Eligible",	"HA Referral",
                      "HA Eligible",	"ONA Referral",	"ONA Eligible",
                      "Utilities Out",	"Auto Damage",	"Emergency Needs",	
                      "Food Needs",	"Shelter Need",	"SBA Eligible",	"SBA Approved",
                      "TSA Eligible",	"TSA CheckedIn",	"Renters", "Owners",
                      "Total Residents Affected",	"Houses and Duplex",	"Mobile Homes",
                      "Apartments",	"Condos",	"Boats",	"Trialers",	"Townhouses",
                      "Correctional Facilities",	"Assisted Living Facilities",
                      "Military Houses",	"College Dorms",	"Unknown Buildings",
                      "Other Residence",	"Average Income",
                      "IHP Amount",	"HA Amount",	"ONA Amount",	"rpfvl", "ppfvl",
                      "Rental Assistance Amount",	"Repair Assistance Amount",
                      "Replacement Assistance Amount",	"Personal Property Amount",
                      "Roof Damage Amount", "Foundation Damage Amount"
          ),
          selected = "Primary Residence"
        ),
        
        selectInput(
          "yvariable",
          label = "Y Variable",
          choices = c("Primary Residence",	"Home Owners Insurance",
                      "Flood Insurance",	"IHP Referral",	"IHP Eligible",	"HA Referral",
                      "HA Eligible",	"ONA Referral",	"ONA Eligible",
                      "Utilities Out",	"Auto Damage",	"Emergency Needs",	
                      "Food Needs",	"Shelter Need",	"SBA Eligible",	"SBA Approved",
                      "TSA Eligible",	"TSA CheckedIn",	"Renters", "Owners",
                      "Total Residents Affected",	"Houses and Duplex",	"Mobile Homes",
                      "Apartments",	"Condos",	"Boats",	"Trialers",	"Townhouses",
                      "Correctional Facilities",	"Assisted Living Facilities",
                      "Military Houses",	"College Dorms",	"Unknown Buildings",
                      "Other Residence",	"Average Income",
                      "IHP Amount",	"HA Amount",	"ONA Amount",	"rpfvl", "ppfvl",
                      "Rental Assistance Amount",	"Repair Assistance Amount",
                      "Replacement Assistance Amount",	"Personal Property Amount",
                      "Roof Damage Amount", "Foundation Damage Amount"
          ),
          selected = "Home Owners Insurance"
        ),
        
        
        sliderInput("Bardates", "Date Range:",
                    min = 2002, max = 2021,
                    value = c(2002,2021),
                    sep = ""),
        
      )
    } else if(input$lineardataset == "Hazard Mitigation Assistance") {

    } else {
      
    }
    
  })
  
  output$clusterselect <- renderUI({
    if(input$clusterdataset == "Individuals and Household Programs") {
      absolutePanel(
        draggable = FALSE, top = 625, right = "1%", width = "30%", fixed=FALSE, 
        height = "100%", cursor = "inherit", 
        span(tags$b(h6("Please select")), style="color:#045a8d"),
        
        
        selectInput(
          "groupby",
          label = "Group By",
          choices = c("Disaster-County", "State", "County", "Incident Type", "Disaster Year", "Disaster Number"),
          selected = "Disaster-County"
        ),
        
        selectInput(
          "xvariable",
          label = "X Variable",
          choices = c("Primary Residence",	"Home Owners Insurance",
                      "Flood Insurance",	"IHP Referral",	"IHP Eligible",	"HA Referral",
                      "HA Eligible",	"ONA Referral",	"ONA Eligible",
                      "Utilities Out",	"Auto Damage",	"Emergency Needs",	
                      "Food Needs",	"Shelter Need",	"SBA Eligible",	"SBA Approved",
                      "TSA Eligible",	"TSA CheckedIn",	"Renters", "Owners",
                      "Total Residents Affected",	"Houses and Duplex",	"Mobile Homes",
                      "Apartments",	"Condos",	"Boats",	"Trialers",	"Townhouses",
                      "Correctional Facilities",	"Assisted Living Facilities",
                      "Military Houses",	"College Dorms",	"Unknown Buildings",
                      "Other Residence",	"Average Income",
                      "IHP Amount",	"HA Amount",	"ONA Amount",	"rpfvl", "ppfvl",
                      "Rental Assistance Amount",	"Repair Assistance Amount",
                      "Replacement Assistance Amount",	"Personal Property Amount",
                      "Roof Damage Amount", "Foundation Damage Amount"
          ),
          selected = "Primary Residence"
        ),
        
        selectInput(
          "yvariable",
          label = "Y Variable",
          choices = c("Primary Residence",	"Home Owners Insurance",
                      "Flood Insurance",	"IHP Referral",	"IHP Eligible",	"HA Referral",
                      "HA Eligible",	"ONA Referral",	"ONA Eligible",
                      "Utilities Out",	"Auto Damage",	"Emergency Needs",	
                      "Food Needs",	"Shelter Need",	"SBA Eligible",	"SBA Approved",
                      "TSA Eligible",	"TSA CheckedIn",	"Renters", "Owners",
                      "Total Residents Affected",	"Houses and Duplex",	"Mobile Homes",
                      "Apartments",	"Condos",	"Boats",	"Trialers",	"Townhouses",
                      "Correctional Facilities",	"Assisted Living Facilities",
                      "Military Houses",	"College Dorms",	"Unknown Buildings",
                      "Other Residence",	"Average Income",
                      "IHP Amount",	"HA Amount",	"ONA Amount",	"rpfvl", "ppfvl",
                      "Rental Assistance Amount",	"Repair Assistance Amount",
                      "Replacement Assistance Amount",	"Personal Property Amount",
                      "Roof Damage Amount", "Foundation Damage Amount"
          ),
          selected = "Home Owners Insurance"
        ),
        
        
        sliderInput("Bardates", "Date Range:",
                    min = 2002, max = 2021,
                    value = c(2002,2021),
                    sep = ""),
        
        verbatimTextOutput("summary")
        
      )
    } else if(input$clusterdataset == "Hazard Mitigation Assistance") {
      absolutePanel(
        draggable = FALSE, top = 625, right = "1%", width = "25%", fixed=FALSE, 
        height = "100%", cursor = "inherit", 
        span(tags$b(h6("Please select")), style="color:#045a8d"),
        
        
        selectInput(
          "groupby",
          label = "Group By",
          choices = c("Project", "State", "County", "Project Area", "Project Type", "Disaster Number"),
          selected = "Project"
        ),
        
        selectInput(
          "xvariable",
          label = "X Variable",
          choices = c("Number Of Properties",	"Number Of Final Properties", "Cost Share Percentage",
                      "Project Amount",	"Federal Share Obligated", "Benefit Cost Ratio", "Net Value Benefits"
          ),
          selected = "Number Of Properties"
        ),
        
        selectInput(
          "yvariable",
          label = "Y Variable",
          choices = c("Number Of Properties",	"Number Of Final Properties", "Cost Share Percentage",
                      "Project Amount",	"Federal Share Obligated", "Benefit Cost Ratio", "Net Value Benefits"
          ),
          selected = "Cost Share Percentage"
        ),
        
        
        # This date range is used to reduce the number of points plotted 
        # so we can see what is there in 2020 or from 2002-2012
        sliderInput("HMAdates", "Date Range:",
                    min = 1998, max = 2021,
                    value = c(1998,2021),
                    sep = ""),
        
        verbatimTextOutput("summary")
        
      )
    } else {
      
    }
  })
  
  output$details = renderUI({
    if(input$mapdataset == "Individuals and Household Programs" && length(input$dates) != 0) {
      
      pIHP = IHP[IHP$disasterYear %in% seq(input$dates[1], input$dates[2], by = 1), ]
      apps_per_state = aggregate(list("numCountiesAffected" = pIHP$damagedStateAbbreviation), by = list(pIHP$damagedStateAbbreviation), FUN = length)
      
      pIHP <- aggregate(list("IHPTotal" = pIHP$sum.ihpAmount.CPIAdjusted,
                             "HATotal" = pIHP$sum.haAmount.CPIAdjusted,
                             "ONATotal" = pIHP$sum.onaAmount.CPIAdjusted,
                             "Rental" = pIHP$sum.rentalAssistanceAmount.CPIAdjusted,
                             "Repair"= pIHP$sum.repairAmount.CPIAdjusted,
                             "Replacement"= pIHP$sum.replacementAmount.CPIAdjusted,
                             "personalNeeds"= pIHP$sum.personalPropertyAmount.CPIAdjusted,
                             "rpfvl" = pIHP$sum.rpfvl.CPIAdjusted,
                             "ppfvl" = pIHP$sum.ppfvl.CPIAdjusted,
                             "roofdmg"= pIHP$sum.roofDamageAmount.CPIAdjusted ,
                             "foundationdmg" = pIHP$sum.foundationDamageAmount.CPIAdjusted,
                             "numApplicants" = pIHP$numApplicant,
                             "primaryResidence" = pIHP$sum.primaryResidence.,
                             "numHomeOwnersInsurance" = pIHP$sum.homeOwnersInsurance.,
                             "numFloodInsurance" = pIHP$sum.floodInsurance.,
                             "numIHPReferral" = pIHP$sum.ihpReferral.,
                             "numIHPEligible" = pIHP$sum.ihpEligible.,
                             "numHAReferral" = pIHP$sum.haReferral.,
                             "numHAEligible" = pIHP$sum.haEligible.,
                             "numONAReferral" = pIHP$sum.onaReferral.,
                             "numONAEligible" = pIHP$sum.onaEligible.,
                             "numUtilitiesOut" = pIHP$sum.utilitiesOut.,
                             "numAutoDamage" = pIHP$sum.autoDamage.,
                             "numEmergencyNeed" = pIHP$sum.emergencyNeeds.,
                             "numFoodNeed" = pIHP$sum.foodNeed.,
                             "numShelterNeed" = pIHP$sum.shelterNeed.,
                             "numSBAEligible" = pIHP$sum.sbaEligible.,
                             "numSBAApproved" = pIHP$sum.sbaApproved.,
                             "numTSAEligible" = pIHP$sum.tsaEligible.,
                             "numTSACheckedIn" = pIHP$sum.tsaCheckedIn.,
                             "numRenter" = pIHP$numRenter,
                             "numOwner" = pIHP$numOwner,
                             "numTotalResidentsAffected" = pIHP$totalResidentsAffected,
                             "numHousesDuplex" = pIHP$numHousesDuplex,
                             "numMobileHome" = pIHP$numMobileHome,
                             "numApartment" = pIHP$numApartment,
                             "numCondo" = pIHP$numCondo,
                             "numBoat" = pIHP$numBoat,
                             "numTrialer" = pIHP$numTrialer,
                             "numTownhouse" = pIHP$numTownhouse,
                             "numCorrectionalFacility" = pIHP$numCorrectionalFacility,
                             "numAssistedLivingFacility" = pIHP$numAssistedLivingFacility,
                             "numMilitaryHousing" = pIHP$numMilitaryHousing,
                             "numCollegeDorm" = pIHP$numCollegeDorm,
                             "numUnknownBuilding" = pIHP$numUnknown,
                             "numOtherResidence" = pIHP$numOtherResidence,
                             "applicantAvgIncome" = pIHP$avgIncome),
                        by = list("X"=pIHP$damagedStateAbbreviation), FUN = sum)
      pIHP <- merge(pIHP, apps_per_state, by.x = c("X"), by.y = c("Group.1"))
      
      pIHP$GEOID <- fips(pIHP$X)
      pIHP <- na.omit(pIHP)
      
      #Remove Hawaii
      pIHP[pIHP$X != "HI", ]
      
      group = c()
      if(length(input$map_groups) >= 1) {
        group = switch(input$map_groups,
                       "Individual Household Program" = "IHPTotal",
                       "Housing Assistance" = "HATotal",
                       "Other Needs Assistance" = "ONATotal", 
                       "Personal Needs Assistance" = "personalNeeds",
                       "Rental Assistance" = "Rental",
                       "Repair Assistance" = "Repair", 
                       "Replacement Assistance" = "Replacement",
                       "Roof Damage" = "roofdmg",
                       "Foundation Damage" = "foundationdmg",
                       "Applicants" = "numApplicants",
                       "Primary Residence" = "primaryResidence",
                       "Home Owners Insurance" = "numHomeOwnersInsurance",
                       "Flood Insurance" = "numFloodInsurance",
                       "IHP Referral" = "numIHPReferral", 
                       "IHP Eligible" = "numIHPEligible", 
                       "HA Referral" = "numHAReferral",
                       "HA Eligible" = "numHAEligible",
                       "ONA Referral" = "numONAReferral",
                       "ONA Eligible" = "numONAEligible",
                       "Utilities Out" = "numUtilitiesOut",
                       "Auto Damage" = "numAutoDamage",
                       "Emergency Needs" = "numEmergencyNeed",
                       "Food Needs" = "numFoodNeed", 
                       "Shelter Needs" = "numShelterNeed",
                       "SBA Eligible" = "numSBAEligible",
                       "SBA Approved" = "numSBAApproved", 
                       "TSA Eligible" = "numTSAEligible",
                       "TSA Checked In" = "numTSACheckedIn",
                       "Renters" = "numRenter",
                       "Owners" = "numOwner",
                       "Residents Affected" = "numTotalResidentsAffected",
                       "House&Duplexes" = "numHousesDuplex",
                       "Mobile Homes" = "numMobileHome",
                       "Apartments" = "numApartment",
                       "Condos" = "numCondo",
                       "Boats" = "numBoat", 
                       "Trialers" = "numTrialer",
                       "Townhouses" = "numTownhouse",
                       "Correctional Facilities" = "numCorrectionalFacility",
                       "Assisted Living Facilities" = "numAssistedLivingFacility",
                       "Military Houses" = "numMilitaryHousing",
                       "College Dorms" = "numCollegeDorm", 
                       "Unknown Buildings" = "numUnknownBuilding",
                       "Other Residences" = "numOtherResidence",
                       "Average Applicant Income" = "applicantAvgIncome"
        )
      }
      
      if(length(input$map_shape_click$id) != 0 && input$map_zoom > 5) {
        pIHP = pIHP[pIHP$X %in% c(state.abb[match(strsplit(input$map_shape_click$id, "_")[[1]][1], state.name)]), ]
      }
  
      if(length(group) >= 1) {
        if(input$map_zoom <= 5 || length(input$map_shape_click$id) == 0) {
          # If United States, look for average over all 48 states; not including Hawaii and Alaska
          div = 48
          the_avg = "Average(States):"
          counties_affect = ""
        } else {
          div = pIHP[["numCountiesAffected"]]
          the_avg = "Average Per Disaster In (Counties):"
          counties_affect = paste0("<font size = '2'>Counties Affected In All Disasters: </font><br>", div)
        }
        
        if(group %in% c("IHPTotal", "HATotal", "ONATotal", "personalNeeds", "Replacement", "Repair", "Rental", "roofdmg", "foundationdmg")) {
          HTML(toString(paste0("<font size='2'>", input$map_groups, "</font>", "<br>", "$", prettyNum(ceiling(sum(pIHP[[group]])), big.mark=",",scientific=FALSE), "<br><br>",
                               "<font size='2'>", the_avg, "</font>" ,"<br>", " $", prettyNum(ceiling(sum(pIHP[[group]]/div)), big.mark=",",scientific=FALSE), "<br><br>",
                               counties_affect
                               , sep = "")
                        ))        
        } else if(group %in% c("applicantAvgIncome")){
          HTML(toString(paste0("<font size='2'>", "Total Income", "</font>", "<br>", "$", prettyNum(ceiling(sum(pIHP[[group]])), big.mark=",",scientific=FALSE), "<br><br>",
                               "<font size='2'>", the_avg, "</font>" ,"<br>", " $", prettyNum(ceiling(sum(pIHP[[group]]/div)), big.mark=",",scientific=FALSE), "<br><br>",
                               counties_affect
                               , sep = "")
                        ))
        } else {
          HTML(toString(paste0("<font size='2'>", input$map_groups, "</font>", "<br>", prettyNum(ceiling(sum(pIHP[[group]])), big.mark=",",scientific=FALSE), "<br><br>" ,
                               "<font size='2'>", the_avg, "</font>" ,"<br>", prettyNum(ceiling(sum(pIHP[[group]]/div)), big.mark=",",scientific=FALSE), "<br><br>",
                               counties_affect
                               
                               , sep = "")
      
                        ))        
        }
      } 
      
    } else if(input$mapdataset == "Hazard Mitigation Assistance") {
      pHMA = HMA[substring(HMA$dateApproved, 1, 4) %in% seq(input$dates[1], input$dates[2], by = 1), ]
      
      aHMA <- aggregate(list("Cost Share Percentage" = pHMA$costSharePercentage,
                             "Benefit Cost Ratio" = pHMA$benefitCostRatio
      ),
      by = list("X"=pHMA$state), FUN = mean, na.rm = TRUE)
      
      pHMA <- aggregate(list("Starting Properties" = pHMA$numberOfProperties,
                             "Ending Properties" = pHMA$numberOfFinalProperties,
                             "Project Amount" = pHMA$projectAmount,
                             "Federal Shares Obligated" = pHMA$federalShareObligated,
                             "Grantee Tribal Indicator" = pHMA$granteeTribalIndicator,
                             "Net Value Benefits" = pHMA$netValueBenefits,
                             "Subgrantee Tribal Indicator" = pHMA$subgranteeTribalIndicator
      ), by = list("X"=pHMA$state), FUN = sum, na.rm = TRUE)
      
      
      pHMA = merge(pHMA, aHMA, by = c("X"))
      pHMA$GEOID <- fips(pHMA$X)
      
      group = c()
      if(length(input$map_groups) >= 1) {
        group = switch(input$map_groups,
                       "Starting Properties" = "Starting.Properties",
                       "Ending Properties" = "Ending.Properties",
                       "Project Amount" = "Project.Amount",
                       "Federal Shares Obligated" = "Federal.Shares.Obligated",
                       "Grantee Tribal Indicator" = "Grantee.Tribal.Indicator",
                       "Net Value Benefits" = "Net.Value.Benefits",
                       "Subgrantee Tribal Indicator" = "Subgrantee.Tribal.Indicator",
                       "Cost Share Percentage" = "Cost.Share.Percentage",
                       "Benefit Cost Ratio" = "Benefit.Cost.Ratio"
        )
      }
      
      if(length(input$map_shape_click$id) != 0 && input$map_zoom > 5) {
        pHMA = pHMA[pHMA$X %in% c(strsplit(input$map_shape_click$id, "_")[[1]][1]), ]
      }
      print(input$map_shape_click$id)
      
      if(length(group) >= 1) {
        if(input$map_zoom <= 5 || length(input$map_shape_click$id) == 0) {
          # If United States, look for average over all 48 states; not including Hawaii and Alaska
          div = 48
          the_avg = "Average(States):"
          counties_affect = ""
        } 
        
        if(group %in% c("Project.Amount", "Federal.Shares.Obligated")) {
          HTML(toString(paste0("<font size='2'>", input$map_groups, "</font>", "<br>", "$", prettyNum(ceiling(sum(pHMA[[group]])), big.mark=",", scientific=FALSE), "<br><br>"
                               , sep = "")
          ))        
        } else if(group %in% c("Cost.Share.Percentage")){
          HTML(toString(paste0("<font size='2'>", input$map_groups, "</font>", "<br> % ", prettyNum((mean(pHMA[[group]])), big.mark=",", scientific=FALSE), "<br><br>" 
                               , sep = "")
          ))        
        } else if(group %in% c("Benefit.Cost.Ratio")) {
          HTML(toString(paste0("<font size='2'>", input$map_groups, "</font>", "<br> Ratio: ", prettyNum((mean(pHMA[[group]])), big.mark=",", scientific=FALSE), "<br><br>" 
                               , sep = "")
          ))    
        } else if(group %in% c("Net.Value.Benefits")){
          HTML(toString(paste0("<font size='2'>", input$map_groups, "</font>", "<br> Total Value: ", prettyNum(ceiling(sum(log(pHMA[[group]], 1.1))), big.mark=",", scientific=FALSE), "<br><br>" 
                               , sep = "")
          ))  
        } else {
          HTML(toString(paste0("<font size='2'>", input$map_groups, "</font>", "<br> Total: ", prettyNum(ceiling(sum(pHMA[[group]])), big.mark=",", scientific=FALSE), "<br><br>" 
                               , sep = "")
          ))  
        }
      } 
      
    }
  })
  
  observeEvent(input$map_groups ,{
    if(input$mapdataset == "Individuals and Household Programs" && length(input$dates) != 0) {
      values$currGroup <- input$map_groups
  
      pIHP = IHP[IHP$disasterYear %in% seq(input$dates[1], input$dates[2], by = 1), ]
      apps_per_state = aggregate(list("numCountiesAffected" = pIHP$damagedStateAbbreviation), by = list(pIHP$damagedStateAbbreviation), FUN = length)
      
      pIHP <- aggregate(list("IHPTotal" = pIHP$sum.ihpAmount.CPIAdjusted,
                             "HATotal" = pIHP$sum.haAmount.CPIAdjusted,
                             "ONATotal" = pIHP$sum.onaAmount.CPIAdjusted,
                             "Rental" = pIHP$sum.rentalAssistanceAmount.CPIAdjusted,
                             "Repair"= pIHP$sum.repairAmount.CPIAdjusted,
                             "Replacement"= pIHP$sum.replacementAmount.CPIAdjusted,
                             "personalNeeds"= pIHP$sum.personalPropertyAmount.CPIAdjusted,
                             "rpfvl" = pIHP$sum.rpfvl.CPIAdjusted,
                             "ppfvl" = pIHP$sum.ppfvl.CPIAdjusted,
                             "roofdmg"= pIHP$sum.roofDamageAmount.CPIAdjusted ,
                             "foundationdmg" = pIHP$sum.foundationDamageAmount.CPIAdjusted,
                             "numApplicants" = pIHP$numApplicant,
                             "primaryResidence" = pIHP$sum.primaryResidence.,
                             "numHomeOwnersInsurance" = pIHP$sum.homeOwnersInsurance.,
                             "numFloodInsurance" = pIHP$sum.floodInsurance.,
                             "numIHPReferral" = pIHP$sum.ihpReferral.,
                             "numIHPEligible" = pIHP$sum.ihpEligible.,
                             "numHAReferral" = pIHP$sum.haReferral.,
                             "numHAEligible" = pIHP$sum.haEligible.,
                             "numONAReferral" = pIHP$sum.onaReferral.,
                             "numONAEligible" = pIHP$sum.onaEligible.,
                             "numUtilitiesOut" = pIHP$sum.utilitiesOut.,
                             "numAutoDamage" = pIHP$sum.autoDamage.,
                             "numEmergencyNeed" = pIHP$sum.emergencyNeeds.,
                             "numFoodNeed" = pIHP$sum.foodNeed.,
                             "numShelterNeed" = pIHP$sum.shelterNeed.,
                             "numSBAEligible" = pIHP$sum.sbaEligible.,
                             "numSBAApproved" = pIHP$sum.sbaApproved.,
                             "numTSAEligible" = pIHP$sum.tsaEligible.,
                             "numTSACheckedIn" = pIHP$sum.tsaCheckedIn.,
                             "numRenter" = pIHP$numRenter,
                             "numOwner" = pIHP$numOwner,
                             "numTotalResidentsAffected" = pIHP$totalResidentsAffected,
                             "numHousesDuplex" = pIHP$numHousesDuplex,
                             "numMobileHome" = pIHP$numMobileHome,
                             "numApartment" = pIHP$numApartment,
                             "numCondo" = pIHP$numCondo,
                             "numBoat" = pIHP$numBoat,
                             "numTrialer" = pIHP$numTrialer,
                             "numTownhouse" = pIHP$numTownhouse,
                             "numCorrectionalFacility" = pIHP$numCorrectionalFacility,
                             "numAssistedLivingFacility" = pIHP$numAssistedLivingFacility,
                             "numMilitaryHousing" = pIHP$numMilitaryHousing,
                             "numCollegeDorm" = pIHP$numCollegeDorm,
                             "numUnknownBuilding" = pIHP$numUnknown,
                             "numOtherResidence" = pIHP$numOtherResidence,
                             "applicantAvgIncome" = pIHP$avgIncome),
                        by = list("X"=pIHP$damagedStateAbbreviation), FUN = sum)
      
      pIHP$GEOID <- fips(pIHP$X)
      pIHP <- na.omit(pIHP)
      pIHP <- merge(pIHP, apps_per_state, by.x = c("X"), by.y = c("Group.1"))
      
      leaflet <- merge(us.state.map, pIHP, by = c("GEOID"))
      bins <- c(1,2,3,4,5,6,7,8,9)
      
      group = switch(input$map_groups,
                     "Individual Household Program" = "IHPTotal",
                     "Housing Assistance" = "HATotal",
                     "Other Needs Assistance" = "ONATotal", 
                     "Personal Needs Assistance" = "personalNeeds",
                     "Rental Assistance" = "Rental",
                     "Repair Assistance" = "Repair", 
                     "Replacement Assistance" = "Replacement",
                     "Roof Damage" = "roofdmg",
                     "Foundation Damage" = "foundationdmg",
                     "Applicants" = "numApplicants",
                     "Primary Residence" = "primaryResidence",
                     "Home Owners Insurance" = "numHomeOwnersInsurance",
                     "Flood Insurance" = "numFloodInsurance",
                     "IHP Referral" = "numIHPReferral", 
                     "IHP Eligible" = "numIHPEligible", 
                     "HA Referral" = "numHAReferral",
                     "HA Eligible" = "numHAEligible",
                     "ONA Referral" = "numONAReferral",
                     "ONA Eligible" = "numONAEligible",
                     "Utilities Out" = "numUtilitiesOut",
                     "Auto Damage" = "numAutoDamage",
                     "Emergency Needs" = "numEmergencyNeed",
                     "Food Needs" = "numFoodNeed", 
                     "Shelter Needs" = "numShelterNeed",
                     "SBA Eligible" = "numSBAEligible",
                     "SBA Approved" = "numSBAApproved", 
                     "TSA Eligible" = "numTSAEligible",
                     "TSA Checked In" = "numTSACheckedIn",
                     "Renters" = "numRenter",
                     "Owners" = "numOwner",
                     "Residents Affected" = "numTotalResidentsAffected",
                     "House&Duplexes" = "numHousesDuplex",
                     "Mobile Homes" = "numMobileHome",
                     "Apartments" = "numApartment",
                     "Condos" = "numCondo",
                     "Boats" = "numBoat", 
                     "Trialers" = "numTrialer",
                     "Townhouses" = "numTownhouse",
                     "Correctional Facilities" = "numCorrectionalFacility",
                     "Assisted Living Facilities" = "numAssistedLivingFacility",
                     "Military Houses" = "numMilitaryHousing",
                     "College Dorms" = "numCollegeDorm", 
                     "Unknown Buildings" = "numUnknownBuilding",
                     "Other Residences" = "numOtherResidence",
                     "Average Applicant Income" = "applicantAvgIncome"
      )
      
  
      
      if(group %in% c("IHPTotal", "HATotal", "ONATotal", "personalNeeds", "Replacement", "Repair", "Rental")) {
        pal = colorQuantile("Greens", NULL, n = 9)
        popup = paste0(leaflet$NAME, "-",input$map_groups,
                       "<br><strong>US Dollars: $</strong>",
                       prettyNum(leaflet[[group]], big.mark=",")
        )
        
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(leaflet[[group]]),
                      group = input$map_groups,
                      fillOpacity = 0.5,
                      color = "#BDBDC3",
                      weight = 1,
                      layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                      popup = popup,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE))
      } else if(group %in% c("roofdmg", "foundationdmg")) {
        pal = colorQuantile("Reds", NULL, n = 9)
        popup = paste0(leaflet$NAME, "-",input$map_groups,
                       "<br><strong>US Dollars: $</strong>",
                       prettyNum(leaflet[[group]], big.mark=",")
        )
        
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(leaflet[[group]]),
                      group = input$map_groups,
                      fillOpacity = 0.5,
                      color = "#BDBDC3",
                      weight = 1,
                      layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                      popup = popup,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE))
      } else if (group %in% c("applicantAvgIncome")) {
        pal = colorQuantile("Greens", NULL, n = 9)
        popup = paste0(leaflet$NAME, "-", input$map_groups,
                       "<br><strong>Average Income(per Applicant): </strong>",
                       prettyNum(leaflet[[group]]/leaflet[["numCountiesAffected"]],  big.mark=",")
        )
      
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(leaflet[[group]]/leaflet[["numCountiesAffected"]]),
                      group = input$map_groups,
                      fillOpacity = 0.5,
                      color = "#BDBDC3",
                      weight = 1,
                      layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                      popup = popup,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE))    
  
      } else if(group %in% c("numApplicants")){
        pal = colorQuantile("Blues", NULL, n = 9)
        popup = paste0(leaflet$NAME, "-", input$map_groups,
                       "<br><strong>Total(Applicants Affected): </strong>",
                       prettyNum(leaflet[[group]],  big.mark=","),
                       "<br><strong>IHP Eligible %: </strong>",
                       prettyNum(leaflet[["numIHPEligible"]]/leaflet[[group]],  big.mark=",")
        )
        
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(leaflet[["numIHPEligible"]]/leaflet[[group]]),
                      group = input$map_groups,
                      fillOpacity = 0.5,
                      color = "#BDBDC3",
                      weight = 1,
                      layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                      popup = popup,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE))      
    
      } else if(group %in% c("numTSAEligible", "numTSACheckedIn", "numBoat", 
                             "numTrialer",
                             "numCorrectionalFacility",
                             "numAssistedLivingFacility",
                             "numMilitaryHousing",
                             "numCollegeDorm", 
                             "numUnknownBuilding",
                             "numOtherResidence")) {
        pal = colorQuantile("Blues", NULL, n = 5)
        leaflet = leaflet[leaflet@data[[group]] != 0, ]
        
        popup = paste0(leaflet$NAME, "-", input$map_groups,
                       "<br><strong>Total(Applicants Affected): </strong>",
                       prettyNum(leaflet[[group]],  big.mark=",")
        )
  
        
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(leaflet[[group]]),
                      group = input$map_groups,
                      fillOpacity = 0.5,
                      color = "#BDBDC3",
                      weight = 1,
                      layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                      popup = popup,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE))      
        
      } else {
        pal = colorQuantile("Blues", NULL, n = 5)
        popup = paste0(leaflet$NAME, "-", input$map_groups,
                       "<br><strong>Total(Applicants Affected): </strong>",
                       prettyNum(leaflet[[group]],  big.mark=","),
                       "<br><strong>Percentage(of Applicants): </strong>",
                       prettyNum(leaflet[[group]]/leaflet[["numApplicants"]],  big.mark=",")
        )
        
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(leaflet[[group]]/leaflet[["numApplicants"]]),
                      group = input$map_groups,
                      fillOpacity = 0.5,
                      color = "#BDBDC3",
                      weight = 1,
                      layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                      popup = popup,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE))      
      }
    } else if(input$mapdataset == "Hazard Mitigation Assistance") {
      values$currGroup <- input$map_groups
      
      pHMA = HMA[substring(HMA$dateApproved, 1, 4) %in% seq(input$dates[1], input$dates[2], by = 1), ]
      
      aHMA <- aggregate(list("Cost Share Percentage" = pHMA$costSharePercentage,
                             "Benefit Cost Ratio" = pHMA$benefitCostRatio
      ),
      by = list("X"=pHMA$state), FUN = mean, na.rm = TRUE)
      
      pHMA <- aggregate(list("Starting Properties" = pHMA$numberOfProperties,
                             "Ending Properties" = pHMA$numberOfFinalProperties,
                             "Project Amount" = pHMA$projectAmount,
                             "Federal Shares Obligated" = pHMA$federalShareObligated,
                             "Grantee Tribal Indicator" = pHMA$granteeTribalIndicator,
                             "Net Value Benefits" = pHMA$netValueBenefits,
                             "Subgrantee Tribal Indicator" = pHMA$subgranteeTribalIndicator
      ), by = list("X"=pHMA$state), FUN = sum, na.rm = TRUE)
      
     
      pHMA = merge(pHMA, aHMA, by = c("X"))
      pHMA$GEOID <- fips(pHMA$X)

      leaflet <- merge(us.state.map, pHMA, by = c("GEOID"))

      pal <- colorQuantile("Greens", NULL, n = 9)
      pal_dmg <- colorQuantile("Reds", NULL, n = 9)
      pal_demographics <- colorQuantile("Blues", NULL, n = 9)
      bins <- c(1,2,3,4,5,6,7,8,9)
      
      group = switch(input$map_groups,
                    "Starting Properties" = "Starting.Properties",
                    "Ending Properties" = "Ending.Properties",
                    "Project Amount" = "Project.Amount",
                    "Federal Shares Obligated" = "Federal.Shares.Obligated",
                    "Grantee Tribal Indicator" = "Grantee.Tribal.Indicator",
                    "Net Value Benefits" = "Net.Value.Benefits",
                    "Subgrantee Tribal Indicator" = "Subgrantee.Tribal.Indicator",
                    "Cost Share Percentage" = "Cost.Share.Percentage",
                    "Benefit Cost Ratio" = "Benefit.Cost.Ratio"
      )
      
      
      if(group %in% c("Starting.Properties", "Ending.Properties", "Project.Amount",
                      "Federal.Shares.Obligated", "Cost.Share.Percentage", "Benefit.Cost.Ratio")) {
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>% 
          addPolygons(fillColor = ~pal(leaflet[[group]]), 
                      group = input$map_groups,
                      fillOpacity = 0.5, 
                      color = "#BDBDC3", 
                        weight = 1,
                        layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                        popup = paste0(leaflet$NAME, 
                                       "<br><strong>Amount: </strong>",
                                       prettyNum(leaflet[[group]], big.mark=",")
                        ),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE))
      } else {
        leafletProxy("map", data = leaflet) %>%
          clearShapes() %>% 
          addPolygons(fillColor = ~pal(log(leaflet[[group]], 1.1)), 
                      group = input$map_groups,
                      fillOpacity = 0.5, 
                      color = "#BDBDC3", 
                      weight = 1,
                      layerId = as.vector(paste(leaflet$NAME, group, "StateID", sep = "_")),
                      popup = paste0(leaflet$NAME, 
                                     "<br><strong>Amount: </strong>",
                                     prettyNum(log(leaflet[[group]], 1.1), big.mark=",")
                      ),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE))
      }


    } else {
      
    }
  })

  
  output$text <-
    renderPrint({reactiveValuesToList(input)})
  
  observeEvent(input$map_shape_click, { 

    leafletProxy("map", session) %>%
      flyTo(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng, zoom = 7)
      
    
    output$curr_state <- renderText({
      strsplit(input$map_shape_click$id, "_")[[1]][1]
    })
  })
  
  output$curr_state <- renderText({
    "United States"
  })
  
  observeEvent(input$map_zoom, {
    if(input$map_zoom <= 5 || length(input$map_shape_click$id) == 0) {
      output$curr_state <- renderText({"United States"})
    } else {
      output$curr_state <-renderText({strsplit(input$map_shape_click$id, "_")[[1]][1]})
    }
  })
  
  
  output$map <- renderLeaflet({
    if(input$mapdataset == "Individuals and Household Programs") {
      pIHP = IHP[IHP$disasterYear %in% seq(2002, 2021, by = 1), ]
      
      pIHP <- aggregate(list("IHPTotal" = pIHP$sum.ihpAmount.CPIAdjusted,
                             "HATotal" = pIHP$sum.haAmount.CPIAdjusted,
                             "ONATotal" = pIHP$sum.onaAmount.CPIAdjusted,
                             "Rental" = pIHP$sum.rentalAssistanceAmount.CPIAdjusted,
                             "Repair"= pIHP$sum.repairAmount.CPIAdjusted,
                             "Replacement"= pIHP$sum.replacementAmount.CPIAdjusted,
                             "personalNeeds"= pIHP$sum.personalPropertyAmount.CPIAdjusted,
                             "rpfvl" = pIHP$sum.rpfvl.CPIAdjusted,
                             "ppfvl" = pIHP$sum.ppfvl.CPIAdjusted,
                             "roofdmg"= pIHP$sum.roofDamageAmount.CPIAdjusted ,
                             "foundationdmg" = pIHP$sum.foundationDamageAmount.CPIAdjusted,
                             "numApplicants" = pIHP$numApplicant,
                             "primaryResidence" = pIHP$sum.primaryResidence.,
                             "numHomeOwnersInsurance" = pIHP$sum.homeOwnersInsurance.,
                             "numFloodInsurance" = pIHP$sum.floodInsurance.,
                             "numIHPReferral" = pIHP$sum.ihpReferral.,
                             "numIHPEligible" = pIHP$sum.ihpEligible.,
                             "numHAReferral" = pIHP$sum.haReferral.,
                             "numHAEligible" = pIHP$sum.haEligible.,
                             "numONAReferral" = pIHP$sum.onaReferral.,
                             "numONAEligible" = pIHP$sum.onaEligible.,
                             "numUtilitiesOut" = pIHP$sum.utilitiesOut.,
                             "numAutoDamage" = pIHP$sum.autoDamage.,
                             "numEmergencyNeed" = pIHP$sum.emergencyNeeds.,
                             "numFoodNeed" = pIHP$sum.foodNeed.,
                             "numShelterNeed" = pIHP$sum.shelterNeed.,
                             "numSBAEligible" = pIHP$sum.sbaEligible.,
                             "numSBAApproved" = pIHP$sum.sbaApproved.,
                             "numTSAEligible" = pIHP$sum.tsaEligible.,
                             "numTSACheckedIn" = pIHP$sum.tsaCheckedIn.,
                             "numRenter" = pIHP$numRenter,
                             "numOwner" = pIHP$numOwner,
                             "numTotalResidentsAffected" = pIHP$totalResidentsAffected,
                             "numHousesDuplex" = pIHP$numHousesDuplex,
                             "numMobileHome" = pIHP$numMobileHome,
                             "numApartment" = pIHP$numApartment,
                             "numCondo" = pIHP$numCondo,
                             "numBoat" = pIHP$numBoat,
                             "numTrialer" = pIHP$numTrialer,
                             "numTownhouse" = pIHP$numTownhouse,
                             "numCorrectionalFacility" = pIHP$numCorrectionalFacility,
                             "numAssistedLivingFacility" = pIHP$numAssistedLivingFacility,
                             "numMilitaryHousing" = pIHP$numMilitaryHousing,
                             "numCollegeDorm" = pIHP$numCollegeDorm,
                             "numUnknownBuilding" = pIHP$numUnknown,
                             "numOtherResidence" = pIHP$numOtherResidence,
                             "applicantAvgIncome" = pIHP$avgIncome),
                        by = list("X"=pIHP$damagedStateAbbreviation), FUN = sum)
      
      pIHP$GEOID <- fips(pIHP$X)
      pIHP <- na.omit(pIHP)
      
      leaflet <- merge(us.state.map, pIHP, by = c("GEOID"))
      
      leaflet(data = leaflet) %>% 
        addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 12)) %>% 
        setView(data, lat = 39, lng = -97, zoom = 5) %>% 
  
        addLayersControl(
          position = "bottomright",
          baseGroups = c("Individual Household Program", "Housing Assistance",
                         "Other Needs Assistance", "Personal Needs Assistance",
                         "Rental Assistance", "Repair Assistance", 
                         "Replacement Assistance", "Roof Damage", "Foundation Damage",
                         "Applicants", "Primary Residence", "Home Owners Insurance",
                         "Flood Insurance", "IHP Referral", "IHP Eligible", "HA Referral",
                         "HA Eligible", "ONA Referral", "ONA Eligible", "Utilities Out",
                         "Auto Damage", "Emergency Needs", "Food Needs", "Shelter Needs",
                         "SBA Eligible", "SBA Approved", "TSA Eligible", "TSA Checked In",
                         "Renters", "Owners", "Residents Affected", "House&Duplexes",
                         "Mobile Homes", "Apartments", "Condos", "Boats", "Trialers",
                         "Townhouses", "Correctional Facilities", "Assisted Living Facilities",
                         "Military Houses", "College Dorms", "Unknown Buildings", "Other Residences",
                         "Average Applicant Income"),
          options = layersControlOptions(collapsed = TRUE))
    } else if(input$mapdataset == "Hazard Mitigation Assistance") {
      #`%!in%` <- Negate(`%in%`)
      #pHMA <- HMA[substring(HMA$dateInitiallyApproved, 1, 4) %!in% append(seq(1990, 2021, by = 1), c("0016", 1018, 7019, NA)), ]
      
      pHMA <- aggregate(list("Starting Properties" = HMA$numberOfProperties,
                             "Ending Properties" = HMA$numberOfFinalProperties,
                             "Project Amount" = HMA$projectAmount,
                             "Federal Shares Obligated" = HMA$federalShareObligated,
                             "Grantee Tribal Indicator" = HMA$granteeTribalIndicator,
                             "Net Value Benefits" = HMA$netValueBenefits,
                             "Subgrantee Tribal Indicator" = HMA$subgranteeTribalIndicator
                             ),
                        by = list("X"=HMA$state), FUN = sum)
      
      aHMA <- aggregate(list("Cost Share Percentage" = HMA$costSharePercentage,
                             "Benefit Cost Ratio" = HMA$benefitCostRatio
                             ),
      by = list("X"=HMA$state), FUN = mean)
      
      pHMA = merge(pHMA, aHMA, by = c("X"))
      pHMA$GEOID <- fips(pHMA$X)
      leaflet <- merge(us.state.map, pHMA, by = c("GEOID"))
      
      leaflet(data = leaflet) %>% 
        addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 12)) %>% 
        setView(data, lat = 39, lng = -97, zoom = 5) %>% 
        
        addLayersControl(
          position = "bottomright",
          baseGroups = c("Starting Properties", "Ending Properties",
                         "Project Amount", "Federal Shares Obligated",
                         "Net Value Benefits", 
                         "Cost Share Percentage", "Benefit Cost Ratio"
                         ),
          options = layersControlOptions(collapsed = TRUE))

    }
  })
  
  
  observe({
    if(input$mapdataset == "Individuals and Household Programs" && length(input$dates) != 0) {
      pIHP = IHP[IHP$disasterYear %in% seq(input$dates[1], input$dates[2], by = 1), ]
      pIHP <- aggregate(list("IHPTotal" = pIHP$sum.ihpAmount.CPIAdjusted,
                             "HATotal" = pIHP$sum.haAmount.CPIAdjusted,
                             "ONATotal" = pIHP$sum.onaAmount.CPIAdjusted,
                             "Rental" = pIHP$sum.rentalAssistanceAmount.CPIAdjusted,
                             "Repair"= pIHP$sum.repairAmount.CPIAdjusted,
                             "Replacement"= pIHP$sum.replacementAmount.CPIAdjusted,
                             "personalNeeds"= pIHP$sum.personalPropertyAmount.CPIAdjusted,
                             "rpfvl" = pIHP$sum.rpfvl.CPIAdjusted,
                             "ppfvl" = pIHP$sum.ppfvl.CPIAdjusted,
                             "roofdmg"= pIHP$sum.roofDamageAmount.CPIAdjusted ,
                             "foundationdmg" = pIHP$sum.foundationDamageAmount.CPIAdjusted,
                             "numApplicants" = pIHP$numApplicant,
                             "primaryResidence" = pIHP$sum.primaryResidence.,
                             "numHomeOwnersInsurance" = pIHP$sum.homeOwnersInsurance.,
                             "numFloodInsurance" = pIHP$sum.floodInsurance.,
                             "numIHPReferral" = pIHP$sum.ihpReferral.,
                             "numIHPEligible" = pIHP$sum.ihpEligible.,
                             "numHAReferral" = pIHP$sum.haReferral.,
                             "numHAEligible" = pIHP$sum.haEligible.,
                             "numONAReferral" = pIHP$sum.onaReferral.,
                             "numONAEligible" = pIHP$sum.onaEligible.,
                             "numUtilitiesOut" = pIHP$sum.utilitiesOut.,
                             "numAutoDamage" = pIHP$sum.autoDamage.,
                             "numEmergencyNeed" = pIHP$sum.emergencyNeeds.,
                             "numFoodNeed" = pIHP$sum.foodNeed.,
                             "numShelterNeed" = pIHP$sum.shelterNeed.,
                             "numSBAEligible" = pIHP$sum.sbaEligible.,
                             "numSBAApproved" = pIHP$sum.sbaApproved.,
                             "numTSAEligible" = pIHP$sum.tsaEligible.,
                             "numTSACheckedIn" = pIHP$sum.tsaCheckedIn.,
                             "numRenter" = pIHP$numRenter,
                             "numOwner" = pIHP$numOwner,
                             "numTotalResidentsAffected" = pIHP$totalResidentsAffected,
                             "numHousesDuplex" = pIHP$numHousesDuplex,
                             "numMobileHome" = pIHP$numMobileHome,
                             "numApartment" = pIHP$numApartment,
                             "numCondo" = pIHP$numCondo,
                             "numBoat" = pIHP$numBoat,
                             "numTrialer" = pIHP$numTrialer,
                             "numTownhouse" = pIHP$numTownhouse,
                             "numCorrectionalFacility" = pIHP$numCorrectionalFacility,
                             "numAssistedLivingFacility" = pIHP$numAssistedLivingFacility,
                             "numMilitaryHousing" = pIHP$numMilitaryHousing,
                             "numCollegeDorm" = pIHP$numCollegeDorm,
                             "numUnknownBuilding" = pIHP$numUnknown,
                             "numOtherResidence" = pIHP$numOtherResidence,
                             "applicantAvgIncome" = pIHP$avgIncome),
                        by = list("X"=pIHP$damagedStateAbbreviation), FUN = sum)
      
      pIHP$GEOID <- fips(pIHP$X)
      pIHP <- na.omit(pIHP)
      
      leaflet <- merge(us.state.map, pIHP, by = c("GEOID"))
      
      pal <- colorQuantile("Greens", NULL, n = 9)
      pal_dmg <- colorQuantile("Reds", NULL, n = 9)
      pal_demographics <- colorQuantile("Blues", NULL, n = 9)
      bins <- c(1,2,3,4,5,6,7,8,9)
      
      leafletProxy("map", data = leaflet) %>%
        clearShapes() %>% 
        addPolygons(fillColor = ~pal(leaflet[["IHPTotal"]]), 
                    group = "Individual Household Program",
                    fillOpacity = 0.5, 
                    color = "#BDBDC3", 
                    weight = 1,
                    layerId = as.vector(paste(leaflet$NAME, "IHPTotal", "StateID", sep = "_")),
                    popup = paste0(leaflet$NAME, 
                                   "<br><strong>US Dollars: $</strong>",
                                   prettyNum(leaflet[["IHPTotal"]], big.mark=",")
                    ),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))
    } else if(input$mapdataset == "Hazard Mitigation Assistance") {
      #pHMA <- HMA[substring(HMA$dateInitiallyApproved, 1, 4) %!in% append(seq(1990, 2021, by = 1), c("0016", 1018, 7019, NA)), ]
      pHMA = HMA[substring(HMA$dateApproved, 1, 4) %in% seq(input$dates[1], input$dates[2], by = 1), ]
      
      aHMA <- aggregate(list("Cost Share Percentage" = pHMA$costSharePercentage,
                             "Benefit Cost Ratio" = pHMA$benefitCostRatio
      ),
      by = list("X"=pHMA$state), FUN = mean)
      
      pHMA <- aggregate(list("Starting Properties" = pHMA$numberOfProperties,
                             "Ending Properties" = pHMA$numberOfFinalProperties,
                             "Project Amount" = pHMA$projectAmount,
                             "Federal Shares Obligated" = pHMA$federalShareObligated,
                             "Grantee Tribal Indicator" = pHMA$granteeTribalIndicator,
                             "Net Value Benefits" = pHMA$netValueBenefits,
                             "Subgrantee Tribal Indicator" = pHMA$subgranteeTribalIndicator
      ), by = list("X"=pHMA$state), FUN = sum)
      
      
      pHMA = merge(pHMA, aHMA, by = c("X"))
      pHMA$GEOID <- fips(pHMA$X)
      leaflet <- merge(us.state.map, pHMA, by = c("GEOID"))
      
      pal <- colorQuantile("Greens", NULL, n = 9)
      pal_dmg <- colorQuantile("Reds", NULL, n = 9)
      pal_demographics <- colorQuantile("Blues", NULL, n = 9)
      bins <- c(1,2,3,4,5,6,7,8,9)
      
      
      leafletProxy("map", data = leaflet) %>%
        clearShapes() %>% 
        addPolygons(fillColor = ~pal(leaflet[["Starting.Properties"]]), 
                    group = "Starting Properties",
                    fillOpacity = 0.5, 
                    color = "#BDBDC3", 
                    weight = 1,
                    layerId = as.vector(paste(leaflet$NAME, "Starting.Properties", "StateID", sep = "_")),
                    popup = paste0(leaflet$NAME, 
                                   "<br><strong>Amount: </strong>",
                                   prettyNum(leaflet[["Starting.Properties"]], big.mark=",")
                    ),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))
    }
  })
  
  
  # create the chart output
  output$barchart <- renderPlot({
    if(input$clusterdataset == "Individuals and Household Programs" && length(input$groupby) != 0) {
      groupby= switch(input$groupby,
                      "Disaster-County" = "",
                      "Disaster Year" = "disasterYear",
                      "Incident Type" = "incidentType", 
                      "Disaster Number" = "disasterNumber",
                      "State" = "damagedStateAbbreviation",
                      "County" = "fip"
      )
      
      xvariable = switch(input$xvariable,
                         "Applicants" = "numApplicant",	
                         "Primary Residence" = "sum.primaryResidence.",	
                         "Home Owners Insurance" = "sum.homeOwnersInsurance.",
                         "Flood Insurance" = "sum.floodInsurance.",	
                         "IHP Referral"= "sum.ihpReferral.",
                         "IHP Eligible" = "sum.ihpEligible.",
                         "HA Referral" = "sum.haReferral.",
                         "HA Eligible" = "sum.haEligible.",
                         "ONA Referral" = "sum.onaReferral.",
                         "ONA Eligible" = "sum.onaEligible.",
                         "Utilities Out" = "sum.utilitiesOut.",
                         "Auto Damage" = "sum.autoDamage.",	
                         "Emergency Needs" = "sum.emergencyNeeds.",	
                         "Food Needs" = "sum.foodNeed.",
                         "Shelter Need" = "sum.shelterNeed.",
                         "SBA Eligible" = "sum.sbaEligible.",
                         "SBA Approved" = "sum.sbaApproved.",
                         "TSA Eligible" = "sum.tsaEligible.",
                         "TSA CheckedIn" = "sum.tsaCheckedIn.",
                         "Renters" = "numRenter",
                         "Owners" = "numOwner",
                         "Total Residents Affected" = "totalResidentsAffected",
                         "Houses and Duplex" = "numHousesDuplex",
                         "Mobile Homes" = "numMobileHome",
                         "Apartments" = "numApartment",
                         "Condos" = "numCondo",
                         "Boats" = "numBoat",	
                         "Trialers" = "numTrialer",
                         "Townhouses" = "numTownhouse",
                         "Correctional Facilities" = "numCorrectionalFacility",
                         "Assisted Living Facilities" = "numAssistedLivingFacility",
                         "Military Houses" = "numMilitaryHousing",
                         "College Dorms" = "numCollegeDorm",	
                         "Unknown Buildings" = "numUnknown",
                         "Other Residence" = "numOtherResidence",
                         "Average Income" = "avgIncome", 	
                         "IHP Amount" = "sum.ihpAmount.CPIAdjusted",
                         "HA Amount" = "sum.haAmount.CPIAdjusted",	
                         "ONA Amount" = "sum.onaAmount.CPIAdjusted",
                         "rpfvl" = "sum.rpfvl",
                         "ppfvl" = "sum.ppfvl",
                         "Rental Assistance Amount" = "sum.rentalAssistanceAmount.CPIAdjusted",
                         "Repair Assistance Amount" = "sum.repairAmount.CPIAdjusted",
                         "Replacement Assistance Amount" = "sum.replacementAmount.CPIAdjusted",
                         "Personal Property Amount" = "sum.personalPropertyAmount.CPIAdjusted",
                         "Roof Damage Amount" = "sum.roofDamageAmount.CPIAdjusted", 
                         "Foundation Damage Amount" = "sum.foundationDamageAmount.CPIAdjusted"
                         
      )
      
      yvariable = switch(input$yvariable,
                         "Applicants" = "numApplicant",	
                         "Primary Residence" = "sum.primaryResidence.",	
                         "Home Owners Insurance" = "sum.homeOwnersInsurance.",
                         "Flood Insurance" = "sum.floodInsurance.",	
                         "IHP Referral"= "sum.ihpReferral.",
                         "IHP Eligible" = "sum.ihpEligible.",
                         "HA Referral" = "sum.haReferral.",
                         "HA Eligible" = "sum.haEligible.",
                         "ONA Referral" = "sum.onaReferral.",
                         "ONA Eligible" = "sum.onaEligible.",
                         "Utilities Out" = "sum.utilitiesOut.",
                         "Auto Damage" = "sum.autoDamage.",	
                         "Emergency Needs" = "sum.emergencyNeeds.",	
                         "Food Needs" = "sum.foodNeed.",
                         "Shelter Need" = "sum.shelterNeed.",
                         "SBA Eligible" = "sum.sbaEligible.",
                         "SBA Approved" = "sum.sbaApproved.",
                         "TSA Eligible" = "sum.tsaEligible.",
                         "TSA CheckedIn" = "sum.tsaCheckedIn.",
                         "Renters" = "numRenter",
                         "Owners" = "numOwner",
                         "Total Residents Affected" = "totalResidentsAffected",
                         "Houses and Duplex" = "numHousesDuplex",
                         "Mobile Homes" = "numMobileHome",
                         "Apartments" = "numApartment",
                         "Condos" = "numCondo",
                         "Boats" = "numBoat",	
                         "Trialers" = "numTrialer",
                         "Townhouses" = "numTownhouse",
                         "Correctional Facilities" = "numCorrectionalFacility",
                         "Assisted Living Facilities" = "numAssistedLivingFacility",
                         "Military Houses" = "numMilitaryHousing",
                         "College Dorms" = "numCollegeDorm",	
                         "Unknown Buildings" = "numUnknown",
                         "Other Residence" = "numOtherResidence",
                         "Average Income" = "avgIncome", 	
                         "IHP Amount" = "sum.ihpAmount.CPIAdjusted",
                         "HA Amount" = "sum.haAmount.CPIAdjusted",	
                         "ONA Amount" = "sum.onaAmount.CPIAdjusted",
                         "rpfvl" = "sum.rpfvl",
                         "ppfvl" = "sum.ppfvl",
                         "Rental Assistance Amount" = "sum.rentalAssistanceAmount.CPIAdjusted",
                         "Repair Assistance Amount" = "sum.repairAmount.CPIAdjusted",
                         "Replacement Assistance Amount" = "sum.replacementAmount.CPIAdjusted",
                         "Personal Property Amount" = "sum.personalPropertyAmount.CPIAdjusted",
                         "Roof Damage Amount" = "sum.roofDamageAmount.CPIAdjusted", 
                         "Foundation Damage Amount" = "sum.foundationDamageAmount.CPIAdjusted"
      )
      
      years = seq(input$Bardates[1], input$Bardates[2], by = 1)
      data = IHP[IHP$disasterYear %in% years, ]
      if(groupby != "") { 
        data <- aggregate(list("sum.ihpAmount.CPIAdjusted" = data$sum.ihpAmount.CPIAdjusted,
                               "sum.haAmount.CPIAdjusted" = data$sum.haAmount.CPIAdjusted,
                               "sum.onaAmount.CPIAdjusted" = data$sum.onaAmount.CPIAdjusted,
                               "sum.rentalAssistanceAmount.CPIAdjusted" = data$sum.rentalAssistanceAmount.CPIAdjusted,
                               "sum.repairAmount.CPIAdjusted" = data$sum.repairAmount.CPIAdjusted,
                               "sum.replacementAmount.CPIAdjusted" = data$sum.replacementAmount.CPIAdjusted,
                               "sum.personalPropertyAmount.CPIAdjusted" =data$sum.personalPropertyAmount.CPIAdjusted,
                               "sum.rpfvl.CPIAdjusted" =data$sum.rpfvl.CPIAdjusted,
                               "sum.ppfvl.CPIAdjusted" =data$sum.ppfvl.CPIAdjusted,
                               "sum.roofDamageAmount.CPIAdjusted" =data$sum.roofDamageAmount.CPIAdjusted ,
                               "sum.foundationDamageAmount.CPIAdjusted" =data$sum.foundationDamageAmount.CPIAdjusted,
                               "numApplicant" =data$numApplicant,
                               "sum.primaryResidence." =data$sum.primaryResidence.,
                               "sum.homeOwnersInsurance." =data$sum.homeOwnersInsurance.,
                               "sum.floodInsurance." =data$sum.floodInsurance.,
                               "sum.ihpReferral." =data$sum.ihpReferral.,
                               "sum.ihpEligible." =data$sum.ihpEligible.,
                               "sum.haReferral." =data$sum.haReferral.,
                               "sum.haEligible." =data$sum.haEligible.,
                               "sum.onaReferral." =data$sum.onaReferral.,
                               "sum.onaEligible." =data$sum.onaEligible.,
                               "sum.utilitiesOut." =data$sum.utilitiesOut.,
                               "sum.autoDamage." =data$sum.autoDamage.,
                               "sum.emergencyNeeds." =data$sum.emergencyNeeds.,
                               "sum.foodNeed." =data$sum.foodNeed.,
                               "sum.shelterNeed." =data$sum.shelterNeed.,
                               "sum.sbaEligible." =data$sum.sbaEligible.,
                               "sum.sbaApproved." =data$sum.sbaApproved.,
                               "sum.tsaEligible." =data$sum.tsaEligible.,
                               "sum.tsaCheckedIn." =data$sum.tsaCheckedIn.,
                               "numRenter" =data$numRenter,
                               "numOwner" =data$numOwner,
                               "totalResidentsAffected" =data$totalResidentsAffected,
                               "numHousesDuplex" =data$numHousesDuplex,
                               "numMobileHome" =data$numMobileHome,
                               "numApartment" =data$numApartment,
                               "numCondo" =data$numCondo,
                               "numBoat" =data$numBoat,
                               "numTrialer" =data$numTrialer,
                               "numTownhouse" =data$numTownhouse,
                               "numCorrectionalFacility" =data$numCorrectionalFacility,
                               "numAssistedLivingFacility" =data$numAssistedLivingFacility,
                               "numMilitaryHousing" =data$numMilitaryHousing,
                               "numCollegeDorm" =data$numCollegeDorm,
                               "numUnknown" =data$numUnknown,
                               "numOtherResidence" =data$numOtherResidence,
                               "avgIncome" =data$avgIncome),
                          by = list(data[[groupby]]), FUN = sum)
      }
      
      
      if(xvariable %in% c("sum.ihpAmount.CPIAdjusted", "sum.haAmount.CPIAdjusted", "sum.onaAmount.CPIAdjusted",
                          "sum.rpfvl", "sum.ppfvl","sum.rentalAssistanceAmount.CPIAdjusted", "sum.repairAmount.CPIAdjusted", 
                          "sum.replacementAmount.CPIAdjusted","sum.personalPropertyAmount.CPIAdjusted",
                          "sum.roofDamageAmount.CPIAdjusted", "sum.foundationDamageAmount.CPIAdjusted", "avgIncome")) {
        x = scale(data[[xvariable]])
      } else {
        x = scale(data[[xvariable]]/data[["numApplicant"]])
      }
      
      if(yvariable %in% c("sum.ihpAmount.CPIAdjusted", "sum.haAmount.CPIAdjusted", "sum.onaAmount.CPIAdjusted",
                          "sum.rpfvl", "sum.ppfvl","sum.rentalAssistanceAmount.CPIAdjusted", "sum.repairAmount.CPIAdjusted", 
                          "sum.replacementAmount.CPIAdjusted","sum.personalPropertyAmount.CPIAdjusted",
                          "sum.roofDamageAmount.CPIAdjusted", "sum.foundationDamageAmount.CPIAdjusted","avgIncome")) {
        
        
        
        y = scale(data[[yvariable]])
      } else {
        y = scale(data[[yvariable]]/data[["numApplicant"]])
      }
      
      data_matrix <- matrix(unlist(list(x, y)), ncol = 2)
      
      colnames(data_matrix) <- c(xvariable, yvariable)
      kNNdistplot(data_matrix, k=input$knnvalue)

      set.seed(1234)
      db = dbscan(data_matrix, input$eps, input$minPts)

      hullplot(data_matrix, db$cluster)
    } else if(input$clusterdataset == "Hazard Mitigation Assistance" && length(input$groupby) != 0) {
      
      groupby= switch(input$groupby,
                      "Project" = "",
                      "State" = "state",
                      "County" = "county", 
                      "Project Area" = "programArea",
                      "Project Type" = "projectType",
                      "Disaster Number" = "disasterNumber"
      )
      
      xvariable = switch(input$xvariable,
                        "Number Of Properties" = "numberOfProperties",
                        "Number Of Final Properties" = "numberOfFinalProperties",
                        "Cost Share Percentage" = "costSharePercentage",
                        "Project Amount" =  "projectAmount",
                        "Federal Share Obligated" = "federalShareObligated",
                        "Benefit Cost Ratio" = "benefitCostRatio",
                        "Net Value Benefits" = "netValueBenefits"

      )
      
      yvariable = switch(input$yvariable,
                         "Number Of Properties" = "numberOfProperties",
                         "Number Of Final Properties" = "numberOfFinalProperties",
                         "Cost Share Percentage" = "costSharePercentage",
                         "Project Amount" =  "projectAmount",
                         "Federal Share Obligated" = "federalShareObligated",
                         "Benefit Cost Ratio" = "benefitCostRatio",
                         "Net Value Benefits" = "netValueBenefits"
      )
     
      data = HMA[substring(HMA$dateApproved, 1, 4) %in% seq(input$HMAdates[1], input$HMAdates[2], by = 1), ]
     

      if(groupby != "") { 
        otherdata <- aggregate(list("costSharePercentage" = data$costSharePercentage,
                                    "benefitCostRatio" = data$benefitCostRatio),
                               by = list(groupby = data[[groupby]]), FUN = mean, na.rm = TRUE)
        data <- aggregate(list("numberOfProperties" = data$numberOfProperties,
                                "numberOfFinalProperties" = data$numberOfFinalProperties,
                                "projectAmount" =  data$projectAmount,
                                "federalShareObligated" = data$federalShareObligated,
                                "netValueBenefits" = data$netValueBenefits),
                          by = list(groupby = data[[groupby]]), FUN = sum, na.rm = TRUE)

        otherdata <-na.omit(otherdata)
        data = merge(data, otherdata, by = c("groupby"))
      }
    
      data <- na.omit(data)
      data[data == 0] = 1
 
      if(xvariable %in% c("numberOfProperties", "numberOfFinalProperties")) {
        x = scale(log(data[[xvariable]]), 2)
        
      } else if(xvariable %in% c("projectAmount", "federalShareObligated")) {
        x = scale(log(data[[xvariable]]), 2)
   
      } else if(xvariable %in% c("netValueBenefits", "benefitCostRatio")) {
        x = scale(log(data[[xvariable]]), 2)
       
      } else {
        x = scale(data[[xvariable]])
       
      }

      if(yvariable %in% c("numberOfProperties", "numberOfFinalProperties")) {
        y = scale(log(data[[yvariable]]), 2)
       
      } else if(yvariable %in% c("projectAmount", "federalShareObligated", "netValueBenefits")) {
        y = scale(log(data[[yvariable]]),2)
        
      } else if(yvariable %in% c("benefitCostRatio")) {
        y = scale(log(data[[yvariable]]),2)
       
      } else {
        y = scale(data[[yvariable]])
      
      }

      
      data_matrix <- matrix(unlist(list(x, y)), ncol = 2)
      colnames(data_matrix) <- c(xvariable, yvariable)
      
  
      kNNdistplot(data_matrix, k=input$knnvalue)

      
      set.seed(1234)
      db = dbscan(data_matrix, input$eps, input$minPts)
      #plot(data_matrix[,xvariable], data_matrix[,yvariable])
      hullplot(data_matrix, db$cluster)
    } else {
      
    }  
  }, height = 500)
  
  
  output$scatterplot <-renderPlot({
    
    
  })
  
  
  
  # create the barchart output
  output$knn <- renderPlot({
    if(input$clusterdataset == "Individuals and Household Programs" && length(input$groupby) != 0){
      groupby= switch(input$groupby,
                      "Disaster-County" = "",
                      "Disaster Year" = "disasterYear",
                      "Incident Type" = "incidentType", 
                      "Disaster Number" = "disasterNumber",
                      "State" = "damagedStateAbbreviation",
                      "County" = "fip"
      )
      
      xvariable = switch(input$xvariable,
                         "Applicants" = "numApplicant",	
                         "Primary Residence" = "sum.primaryResidence.",	
                         "Home Owners Insurance" = "sum.homeOwnersInsurance.",
                         "Flood Insurance" = "sum.floodInsurance.",	
                         "IHP Referral"= "sum.ihpReferral.",
                         "IHP Eligible" = "sum.ihpEligible.",
                         "HA Referral" = "sum.haReferral.",
                         "HA Eligible" = "sum.haEligible.",
                         "ONA Referral" = "sum.onaReferral.",
                         "ONA Eligible" = "sum.onaEligible.",
                         "Utilities Out" = "sum.utilitiesOut.",
                         "Auto Damage" = "sum.autoDamage.",	
                         "Emergency Needs" = "sum.emergencyNeeds.",	
                         "Food Needs" = "sum.foodNeed.",
                         "Shelter Need" = "sum.shelterNeed.",
                         "SBA Eligible" = "sum.sbaEligible.",
                         "SBA Approved" = "sum.sbaApproved.",
                         "TSA Eligible" = "sum.tsaEligible.",
                         "TSA CheckedIn" = "sum.tsaCheckedIn.",
                         "Renters" = "numRenter",
                         "Owners" = "numOwner",
                         "Total Residents Affected" = "totalResidentsAffected",
                         "Houses and Duplex" = "numHousesDuplex",
                         "Mobile Homes" = "numMobileHome",
                         "Apartments" = "numApartment",
                         "Condos" = "numCondo",
                         "Boats" = "numBoat",	
                         "Trialers" = "numTrialer",
                         "Townhouses" = "numTownhouse",
                         "Correctional Facilities" = "numCorrectionalFacility",
                         "Assisted Living Facilities" = "numAssistedLivingFacility",
                         "Military Houses" = "numMilitaryHousing",
                         "College Dorms" = "numCollegeDorm",	
                         "Unknown Buildings" = "numUnknown",
                         "Other Residence" = "numOtherResidence",
                         "Average Income" = "avgIncome", 	
                         "IHP Amount" = "sum.ihpAmount.CPIAdjusted",
                         "HA Amount" = "sum.haAmount.CPIAdjusted",	
                         "ONA Amount" = "sum.onaAmount.CPIAdjusted",
                         "rpfvl" = "sum.rpfvl",
                         "ppfvl" = "sum.ppfvl",
                         "Rental Assistance Amount" = "sum.rentalAssistanceAmount.CPIAdjusted",
                         "Repair Assistance Amount" = "sum.repairAmount.CPIAdjusted",
                         "Replacement Assistance Amount" = "sum.replacementAmount.CPIAdjusted",
                         "Personal Property Amount" = "sum.personalPropertyAmount.CPIAdjusted",
                         "Roof Damage Amount" = "sum.roofDamageAmount.CPIAdjusted", 
                         "Foundation Damage Amount" = "sum.foundationDamageAmount.CPIAdjusted"
                         
      )
      
      yvariable = switch(input$yvariable,
                         "Applicants" = "numApplicant",	
                         "Primary Residence" = "sum.primaryResidence.",	
                         "Home Owners Insurance" = "sum.homeOwnersInsurance.",
                         "Flood Insurance" = "sum.floodInsurance.",	
                         "IHP Referral"= "sum.ihpReferral.",
                         "IHP Eligible" = "sum.ihpEligible.",
                         "HA Referral" = "sum.haReferral.",
                         "HA Eligible" = "sum.haEligible.",
                         "ONA Referral" = "sum.onaReferral.",
                         "ONA Eligible" = "sum.onaEligible.",
                         "Utilities Out" = "sum.utilitiesOut.",
                         "Auto Damage" = "sum.autoDamage.",	
                         "Emergency Needs" = "sum.emergencyNeeds.",	
                         "Food Needs" = "sum.foodNeed.",
                         "Shelter Need" = "sum.shelterNeed.",
                         "SBA Eligible" = "sum.sbaEligible.",
                         "SBA Approved" = "sum.sbaApproved.",
                         "TSA Eligible" = "sum.tsaEligible.",
                         "TSA CheckedIn" = "sum.tsaCheckedIn.",
                         "Renters" = "numRenter",
                         "Owners" = "numOwner",
                         "Total Residents Affected" = "totalResidentsAffected",
                         "Houses and Duplex" = "numHousesDuplex",
                         "Mobile Homes" = "numMobileHome",
                         "Apartments" = "numApartment",
                         "Condos" = "numCondo",
                         "Boats" = "numBoat",	
                         "Trialers" = "numTrialer",
                         "Townhouses" = "numTownhouse",
                         "Correctional Facilities" = "numCorrectionalFacility",
                         "Assisted Living Facilities" = "numAssistedLivingFacility",
                         "Military Houses" = "numMilitaryHousing",
                         "College Dorms" = "numCollegeDorm",	
                         "Unknown Buildings" = "numUnknown",
                         "Other Residence" = "numOtherResidence",
                         "Average Income" = "avgIncome", 	
                         "IHP Amount" = "sum.ihpAmount.CPIAdjusted",
                         "HA Amount" = "sum.haAmount.CPIAdjusted",	
                         "ONA Amount" = "sum.onaAmount.CPIAdjusted",
                         "rpfvl" = "sum.rpfvl",
                         "ppfvl" = "sum.ppfvl",
                         "Rental Assistance Amount" = "sum.rentalAssistanceAmount.CPIAdjusted",
                         "Repair Assistance Amount" = "sum.repairAmount.CPIAdjusted",
                         "Replacement Assistance Amount" = "sum.replacementAmount.CPIAdjusted",
                         "Personal Property Amount" = "sum.personalPropertyAmount.CPIAdjusted",
                         "Roof Damage Amount" = "sum.roofDamageAmount.CPIAdjusted", 
                         "Foundation Damage Amount" = "sum.foundationDamageAmount.CPIAdjusted"
      )
      
      years = seq(input$Bardates[1], input$Bardates[2], by = 1)
      data = IHP[IHP$disasterYear %in% years, ]
      if(groupby != "") { 
        data <- aggregate(list("sum.ihpAmount.CPIAdjusted" = data$sum.ihpAmount.CPIAdjusted,
                               "sum.haAmount.CPIAdjusted" = data$sum.haAmount.CPIAdjusted,
                               "sum.onaAmount.CPIAdjusted" = data$sum.onaAmount.CPIAdjusted,
                               "sum.rentalAssistanceAmount.CPIAdjusted" = data$sum.rentalAssistanceAmount.CPIAdjusted,
                               "sum.repairAmount.CPIAdjusted" = data$sum.repairAmount.CPIAdjusted,
                               "sum.replacementAmount.CPIAdjusted" = data$sum.replacementAmount.CPIAdjusted,
                               "sum.personalPropertyAmount.CPIAdjusted" =data$sum.personalPropertyAmount.CPIAdjusted,
                               "sum.rpfvl.CPIAdjusted" =data$sum.rpfvl.CPIAdjusted,
                               "sum.ppfvl.CPIAdjusted" =data$sum.ppfvl.CPIAdjusted,
                               "sum.roofDamageAmount.CPIAdjusted" =data$sum.roofDamageAmount.CPIAdjusted ,
                               "sum.foundationDamageAmount.CPIAdjusted" =data$sum.foundationDamageAmount.CPIAdjusted,
                               "numApplicant" =data$numApplicant,
                               "sum.primaryResidence." =data$sum.primaryResidence.,
                               "sum.homeOwnersInsurance." =data$sum.homeOwnersInsurance.,
                               "sum.floodInsurance." =data$sum.floodInsurance.,
                               "sum.ihpReferral." =data$sum.ihpReferral.,
                               "sum.ihpEligible." =data$sum.ihpEligible.,
                               "sum.haReferral." =data$sum.haReferral.,
                               "sum.haEligible." =data$sum.haEligible.,
                               "sum.onaReferral." =data$sum.onaReferral.,
                               "sum.onaEligible." =data$sum.onaEligible.,
                               "sum.utilitiesOut." =data$sum.utilitiesOut.,
                               "sum.autoDamage." =data$sum.autoDamage.,
                               "sum.emergencyNeeds." =data$sum.emergencyNeeds.,
                               "sum.foodNeed." =data$sum.foodNeed.,
                               "sum.shelterNeed." =data$sum.shelterNeed.,
                               "sum.sbaEligible." =data$sum.sbaEligible.,
                               "sum.sbaApproved." =data$sum.sbaApproved.,
                               "sum.tsaEligible." =data$sum.tsaEligible.,
                               "sum.tsaCheckedIn." =data$sum.tsaCheckedIn.,
                               "numRenter" =data$numRenter,
                               "numOwner" =data$numOwner,
                               "totalResidentsAffected" =data$totalResidentsAffected,
                               "numHousesDuplex" =data$numHousesDuplex,
                               "numMobileHome" =data$numMobileHome,
                               "numApartment" =data$numApartment,
                               "numCondo" =data$numCondo,
                               "numBoat" =data$numBoat,
                               "numTrialer" =data$numTrialer,
                               "numTownhouse" =data$numTownhouse,
                               "numCorrectionalFacility" =data$numCorrectionalFacility,
                               "numAssistedLivingFacility" =data$numAssistedLivingFacility,
                               "numMilitaryHousing" =data$numMilitaryHousing,
                               "numCollegeDorm" =data$numCollegeDorm,
                               "numUnknown" =data$numUnknown,
                               "numOtherResidence" =data$numOtherResidence,
                               "avgIncome" =data$avgIncome),
                          by = list(data[[groupby]]), FUN = sum)
      }
      
      
      if(xvariable %in% c("sum.ihpAmount.CPIAdjusted", "sum.haAmount.CPIAdjusted", "sum.onaAmount.CPIAdjusted",
                          "sum.rpfvl", "sum.ppfvl","sum.rentalAssistanceAmount.CPIAdjusted", "sum.repairAmount.CPIAdjusted", 
                          "sum.replacementAmount.CPIAdjusted","sum.personalPropertyAmount.CPIAdjusted",
                          "sum.roofDamageAmount.CPIAdjusted", "sum.foundationDamageAmount.CPIAdjusted")) {
        x = scale(data[[xvariable]])
        x2 = data[[xvariable]]
      } else {
        x = scale(data[[xvariable]]/data[["numApplicant"]])
        x2 = data[[xvariable]]/data[["numApplicant"]]
      }
      
      if(yvariable %in% c("sum.ihpAmount.CPIAdjusted", "sum.haAmount.CPIAdjusted", "sum.onaAmount.CPIAdjusted",
                          "sum.rpfvl", "sum.ppfvl","sum.rentalAssistanceAmount.CPIAdjusted", "sum.repairAmount.CPIAdjusted", 
                          "sum.replacementAmount.CPIAdjusted","sum.personalPropertyAmount.CPIAdjusted",
                          "sum.roofDamageAmount.CPIAdjusted", "sum.foundationDamageAmount.CPIAdjusted")) {
        
        
        
        y = scale(data[[yvariable]])
        y2 = data[[yvariable]]
      } else {
        y = scale(data[[yvariable]]/data[["numApplicant"]])
        y2 = data[[yvariable]]/data[["numApplicant"]]
      }
      
      statoutput <- matrix(unlist(list(x2, y2)), ncol = 2)
      colnames(statoutput) <- c(xvariable, yvariable)
      
      output$summary <- renderPrint({summary(statoutput)})
      
      data_matrix <- matrix(unlist(list(x, y)), ncol = 2)
      colnames(data_matrix) <- c(xvariable, yvariable)

      kNNdistplot(data_matrix, k=input$knnvalue)
      
    } else if(input$clusterdataset == "Hazard Mitigation Assistance") {
      groupby= switch(input$groupby,
                      "Project" = "",
                      "State" = "state",
                      "County" = "county", 
                      "Project Area" = "programArea",
                      "Project Type" = "projectType",
                      "Disaster Number" = "disasterNumber"
      )
      
      xvariable = switch(input$xvariable,
                         "Number Of Properties" = "numberOfProperties",
                         "Number Of Final Properties" = "numberOfFinalProperties",
                         "Cost Share Percentage" = "costSharePercentage",
                         "Project Amount" =  "projectAmount",
                         "Federal Share Obligated" = "federalShareObligated",
                         "Benefit Cost Ratio" = "benefitCostRatio",
                         "Net Value Benefits" = "netValueBenefits"
                         
      )
      
      yvariable = switch(input$yvariable,
                         "Number Of Properties" = "numberOfProperties",
                         "Number Of Final Properties" = "numberOfFinalProperties",
                         "Cost Share Percentage" = "costSharePercentage",
                         "Project Amount" =  "projectAmount",
                         "Federal Share Obligated" = "federalShareObligated",
                         "Benefit Cost Ratio" = "benefitCostRatio",
                         "Net Value Benefits" = "netValueBenefits"
      )
      
      data = HMA[substring(HMA$dateApproved, 1, 4) %in% seq(input$HMAdates[1], input$HMAdates[2], by = 1), ]
      
      
      if(groupby != "") { 
        otherdata <- aggregate(list("costSharePercentage" = data$costSharePercentage,
                                    "benefitCostRatio" = data$benefitCostRatio),
                               by = list(groupby = data[[groupby]]), FUN = mean, na.rm = TRUE)
        data <- aggregate(list("numberOfProperties" = data$numberOfProperties,
                               "numberOfFinalProperties" = data$numberOfFinalProperties,
                               "projectAmount" =  data$projectAmount,
                               "federalShareObligated" = data$federalShareObligated,
                               "netValueBenefits" = data$netValueBenefits),
                          by = list(groupby = data[[groupby]]), FUN = sum, na.rm = TRUE)
        
        otherdata <-na.omit(otherdata)
        data = merge(data, otherdata, by = c("groupby"))
      }
      
      

      data <- na.omit(data)
      data[data == 0] = 1
      
      statoutput <- matrix(unlist(list(data[[xvariable]], data[[yvariable]])), ncol = 2)
      colnames(statoutput) <- c(xvariable, yvariable)
      
      output$summary <- renderPrint({summary(statoutput)})
      
      
      if(xvariable %in% c("numberOfProperties", "numberOfFinalProperties")) {
        x = scale(log(data[[xvariable]]), 2 )
        x[is.na(x)] <- 0
      } else if(xvariable %in% c("projectAmount", "federalShareObligated")) {
        x = scale(log(data[[xvariable]]), 2)
        x[is.na(x)] <- 0
      } else if(xvariable %in% c("netValueBenefits", "benefitCostRatio")) {
        x = scale(log(data[[xvariable]]),2)
        x[is.na(x)] <- 0
      } else {
        x = scale(data[[xvariable]])
        x[is.na(x)] <- 0
      }
 
      if(yvariable %in% c("numberOfProperties", "numberOfFinalProperties")) {
        y = scale(log(data[[yvariable]]),2)
        y[is.na(y)] <- 0
      } else if(yvariable %in% c("projectAmount", "federalShareObligated", "netValueBenefits")) {
        y = scale(log(data[[yvariable]]),2)
        y[is.na(y)] <- 0
      } else if(yvariable %in% c("benefitCostRatio")) {
        y = scale(log(data[[yvariable]]),2)
        y[is.na(y)] <- 0
      } else {
        y = scale(data[[yvariable]])
        y[is.na(y)] <- 0
      }
      
      data_matrix <- matrix(unlist(list(x, y)), ncol = 2)
      colnames(data_matrix) <- c(xvariable, yvariable)

      kNNdistplot(data_matrix, k=input$knnvalue)
    } else {
      
    }

  }, height = 500)
  
  
  
  
  # print out the raw table used for analysis
  output$rawtable <- renderPrint({
    if(input$dataset == "IHP") {
      orig <- options(width = 1000)
      print(tail(IHP, input$maxrows), row.names = FALSE)
      options(orig)
    } else if(input$dataset == "HMA") {
      orig <- options(width = 1000)
      print(tail(HMA, input$maxrows), row.names = FALSE)
      options(orig)
    } else if(input$dataset == "NOAA") {
      orig <- options(width = 1000)
      print(tail(NOAA, input$maxrows), row.names = FALSE)
      options(orig)
    } else if(input$dataset == "PA") {
      orig <- options(width = 1000)
      print(tail(PA, input$maxrows), row.names = FALSE)
      options(orig)
    } else {
      orig <- options(width = 1000)
      print(tail(CDBG.DR, input$maxrows), row.names = FALSE)
      options(orig)
    }
    
  })
  
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep="")
    },
    content = function(file) {
      if(input$dataset == "IHP") {
        write.csv(IHP, file)
      } else if(input$dataset == "HMA") {
        write.csv(HMA, file)
      } else if(input$dataset == "NOAA") {
        write.csv(NOAA, file)
      } else if(input$dataset == "PA") {
        write.csv(PA, file)
      } else {
        write.csv(CDBG.DR, file)
      }
 

    }
  )
}

shinyApp(ui = ui, server = server)