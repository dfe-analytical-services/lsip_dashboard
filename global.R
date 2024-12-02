# ---------------------------------------------------------
# File name: server.R
# Date created: 06/06/2022
#
# ---------------------------------------------------------

# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest2))
shhh(library(diffviewer))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(janitor))
shhh(library(openxlsx))
shhh(library(tidyverse)) # various data manipulation
shhh(library(scales)) # formatting numbers
shhh(library(plotly)) # interactive plots
shhh(library(DT)) # create datatables
shhh(library(writexl)) # write data to xls for download
shhh(library(leaflet)) # create maps
shhh(library(sf)) # load map data
shhh(library(capture)) # screenshots
shhh(library(shinyalert)) # cookie pop up
shhh(library(shinycssloaders)) # spinners
shhh(library(RColorBrewer)) # map colours
shhh(library(dfeshiny)) # map colours

google_analytics_key <- "MMB6NG2FE1"

area_select <- c("Coast to Capital", "Greater Manchester", "England")

metricChoices <- list(
  "Employment" = list(
    "Employment rate" = "inemploymentRate",
    "Self-employment rate" = "selfemployedRate",
    "Unemployment rate" = "unemployedRate",
    "Inactive rate" = "inactiveRate",
    "Employment" = "inemployment",
    "Self-employment" = "selfemployed",
    "Unemployed" = "unemployed",
    "Inactive" = "inactive"
  ),
  "Jobs" = list(
    "Online job adverts" = "vacancies",
    "Employment projections (Skills imperative 2035)" = "employmentProjection"
  ),
  "Businesses" = list(
    "Business count" = "enterpriseCount",
    "Business birth rate" = "birthRate",
    "Business death rate" = "deathRate"
  ),
  "Skills" = list(
    "FE achievement" = "achievements",
    "FE participation" = "participation",
    # "FE starts" = "starts",
    "FE achievement rate per 100,000" = "achievements_rate_per_100000_population",
    "FE participation rate per 100,000" = "participation_rate_per_100000_population",
    # "FE start rate" = "starts_rate_per_100000_population",
    "Qualified at Level 3 or above" = "L3PlusRate",
    "Qualified at Level 4 or above" = "L4PlusRate"
  ),
  "Destinations" = list(
    "KS4 sustained positive destination rate" = "sustainedPositiveDestinationKS4Rate",
    "KS5 sustained positive destination rate" = "sustainedPositiveDestinationKS5Rate"
  )
  # ,
  # "Mismatch" = list("Supply vs demand TO COME" = "mismatch")
)

areaChoices <- list(
  "Local enterprise partnership" = list(
    "Buckinghamshire" = "Buckinghamshire LEP",
    "Cheshire and Warrington" = "Cheshire and Warrington LEP",
    "Coast to Capital" = "Coast to Capital LEP",
    "Cornwall and Isles of Scilly" = "Cornwall and Isles of Scilly LEP",
    "Cumbria" = "Cumbria LEP",
    "D2N2" = "D2N2 LEP",
    "Dorset" = "Dorset LEP",
    "Enterprise M3" = "Enterprise M3 LEP",
    "GFirst" = "GFirst LEP",
    "Greater Birmingham and Solihull" = "Greater Birmingham and Solihull LEP",
    "The Business Board" = "The Business Board LEP",
    "Greater Lincolnshire" = "Greater Lincolnshire LEP",
    "Greater Manchester" = "Greater Manchester LEP",
    "Heart of the South West" = "Heart of the South West LEP",
    "Hertfordshire" = "Hertfordshire LEP",
    "Hull and East Yorkshire" = "Hull and East Yorkshire LEP",
    "Lancashire" = "Lancashire LEP",
    "Leeds City Region" = "Leeds City Region LEP",
    "Leicester and Leicestershire" = "Leicester and Leicestershire LEP",
    "Liverpool City Region" = "Liverpool City Region LEP",
    "The London Economic Action Partnership" = "The London Economic Action Partnership LEP",
    "New Anglia" = "New Anglia LEP",
    "North East" = "North East LEP",
    "OxLEP" = "OxLEP LEP",
    "Solent" = "Solent LEP",
    "South East" = "South East LEP",
    "South East Midlands" = "South East Midlands LEP",
    "Stoke-on-Trent and Staffordshire" = "Stoke-on-Trent and Staffordshire LEP",
    "South Yorkshire" = "South Yorkshire LEP",
    "Swindon and Wiltshire" = "Swindon and Wiltshire LEP",
    "Tees Valley" = "Tees Valley LEP",
    "Thames Valley Berkshire" = "Thames Valley Berkshire LEP",
    "The Marches" = "The Marches LEP",
    "West of England" = "West of England LEP",
    "Worcestershire" = "Worcestershire LEP",
    "York and North Yorkshire" = "York and North Yorkshire LEP"
  ),
  "Local skills partnership area" = list(
    "Brighton and Hove, East Sussex, West Sussex" = "Brighton and Hove, East Sussex, West Sussex LSIP",
    "Buckinghamshire" = "Buckinghamshire LSIP",
    "Cambridgeshire and Peterborough" = "Cambridgeshire and Peterborough LSIP",
    "Cheshire and Warrington" = "Cheshire and Warrington LSIP",
    "Cornwall and the Isles of Scilly" = "Cornwall and the Isles of Scilly LSIP",
    "Cumbria" = "Cumbria LSIP",
    "Derbyshire and Nottinghamshire" = "Derbyshire and Nottinghamshire LSIP",
    "Dorset" = "Dorset LSIP",
    "Enterprise M3" = "Enterprise M3 LSIP",
    "Essex, Southend-on-Sea and Thurrock" = "Essex, Southend-on-Sea and Thurrock LSIP",
    "G First (Gloucestershire)" = "G First (Gloucestershire) LSIP",
    "Greater Lincolnshire" = "Greater Lincolnshire LSIP",
    "Greater London" = "Greater London LSIP",
    "Greater Manchester" = "Greater Manchester LSIP",
    "Heart of the South-West" = "Heart of the South-West LSIP",
    "Hertfordshire" = "Hertfordshire LSIP",
    "Hull and East Yorkshire" = "Hull and East Yorkshire LSIP",
    "Kent and Medway" = "Kent and Medway LSIP",
    "Lancashire" = "Lancashire LSIP",
    "Leicester and Leicestershire" = "Leicester and Leicestershire LSIP",
    "Liverpool City Region" = "Liverpool City Region LSIP",
    "New Anglia" = "New Anglia LSIP",
    "North East" = "North East LSIP",
    "North of Tyne" = "North of Tyne LSIP",
    "Oxfordshire" = "Oxfordshire LSIP",
    "Solent" = "Solent LSIP",
    "South-East Midlands" = "South-East Midlands LSIP",
    "South Yorkshire" = "South Yorkshire LSIP",
    "Stoke-on-Trent and Staffordshire" = "Stoke-on-Trent and Staffordshire LSIP",
    "Swindon and Wiltshire" = "Swindon and Wiltshire LSIP",
    "Tees Valley" = "Tees Valley LSIP",
    "Thames Valley Berkshire" = "Thames Valley Berkshire LSIP",
    "The Marches" = "The Marches LSIP",
    "West Midlands and Warwickshire" = "West Midlands and Warwickshire LSIP",
    "West of England and North Somerset" = "West of England and North Somerset LSIP",
    "West Yorkshire" = "West Yorkshire LSIP",
    "Worcestershire" = "Worcestershire LSIP",
    "York and North Yorkshire" = "York and North Yorkshire LSIP"
  ),
  "Mayoral combined authority" = list(
    "Cambridgeshire and Peterborough" = "Cambridgeshire and Peterborough MCA",
    "Greater London Authority" = "Greater London Authority MCA",
    "Greater Manchester" = "Greater Manchester MCA",
    "Liverpool City Region" = "Liverpool City Region MCA",
    "South Yorkshire" = "South Yorkshire MCA",
    "Tees Valley" = "Tees Valley MCA",
    "West Midlands" = "West Midlands MCA",
    "West of England" = "West of England MCA",
    "West Yorkshire" = "West Yorkshire MCA"
  ),
  "Country" = list(
    "England" = "England "
  ),
  "Local authority" = list(
    "Adur" = "Adur LADU",
    "Amber Valley" = "Amber Valley LADU",
    "Arun" = "Arun LADU",
    "Ashfield" = "Ashfield LADU",
    "Ashford" = "Ashford LADU",
    "Babergh" = "Babergh LADU",
    "Barking and Dagenham" = "Barking and Dagenham LADU",
    "Barnet" = "Barnet LADU",
    "Barnsley" = "Barnsley LADU",
    "Basildon" = "Basildon LADU",
    "Basingstoke and Deane" = "Basingstoke and Deane LADU",
    "Bassetlaw" = "Bassetlaw LADU",
    "Bath and North East Somerset" = "Bath and North East Somerset LADU",
    "Bedford" = "Bedford LADU",
    "Bexley" = "Bexley LADU",
    "Birmingham" = "Birmingham LADU",
    "Blaby" = "Blaby LADU",
    "Blackburn with Darwen" = "Blackburn with Darwen LADU",
    "Blackpool" = "Blackpool LADU",
    "Bolsover" = "Bolsover LADU",
    "Bolton" = "Bolton LADU",
    "Boston" = "Boston LADU",
    "Bournemouth, Christchurch and Poole" = "Bournemouth, Christchurch and Poole LADU",
    "Bracknell Forest" = "Bracknell Forest LADU",
    "Bradford" = "Bradford LADU",
    "Braintree" = "Braintree LADU",
    "Breckland" = "Breckland LADU",
    "Brent" = "Brent LADU",
    "Brentwood" = "Brentwood LADU",
    "Brighton and Hove" = "Brighton and Hove LADU",
    "Bristol, City of" = "Bristol, City of LADU",
    "Broadland" = "Broadland LADU",
    "Bromley" = "Bromley LADU",
    "Bromsgrove" = "Bromsgrove LADU",
    "Broxbourne" = "Broxbourne LADU",
    "Broxtowe" = "Broxtowe LADU",
    "Buckinghamshire" = "Buckinghamshire LADU",
    "Burnley" = "Burnley LADU",
    "Bury" = "Bury LADU",
    "Calderdale" = "Calderdale LADU",
    "Cambridge" = "Cambridge LADU",
    "Camden" = "Camden LADU",
    "Cannock Chase" = "Cannock Chase LADU",
    "Canterbury" = "Canterbury LADU",
    "Castle Point" = "Castle Point LADU",
    "Central Bedfordshire" = "Central Bedfordshire LADU",
    "Charnwood" = "Charnwood LADU",
    "Chelmsford" = "Chelmsford LADU",
    "Cheltenham" = "Cheltenham LADU",
    "Cherwell" = "Cherwell LADU",
    "Cheshire East" = "Cheshire East LADU",
    "Cheshire West and Chester" = "Cheshire West and Chester LADU",
    "Chesterfield" = "Chesterfield LADU",
    "Chichester" = "Chichester LADU",
    "Chorley" = "Chorley LADU",
    "City of London" = "City of London LADU",
    "Colchester" = "Colchester LADU",
    "Cornwall" = "Cornwall LADU",
    "Cotswold" = "Cotswold LADU",
    "County Durham" = "County Durham LADU",
    "Coventry" = "Coventry LADU",
    "Craven" = "Craven LADU",
    "Crawley" = "Crawley LADU",
    "Croydon" = "Croydon LADU",
    "Cumberland" = "Cumberland LADU",
    "Dacorum" = "Dacorum LADU",
    "Darlington" = "Darlington LADU",
    "Dartford" = "Dartford LADU",
    "Derby" = "Derby LADU",
    "Derbyshire Dales" = "Derbyshire Dales LADU",
    "Doncaster" = "Doncaster LADU",
    "Dorset" = "Dorset LADU",
    "Dover" = "Dover LADU",
    "Dudley" = "Dudley LADU",
    "Ealing" = "Ealing LADU",
    "East Cambridgeshire" = "East Cambridgeshire LADU",
    "East Devon" = "East Devon LADU",
    "East Hampshire" = "East Hampshire LADU",
    "East Hertfordshire" = "East Hertfordshire LADU",
    "East Lindsey" = "East Lindsey LADU",
    "East Riding of Yorkshire" = "East Riding of Yorkshire LADU",
    "East Staffordshire" = "East Staffordshire LADU",
    "East Suffolk" = "East Suffolk LADU",
    "Eastbourne" = "Eastbourne LADU",
    "Eastleigh" = "Eastleigh LADU",
    "Elmbridge" = "Elmbridge LADU",
    "Enfield" = "Enfield LADU",
    "Epping Forest" = "Epping Forest LADU",
    "Epsom and Ewell" = "Epsom and Ewell LADU",
    "Erewash" = "Erewash LADU",
    "Exeter" = "Exeter LADU",
    "Fareham" = "Fareham LADU",
    "Fenland" = "Fenland LADU",
    "Folkestone and Hythe" = "Folkestone and Hythe LADU",
    "Forest of Dean" = "Forest of Dean LADU",
    "Fylde" = "Fylde LADU",
    "Gateshead" = "Gateshead LADU",
    "Gedling" = "Gedling LADU",
    "Gloucester" = "Gloucester LADU",
    "Gosport" = "Gosport LADU",
    "Gravesham" = "Gravesham LADU",
    "Great Yarmouth" = "Great Yarmouth LADU",
    "Greenwich" = "Greenwich LADU",
    "Guildford" = "Guildford LADU",
    "Hackney" = "Hackney LADU",
    "Halton" = "Halton LADU",
    "Hambleton" = "Hambleton LADU",
    "Hammersmith and Fulham" = "Hammersmith and Fulham LADU",
    "Harborough" = "Harborough LADU",
    "Haringey" = "Haringey LADU",
    "Harlow" = "Harlow LADU",
    "Harrogate" = "Harrogate LADU",
    "Harrow" = "Harrow LADU",
    "Hart" = "Hart LADU",
    "Hartlepool" = "Hartlepool LADU",
    "Hastings" = "Hastings LADU",
    "Havant" = "Havant LADU",
    "Havering" = "Havering LADU",
    "Herefordshire, County of" = "Herefordshire, County of LADU",
    "Hertsmere" = "Hertsmere LADU",
    "High Peak" = "High Peak LADU",
    "Hillingdon" = "Hillingdon LADU",
    "Hinckley and Bosworth" = "Hinckley and Bosworth LADU",
    "Horsham" = "Horsham LADU",
    "Hounslow" = "Hounslow LADU",
    "Huntingdonshire" = "Huntingdonshire LADU",
    "Hyndburn" = "Hyndburn LADU",
    "Ipswich" = "Ipswich LADU",
    "Isle of Wight" = "Isle of Wight LADU",
    "Isles of Scilly" = "Isles of Scilly LADU",
    "Islington" = "Islington LADU",
    "Kensington and Chelsea" = "Kensington and Chelsea LADU",
    "King's Lynn and West Norfolk" = "King's Lynn and West Norfolk LADU",
    "Kingston upon Hull, City of" = "Kingston upon Hull, City of LADU",
    "Kingston upon Thames" = "Kingston upon Thames LADU",
    "Kirklees" = "Kirklees LADU",
    "Knowsley" = "Knowsley LADU",
    "Lambeth" = "Lambeth LADU",
    "Lancaster" = "Lancaster LADU",
    "Leeds" = "Leeds LADU",
    "Leicester" = "Leicester LADU",
    "Lewes" = "Lewes LADU",
    "Lewisham" = "Lewisham LADU",
    "Lichfield" = "Lichfield LADU",
    "Lincoln" = "Lincoln LADU",
    "Liverpool" = "Liverpool LADU",
    "Luton" = "Luton LADU",
    "Maidstone" = "Maidstone LADU",
    "Maldon" = "Maldon LADU",
    "Malvern Hills" = "Malvern Hills LADU",
    "Manchester" = "Manchester LADU",
    "Mansfield" = "Mansfield LADU",
    "Medway" = "Medway LADU",
    "Melton" = "Melton LADU",
    "Mendip" = "Mendip LADU",
    "Merton" = "Merton LADU",
    "Mid Devon" = "Mid Devon LADU",
    "Mid Suffolk" = "Mid Suffolk LADU",
    "Mid Sussex" = "Mid Sussex LADU",
    "Middlesbrough" = "Middlesbrough LADU",
    "Milton Keynes" = "Milton Keynes LADU",
    "Mole Valley" = "Mole Valley LADU",
    "New Forest" = "New Forest LADU",
    "Newark and Sherwood" = "Newark and Sherwood LADU",
    "Newcastle upon Tyne" = "Newcastle upon Tyne LADU",
    "Newcastle-under-Lyme" = "Newcastle-under-Lyme LADU",
    "Newham" = "Newham LADU",
    "North Devon" = "North Devon LADU",
    "North East Derbyshire" = "North East Derbyshire LADU",
    "North East Lincolnshire" = "North East Lincolnshire LADU",
    "North Hertfordshire" = "North Hertfordshire LADU",
    "North Kesteven" = "North Kesteven LADU",
    "North Lincolnshire" = "North Lincolnshire LADU",
    "North Norfolk" = "North Norfolk LADU",
    "North Northamptonshire" = "North Northamptonshire LADU",
    "North Somerset" = "North Somerset LADU",
    "North Tyneside" = "North Tyneside LADU",
    "North Warwickshire" = "North Warwickshire LADU",
    "North West Leicestershire" = "North West Leicestershire LADU",
    "Northumberland" = "Northumberland LADU",
    "Norwich" = "Norwich LADU",
    "Nottingham" = "Nottingham LADU",
    "Nuneaton and Bedworth" = "Nuneaton and Bedworth LADU",
    "Oadby and Wigston" = "Oadby and Wigston LADU",
    "Oldham" = "Oldham LADU",
    "Oxford" = "Oxford LADU",
    "Pendle" = "Pendle LADU",
    "Peterborough" = "Peterborough LADU",
    "Plymouth" = "Plymouth LADU",
    "Portsmouth" = "Portsmouth LADU",
    "Preston" = "Preston LADU",
    "Reading" = "Reading LADU",
    "Redbridge" = "Redbridge LADU",
    "Redcar and Cleveland" = "Redcar and Cleveland LADU",
    "Redditch" = "Redditch LADU",
    "Reigate and Banstead" = "Reigate and Banstead LADU",
    "Ribble Valley" = "Ribble Valley LADU",
    "Richmond upon Thames" = "Richmond upon Thames LADU",
    "Richmondshire" = "Richmondshire LADU",
    "Rochdale" = "Rochdale LADU",
    "Rochford" = "Rochford LADU",
    "Rossendale" = "Rossendale LADU",
    "Rother" = "Rother LADU",
    "Rotherham" = "Rotherham LADU",
    "Rugby" = "Rugby LADU",
    "Runnymede" = "Runnymede LADU",
    "Rushcliffe" = "Rushcliffe LADU",
    "Rushmoor" = "Rushmoor LADU",
    "Rutland" = "Rutland LADU",
    "Ryedale" = "Ryedale LADU",
    "Salford" = "Salford LADU",
    "Sandwell" = "Sandwell LADU",
    "Scarborough" = "Scarborough LADU",
    "Sedgemoor" = "Sedgemoor LADU",
    "Sefton" = "Sefton LADU",
    "Selby" = "Selby LADU",
    "Sevenoaks" = "Sevenoaks LADU",
    "Sheffield" = "Sheffield LADU",
    "Shropshire" = "Shropshire LADU",
    "Slough" = "Slough LADU",
    "Solihull" = "Solihull LADU",
    "Somerset West and Taunton" = "Somerset West and Taunton LADU",
    "South Cambridgeshire" = "South Cambridgeshire LADU",
    "South Derbyshire" = "South Derbyshire LADU",
    "South Gloucestershire" = "South Gloucestershire LADU",
    "South Hams" = "South Hams LADU",
    "South Holland" = "South Holland LADU",
    "South Kesteven" = "South Kesteven LADU",
    "South Norfolk" = "South Norfolk LADU",
    "South Oxfordshire" = "South Oxfordshire LADU",
    "South Ribble" = "South Ribble LADU",
    "South Somerset" = "South Somerset LADU",
    "South Staffordshire" = "South Staffordshire LADU",
    "South Tyneside" = "South Tyneside LADU",
    "Southampton" = "Southampton LADU",
    "Southend-on-Sea" = "Southend-on-Sea LADU",
    "Southwark" = "Southwark LADU",
    "Spelthorne" = "Spelthorne LADU",
    "St Albans" = "St Albans LADU",
    "St. Helens" = "St. Helens LADU",
    "Stafford" = "Stafford LADU",
    "Staffordshire Moorlands" = "Staffordshire Moorlands LADU",
    "Stevenage" = "Stevenage LADU",
    "Stockport" = "Stockport LADU",
    "Stockton-on-Tees" = "Stockton-on-Tees LADU",
    "Stoke-on-Trent" = "Stoke-on-Trent LADU",
    "Stratford-on-Avon" = "Stratford-on-Avon LADU",
    "Stroud" = "Stroud LADU",
    "Sunderland" = "Sunderland LADU",
    "Surrey Heath" = "Surrey Heath LADU",
    "Sutton" = "Sutton LADU",
    "Swale" = "Swale LADU",
    "Swindon" = "Swindon LADU",
    "Tameside" = "Tameside LADU",
    "Tamworth" = "Tamworth LADU",
    "Tandridge" = "Tandridge LADU",
    "Teignbridge" = "Teignbridge LADU",
    "Telford and Wrekin" = "Telford and Wrekin LADU",
    "Tendring" = "Tendring LADU",
    "Test Valley" = "Test Valley LADU",
    "Tewkesbury" = "Tewkesbury LADU",
    "Thanet" = "Thanet LADU",
    "Three Rivers" = "Three Rivers LADU",
    "Thurrock" = "Thurrock LADU",
    "Tonbridge and Malling" = "Tonbridge and Malling LADU",
    "Torbay" = "Torbay LADU",
    "Torridge" = "Torridge LADU",
    "Tower Hamlets" = "Tower Hamlets LADU",
    "Trafford" = "Trafford LADU",
    "Tunbridge Wells" = "Tunbridge Wells LADU",
    "Uttlesford" = "Uttlesford LADU",
    "Vale of White Horse" = "Vale of White Horse LADU",
    "Wakefield" = "Wakefield LADU",
    "Walsall" = "Walsall LADU",
    "Waltham Forest" = "Waltham Forest LADU",
    "Wandsworth" = "Wandsworth LADU",
    "Warrington" = "Warrington LADU",
    "Warwick" = "Warwick LADU",
    "Watford" = "Watford LADU",
    "Waverley" = "Waverley LADU",
    "Wealden" = "Wealden LADU",
    "Welwyn Hatfield" = "Welwyn Hatfield LADU",
    "West Berkshire" = "West Berkshire LADU",
    "West Devon" = "West Devon LADU",
    "West Lancashire" = "West Lancashire LADU",
    "West Lindsey" = "West Lindsey LADU",
    "West Northamptonshire" = "West Northamptonshire LADU",
    "West Oxfordshire" = "West Oxfordshire LADU",
    "West Suffolk" = "West Suffolk LADU",
    "Westminster" = "Westminster LADU",
    "Westmorland and Furness" = "Westmorland and Furness LADU",
    "Wigan" = "Wigan LADU",
    "Wiltshire" = "Wiltshire LADU",
    "Winchester" = "Winchester LADU",
    "Windsor and Maidenhead" = "Windsor and Maidenhead LADU",
    "Wirral" = "Wirral LADU",
    "Woking" = "Woking LADU",
    "Wokingham" = "Wokingham LADU",
    "Wolverhampton" = "Wolverhampton LADU",
    "Worcester" = "Worcester LADU",
    "Worthing" = "Worthing LADU",
    "Wychavon" = "Wychavon LADU",
    "Wyre" = "Wyre LADU",
    "Wyre Forest" = "Wyre Forest LADU",
    "York" = "York LADU"
  )
)


# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  R_scripts <- eval(styler::style_dir("R/", filetype = "r")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
}

# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"
