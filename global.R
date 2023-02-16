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
shhh(library(shinytest))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(dplyr))
shhh(library(data.table))
shhh(library(tidyverse))
shhh(library(plotly))
shhh(library(openxlsx))
shhh(library(janitor))
shhh(library(DT))
shhh(library(writexl))
shhh(library(scales)) # for comma 1,000s
shhh(library(leaflet))
shhh(library(lubridate))
shhh(library(sf))
shhh(library(capture))

# renv::snapshot()

google_analytics_key <- "MMB6NG2FE1"

area_select <- c("Coast to Capital", "Greater Manchester", "England")

metricChoices <- list(
  "Employment" = list(
    "Employment rate" = "empRate",
    "Self-employment rate" = "selfempRate",
    "Unemployment rate" = "unempRate",
    "Inactive rate" = "inactiveRate",
    "Employment volume" = "Employment",
    "Self-employment volume" = "SelfEmployed",
    "Unemployed volume" = "Unemployed",
    "Inactive volume" = "Inactive"),
  "Jobs" = list(
    "Online job adverts" = "vacancies",
    "Job projections (Skills imperative 2035) - TO COME" = "workingFutures"
  ),
  "Businesses" = list(
    "Business count" = "enterpriseCount",
    "Business birth rate" = "birthRate",
    "Business death rate" = "deathRate"
  ),
  "FE and skills" = list(
    "FE achievement rate" = "achievements_rate_per_100000_population",
    "FE participation rate" = "participation_rate_per_100000_population",
    # "FE start rate" = "starts_rate_per_100000_population",
    "FE achievement volumes" = "achievements",
    "FE participation volumes" = "participation",
    # "FE starts" = "starts",
    "Qualified at Level 3 or above" = "level3AndAboveRate"
  ),
  "Destinations" = list(
    "Key stage 4 completers sustained positive destination rate" = "sustainedPositiveDestinationKS4Rate",
    "Key stage 5 completers sustained positive destination rate" = "sustainedPositiveDestinationKS5Rate"
  )
  # ,
  # "Mismatch" = list("Supply vs demand TO COME" = "mismatch")
)

areaChoices<-list(
  "Local enterprise partnership"=list(
    "Black Country"="Black CountryLEP",
    "Cheshire and Warrington"="Cheshire and WarringtonLEP",
    "Cornwall and Isles of Scilly"="Cornwall and Isles of ScillyLEP",
    "Coventry and Warwickshire"="Coventry and WarwickshireLEP",
    "Cumbria"="CumbriaLEP",
    "Dorset"="DorsetLEP",
    "Gloucestershire"="GloucestershireLEP",
    "Greater Birmingham and Solihull"="Greater Birmingham and SolihullLEP",
    "Greater Manchester"="Greater ManchesterLEP",
    "Heart of the South West"="Heart of the South WestLEP",
    "Lancashire"="LancashireLEP",
    "Leicester and Leicestershire"="Leicester and LeicestershireLEP",
    "Liverpool City Region"="Liverpool City RegionLEP",
    "North East"="North EastLEP",
    "Stoke-on-Trent and Staffordshire"="Stoke-on-Trent and StaffordshireLEP",
    "Swindon and Wiltshire"="Swindon and WiltshireLEP",
    "Tees Valley"="Tees ValleyLEP",
    "Thames Valley Berkshire"="Thames Valley BerkshireLEP",
    "The Marches"="The MarchesLEP",
    "West of England"="West of EnglandLEP",
    "Worcestershire"="WorcestershireLEP",
    "Buckinghamshire"="BuckinghamshireLEP",
    "Coast to Capital"="Coast to CapitalLEP",
    "Derby, Derbyshire, Nottingham and Nottinghamshire"="Derby, Derbyshire, Nottingham and NottinghamshireLEP",
    "Enterprise M3"="Enterprise M3LEP",
    "Hertfordshire"="HertfordshireLEP",
    "London"="LondonLEP",
    "New Anglia"="New AngliaLEP",
    "Oxfordshire"="OxfordshireLEP",
    "Sheffield City Region"="Sheffield City RegionLEP",
    "Solent"="SolentLEP",
    "South East"="South EastLEP",
    "South East Midlands"="South East MidlandsLEP",
    "York and North Yorkshire"="York and North YorkshireLEP",
    "Greater Cambridge and Greater Peterborough"="Greater Cambridge and Greater PeterboroughLEP",
    "Greater Lincolnshire"="Greater LincolnshireLEP",
    "Hull and East Yorkshire"="Hull and East YorkshireLEP",
    "Leeds City Region"="Leeds City RegionLEP"),
  
  "Local skills partnership area"=list(
    "Brighton and Hove, East Sussex, West Sussex"="Brighton and Hove, East Sussex, West SussexLSIP",
    "Buckinghamshire"="BuckinghamshireLSIP",
    "Cambridgeshire and Peterborough"="Cambridgeshire and PeterboroughLSIP",
    "Cheshire and Warrington"="Cheshire and WarringtonLSIP",
    "Cornwall and the Isles of Scilly"="Cornwall and the Isles of ScillyLSIP",
    "Cumbria"="CumbriaLSIP",
    "Derbyshire and Nottinghamshire"="Derbyshire and NottinghamshireLSIP",
    "Dorset"="DorsetLSIP",
    "Enterprise M3 LEP (including all of Surrey)"="Enterprise M3 LEP (including all of Surrey)LSIP",
    "Essex, Southend-on-Sea and Thurrock"="Essex, Southend-on-Sea and ThurrockLSIP",
    "Gloucestershire"="GloucestershireLSIP",
    "Greater Lincolnshire"="Greater LincolnshireLSIP",
    "Greater London"="Greater LondonLSIP",
    "Greater Manchester"="Greater ManchesterLSIP",
    "Heart of the South-West"="Heart of the South-WestLSIP",
    "Hertfordshire"="HertfordshireLSIP",
    "Hull and East Yorkshire"="Hull and East YorkshireLSIP",
    "Kent and Medway"="Kent and MedwayLSIP",
    "Lancashire"="LancashireLSIP",
    "Leicester and Leicestershire"="Leicester and LeicestershireLSIP",
    "Liverpool City Region"="Liverpool City RegionLSIP",
    "Norfolk and Suffolk"="Norfolk and SuffolkLSIP",
    "North East"="North EastLSIP",
    "North of Tyne"="North of TyneLSIP",
    "Oxfordshire"="OxfordshireLSIP",
    "Solent"="SolentLSIP",
    "South East Midlands"="South East MidlandsLSIP",
    "South Yorkshire"="South YorkshireLSIP",
    "Stoke-on-Trent and Staffordshire"="Stoke-on-Trent and StaffordshireLSIP",
    "Swindon and Wiltshire"="Swindon and WiltshireLSIP",
    "Tees Valley"="Tees ValleyLSIP",
    "Thames Valley Berkshire"="Thames Valley BerkshireLSIP",
    "The Marches"="The MarchesLSIP",
    "West Midlands and Warwickshire"="West Midlands and WarwickshireLSIP",
    "West Yorkshire"="West YorkshireLSIP",
    "West of England and North Somerset"="West of England and North SomersetLSIP",
    "Worcestershire"="WorcestershireLSIP",
    "York and North Yorkshire"="York and North YorkshireLSIP"),
  
  "Mayoral combined authority" = list(
    "Greater Manchester"="Greater ManchesterMCA",
    "South Yorkshire"="South YorkshireMCA",
    "West Yorkshire"="West YorkshireMCA",
    "Liverpool City Region"="Liverpool City RegionMCA",
    "Tees Valley"="Tees ValleyMCA",
    "West Midlands"="West MidlandsMCA",
    "Cambridgeshire and Peterborough"="Cambridgeshire and PeterboroughMCA",
    "West of England"="West of EnglandMCA",
    "North East"="North EastMCA",
    "North of Tyne"="North of TyneMCA"),
 
  "Country"=list(
    "England"="EnglandCOUNTRY"),
  "Local authority"=list(
    "Hartlepool"="HartlepoolLADU",
    "Middlesbrough"="MiddlesbroughLADU",
    "Redcar and Cleveland"="Redcar and ClevelandLADU",
    "Stockton-on-Tees"="Stockton-on-TeesLADU",
    "Darlington"="DarlingtonLADU",
    "Halton"="HaltonLADU",
    "Warrington"="WarringtonLADU",
    "Blackburn with Darwen"="Blackburn with DarwenLADU",
    "Blackpool"="BlackpoolLADU",
    "Kingston upon Hull, City of"="Kingston upon Hull, City ofLADU",
    "East Riding of Yorkshire"="East Riding of YorkshireLADU",
    "North East Lincolnshire"="North East LincolnshireLADU",
    "North Lincolnshire"="North LincolnshireLADU",
    "York"="YorkLADU",
    "Derby"="DerbyLADU",
    "Leicester"="LeicesterLADU",
    "Rutland"="RutlandLADU",
    "Nottingham"="NottinghamLADU",
    "Herefordshire, County of"="Herefordshire, County ofLADU",
    "Telford and Wrekin"="Telford and WrekinLADU",
    "Stoke-on-Trent"="Stoke-on-TrentLADU",
    "Bath and North East Somerset"="Bath and North East SomersetLADU",
    "Bristol, City of"="Bristol, City ofLADU",
    "North Somerset"="North SomersetLADU",
    "South Gloucestershire"="South GloucestershireLADU",
    "Plymouth"="PlymouthLADU",
    "Torbay"="TorbayLADU",
    "Swindon"="SwindonLADU",
    "Peterborough"="PeterboroughLADU",
    "Luton"="LutonLADU",
    "Southend-on-Sea"="Southend-on-SeaLADU",
    "Thurrock"="ThurrockLADU",
    "Medway"="MedwayLADU",
    "Bracknell Forest"="Bracknell ForestLADU",
    "West Berkshire"="West BerkshireLADU",
    "Reading"="ReadingLADU",
    "Slough"="SloughLADU",
    "Windsor and Maidenhead"="Windsor and MaidenheadLADU",
    "Wokingham"="WokinghamLADU",
    "Milton Keynes"="Milton KeynesLADU",
    "Brighton and Hove"="Brighton and HoveLADU",
    "Portsmouth"="PortsmouthLADU",
    "Southampton"="SouthamptonLADU",
    "Isle of Wight"="Isle of WightLADU",
    "County Durham"="County DurhamLADU",
    "Cheshire East"="Cheshire EastLADU",
    "Cheshire West and Chester"="Cheshire West and ChesterLADU",
    "Shropshire"="ShropshireLADU",
    "Cornwall"="CornwallLADU",
    "Isles of Scilly"="Isles of ScillyLADU",
    "Wiltshire"="WiltshireLADU",
    "Bedford"="BedfordLADU",
    "Central Bedfordshire"="Central BedfordshireLADU",
    "Northumberland"="NorthumberlandLADU",
    "Bournemouth, Christchurch and Poole"="Bournemouth, Christchurch and PooleLADU",
    "Dorset"="DorsetLADU",
    "Buckinghamshire"="BuckinghamshireLADU",
    "North Northamptonshire"="North NorthamptonshireLADU",
    "West Northamptonshire"="West NorthamptonshireLADU",
    "Cambridge"="CambridgeLADU",
    "East Cambridgeshire"="East CambridgeshireLADU",
    "Fenland"="FenlandLADU",
    "Huntingdonshire"="HuntingdonshireLADU",
    "South Cambridgeshire"="South CambridgeshireLADU",
    "Allerdale"="AllerdaleLADU",
    "Barrow-in-Furness"="Barrow-in-FurnessLADU",
    "Carlisle"="CarlisleLADU",
    "Copeland"="CopelandLADU",
    "Eden"="EdenLADU",
    "South Lakeland"="South LakelandLADU",
    "Amber Valley"="Amber ValleyLADU",
    "Bolsover"="BolsoverLADU",
    "Chesterfield"="ChesterfieldLADU",
    "Derbyshire Dales"="Derbyshire DalesLADU",
    "Erewash"="ErewashLADU",
    "High Peak"="High PeakLADU",
    "North East Derbyshire"="North East DerbyshireLADU",
    "South Derbyshire"="South DerbyshireLADU",
    "East Devon"="East DevonLADU",
    "Exeter"="ExeterLADU",
    "Mid Devon"="Mid DevonLADU",
    "North Devon"="North DevonLADU",
    "South Hams"="South HamsLADU",
    "Teignbridge"="TeignbridgeLADU",
    "Torridge"="TorridgeLADU",
    "West Devon"="West DevonLADU",
    "Eastbourne"="EastbourneLADU",
    "Hastings"="HastingsLADU",
    "Lewes"="LewesLADU",
    "Rother"="RotherLADU",
    "Wealden"="WealdenLADU",
    "Basildon"="BasildonLADU",
    "Braintree"="BraintreeLADU",
    "Brentwood"="BrentwoodLADU",
    "Castle Point"="Castle PointLADU",
    "Chelmsford"="ChelmsfordLADU",
    "Colchester"="ColchesterLADU",
    "Epping Forest"="Epping ForestLADU",
    "Harlow"="HarlowLADU",
    "Maldon"="MaldonLADU",
    "Rochford"="RochfordLADU",
    "Tendring"="TendringLADU",
    "Uttlesford"="UttlesfordLADU",
    "Cheltenham"="CheltenhamLADU",
    "Cotswold"="CotswoldLADU",
    "Forest of Dean"="Forest of DeanLADU",
    "Gloucester"="GloucesterLADU",
    "Stroud"="StroudLADU",
    "Tewkesbury"="TewkesburyLADU",
    "Basingstoke and Deane"="Basingstoke and DeaneLADU",
    "East Hampshire"="East HampshireLADU",
    "Eastleigh"="EastleighLADU",
    "Fareham"="FarehamLADU",
    "Gosport"="GosportLADU",
    "Hart"="HartLADU",
    "Havant"="HavantLADU",
    "New Forest"="New ForestLADU",
    "Rushmoor"="RushmoorLADU",
    "Test Valley"="Test ValleyLADU",
    "Winchester"="WinchesterLADU",
    "Broxbourne"="BroxbourneLADU",
    "Dacorum"="DacorumLADU",
    "Hertsmere"="HertsmereLADU",
    "North Hertfordshire"="North HertfordshireLADU",
    "Three Rivers"="Three RiversLADU",
    "Watford"="WatfordLADU",
    "Ashford"="AshfordLADU",
    "Canterbury"="CanterburyLADU",
    "Dartford"="DartfordLADU",
    "Dover"="DoverLADU",
    "Gravesham"="GraveshamLADU",
    "Maidstone"="MaidstoneLADU",
    "Sevenoaks"="SevenoaksLADU",
    "Folkestone and Hythe"="Folkestone and HytheLADU",
    "Swale"="SwaleLADU",
    "Thanet"="ThanetLADU",
    "Tonbridge and Malling"="Tonbridge and MallingLADU",
    "Tunbridge Wells"="Tunbridge WellsLADU",
    "Burnley"="BurnleyLADU",
    "Chorley"="ChorleyLADU",
    "Fylde"="FyldeLADU",
    "Hyndburn"="HyndburnLADU",
    "Lancaster"="LancasterLADU",
    "Pendle"="PendleLADU",
    "Preston"="PrestonLADU",
    "Ribble Valley"="Ribble ValleyLADU",
    "Rossendale"="RossendaleLADU",
    "South Ribble"="South RibbleLADU",
    "West Lancashire"="West LancashireLADU",
    "Wyre"="WyreLADU",
    "Blaby"="BlabyLADU",
    "Charnwood"="CharnwoodLADU",
    "Harborough"="HarboroughLADU",
    "Hinckley and Bosworth"="Hinckley and BosworthLADU",
    "Melton"="MeltonLADU",
    "North West Leicestershire"="North West LeicestershireLADU",
    "Oadby and Wigston"="Oadby and WigstonLADU",
    "Boston"="BostonLADU",
    "East Lindsey"="East LindseyLADU",
    "Lincoln"="LincolnLADU",
    "North Kesteven"="North KestevenLADU",
    "South Holland"="South HollandLADU",
    "South Kesteven"="South KestevenLADU",
    "West Lindsey"="West LindseyLADU",
    "Breckland"="BrecklandLADU",
    "Broadland"="BroadlandLADU",
    "Great Yarmouth"="Great YarmouthLADU",
    "King's Lynn and West Norfolk"="King's Lynn and West NorfolkLADU",
    "North Norfolk"="North NorfolkLADU",
    "Norwich"="NorwichLADU",
    "South Norfolk"="South NorfolkLADU",
    "Craven"="CravenLADU",
    "Hambleton"="HambletonLADU",
    "Harrogate"="HarrogateLADU",
    "Richmondshire"="RichmondshireLADU",
    "Ryedale"="RyedaleLADU",
    "Scarborough"="ScarboroughLADU",
    "Selby"="SelbyLADU",
    "Ashfield"="AshfieldLADU",
    "Bassetlaw"="BassetlawLADU",
    "Broxtowe"="BroxtoweLADU",
    "Gedling"="GedlingLADU",
    "Mansfield"="MansfieldLADU",
    "Newark and Sherwood"="Newark and SherwoodLADU",
    "Rushcliffe"="RushcliffeLADU",
    "Cherwell"="CherwellLADU",
    "Oxford"="OxfordLADU",
    "South Oxfordshire"="South OxfordshireLADU",
    "Vale of White Horse"="Vale of White HorseLADU",
    "West Oxfordshire"="West OxfordshireLADU",
    "Mendip"="MendipLADU",
    "Sedgemoor"="SedgemoorLADU",
    "South Somerset"="South SomersetLADU",
    "Cannock Chase"="Cannock ChaseLADU",
    "East Staffordshire"="East StaffordshireLADU",
    "Lichfield"="LichfieldLADU",
    "Newcastle-under-Lyme"="Newcastle-under-LymeLADU",
    "South Staffordshire"="South StaffordshireLADU",
    "Stafford"="StaffordLADU",
    "Staffordshire Moorlands"="Staffordshire MoorlandsLADU",
    "Tamworth"="TamworthLADU",
    "Babergh"="BaberghLADU",
    "Ipswich"="IpswichLADU",
    "Mid Suffolk"="Mid SuffolkLADU",
    "Elmbridge"="ElmbridgeLADU",
    "Epsom and Ewell"="Epsom and EwellLADU",
    "Guildford"="GuildfordLADU",
    "Mole Valley"="Mole ValleyLADU",
    "Reigate and Banstead"="Reigate and BansteadLADU",
    "Runnymede"="RunnymedeLADU",
    "Spelthorne"="SpelthorneLADU",
    "Surrey Heath"="Surrey HeathLADU",
    "Tandridge"="TandridgeLADU",
    "Waverley"="WaverleyLADU",
    "Woking"="WokingLADU",
    "North Warwickshire"="North WarwickshireLADU",
    "Nuneaton and Bedworth"="Nuneaton and BedworthLADU",
    "Rugby"="RugbyLADU",
    "Stratford-on-Avon"="Stratford-on-AvonLADU",
    "Warwick"="WarwickLADU",
    "Adur"="AdurLADU",
    "Arun"="ArunLADU",
    "Chichester"="ChichesterLADU",
    "Crawley"="CrawleyLADU",
    "Horsham"="HorshamLADU",
    "Mid Sussex"="Mid SussexLADU",
    "Worthing"="WorthingLADU",
    "Bromsgrove"="BromsgroveLADU",
    "Malvern Hills"="Malvern HillsLADU",
    "Redditch"="RedditchLADU",
    "Worcester"="WorcesterLADU",
    "Wychavon"="WychavonLADU",
    "Wyre Forest"="Wyre ForestLADU",
    "St Albans"="St AlbansLADU",
    "Welwyn Hatfield"="Welwyn HatfieldLADU",
    "East Hertfordshire"="East HertfordshireLADU",
    "Stevenage"="StevenageLADU",
    "East Suffolk"="East SuffolkLADU",
    "West Suffolk"="West SuffolkLADU",
    "Somerset West and Taunton"="Somerset West and TauntonLADU",
    "Bolton"="BoltonLADU",
    "Bury"="BuryLADU",
    "Manchester"="ManchesterLADU",
    "Oldham"="OldhamLADU",
    "Rochdale"="RochdaleLADU",
    "Salford"="SalfordLADU",
    "Stockport"="StockportLADU",
    "Tameside"="TamesideLADU",
    "Trafford"="TraffordLADU",
    "Wigan"="WiganLADU",
    "Knowsley"="KnowsleyLADU",
    "Liverpool"="LiverpoolLADU",
    "St. Helens"="St. HelensLADU",
    "Sefton"="SeftonLADU",
    "Wirral"="WirralLADU",
    "Barnsley"="BarnsleyLADU",
    "Doncaster"="DoncasterLADU",
    "Rotherham"="RotherhamLADU",
    "Sheffield"="SheffieldLADU",
    "Newcastle upon Tyne"="Newcastle upon TyneLADU",
    "North Tyneside"="North TynesideLADU",
    "South Tyneside"="South TynesideLADU",
    "Sunderland"="SunderlandLADU",
    "Birmingham"="BirminghamLADU",
    "Coventry"="CoventryLADU",
    "Dudley"="DudleyLADU",
    "Sandwell"="SandwellLADU",
    "Solihull"="SolihullLADU",
    "Walsall"="WalsallLADU",
    "Wolverhampton"="WolverhamptonLADU",
    "Bradford"="BradfordLADU",
    "Calderdale"="CalderdaleLADU",
    "Kirklees"="KirkleesLADU",
    "Leeds"="LeedsLADU",
    "Wakefield"="WakefieldLADU",
    "Gateshead"="GatesheadLADU",
    "City of London"="City of LondonLADU",
    "Barking and Dagenham"="Barking and DagenhamLADU",
    "Barnet"="BarnetLADU",
    "Bexley"="BexleyLADU",
    "Brent"="BrentLADU",
    "Bromley"="BromleyLADU",
    "Camden"="CamdenLADU",
    "Croydon"="CroydonLADU",
    "Ealing"="EalingLADU",
    "Enfield"="EnfieldLADU",
    "Greenwich"="GreenwichLADU",
    "Hackney"="HackneyLADU",
    "Hammersmith and Fulham"="Hammersmith and FulhamLADU",
    "Haringey"="HaringeyLADU",
    "Harrow"="HarrowLADU",
    "Havering"="HaveringLADU",
    "Hillingdon"="HillingdonLADU",
    "Hounslow"="HounslowLADU",
    "Islington"="IslingtonLADU",
    "Kensington and Chelsea"="Kensington and ChelseaLADU",
    "Kingston upon Thames"="Kingston upon ThamesLADU",
    "Lambeth"="LambethLADU",
    "Lewisham"="LewishamLADU",
    "Merton"="MertonLADU",
    "Newham"="NewhamLADU",
    "Redbridge"="RedbridgeLADU",
    "Richmond upon Thames"="Richmond upon ThamesLADU",
    "Southwark"="SouthwarkLADU",
    "Sutton"="SuttonLADU",
    "Tower Hamlets"="Tower HamletsLADU",
    "Waltham Forest"="Waltham ForestLADU",
    "Wandsworth"="WandsworthLADU",
    "Westminster"="WestminsterLADU")
  
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
