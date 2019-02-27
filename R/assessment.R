#' Get assessment data
#' Expects assessment.dbname, assessment.host, assessment.user, assessment.password to be set for access to the assessment database
#' @export
#'
get_assessmemt <- function(geography=NA){
  if (nchar(getOption("assessment.password"))>1) {
    conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = getOption("assessment.dbname"), host=getOption("assessment.host"), user=getOption("assessment.user"), password=getOption("assessment.password"))
  } else {
    conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = getOption("assessment.dbname"), host=getOption("assessment.host"), user=getOption("assessment.user"))
  }
  conditions=c("aggregation_level = 1",
               "not flag = 20",
               "duplicate = 0")
  if (!is.na(geography)) conditions <- c(
    paste0("ST_Intersects(tax_aggregates::geometry, ST_GeometryFromText('",sf::st_as_text(region),"',",attributes(region)$crs$epsg,")::geometry)"),
    conditions
  )

  where_string <- paste0(conditions,collapse = " and ")
  query_string <- paste0("select * from tax_aggregates where ", where_string,";")
  data <- sf::st_read(conn, query = query_string)
  RPostgreSQL::dbDisconnect(conn)

  return(data)
}

#' Get buildings data
#' Expects assessment.dbname, assessment.host, assessment.user, assessment.password to be set for access to the assessment database
#' @export
#'
get_buildings <- function(geography=NA){
  if (length(getOption("assessment.password"))>1) {
    conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = getOption("assessment.dbname"), host=getOption("assessment.host"), user=getOption("assessment.user"), password=getOption("assessment.password"))
  } else {
    conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = getOption("assessment.dbname"), host=getOption("assessment.host"), user=getOption("assessment.user"))
  }
  conditions=c()
  if (!is.na(geography)) conditions <- c(
    paste0("ST_Intersects(building_lines::geometry, ST_GeometryFromText('",sf::st_as_text(region),"',",attributes(region)$crs$epsg,")::geometry)"),
    conditions
  )

  query_string=paste(conditions,collapse = " and ")
  if (nchar(query_string)>0) query_string = paste0(" where ",query_string)
  data <- sf::st_read(conn, query = paste0("select * from building_lines",
                                              query_string,";"), geom_column = "geometry")
  RPostgreSQL::dbDisconnect(conn)

  return(data)
}


#' Get tax data
#' Expects assessment.dbname, assessment.host, assessment.user, assessment.password to be set for access to the assessment database
#' @export
#'
get_tax <- function(year=2017,tax_coord=NA){
  if (length(getOption("assessment.password"))>1) {
    conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = getOption("assessment.dbname"), host=getOption("assessment.host"), user=getOption("assessment.user"), password=getOption("assessment.password"))
  } else {
    conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = getOption("assessment.dbname"), host=getOption("assessment.host"), user=getOption("assessment.user"))
  }
  conditions=c(paste0("tax_assessment_year = ",year))
  if (!is.na(tax_coord)) conditions <- c(
      paste0("land_coordinate in (",tax_coord,")"),
        conditions
  )

  query_string=paste(conditions,collapse = " and ")
  # data <- sf::st_read_db(conn, query = paste0("select * from tax_data where ",
  #                                             query_string,";"), geom_column = NULL)
  data <- RPostgreSQL::dbGetQuery(conn, paste0("select * from tax_data where ", query_string,";"))
  RPostgreSQL::dbDisconnect(conn)

  return(data)
}

#' Residential land use codes
#' @export
residential_land_use_codes <- c("S230","S235","S131","S135","S130","S110","S410")

#' Filter to just get properties with residential land use
#'
#' @export
residential_land_use <- function(data){
  return(filter(data,lu_code %in% residential_land_use_codes))
}

#' regular filter
#' @export
regular_properties <- function(data){
  filter(data,lu_flag==1 & flag!=20)
}

#' sfh filter
#' @export
sfh_properties <- function(data){
  filter(data,(lu_code=='S110' & unit_count==1) | (lu_code=='U100' & grepl("^RS-",zone_name))) %>% regular_properties %>% for_level(1)
}



#' filter by level
#' @export
for_level <-function(data,level){
  filter(data,aggregation_level==level)
}

#' Land Use Labels
#' @export
land_use_labels <- list(
    "S110"="Residential - Single Detached & Duplex",
    "S131"="Residential – Townhouse",
    "S130"="Residential - Low-rise Apartment",
    "S135"="Residential - High-rise Apartment",
    "S410"="Residential - Institutional and Non-Market Housing",
    "S200"="Commercial",
    "S230"="Mixed Residential Commercial - Low-rise Apartment",
    "S235"="Mixed Residential Commercial - High-rise Apartment",
    "S400"="Institutional",
    "A500"="Agriculture",
    "S300"="Industrial",
    "R100"="Recreation, Open Space and Protected Natural Areas",
    "S420"="Cemetery",
    "U100"="Undeveloped and Unclassified",
    "M300"="Industrial – Extractive",
    "S120"="Residential – Rural",
    "S100"="Residential - Mobile Home Park",
    "S700"="Rail, Rapid Transit, Other Transportation, Utility and Communication",
    "S600"="Port Metro Vancouver",
    "R200"="Lakes, Large Rivers and Other Water",
    "S500"="Road Right-of-Way",
    "W400"="Protected Watershed",
    "S650"="Airport/Airstrip",
    "F100"="Harvesting and Research",
    "J000"="Recent Redeveloped / Misclassified"
)


#' @export
city_types <- list(
  'CAPITAL FUND'= 101,
  'PROPERTY ENDOWMENT FUND'= 102,
  'Vancouver Public Housing Corp'= 103,
  'SEFC LOAN'= 104
)
