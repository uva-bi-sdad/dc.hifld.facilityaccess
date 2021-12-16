# load packages
for (pkg in c("tidyverse", "data.table", "stringr", "stringi", "mosaic", "dplyr", "readr","gt", "dataverse")) {
  library(pkg, character.only = TRUE)
}

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

dc_dbWriteTable <-
  function(
    db_conn,
    schema_name,
    table_name,
    table_data,
    table_owner = "data_commons"
  ) {
    # check for geometry/geography columns
    tf <- sapply(table_data, {function(x) inherits(x, 'sfc')})
    # if TRUE, use sf
    if (TRUE %in% tf) {
      sf_write_result <- sf::st_write(obj = table_data, dsn = db_conn, layer = c(schema_name, table_name), row.names = FALSE)
      print(sf_write_result)
      # if FALSE, use DBI
    } else {
      write_result <- DBI::dbWriteTable(conn = db_conn, name = c(schema_name, table_name), value = table_data, row.names = FALSE)
      print(write_result)
    }
    # change table owner
    chgown_result <- DBI::dbSendQuery(conn = db_conn, statement = paste0("ALTER TABLE ", schema_name, ".", table_name, " OWNER TO ", table_owner))
    print(chgown_result)
  }

# Get tables for mental_access, ems_access, hospital_access, substance_access (hd, ct, tr)
# Add region_type column?
# Merge tables to hdcttr
# Write tables to the dataverse

con <- get_db_conn()
# substance access
type = "substance"
tr <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_tr_sdad_2021_", type, "_acccess_scores")))
ct <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_ct_sdad_2021_", type, "_acccess_scores")))
hd <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_hd_sdad_2021_", type, "_acccess_scores")))
substance_access <- rbind(tr, ct, hd)
# hospital access
type = "hospital"
tr <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_tr_sdad_2021_", type, "_acccess_scores")))
ct <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_ct_sdad_2021_", type, "_acccess_scores")))
hd <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_hd_sdad_2021_", type, "_acccess_scores")))
hospital_access <- rbind(tr, ct, hd)
# ems access
type = "ems"
tr <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_tr_sdad_2021_", type, "_acccess_scores")))
ct <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_ct_sdad_2021_", type, "_acccess_scores")))
hd <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_hd_sdad_2021_", type, "_acccess_scores")))
ems_access <- rbind(tr, ct, hd)
# mental access
type = "mental"
tr <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_tr_sdad_2021_", type, "_acccess_scores")))
ct <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_ct_sdad_2021_", type, "_acccess_scores")))
hd <- DBI::dbReadTable(con, c("dc_health_behavior_diet", paste0("va_hd_sdad_2021_", type, "_acccess_scores")))
mental_access <- rbind(tr, ct, hd)
DBI::dbDisconnect(con)

con <- get_db_conn()
dc_dbWriteTable(con, "dc_health_behavior_diet", "va_hdcttr_sdad_2021_substance_access_scores", substance_access)
dc_dbWriteTable(con, "dc_health_behavior_diet", "va_hdcttr_sdad_2021_hospital_access_scores", hospital_access)
dc_dbWriteTable(con, "dc_health_behavior_diet", "va_hdcttr_sdad_2021_ems_access_scores", ems_access)
dc_dbWriteTable(con, "dc_health_behavior_diet", "va_hdcttr_sdad_2021_mental_access_scores", mental_access)
DBI::dbDisconnect(con)
