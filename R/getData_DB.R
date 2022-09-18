
#' Get/write data from a PostgreSQL database
#' 
#' @description
#' getData: get the entire table or some rows from the table from a PostgreSQL database.
#' writeTable: write the table into a PostgreSQL database.
#' 
#' @param id string. The id of the data to get from the table. When NULL, the entire table will be returned.
#' @param tname string. The table name of the data.  A table name can contain numeric alphabets and underline (e.g. table_1 ). The table name  should begin with an alphabet. The 8 table names below are specified:
#'                 * "edge": matrix data for network.
#'                 * "node": dictionary of the nodes.
#'                 * "synonyms": synonyms of CUI nodes.
#'                 * "rollup": rollup for CUI nodes.
#'                 * some other names can't be used: "details".
#' @param db string. The name of the database.
#' @param field string. Default from. The field name of the id to get from the table.
#' @param data data.frame. The data to write into the database.
#' @param title string. Default NULL. Parameter for more data. The title for more information data.
#' @param note string. Default "". Parameter for more data. The description for more information data.
#' @return NULL
#' @examples
#' \dontrun{
#' getData("PheCode:250", "node", "app_cuinetwork.db")
#' data2db("edge", "test_data2db.db", "test_data2db.rds")
#' }
#' @export
getData <- function(id, tname, db, field = "from"){
  if(is.null(id)){
    sql <- paste0('SELECT * FROM "', tname, '";')
  } else {
    sql <- paste0("SELECT * FROM ", tname, " WHERE \"", field, "\" = '", id, "';")
  }
  readDB(sql, tname, db)
}


#' @rdname getData
#' @export
data2db <- function(data, tname, db, title = NA, note = NA){
  cat("Database: ", db, "\n")
  cat("Table: ", tname, "\n")
  if(!tname %in% c("df_edges", "dict")) {
    if(!"details" %in% RPostgres::dbListTables(con(db))){
      details <- data.frame(tname = tname, title = title, note = note)
    } else {
      details <- getData(NULL, "details", db)
      if(tname %in% details$tname){
        details$title[details$tname == tname] <- title
        details$note[details$tname == tname] <- note
      } else {
        details <- rbind(details, data.frame(tname = tname, title = title, note = note))
      }
    }
    writeTable(details, "details", db)
  }
  writeTable(data, tname, db)
}

#' @rdname getData
#' @export
writeTable <- function(data, tname, db){
  on.exit(RPostgres::dbDisconnect(con(db)), add = TRUE)
  conn <- con(db)
  print("write to conn")
  RPostgres::dbWriteTable(conn, tname, data, overwrite = TRUE)
  RPostgres::dbDisconnect(conn)
  print("Done!")
}


getDetailsFromDB <- function(db, id){
  details <- getData(NULL, "details", db)
  for(t in details$tname){
    getData(id, t, db)
  }
}

con <- function(db){
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = db,
    host = Sys.getenv("DB_HOST"),
    port = as.numeric(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PSWD")
    # password = rstudioapi::askForPassword("Database password")
  )
}

readDB <- function(sql, tname, db){
  on.exit(RPostgres::dbDisconnect(con(db)), add = TRUE)
  conn <- con(db)
  if(tname %in% RPostgres::dbListTables(conn)){
    RPostgres::dbGetQuery(conn = conn, sql)
  } else {
    print(paste0(tname, " is't in the database!"))
    NULL
  }
}

