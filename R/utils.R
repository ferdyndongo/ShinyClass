#' give the list of sheets in a workbook excel or tables in a microsoft access or SQLite database
#'
#' @param name file name with extension
#' @param path the full path of the file
#' @return a character vector of table names or sheet names
sheet_list <- function(name, path){
  ext <- tools::file_ext(name)
  base::switch(ext,
               xls = readxl::excel_sheets(path = path),
               xlsx = readxl::excel_sheets(path = path),
               mdb = Hmisc::mdb.get(file = path,tables = TRUE),
               db = RSQLite::dbListTables(conn = RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname=path)),
               s3db = RSQLite::dbListTables(conn = RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname=path)),
               shiny::validate("Invalid file; Please upload a .db, .s3db, .mdb, .xlsx or .xls file")
  )
}

#' Read data from file with extension db, s3db, xls, xlsx, mdb, csv, txt, tsv and RDS
#'
#' @param name file name with extension
#' @param path full path of file
#' @param sheet_name sheet name or table name if the file is excel, microsoft access or sqlite db file, otherwise is set to NULL for txt, csv, tsv and RDS files
#' @return a saved model of class train.formula from caret package if it is RDS file otherwise a dataframe
load_file <- function(name, path, sheet_name=NULL) {
  ext <- tools::file_ext(name)
  base::switch(ext,
               xls = readxl::read_excel(path = path,sheet = sheet_name),
               xlsx = readxl::read_excel(path = path,sheet = sheet_name),
               csv = readr::read_csv(file = path),
               txt = utils::read.table(file = path),
               tsv = utils::read.table(file = path, header = TRUE),
               mdb = Hmisc::mdb.get(file = path,tables = sheet_name),
               RDS = base::readRDS(file = path),
               db = RSQLite::dbReadTable(conn = RSQLite::dbConnect(drv = RSQLite::SQLite(),dbname=path),name = sheet_name),
               s3db = RSQLite::dbReadTable(conn = RSQLite::dbConnect(drv = RSQLite::SQLite(),dbname=path),name = sheet_name),
               shiny::validate("Invalid file; Please upload a .db, .s3db, .csv, .tsv, .txt, .xlsx, .xls, .mdb or .RDS file")
  )
}

#' Upload data file in sqlite database through available DSN.
#'
#' @param dsn data source name
#' @param data data to be uploaded in the database
#' @param tablename data base table name
#' @param overwrite boolean value overwriting or not an existing table
#' @param append boolean value updating an existing table. If equals FALSE a new table is created
file_to_sqlite_dsn <- function(dsn, data, tablename, overwrite=FALSE, append=TRUE){
  db <- DBI::dbConnect(odbc::odbc(), dsn)
  odbc::dbWriteTable(conn = db, name =  tablename, value = data,overwrite=overwrite,append=append)
  odbc::dbDisconnect(db)
}
