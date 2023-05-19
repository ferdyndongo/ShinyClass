#' give the list of sheets in a workbook excel or tables in a microsoft access
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
               shiny::validate("Invalid file; Please upload a .mdb, .xlsx or .xls file")
  )
}

#' Read data from file with extension xls, xlsx, mdb, csv, txt, tsv and RDS
#'
#' @param name file name with extension
#' @param path full path of file
#' @param sheet_name sheet name or table name if the file is excel or microsoft access, otherwise is set to NULL for txt, csv, tsv and RDS files
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
               shiny::validate("Invalid file; Please upload a .csv, .tsv, .txt, .xlsx, .xls, .mdb or .RDS file")
  )
}
