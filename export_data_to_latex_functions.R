
#' Function that exports a data.frame to a tex file which can be included in a table enviroment 
#'
#' @param data a data.frame to be exported as tex-document
#' @param path path and name of the exported file (must end with '.tex')
#' @param round_precison for numerical columns, the number of decimal digits to be exported
#' @param row_names row names to be exported, can be NULL
#' @param italic_rows logical vector defining which rows should be made italic, can be NULL
#' @param bold_rows logical vector defining which rows should be made bold, can be NULL
#'
#' @return does not return any object
#' 
export_data_to_latex <- function(data, path, round_precison = NULL, row_names = NULL, italic_rows = NULL, bold_rows = NULL) {
  require(dplyr)
  require(tibble)
  require(purrr)
  require(stringr)
  # Make checks of the inputs:
  if (!is.integer(round_precison) & !is.null(round_precison)) stop("'round_precison' must be an integer or NULL")
  if (!is.null(row_names) & length(row_names) != nrow(data)) stop("'row_names' must have the same length as the number of rows in 'data'")
  if (!is.null(italic_rows) & !is.logical(italic_rows) ) stop("'italic_rows' must either be NULL or a logical vector")
  if (!is.null(italic_rows) & length(italic_rows) != nrow(data)) stop("'italic_rows' must have the same length as the number of rows in 'data'")
  if (!is.null(bold_rows) & !is.logical(bold_rows) ) stop("'bold_rows' must either be NULL or a logical vector")
  if (!is.null(bold_rows) & length(bold_rows) != nrow(data)) stop("'bold_rows' must have the same length as the number of rows in 'data'")
  
  data %>% 
    # Ensure that the data is in a tibble-object:
    as_tibble() %>%
    # Round numeric columns:
    mutate_if(is.numeric, ~format_numeric_column(., nsmall = round_precison)) %>%
    # Add math enviroment for numeric coulmns (note that this columns are then characters, and no longer numeric)
    mutate_if(is.numeric, ~paste0("$", ., "$") ) %>% 
    # Add escapes to latex symbols
    mutate_all(~str_replace_all(string = ., pattern = "([%&]{1})", replacement = "\\\\\\1")) %>%
    # If specified, add row names:
    {if (!is.null(row_names)) add_column(., rownames = as.character(row_names), .before = 1) else .} %>%
    # Make specific rows italic:
    {if (!is.null(italic_rows)) mutate_all(., ~ifelse(italic_rows, paste0("\\textit{",.,"}"), .)) else . } %>%
    # Make specific rows bold:
    {if (!is.null(bold_rows)) mutate_all(., ~ifelse(bold_rows, paste0("\\textit{",.,"}"), .)) else . } %>%
    # Put data into a list, togheter with a column count (we will use this to add the coulmn separaators as defined by latex)
    {append(list(.), 2:ncol(.))} %>% 
    # Between each coulmn, add the separator symbol '&'
    reduce(.f = function(x,y) add_column(x, tmp = rep("&", nrow(x)), .before = 2*y -2 ) %>% rename(!!as.character(y) := "tmp")) %>%
    # Add "new row" Latex-Symbol
    add_column( last_col = "\\\\") %>%
    # Export file:
    write.table(file = path ,sep=" ", row.names=FALSE, col.names = FALSE, quote=FALSE)
}


#' Function that formats a numeric column for the export to latex. Besides rounding the 
#' numbers and converting them to characters, the function adds phantome spaces such that
#' the numbers are aligned when displayed in Latex, besides possible "-" signs
#'
#' @param col a numeric vector
#' @param nsmall the number of decimals to be exported or NULL (the default)
#'
#' @return the formatted column, i.e. when nsmall is not NULL, a character vector
#'
format_numeric_column <- function(col, nsmall = NULL) {
  require(magrittr)
  require(stringr)
  # If the rounding argument is NULL, the column is returned in its current state
  if (is.null(nsmall)) return(col)
  # A help function that replaces leading white spaces with phantom characters
  add_phantom <- function(st) {
    # Get leading white spaces
    leading_white <- str_extract(st, pattern = "^\\s+")
    # If there are leading white spaces, add phantom charcters
    if (!is.na(leading_white)) {
      # Replace leading white spaces with latex's phantom characters
      st <- str_replace(st, 
                        pattern = "^\\s+", 
                        replacement= paste0("\\\\leavevmode\\\\phantom{",
                                            paste(rep("0", nchar(leading_white)), 
                                                  collapse = ""), 
                                            "}"))
    }
    return(st)
  }
  
  col %>% 
    # round all numbers in the array
    round(digits = nsmall) %>% 
    # format the numbers such that they have 'nsmall' decimals,
    # note that this returns a character vector with leading white 
    # spaces to ensure that all entries have the same length
    format(nsmall= nsmall) %>% 
    # replace the leading white spaces with phantom characters,
    # this step is necessary since Latex does not recognize the
    # leading white spaces as white spaces
    sapply(FUN = add_phantom, USE.NAMES = FALSE)
}
