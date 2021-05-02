
#' Function that exports a data.frame to a tex file which can be included in a table enviroment 
#'
#' @param data a data.frame to be exported as tex-document
#' @param path path and name of the exported file (must end with '.tex')
#' @param round_precison for numerical columns, the number of decimal digits to be exported
#' @param row_names row names to be exported, can be NULL
#' @param italic_rows logical vector defining which rows should be made italic, can be NULL
#' @param bold_rows logical vector defining which rows should be made bold, can be NULL
#' @param extra_space character expression that will be added after the newline-command "\\"
#'
#' @return does not return any object
#' 
export_data_to_latex <- function(data, path, round_precison = NULL, row_names = NULL, italic_rows = NULL, bold_rows = NULL, extra_space = NULL) {
  require(dplyr)
  require(tibble)
  # Make checks of the inputs:
  if (!is.integer(round_precison) & !is.null(round_precison)) stop("'round_precison' must be an integer or NULL")
  if (!is.null(row_names) & length(row_names) != nrow(data)) stop("'row_names' must have the same length as the number of rows in 'data'")
  if (!is.null(italic_rows) & !is.logical(italic_rows) ) stop("'italic_rows' must either be NULL or a logical vector")
  if (!is.null(italic_rows) & length(italic_rows) != nrow(data)) stop("'italic_rows' must have the same length as the number of rows in 'data'")
  if (!is.null(bold_rows) & !is.logical(bold_rows) ) stop("'bold_rows' must either be NULL or a logical vector")
  if (!is.null(bold_rows) & length(bold_rows) != nrow(data)) stop("'bold_rows' must have the same length as the number of rows in 'data'")
  if (!is.null(extra_space) & length(extra_space) > 1 & length(extra_space) != nrow(data)) stop("'extra_space' must be a string of length 1 or 'nrow(data)'")
  
  if (is.null(extra_space)) {
    new_line_str <- "\\\\"
  } else {
    new_line_str <- paste0("\\\\", extra_space)
  }
  
  data %>% 
    # Ensure that the data is in a tibble-object:
    as_tibble() %>%
    # Round numeric columns:
    mutate_if(is.numeric, ~format_numeric_column(., nsmall = round_precison)) %>%
    # Add math enviroment for numeric coulmns (note that this columns are then characters, and no longer numeric)
    mutate_if(is.numeric, ~paste0("$", ., "$") ) %>% 
    # Add escapes to latex symbols
    mutate_all(~stringr::str_replace_all(string = ., pattern = "([%&]{1})", replacement = "\\\\\\1")) %>%
    # If specified, add row names:
    {if (!is.null(row_names)) add_column(., rownames = as.character(row_names), .before = 1) else .} %>%
    # Make specific rows italic:
    {if (!is.null(italic_rows)) mutate_all(., ~ifelse(italic_rows, paste0("\\textit{",.,"}"), .)) else . } %>%
    # Make specific rows bold:
    {if (!is.null(bold_rows)) mutate_all(., ~ifelse(bold_rows, paste0("\\textit{",.,"}"), .)) else . } %>%
    # Put data into a list, togheter with a column count (we will use this to add the column separators as defined by latex)
    {append(list(.), 2:ncol(.))} %>% 
    # Between each coulmn, add the separator symbol '&'
    purrr::reduce(.f = function(x,y) add_column(x, tmp = rep("&", nrow(x)), .before = 2*y -2 ) %>% rename(!!as.character(y) := "tmp")) %>%
    # Add "new row" Latex-Symbol
    add_column( last_col = new_line_str) %>%
    # Export file:
    write.table(file = path ,sep=" ", row.names=FALSE, col.names = FALSE, quote=FALSE)
}


#' Function that formats a numeric column for the export to latex. Besides rounding the 
#' numbers and converting them to characters, the function adds phantom spaces such that
#' the numbers are aligned when displayed in Latex, besides possible "-" signs
#'
#' @param col a numeric vector
#' @param nsmall the number of decimals to be exported or NULL (the default)
#'
#' @return the formatted column, i.e. when 'nsmall' is not NULL, a character vector
#'
format_numeric_column <- function(col, nsmall = NULL, ...) {
  require(magrittr)
  # If the rounding argument is NULL, the column is returned in its current state
  if (is.null(nsmall)) return(col)
  # Help function that returns the length of a numbers integer
  get_integer_size <- function(n) {
    return(nchar(as.character(floor(abs(n)))))
  }
  # A help function that formats the numbers in a vector such that the decimal points are aligned
  add_phantom <- function(x, nsmall, neg, max_int, ...) {
    # check how long is the integer part of the number
    size_int <- get_integer_size(x)
    # check if the number is negative, i.e. has a minus sign
    is_neg <- x<0
    # format the number such that it has exactly 'nsmall' decimal numbers
    st <- format(round(x, digits = nsmall), nsmall=nsmall, ...)
    # put the number is math-mode
    st <- paste0("$", st,"$")
    # if the length of the number is the same as the maximal length of a number in the vector,
    # and if the number is either negative or if there are no negative numbers in the vector, 
    # we are done and can return the formatted number
    if (is_neg==neg & size_int == max_int) {
      return(st)
    } 
    # otherwise we have to add some phantom characters
    phantom_string <- ""
    # add phantom leading zeros such that the number length matches that of the longest number
    if (size_int < max_int) {
      phantom_string <- paste0(paste(rep("0", max_int-size_int), collapse = ""), phantom_string)
    }
    # add phantom minus sign if any number is negative in the vector
    if (is_neg!=neg) {
      phantom_string <- paste0("$-$", phantom_string)
    }
    st <- paste0("\\leavevmode\\phantom{", phantom_string, "}", st)
    return(st)
  }
  # check if there are any negative numbers
  neg_numbers <- any(col<0)
  # check max size of numbers (without decimals)
  max_int <- max(sapply(col, get_integer_size))
  
  # Initialize the vector of formatted numbers as a character vector
  col_formated <- character(length=length(col))
  # Format all numbers that are not NAs (NAs are simply left as empty characters)
  col_formated[!is.na(col)] <- col[!is.na(col)] %>% 
    # format the numbers and add phantom characters such that all the decimal points are 
    # aligned
    sapply(FUN = function(x) add_phantom(x, nsmall, neg_numbers, max_int, ...), USE.NAMES = FALSE)
  
  return(col_formated)
}
