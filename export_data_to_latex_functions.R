
#' Function that exports a data.frame to a tex file which can be included in a table environment 
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
  if (!is.null(row_names) & length(row_names) != nrow(data)) stop("'row_names' must have the same length as the number of rows in 'data'")
  if (!is.null(italic_rows) & !is.logical(italic_rows) ) stop("'italic_rows' must either be NULL or a logical vector")
  if (!is.null(italic_rows) & length(italic_rows) != nrow(data)) stop("'italic_rows' must have the same length as the number of rows in 'data'")
  if (!is.null(bold_rows) & !is.logical(bold_rows) ) stop("'bold_rows' must either be NULL or a logical vector")
  if (!is.null(bold_rows) & length(bold_rows) != nrow(data)) stop("'bold_rows' must have the same length as the number of rows in 'data'")
  if (!is.null(extra_space) & length(extra_space) > 1 & length(extra_space) != nrow(data)) stop("'extra_space' must be a string of length 1 or 'nrow(data)'")
  # Ensure that round precision is an integer
  if (!is.null(round_precison)) {
    round_precison <- as.integer(round_precison)
  }
  
  
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


#' Function that exports estimated coefficients to LaTeX and adds additional information (e.g. standard errors or t-statistics) in parentheses 
#' below the respective coefficient. Significant coefficients are marked by alphabetic superscripts.
#'
#' @param path path and name of the exported file (must end with '.tex')
#' @param data_coef data frame or matrix containing the coefficients
#' @param data_info data frame or matrix containing the additional information (e.g. standard errors or t-statistics), must have the same dimensions as `data_coef`
#' @param data_sig data frame or matrix containing the p-values, must have the same dimensions as `data_coef`
#' @param sig_levels levels at which significance is reported, default is 0.01, 0.05, and 0.1
#' @param round_precison integer defining the how many decimals are exported 
#' @param row_names optional character vector of row-names, must have the same length as the number of rows in `data_coef`
#'
export_coef_to_latex <- function(path, data_coef, data_info, data_sig, sig_levels = c(0.01, 0.05, 0.1), round_precison = NULL, row_names = NULL) {
  require(dplyr)
  require(tibble)
  # ensure that rounding number is integer
  if (!is.null(round_precison)) {
    round_precison <- as.integer(round_precison)
  } else {
    round_precison <- 2L
  }
  # check that the significance levels are a numeric vector
  if (!is.numeric(sig_levels)) stop("'sig_levels' must be a numeric vector!")
  # ensure that the levels are in the right order
  sig_levels <- sort(sig_levels)
  # define the symbols for significance
  sig_symbols <- letters[seq_along(sig_levels)]
  # ensure all data are matrices
  data_coef <- as.matrix(data_coef)
  data_info <- as.matrix(data_info)
  data_sig <- as.matrix(data_sig)
  # check that the dimensions of the data match:
  if (any(dim(data_coef)!=dim(data_info))) stop("'data_info' must have the same dimension as 'data_coef'")
  if (any(dim(data_coef)!=dim(data_sig))) stop("'data_sig' must have the same dimension as 'data_coef'")
  
  # define dimensions
  n_cols <- ncol(data_coef)
  n_rows <- nrow(data_coef)
  
  # prepare row-names
  if (!is.null(row_names) & length(row_names)==n_rows) {
    row_names <- rep(row_names, each = 2)
    row_names[seq(2, n_rows*2, by=2)] <- ""
  } else if (!is.null(row_names) & length(row_names)!=n_rows) {
    warning("'row_names' must have the same length as 'data_coef' has rows, exporting data without row-names!")
  }
  
  # define copies of the coefficient and info matrices
  data_coef_formatted <- data_coef
  data_info_formatted <- data_info
  data_coef_formatted[,] <- data_info_formatted[,] <- ""
  # iterate over each column 
  for (i in 1:n_cols) {
    coef_i <- data_coef[,i]
    info_i <- data_info[,i]
    sig_i <- data_sig[,i]
    # maximal length of a number in this column
    max_int_length <- max(get_integer_size(coef_i), get_integer_size(info_i))
    # check if there are any negative numbers in the column
    any_neg <- any(coef_i<0, info_i<0)
    # check if there are any significant numbers
    any_sig <- any(sig_i<max(sig_levels))
    # iterate over each element in column i
    for (j in 1:n_rows) {
      # check if jth coefficient of column i is significant
      is_sig <- sig_i[j]<sig_levels
      # check if the coefficient and its information are negative
      is_coef_neg <- coef_i[j]<0
      is_info_neg <- info_i[j]<0
      # get the length of the integer part of the coefficient and its information
      int_length_coef <- get_integer_size(coef_i[j])
      int_length_info <- get_integer_size(info_i[j])
      
      # format the coefficient: round, add significance and add lead and trail phantom characters
      coef_ij <- format(round(coef_i[j], digits=round_precison), nsmall=round_precison)
      lead_phantom <- "("
      trail_phantom <- ")"
      if (any(is_sig)) {
        coef_ij <- paste0(coef_ij, "^{", sig_symbols[sum(is_sig)], "}")
      } else if (all(!is_sig) & any_sig) {
        trail_phantom <- paste0("$^{a}$", trail_phantom)
      }
      if (!is_coef_neg & any_neg) lead_phantom <- paste0(lead_phantom, "$-$")
      if (max_int_length>int_length_coef) lead_phantom <- paste0(lead_phantom, rep("0", max_int_length-int_length_coef))
      # save the formatted coefficient
      data_coef_formatted[j,i] <- paste0("\\leavevmode\\phantom{", lead_phantom, "}$", coef_ij, "$\\phantom{", trail_phantom, "}")
      
      # format the information: round and add lead and trail phantom characters
      info_ij <- format(round(info_i[j], digits=round_precison), nsmall=round_precison)
      lead_phantom <- ""
      trail_phantom <- ""
      if (any_sig) trail_phantom <- paste0("$^{a}$", trail_phantom)
      if (!is_info_neg & any_neg) lead_phantom <- paste0(lead_phantom, "$-$")
      if (max_int_length>int_length_info) lead_phantom <- paste0(lead_phantom, rep("0", max_int_length-int_length_info))
      # save the formatted information
      data_info_formatted[j,i] <- paste0("\\leavevmode\\phantom{", lead_phantom, "}($", info_ij, "$)\\phantom{", trail_phantom, "}")
      
    }
  }
  # temporary column names (needed for the transformation in a data frame)
  colnames(data_coef_formatted) <- colnames(data_info_formatted) <- paste0("V", 1:n_cols)
  # cast the formatted coefficients to a tibble-object and add identification info for sorting when merged with the info data
  data_coef_formatted <- as_tibble(data_coef_formatted) %>% 
    mutate(id = 1:n_rows) %>% 
    mutate(what = "coef")
  # cast the formatted information to a tibble-object and add identification info for sorting when merged with the coef data
  data_info_formatted <- as_tibble(data_info_formatted) %>% 
    mutate(id = 1:n_rows) %>% 
    mutate(what = "info")
  # combine coefficients and information 
  bind_rows(data_coef_formatted, data_info_formatted) %>%
    # arrange such that the information is reported below the corresponding coefficient
    arrange(id, match(what, c("coef", "info"))) %>% 
    # remove identifiers
    select(-id, -what) %>% 
    # if specified, add row names:
    {if (!is.null(row_names)) add_column(., rownames = as.character(row_names), .before = 1) else .} %>%
    {append(list(.), 2:ncol(.))} %>% 
    # between each column, add the separator symbol '&'
    purrr::reduce(.f = function(x,y) add_column(x, tmp = rep("&", nrow(x)), .before = 2*y -2 ) %>% rename(!!as.character(y) := "tmp")) %>%
    # add "new row" Latex-Symbol
    add_column( last_col = "\\\\") %>%
    # export file:
    write.table(file = path ,sep=" ", row.names=FALSE, col.names = FALSE, quote=FALSE)
}


#' Help function that returns the length of the integer part of a number
#'
#' @param n a double
#'
#' @return length of the integer part of the number
#' 
get_integer_size <- function(n) {
  return(nchar(as.character(floor(abs(n)))))
}