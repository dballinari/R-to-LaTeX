# Export data to LaTeX
## Description
This repository provides two main functions that help exporting data from R to LaTeX. 
The function `export_data_to_latex` simply exports a data frame to LaTeX, and formats 
the numeric columns such that the numbers are aligned by the decimal point. 
The function `export_coef_to_latex` exports estimated coefficient by reporting additional 
information in parentheses below the respective estimate and marking significant results 
by alphabetic superscripts. All numbers in the exported coefficient table are again 
aligned at the decimal point.

## Requirments
The packages required for these functions are `dplyr`, `tibble`, `purrr`, `stringr`, `magrittr`.

## Usage `export_data_to_latex`
Given a data frame (ideally a `tibble`-object), the function exports the data to a tex file data can be included in a 
LaTeX table environment.   
```R
data <- tibble(a = 1:5, b = 6:10)
data %>% export_data_to_latex(path="data_for_latex.tex")
```
The exported data can then be included in LaTeX table environment:
```latex
\begin{table}
    \begin{tabular}{cc}
        a & b \\
        \hline
        \input{"data_for_latex.tex"}
    \end{tabular}
\end{table}
```
The following options are available:
* `round_precison`: integer number of decimal places for numeric columns (default `NULL`)
* `row_names`: character array of row-names (default `NULL`)
* `italic_rows`: boolean array indicating which rows should be exported italic faced (default `NULL`)
* `bold_rows`: boolean array indicating which rows should be exported bold faced (default `NULL`)
* `extra_space`: character array of length 1 or `nrow(data)` that is added after the LaTeX's new-line command (default `NULL`)

Note that, when round precision is an integer number, all numbers are exported with the same decimal precision, e.g.
"5" if exported as "5.00" if `round_precison=2L`.

## Usage `export_coef_to_latex`
Given a data frames or matrices of coefficients, information (e.g. standard errors), and p-values (all of the same dimension), 
the function exports the formatted coefficients to a tex file data can be included in a 
LaTeX table environment.
```R
estimated_coef <- tibble(a = c(1.367, 0.109, -3.4), b = c(22.113, -12.4, 5.34))
standard_errors <- tibble(a = c(2.786, 0.542, -1.25), b = c(8.6523, -8.745, 2.94))
pvalues <- tibble(a = c(0.6237, 0.8406, 0.00653), b = c(0.0106, 0.1562, 0.06932))
export_coef_to_latex("coef_for_latex.tex", data_coef = estimated_coef, data_info = standard_errors, data_sig = pvalues)
```
The exported data can then be included in LaTeX table environment:
```latex
\begin{table}
    \begin{tabular}{cc}
        a & b \\
        \hline
        \input{"coef_for_latex.tex"}
    \end{tabular}
\end{table}
```
The following options are available:
* `round_precison`: integer number of decimal places for numeric columns (default `NULL`)
* `sig_levels`: numeric array of levels at which the significance is reported (default `0.01, 0.05, 0.1`)
* `row_names`: character array of row-names with the same length as the number of rows of `data_coef` (default `NULL`)


