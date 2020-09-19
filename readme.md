# Export data to LaTeX
## Requirments
The packages required for these functions are `dplyr`, `tibble`, `purrr`, `stringr`, `magrittr`.

## Usage
Given a data frame (ideally a `tibble`-object), the function exports the data to a tex file data can be included in a 
LaTeX table environment.   
```R
data <- tibble(a = 1:5, b = 6:10)
data %>% export_data_to_latex(path="data_for_latex.tex")
```
The exported data can then be included in LaTeX table environment:
```latex
\begin{table}
    \begin{tabular}
        a & b \\
        \cline
        \input{"data_for_latex.tex"}
    \end{tabular}
\end{table}
```

## Options
The following options are available:
* `round_precison`: integer number of decimal places for numeric columns (default `NULL`)
* `row_names`: character array of row-names (default `NULL`)
* `italic_rows`: boolean array indicating which rows should be exported italic faced (default `NULL`)
* `bold_rows`: boolean array indicating which rows should be exported bold faced (default `NULL`)
* `extra_space`: character array of length 1 or `nrow(data)` that is added after the LaTeX's new-line command (default `NULL`)

Note that, when round precision is an integer number, all numbers are exported with the same decimal precision, e.g.
"5" if exported as "5.00" if `round_precison=2L`.