dplyrExtras
==============

Some extra functionality that was mainly helpful for older dplyr versions but is less relevant now:

  - `mutate_rows` mutate selected rows (see https://github.com/hadley/dplyr/issues/425)
  
    Note: dplyr 0.7 introduced the functions `recode` and `case_when` that also allow to
    solve the issues `mutate_rows` adresses in a convenient way. For new projects, I would recommend to use these functions now, rather than `mutate_rows`. 
    
    Note 2: The function was originally called `mutate_if`.
    The original name still exists, but using `mutate_rows` is preferred
    since dplyr 0.5 introduced an own `mutate_if` function with quite
    different behavior.
    
    
  - `s_filter`, `s_arrange` ,... allowing string arguments (see https://github.com/hadley/dplyr/issues/333)

To install in R, first install the package devtools from CRAN and then run

```
library(devtools);
install_github(repo="skranz/dplyrExtras")
```
