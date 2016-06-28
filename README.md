dplyrExtras
==============

Some extra functionality that is not yet included in dplyr:

  - `mutate_rows` mutate selected rows (see https://github.com/hadley/dplyr/issues/425)
    The function was originally called `mutate_if`.
    The original name still exists, but using `mutate_rows` is preferred
    since dplyr 5.0 introduced an own `mutate_if` function with quite
    different behavior.
  - `s_filter`, `s_arrange` ,... allowing string arguments (see https://github.com/hadley/dplyr/issues/333)

To install in R, first install the package devtools from CRAN and then run

```
library(devtools);
install_github(repo="skranz/dplyrExtras")
```
