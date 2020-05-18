dplyrExtras
==============

Some extra functionality that was mainly helpful for older dplyr versions can still be useful but is somewhat less relevant now:

  - `mutate_rows` mutate selected rows (see https://github.com/hadley/dplyr/issues/425)
  
    Note 1: The function was originally called `mutate_if`.
    It was renamed to `mutate_rows` since dplyr 0.5 introduced an own `mutate_if` function with quite
    different behavior.

  - `s_filter`, `s_arrange` ,... allowing string arguments (see https://github.com/hadley/dplyr/issues/333)

To install in R, first install the package devtools from CRAN and then run

```
library(devtools);
install_github(repo="skranz/dplyrExtras")
```
