dplyrExtras
==============

Some extra functionality that is not yet included in dplyr:

  - `mutate_if` mutate selected rows (see https://github.com/hadley/dplyr/issues/425)
  - `s_filter`, `s_arrange` ,... allowing string arguments (see https://github.com/hadley/dplyr/issues/333)

To install in R, first install the package devtools from CRAN and then run

```
library(devtools);
install_github(repo="dplyrExtras", username="skranz")
```
