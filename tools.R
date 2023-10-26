library(usethis)
library(pkgdown)
use_r('gsave')
use_addin()
use_readme_rmd()
devtools::build_readme()

use_pkgdown_github_pages()

build_site()
preview_site()

