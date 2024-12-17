library(usethis)
library(pkgdown)
use_r('search_r_files')
use_addin()
use_readme_rmd()
devtools::build_readme()

usethis::use_package("marker")

use_pkgdown_github_pages()

build_site()
preview_site()
build_reference()

