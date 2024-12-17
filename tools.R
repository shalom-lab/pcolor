library(usethis)
library(pkgdown)
use_r('search_r_files')
use_addin()
use_readme_rmd()
devtools::build_readme()
use_pkgdown_github_pages()

devtools::document()
usethis::use_package("marker")


clean_site()
build_site()
preview_site()
build_reference()

