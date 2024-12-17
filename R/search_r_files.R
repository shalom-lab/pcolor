#' @title  Searching in Multiple .R Files
#' @description This function run as a function or Rstudio Addin.
#' This function searches for a specified string in all `.R` files within the current working directory and its subdirectories.
#' @usage search_r_files() search strings in Current Working Directory
#' @import shiny
#' @import miniUI
#' @import marker
#' @export
search_r_files <- function() {
  # 加载必要的库
  # library(miniUI)
  # library(shiny)
  # library(marker)

  # Shiny UI 和 Server 定义
  ui <- miniUI::miniPage(
    marker::useMarker(),
    gadgetTitleBar(base::sprintf("Search .R Files in %s",base::getwd())),
    miniUI::miniContentPanel(
      shiny::textInput("search_string",label="Enter search string(supports regex)",value = "",placeholder = ""),
      shiny::checkboxInput("included", label = "Include R/qmd?", value = FALSE),
      shiny::verbatimTextOutput("results")
    )
  )

  server <- function(input, output, session) {
    marker <- marker::marker$new("#results")
    search_results <- shiny::reactiveVal("")

    observe({
      input$search_string
      input$included
      req(input$search_string)

      # 获取当前目录
      current_dir <- base::getwd()

      # 查找所有 .R 文件
      pattern=ifelse(input$included,"\\.(R|qmd|Rmd)$","\\.R$")
      r_files <- list.files(path = current_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)

      # 搜索指定字符串并记录结果
      results <- lapply(r_files, function(file) {
        lines <- readLines(file, warn = FALSE)
        matched <- grep(input$search_string, lines, value = TRUE)
        if (length(matched) > 0) {
          paste0("👉 File: ", file, "\n", paste0("    Line ", which(grepl(input$search_string, lines)), ": ", matched, collapse = "\n"))
        } else {
          NULL
        }
      })

      # 汇总结果
      results <- base::Filter(base::Negate(is.null), results)
      if (length(results) == 0) {
        search_results("No matches found.")
      } else {
        search_results(paste(results, collapse = "\n\n"))
      }
      #marker
      marker$
        unmark()$ # unmark all before we mark
        mark(input$search_text) # highlight text
    })
    output$results <- shiny::renderText({
      search_results()
    })
    observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  # 运行 Gadget
  shiny::runGadget(ui, server)
}

