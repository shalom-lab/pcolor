#' @title  Pick Color Freely in Rstudio
#' @description This function takes a numeric value or run as a Rstudio Addin
#' and helps your pick color freely in RStudio
#' @param n the length of the colors need to generate
#' @usage pcolor()
#' pcolor(5)
#' @import shiny
#' @import miniUI
#' @import colourpicker
#' @import rstudioapi
#' @export
pcolor <- function(n=NULL) {
  ct<-rstudioapi::getSourceEditorContext()
  #print(ct)
  text1<-ct$selection[[1]]$text
  #text1<-"'red','blue'"
  text2<-gsub("['\"]","",text1)

  #print(text2)
  vec<-strsplit(text2,',')[[1]]

  colors <- c("#7BC4A4", "#F7A7A8", "#B19CD9", "#FF7F50", "#7FBFCF",
                       "#98DDCA", "#36454F", "#FFC0CB", "#F5D76E", "#87CEEB")

  if(text1=='') vec=colors
  if(!is.null(n)){
    stopifnot("n must be integer"=is.integer(as.integer(n)),
              "n must be greater than zero"=as.integer(n)>0)
    vec = colors[1:n %% 11]
  }
  #print(vec)
  range<-ct$selection[[1]]$range
  #print(text)
  #print(range)
  ui <- miniUI::miniPage(
    shiny::tags$head(
      # Note the wrapping of the string in HTML()
      shiny::tags$style(shiny::HTML("
      div.shiny-input-container:not(.shiny-input-container-inline) {
        width:100%
      }
      "))
    ),
    miniUI::gadgetTitleBar("Pick Color in RStudio",
                           left = miniUI::miniTitleBarCancelButton('cancel','Finish'),
                           right = miniUI::miniTitleBarButton("done", "Pick", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::div(
        shiny::uiOutput('ui')
      )
    )
  )

  server <- function(input, output, session) {
    output$ui <- shiny::renderUI({
      shiny::tagList(
        lapply(seq_along(vec), function(i) {
          colourpicker::colourInput(
            paste0("col_",i), NULL, vec[[i]],
            allowTransparent = TRUE,
            closeOnClick = TRUE)
        })
      )
    })
    shiny::observeEvent(input$done,{
      n<-length(vec)
      name<-paste0('col_',1:n)

      li<-shiny::reactiveValuesToList(input)[name]
      #print(li)
      s1<-paste0('"',unlist(li),'"')
      s2<-paste0(s1,collapse = ',')
      rstudioapi::insertText(location=range,text=s2,id=ct$id)
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server,stopOnCancel = FALSE)
}
