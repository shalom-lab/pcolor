#' @title  Save ggplot in (WYSIWYG) Style
#' @description This function run as a Rstudio Addin
#' and helps you save ggplot in RStudio
#' @usage gsave() # Select your ggplot code at first and then run gsave()
#' @import shiny
#' @import miniUI
#' @import rstudioapi
#' @export
gsave <- function(n=NULL) {
  ct<-rstudioapi::getSourceEditorContext()
  #print(ct)
  code<-ct$selection[[1]]$text
  #print(code)
  if(code=='')code<-'ggplot(mtcars,aes(mpg,cyl))+geom_point()'
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Save ggplot",
                           left = miniUI::miniTitleBarCancelButton('cancel','Finish'),
                           right = miniUI::miniTitleBarButton("done", "Save", primary = TRUE)),
    miniUI::miniContentPanel(style="overflow-y:scroll",
      shiny::fillRow(flex=c(1,3),
                     shiny::wellPanel(
                       shiny::tabsetPanel(
                         shiny::tabPanel('基础',
                                         shiny::sliderInput('width','width',500),
                                         shiny::sliderInput('height','height',500),
                                         shiny::radioButtons('units','units',selected = 'px',inline=T,
                                                            choices = c('px','cm','in','mm')),
                                         shiny::sliderInput('dpi','dpi',value = 200,min=50,max=500),
                                         shiny::sliderInput('scale','scale',value = 1,min=0,max=10),
                                         shiny::checkboxInput('limitsize','limitsize',value = F),
                                         shiny::radioButtons('device','device',selected = 'png',inline=T,
                                                            choices=c("jpeg","png","bmp","svg"))
                         ),
                         shiny::tabPanel('字号',
                                         shiny::sliderInput('text','text',value = 10,min=0,max=100),
                                         shiny::sliderInput('axis.title','axis.title',value = 10,min=0,max=100),
                                         shiny::sliderInput('axis.text.x','axis.text.x',value = 10,min=0,max=100),
                                         shiny::sliderInput('axis.text.y','axis.text.y',value = 10,min=0,max=100)
                         )
                       ),
                       shiny::downloadButton('download','Download',class = "btn-primary btn-block")
                     ),
                     shiny::div(
                       style="overflow-x:scroll;height:100%",
                       shiny::imageOutput('plot')
                     )
      )
    )
  )

  server <- function(input, output, session) {
    path<-reactiveVal()
    output$plot <- shiny::renderImage({
      outfile <- tempfile(fileext=paste0('.',input$device))
      p<-eval(parse(text = code),envir = globalenv())+
        ggplot2::theme(text =  ggplot2::element_text(size = input$text),
              axis.title =  ggplot2::element_text(size =input$axis.title),
              axis.text.x =  ggplot2::element_text(size=input$axis.text.x),
              axis.text.y =  ggplot2::element_text(size=input$axis.text.y))
      ggsave(filename = outfile,plot = p,
             width = input$width,
             height= input$height,
             units = input$units,
             dpi = input$dpi,
             scale = input$scale,
             limitsize= input$limitsize,
             device = input$device)
      path(outfile)
      print(path())
      file_ext <- tools::file_ext(outfile)
      content_type <- switch(
        tolower(file_ext),
        jpg = "image/jpeg",
        jpeg = "image/jpeg",
        png = "image/png",
        svg = "image/svg+xml",
        tiff = "image/tiff",
        pdf = "application/pdf",
        "image/jpeg"  # Default content type for unsupported formats
      )
      list(src = outfile,
           contentType = content_type,
           alt = "This is alternate text")

    },deleteFile = F)
    output$download <- shiny::downloadHandler(
      filename = function(){
        gsub('^.*\\\\','',path())
      },
      content = function(file) {
        file.copy(path(), file)
      }
    )
    shiny::observeEvent(input$done,{
      shiny::stopApp()
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }
  shiny::runGadget(ui, server,viewer = browserViewer(),stopOnCancel = FALSE)
}

