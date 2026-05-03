# ==========================================
# APP Name: Line Sticker Factory - V9 Mobile Photo Edition
# Author: EBDS RStudio Expert
# Philosophy: 配合終端載具，降維輸出標準 JPG
# Governance: 確保手機相簿 100% 可讀取
# ==========================================

library(shiny)
library(magick)
library(base64enc)
library(zip)

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .btn-file { background-color: #f8f9fa; border: 1px solid #ced4da; }
      .well { background-color: #ffffff; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('download_zip_js', function(message) {
          var a = document.createElement('a');
          a.href = 'data:application/zip;base64,' + message.base64;
          a.download = message.filename;
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
      });
    "))
  ),
  
  titlePanel("Line 貼圖自動化裁切工廠 - 手機照片版"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("main_image", "1. 選擇 AI 生成大圖 (PNG/JPG)", 
                buttonLabel = "選擇檔案...", placeholder = "尚未選取圖片",
                accept = c("image/png", "image/jpeg")),
      numericInput("rows", "矩陣行數 (Rows)", value = 4, min = 1),
      numericInput("cols", "矩陣列數 (Cols)", value = 4, min = 1),
      hr(),
      helpText("提示：系統將產出純白背景的標準 JPG 照片，方便存入手機相簿。"),
      actionButton("process_btn", "開始自動加工", class = "btn-success", style = "width: 100%; height: 50px; font-size: 18px;"),
      br(), br(),
      actionButton("download_zip_btn", "下載 JPG 照片壓縮包", icon = icon("download"), class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      h4("照片預覽 (標準純白背景)"),
      uiOutput("preview_grid")
    )
  )
)

server <- function(input, output, session) {
  
  processed_images <- reactiveVal(list())
  
  observeEvent(input$process_btn, {
    req(input$main_image)
    
    withProgress(message = '初始化數據維度...', value = 0, {
      raw_img <- image_read(input$main_image$datapath)
      img_info <- image_info(raw_img)
      
      single_w <- floor(img_info$width / input$cols)
      single_h <- floor(img_info$height / input$rows)
      
      img_list <- list()
      total <- input$rows * input$cols
      count <- 0
      
      setProgress(message = '執行精算級加工...', value = 0.1)
      
      for (r in 1:input$rows) {
        for (c in 1:input$cols) {
          count <- count + 1
          if(count > total) break
          
          # 1. 精準裁切
          off_x <- floor((c - 1) * single_w)
          off_y <- floor((r - 1) * single_h)
          crop_geom <- paste0(single_w, "x", single_h, "+", off_x, "+", off_y)
          tile <- image_crop(raw_img, crop_geom)
          
          # 2. 安全縮放 (限縮在 350x300 內)
          tile_scaled <- image_scale(tile, "350x300") 
          
          # 3. 擴充為標準尺寸，並強制填入「純白背景 (white)」以符合手機照片格式
          tile_final <- image_extent(tile_scaled, "370x320", gravity = "center", color = "white")
          
          img_list[[count]] <- tile_final
          incProgress(0.9/total)
        }
      }
    })
    processed_images(img_list)
  })
  
  output$preview_grid <- renderUI({
    imgs <- processed_images()
    if(length(imgs) == 0) return(helpText("尚未有處理結果，請點擊 [開始自動加工]"))
    
    fluidRow(
      lapply(1:length(imgs), function(i) {
        tryCatch({
          # 預覽轉換為 JPEG
          raw_bytes <- image_write(imgs[[i]], format = "jpeg")
          base64_str <- base64enc::base64encode(raw_bytes)
          data_uri <- paste0("data:image/jpeg;base64,", base64_str)
          
          column(3, 
                 wellPanel(
                   style = "padding: 5px; margin-bottom: 10px; background: #ffffff;",
                   tags$img(src = data_uri, style = "width: 100%; border: 1px solid #ccc;"),
                   tags$p(style = "text-align: center; font-size: 11px; margin-top:5px;", paste0("照片 ", sprintf("%02d", i)))
                 )
          )
        }, error = function(e) {
          column(3, helpText("渲染失敗"))
        })
      })
    )
  })
  
  observeEvent(input$download_zip_btn, {
    imgs <- processed_images()
    if(length(imgs) == 0) {
      showNotification("請先上傳圖片並點擊【開始自動加工】！", type = "error")
      return()
    }
    
    withProgress(message = '封裝壓縮包中...', value = 0.5, {
      tmp_dir <- tempdir()
      fs <- c()
      
      # 寫入 JPG 檔案
      for(i in 1:length(imgs)) {
        fname <- file.path(tmp_dir, paste0(sprintf("%02d", i), ".jpg"))
        image_write(imgs[[i]], fname, format = "jpeg", quality = 95)
        fs <- c(fs, fname)
      }
      
      zip_path <- file.path(tmp_dir, "Line_Stickers_Photos.zip")
      if(file.exists(zip_path)) file.remove(zip_path) 
      zip::zipr(zip_path, files = fs)
      
      raw_zip <- readBin(zip_path, "raw", file.info(zip_path)$size)
      b64_zip <- base64enc::base64encode(raw_zip)
      
      session$sendCustomMessage("download_zip_js", list(
        base64 = b64_zip,
        filename = paste0("Line_Stickers_Photos_", Sys.Date(), ".zip")
      ))
    })
  })
}

shinyApp(ui, server)