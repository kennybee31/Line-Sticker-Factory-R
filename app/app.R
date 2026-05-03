# ==========================================
# APP Name: Line Sticker Factory - V10 Dual-Track Edition
# Author: EBDS RStudio Expert
# Philosophy: "上善若水" (Adaptable to any container - Web or Mobile)
# Governance: ISO 42001 Robustness (Dual Pipeline Integration)
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
      .radio-inline { margin-bottom: 10px; }
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
  
  titlePanel("Line 貼圖自動化裁切工廠 - 跨載具雙軌版"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("main_image", "1. 選擇 AI 生成大圖 (PNG/JPG)", 
                buttonLabel = "選擇檔案...", placeholder = "尚未選取圖片",
                accept = c("image/png", "image/jpeg")),
      
      # 【V10 核心升級】雙軌輸出切換開關
      radioButtons("output_format", "2. 選擇輸出載具模式：",
                   choices = c("💻 電腦網頁版直通 (透明背景 PNG)" = "png",
                               "📱 手機拍貼 APP 專用 (純白背景 JPG)" = "jpg"),
                   selected = "png"),
      
      numericInput("rows", "3. 矩陣行數 (Rows)", value = 4, min = 1),
      numericInput("cols", "4. 矩陣列數 (Cols)", value = 4, min = 1),
      hr(),
      
      helpText("提示：系統將根據您選擇的載具，自動套用最佳化的去背與底色參數。"),
      actionButton("process_btn", "開始自動加工", class = "btn-success", style = "width: 100%; height: 50px; font-size: 18px;"),
      br(), br(),
      
      actionButton("download_zip_btn", "下載合規壓縮包", icon = icon("download"), class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      h4("影像預覽儀表板"),
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
      raw_img <- image_convert(raw_img, format = "png") 
      
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
          
          # 1. 矩陣裁切
          off_x <- floor((c - 1) * single_w)
          off_y <- floor((r - 1) * single_h)
          crop_geom <- paste0(single_w, "x", single_h, "+", off_x, "+", off_y)
          tile <- image_crop(raw_img, crop_geom)
          
          # 2. 保守去背 (容差 5%)
          tile_bg_removed <- image_transparent(tile, "white", fuzz = 5)
          
          # 3. 安全縮放
          tile_scaled <- image_scale(tile_bg_removed, "350x300") 
          
          # 4. 【雙軌邏輯分流】依據載具選擇底色
          bg_color <- ifelse(input$output_format == "png", "none", "white")
          tile_final <- image_extent(tile_scaled, "370x320", gravity = "center", color = bg_color)
          
          img_list[[count]] <- tile_final
          incProgress(0.9/total)
        }
      }
    })
    processed_images(img_list)
  })
  
  output$preview_grid <- renderUI({
    imgs <- processed_images()
    if(length(imgs) == 0) return(helpText("尚未有處理結果，請先點擊 [開始自動加工]"))
    
    fluidRow(
      lapply(1:length(imgs), function(i) {
        tryCatch({
          # 依據格式動態渲染
          fmt <- ifelse(input$output_format == "png", "png", "jpeg")
          mime <- ifelse(input$output_format == "png", "image/png", "image/jpeg")
          
          raw_bytes <- image_write(imgs[[i]], format = fmt)
          base64_str <- base64enc::base64encode(raw_bytes)
          data_uri <- paste0("data:", mime, ";base64,", base64_str)
          
          # 若為 PNG 則顯示棋盤格背景以凸顯透明度，JPG 則顯示純白
          bg_style <- ifelse(input$output_format == "png", 
                             "background-image: linear-gradient(45deg, #f0f0f0 25%, transparent 25%), linear-gradient(-45deg, #f0f0f0 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #f0f0f0 75%), linear-gradient(-45deg, transparent 75%, #f0f0f0 75%); background-size: 20px 20px; background-position: 0 0, 0 10px, 10px -10px, -10px 0px;", 
                             "background-color: #ffffff;")
          
          column(3, 
                 wellPanel(
                   style = paste0("padding: 5px; margin-bottom: 10px; border: 1px solid #ddd; ", bg_style),
                   tags$img(src = data_uri, style = "width: 100%;"),
                   tags$p(style = "text-align: center; font-size: 11px; margin-top:5px; background: rgba(255,255,255,0.8);", paste0("貼圖 ", sprintf("%02d", i)))
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
      
      ext <- input$output_format
      fmt <- ifelse(ext == "png", "png", "jpeg")
      
      for(i in 1:length(imgs)) {
        fname <- file.path(tmp_dir, paste0(sprintf("%02d", i), ".", ext))
        image_write(imgs[[i]], fname, format = fmt, quality = 95)
        fs <- c(fs, fname)
      }
      
      main_p <- file.path(tmp_dir, paste0("main.", ext))
      image_write(image_resize(imgs[[1]], "240x240"), main_p, format = fmt)
      tab_p <- file.path(tmp_dir, paste0("tab.", ext))
      image_write(image_resize(imgs[[1]], "96x74"), tab_p, format = fmt)
      
      fs <- c(fs, main_p, tab_p)
      
      zip_path <- file.path(tmp_dir, "Line_Stickers.zip")
      if(file.exists(zip_path)) file.remove(zip_path) 
      zip::zipr(zip_path, files = fs)
      
      raw_zip <- readBin(zip_path, "raw", file.info(zip_path)$size)
      b64_zip <- base64enc::base64encode(raw_zip)
      
      session$sendCustomMessage("download_zip_js", list(
        base64 = b64_zip,
        filename = paste0("Line_Stickers_", toupper(ext), "_", Sys.Date(), ".zip")
      ))
    })
  })
}

shinyApp(ui, server)