# ==========================================
# APP Name: Line Sticker Factory - V8 Master Edition
# Author: EBDS RStudio Expert
# Philosophy: "執大象，天下往" (Hold the core, and all will follow)
# Governance: ISO 42001 Robustness (Full Pipeline Integration)
# ==========================================

library(shiny)
library(magick)
library(base64enc)
library(zip)

# 將上傳限制提升至 30MB，確保高解析 AI 圖片能順流而下
options(shiny.maxRequestSize = 30 * 1024^2)

ui <- fluidPage(
  # 【前端水管】植入 JavaScript，攔截並直接下載記憶體中的 Base64 數據
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
  
  titlePanel("Line 貼圖自動化裁切工廠 - 雲端大一統版"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("main_image", "1. 選擇 AI 生成大圖 (PNG/JPG)", 
                buttonLabel = "選擇檔案...", placeholder = "尚未選取圖片",
                accept = c("image/png", "image/jpeg")),
      numericInput("rows", "矩陣行數 (Rows)", value = 4, min = 1),
      numericInput("cols", "矩陣列數 (Cols)", value = 4, min = 1),
      hr(),
      helpText("提示：系統將自動執行一鍵去背、裁切與 10px 留白對齊。"),
      actionButton("process_btn", "開始自動加工", class = "btn-success", style = "width: 100%; height: 50px; font-size: 18px;"),
      br(), br(),
      
      # 改為一般的 actionButton，觸發前端 JS 下載
      actionButton("download_zip_btn", "下載 Line 合規壓縮包", icon = icon("download"), class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      h4("貼圖預覽 (已去背並預留 10px 邊距)"),
      uiOutput("preview_grid")
    )
  )
)

server <- function(input, output, session) {
  
  processed_images <- reactiveVal(list())
  
  # ==========================================
  # 模組 1：影像加工引擎 (之前不小心被您刪除的 V6 核心)
  # ==========================================
  observeEvent(input$process_btn, {
    req(input$main_image)
    
    withProgress(message = '初始化數據維度...', value = 0, {
      # 數據正規化：強制賦予 Alpha 透明通道
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
          
          # 1. 精準裁切
          off_x <- floor((c - 1) * single_w)
          off_y <- floor((r - 1) * single_h)
          crop_geom <- paste0(single_w, "x", single_h, "+", off_x, "+", off_y)
          tile <- image_crop(raw_img, crop_geom)
          
          # 2. 保守去背 (容差 5%)
          tile_bg_removed <- image_transparent(tile, "white", fuzz = 5)
          
          # 3. 安全縮放
          tile_scaled <- image_scale(tile_bg_removed, "350x300") 
          
          # 4. 原生擴充畫布 (直接將圖片外擴至 370x320，確保穩定)
          tile_final <- image_extent(tile_scaled, "370x320", gravity = "center", color = "none")
          
          img_list[[count]] <- tile_final
          incProgress(0.9/total)
        }
      }
    })
    processed_images(img_list)
  })
  
  # ==========================================
  # 模組 2：預覽渲染引擎 (之前不小心被您刪除的記憶體渲染)
  # ==========================================
  output$preview_grid <- renderUI({
    imgs <- processed_images()
    if(length(imgs) == 0) return(helpText("尚未有處理結果，請點擊 [開始自動加工]"))
    
    fluidRow(
      lapply(1:length(imgs), function(i) {
        tryCatch({
          # 記憶體內流動，避免寫入硬碟
          raw_bytes <- image_write(imgs[[i]], format = "png")
          base64_str <- base64enc::base64encode(raw_bytes)
          data_uri <- paste0("data:image/png;base64,", base64_str)
          
          column(3, 
                 wellPanel(
                   style = "padding: 5px; margin-bottom: 10px; background: #ffffff;",
                   tags$img(src = data_uri, style = "width: 100%; border: 1px solid #eee; background-image: linear-gradient(45deg, #f0f0f0 25%, transparent 25%), linear-gradient(-45deg, #f0f0f0 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #f0f0f0 75%), linear-gradient(-45deg, transparent 75%, #f0f0f0 75%); background-size: 20px 20px; background-position: 0 0, 0 10px, 10px -10px, -10px 0px;"),
                   tags$p(style = "text-align: center; font-size: 11px; margin-top:5px;", paste0("貼圖 ", sprintf("%02d", i)))
                 )
          )
        }, error = function(e) {
          column(3, helpText("渲染失敗"))
        })
      })
    )
  })
  
  # ==========================================
  # 模組 3：雲端下載引擎 (V7 最新加入的無伺服器下載)
  # ==========================================
  observeEvent(input$download_zip_btn, {
    imgs <- processed_images()
    if(length(imgs) == 0) {
      showNotification("請先上傳圖片並點擊【開始自動加工】！", type = "error")
      return()
    }
    
    withProgress(message = '封裝壓縮包中...', value = 0.5, {
      tmp_dir <- tempdir()
      fs <- c()
      
      for(i in 1:length(imgs)) {
        fname <- file.path(tmp_dir, paste0(sprintf("%02d", i), ".png"))
        image_write(imgs[[i]], fname, format = "png", quality = 80)
        fs <- c(fs, fname)
      }
      
      main_p <- file.path(tmp_dir, "main.png")
      image_write(image_resize(imgs[[1]], "240x240"), main_p)
      tab_p <- file.path(tmp_dir, "tab.png")
      image_write(image_resize(imgs[[1]], "96x74"), tab_p)
      
      fs <- c(fs, main_p, tab_p)
      
      zip_path <- file.path(tmp_dir, "Line_Stickers.zip")
      if(file.exists(zip_path)) file.remove(zip_path) 
      zip::zipr(zip_path, files = fs)
      
      # 讀取 ZIP 為二進制原始碼，送給前端 JS
      raw_zip <- readBin(zip_path, "raw", file.info(zip_path)$size)
      b64_zip <- base64enc::base64encode(raw_zip)
      
      session$sendCustomMessage("download_zip_js", list(
        base64 = b64_zip,
        filename = paste0("Line_Stickers_Ready_", Sys.Date(), ".zip")
      ))
    })
  })
}

shinyApp(ui, server)