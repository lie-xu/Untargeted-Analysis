install.packages("shiny")
install.packages("plotly")
install.packages("ggplot2")
install.packages("DT")
install.packages("pheatmap")
install.packages("factoextra")
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(pheatmap)
library(factoextra)

# 读取数据（假设数据已保存为data.csv）
data <- read.csv("D:\\20250227非靶分析完数据.csv", check.names = FALSE)

# 预处理数据
numeric_cols <- grep("^X\\d+[AFP]", names(data), value = TRUE)
metadata_cols <- c("Compound", "Neutral.mass..Da.", "m.z", "Retention.time..min.", 
                   "Chromatographic.peak.width..min.", "Identifications", 
                   "Anova..p.", "q.Value", "Max.Fold.Change")

# UI设计
ui <- fluidPage(
  titlePanel("质谱数据分析平台"),
  sidebarLayout(
    sidebarPanel(
      selectInput("compound", "选择化合物", choices = unique(data$Compound)),
      sliderInput("mz_range", "m/z范围", 
                  min = floor(min(data$m.z, na.rm = TRUE)),
                  max = ceiling(max(data$m.z, na.rm = TRUE)),
                  value = c(300, 900)),
      sliderInput("rt_range", "保留时间范围 (min)",
                  min = floor(min(data$Retention.time..min., na.rm = TRUE)),
                  max = ceiling(max(data$Retention.time..min., na.rm = TRUE)),
                  value = c(5, 15)),
      checkboxGroupInput("samples", "选择样本组",
                         choices = numeric_cols,
                         selected = numeric_cols[1:6]),
      numericInput("pval_cutoff", "ANOVA p值阈值", value = 0.05, step = 0.01),
      actionButton("update", "更新分析")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("总离子流图", plotlyOutput("tic_plot")),
        tabPanel("统计结果", DTOutput("stats_table"))
      )
    )
  )
)

# Server逻辑
server <- function(input, output) {
  
  filtered_data <- eventReactive(input$update, {
    df <- data %>%
      filter(m.z >= input$mz_range[1],
             m.z <= input$mz_range[2],
             Retention.time..min. >= input$rt_range[1],
             Retention.time..min. <= input$rt_range[2],
             Anova..p. <= input$pval_cutoff)
    
    # 处理技术重复（假设.1结尾为技术重复）
    tech_reps <- grep("\\.1$", numeric_cols, value = TRUE)
    if(length(tech_reps) > 0){
      orig_cols <- gsub("\\.1$", "", tech_reps)
      df[orig_cols] <- rowMeans(cbind(df[orig_cols], df[tech_reps]), na.rm = TRUE)
    }
    
    df
  })
  
  # 总离子流图
  output$tic_plot <- renderPlotly({
    df <- filtered_data()
    ggplotly(
      ggplot(df, aes(x = Retention.time..min., y = Maximum.Abundance)) +
        geom_line(color = "steelblue") +
        labs(title = "总离子流图", x = "保留时间 (min)", y = "丰度") +
        theme_minimal()
    )
  })
  
  # 统计结果表
  output$stats_table <- renderDT({
    df <- filtered_data() %>%
      select(Compound, Neutral.mass..Da., m.z, Retention.time..min.,
             Anova..p., q.Value, Max.Fold.Change)
    
    datatable(df,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatSignif(columns = c("Neutral.mass..Da.", "m.z"), digits = 4) %>%
      formatRound(columns = c("Anova..p.", "q.Value"), digits = 5)
  })
}

# 运行应用
shinyApp(ui = ui, server = server)

