# app.R
library(shiny)

# ========== 根据您的模型系数填写 ==========
intercept     <- 1.855695   # 截距
coef_VAS      <- 0.056316   # 术前VAS评分
coef_duration <- -0.184894  # 病程
coef_LMR      <- 0.350492   # LMR
coef_glucose  <- -0.719610  # 空腹葡萄糖
coef_OSA_yes  <- -2.409563  # 是否伴OSA（是 vs 否）

# 吸入变应原分级（以0级为参照）
coef_phad1 <- -0.876086
coef_phad2 <- 1.716486
coef_phad3 <- 2.231243
coef_phad4 <- 1.480145
coef_phad5 <- 0.444274
coef_phad6 <- 0.624587
# ===========================================

# 预测函数：输入变量值，返回概率（百分比）
predict_prob <- function(VAS, duration, LMR, glucose, OSA, phadiatop) {
  logit <- intercept +
    coef_VAS * VAS +
    coef_duration * duration +
    coef_LMR * LMR +
    coef_glucose * glucose +
    ifelse(OSA == "是", coef_OSA_yes, 0)
  
  # 吸入变应原分级哑变量
  if(phadiatop == 1) logit <- logit + coef_phad1
  if(phadiatop == 2) logit <- logit + coef_phad2
  if(phadiatop == 3) logit <- logit + coef_phad3
  if(phadiatop == 4) logit <- logit + coef_phad4
  if(phadiatop == 5) logit <- logit + coef_phad5
  if(phadiatop == 6) logit <- logit + coef_phad6
  
  prob <- 1 / (1 + exp(-logit))
  return(round(prob * 100, 1))  # 返回百分比，保留1位小数
}

# UI界面
ui <- fluidPage(
  titlePanel("中-重度持续性过敏性鼻炎神经阻断术疗效预测计算器"),
  sidebarLayout(
    sidebarPanel(
      numericInput("VAS", "术前VAS评分 (0-60)", value = 30, min = 0, max = 60),
      numericInput("duration", "病程 (年)", value = 5, min = 0, max = 50),
      numericInput("LMR", "淋巴细胞与单核细胞比率 (LMR)", value = 4.5, min = 0, max = 20, step = 0.1),
      numericInput("glucose", "空腹血糖 (mmol/L)", value = 5.0, min = 2, max = 15, step = 0.1),
      selectInput("OSA", "是否伴OSA", choices = c("否", "是")),
      selectInput("phadiatop", "吸入变应原筛查分级", 
                  choices = c("0级" = 0, "1级" = 1, "2级" = 2, "3级" = 3,
                              "4级" = 4, "5级" = 5, "6级" = 6), selected = 0),
      actionButton("predict", "预测", class = "btn-primary")
    ),
    mainPanel(
      h3("预测结果"),
      verbatimTextOutput("result"),
      br(),
      h4("⚠️ 使用说明"),
      p("本模型基于温州医科大学附属第一医院耳鼻咽喉科研究数据开发，仅适用于接受过敏性鼻炎神经阻断术（鼻后神经联合筛前神经切断术）的患者。"),
      p("预测结果仅供临床参考，不作为决策唯一依据。"),
      p("模型开发版本：v1.0（最后更新：2026年4月）")
    )
  )
)

# Server端
server <- function(input, output) {
  observeEvent(input$predict, {
    prob <- predict_prob(
      VAS       = input$VAS,
      duration  = input$duration,
      LMR       = input$LMR,
      glucose   = input$glucose,
      OSA       = input$OSA,
      phadiatop = as.numeric(input$phadiatop)
    )
    output$result <- renderText({
      paste0("术后显效概率: ", prob, "%")
    })
  })
}

shinyApp(ui = ui, server = server)