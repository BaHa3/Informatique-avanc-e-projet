library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(d3r)
library(DescTools)
library(corrplot)
library(ggpubr)
library(sunburstR)
library(FactoMineR)
library(factoextra)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("View data set", tabName = "data_set"),
    menuItem("Exploratory Data Analysis", tabName = "EDA",icon=icon("area-chart "),
             menuSubItem("Univariate Analysis",tabName = "UA"),
             menuSubItem("multivariate analysis",tabName="MA")),
    menuItem("Multiple Correspondence Analysis",tabName = "MCA"),
    menuItem("Predictive analysis",tabName = "PA",
             menuSubItem("Random forest",tabName = "RF"),
             menuSubItem("Artificial neural network",tabName = "ANN"),
             menuSubItem("Decision tree",tabName = "CART"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "data_set",
            fluidRow(box(width = 12,title="Data set",DT::dataTableOutput("filetable")))
    ),
    tabItem(tabName = "UA",
            fluidRow(
              
              tabBox(side = "right",width=12,
                     tabPanel("Categorical variables",
                              selectInput("varCat", h4("Select a variable :"), 
                                          choices = list("Gender" = "sex","Urban or Rural"="address", "family size" = "famsize",
                                                         "parent's cohabitation status " = "Pstatus","Mother's education"="Medu","Mother's job"="Mjob",
                                                         "Why did you choose this school"="reason","extra educational support"="schoolsup",
                                                         "family educational support"="famsup","extra paid classes"="paid",
                                                         "extra-curricular activities"="activities","attended nursery school"="nursery",
                                                         "wants to take higher education"="higher","internet access"="internet",
                                                         "romantic relationship"="romantic","quality of family relationships"="famrel",
                                                         "free time after school"="freetime","going out with friends"="goout",
                                                         "workday alcohol consumption"="Dalc","weekend alcohol consumption"="Walc",
                                                         "current health status"="health","Target variable"="Class"), selected = "sex"),
                              br(),
                              tabBox(title = "",width = 12,
                                     tabPanel(title="summary",verbatimTextOutput("desc1")),
                                     tabPanel(title="Barplot",plotlyOutput(outputId="barplotUA"))
                              )
                              
                              
                     ),
                     tabPanel("Numerical variables ",
                              fluidRow(
                                selectInput("varNum", h3("Select a variable :"), 
                                            choices = list("Age"="age","weekly study time"="studytime",
                                                           "number of past class failures"="failures"," number of school absences(in days)"="absences",
                                                           "Grade in the subject"="G3"), selected = "age"),
                                br(),
                                
                                box(title="summary",width = 12,verbatimTextOutput("desc2")),
                                box(title="Boxplot",width = 12,plotlyOutput(outputId = "boxplotUA")),
                                box(title="Histogram",width = 12,plotlyOutput(outputId = "histUA")),
                                box(title="Q-Q plot",width = 12,plotOutput(outputId = "qqplot")),
                                
                                box(title = "Shapiro-Wilk normality test",width = 12,verbatimTextOutput("SWNorm"))
                              )
                     )
              )
            )),
    tabItem(tabName = "MA",
            fluidRow(
              tabBox(side = "right",width=12,
                     tabPanel(title="Categorical Vs Categorical",
                              fluidRow(
                                selectInput("varCat5", width="100%",h3("Select a variable :"),
                                            multiple = TRUE,
                                            choices = list("Gender" = "sex","Urban or Rural"="address", "family size" = "famsize",
                                                           "parent's cohabitation status " = "Pstatus","Mother's education"="Medu","Mother's job"="Mjob",
                                                           "Why did you choose this school"="reason","extra educational support"="schoolsup",
                                                           "family educational support"="famsup","extra paid classes"="paid",
                                                           "extra-curricular activities"="activities","attended nursery school"="nursery",
                                                           "wants to take higher education"="higher","internet access"="internet",
                                                           "romantic relationship"="romantic","quality of family relationships"="famrel",
                                                           "free time after school"="freetime","going out with friends"="goout",
                                                           "workday alcohol consumption"="Dalc","weekend alcohol consumption"="Walc",
                                                           "current health status"="health","Target variable"="Class"), selected = c("sex","famsize","Pstatus"))
                              ),
                              fluidRow(
                                box(title = "Contingency Table",width=12,verbatimTextOutput("CT"))  
                              ),
                              fluidRow(
                                column(width=6,
                                       selectInput("varCat3", h3("Select a variable :"), 
                                                   choices = list("Gender" = "sex","Urban or Rural"="address", "family size" = "famsize",
                                                                  "parent's cohabitation status " = "Pstatus","Mother's education"="Medu","Mother's job"="Mjob",
                                                                  "Why did you choose this school"="reason","extra educational support"="schoolsup",
                                                                  "family educational support"="famsup","extra paid classes"="paid",
                                                                  "extra-curricular activities"="activities","attended nursery school"="nursery",
                                                                  "wants to take higher education"="higher","internet access"="internet",
                                                                  "romantic relationship"="romantic","quality of family relationships"="famrel",
                                                                  "free time after school"="freetime","going out with friends"="goout",
                                                                  "workday alcohol consumption"="Dalc","weekend alcohol consumption"="Walc",
                                                                  "current health status"="health","Target variable"="Class"), selected = "sex")
                                ),
                                column(width=6,
                                       selectInput("varCat4", h3("Select a variable :"), 
                                                   choices = list("Gender" = "sex","Urban or Rural"="address", "family size" = "famsize",
                                                                  "parent's cohabitation status " = "Pstatus","Mother's education"="Medu","Mother's job"="Mjob",
                                                                  "Why did you choose this school"="reason","extra educational support"="schoolsup",
                                                                  "family educational support"="famsup","extra paid classes"="paid",
                                                                  "extra-curricular activities"="activities","attended nursery school"="nursery",
                                                                  "wants to take higher education"="higher","internet access"="internet",
                                                                  "romantic relationship"="romantic","quality of family relationships"="famrel",
                                                                  "free time after school"="freetime","going out with friends"="goout",
                                                                  "workday alcohol consumption"="Dalc","weekend alcohol consumption"="Walc",
                                                                  "current health status"="health","Target variable"="Class"), selected = "Class"))
                              ),
                              fluidRow(
                                tabBox(title = "",width = 12,
                                       tabPanel(title="Chi-squared test",verbatimTextOutput("khi2")),
                                       tabPanel(title="G-test",verbatimTextOutput("Gtest"))
                                )  
                              ),
                              fluidRow(
                                selectInput("varCat2",width = "100%", h3("Select a variable :"),
                                            multiple = TRUE,
                                            choices = list("Gender" = "sex","Urban or Rural"="address", "family size" = "famsize",
                                                           "parent's cohabitation status" = "Pstatus","Mother's education"="Medu","Mother's job"="Mjob",
                                                           "Why did you choose this school"="reason","extra educational support"="schoolsup",
                                                           "family educational support"="famsup","extra paid classes"="paid",
                                                           "extra-curricular activities"="activities","attended nursery school"="nursery",
                                                           "wants to take higher education"="higher","internet access"="internet",
                                                           "romantic relationship"="romantic","quality of family relationships"="famrel",
                                                           "free time after school"="freetime","going out with friends"="goout",
                                                           "workday alcohol consumption"="Dalc","weekend alcohol consumption"="Walc",
                                                           "current health status"="health","Target variable"="Class"), selected = c("address","Class"))
                              ),
                              fluidRow(
                                box(title = "Sunburst graph",width = 12,sunburstOutput("sunburst",width="100%"))
                              )        
                     ),
                     tabPanel(title="Numerical Vs Numerical",
                              fluidRow(
                                tabBox(title="",
                                       tabPanel(title = "Correlation matrix",width = 12,plotOutput("corrplot")),
                                       tabPanel(title="Scatter plots",
                                                fluidRow(
                                                  column(width=6,
                                                         selectInput("varNum3", h3("Select a variable :"), 
                                                                     choices = list("Age"="age","weekly study time"="studytime",
                                                                                    "number of past class failures"="failures"," number of school absences(in days)"="absences",
                                                                                    "Grade in the subject"="G3"), selected = "age")
                                                  ),
                                                  column(width=6,
                                                         selectInput("varNum4", h3("Select a variable :"), 
                                                                     choices = list("Age"="age","weekly study time"="studytime",
                                                                                    "number of past class failures"="failures","number of school absences(in days)"="absences",
                                                                                    "Grade in the subject"="G3"), selected = "age")
                                                  )
                                                ),
                                                fluidRow(
                                                  plotOutput("scatter")
                                                )
                                       )
                                )),
                              fluidRow(
                                column(width=6,
                                       selectInput("varNum1", h3("Select a variable :"), 
                                                   choices = list("Age"="age","weekly study time"="studytime",
                                                                  "number of past class failures"="failures"," number of school absences(in days)"="absences",
                                                                  "Grade in the subject"="G3"), selected = "age")
                                ),
                                column(width=6,
                                       selectInput("varNum2", h3("Select a variable :"), 
                                                   choices = list("Age"="age","weekly study time"="studytime",
                                                                  "number of past class failures"="failures"," number of school absences(in days)"="absences",
                                                                  "Grade in the subject"="G3"), selected = "age")
                                )
                              ),
                              fluidRow(
                                tabBox(title = "",width = 12,
                                       tabPanel("Pearson correlation test",verbatimTextOutput("PearsonCorr")),
                                       tabPanel("Kendall rank correlation test",verbatimTextOutput("KendallCorr")),
                                       tabPanel("Spearman rank correlation coefficient",verbatimTextOutput("SpearmanCorr"))
                                )
                              ),
                              fluidRow(
                                selectInput("varNum5", h5("Select a variable :"), 
                                            choices = list("Age"="age","weekly study time"="studytime",
                                                           "number of past class failures"="failures"," number of school absences(in days)"="absences",
                                                           "Grade in the subject"="G3"), selected = "age")
                              ),
                              fluidRow(
                                box(title = "one way ANOVA",width = 12,verbatimTextOutput("anova"))
                              )
                              
                     )
              )
              
              
            )
    ),tabItem(tabName = "MCA",
              fluidRow(
                box(width = 12,title="Scree plot",plotOutput(outputId="screeP")),
                box(width = 12,title="Correlation between variables and principal dimensions",plotOutput(outputId="corrP")),
                box(width = 12,title = "Coordinates of variable categories",plotOutput(outputId="varP")),
                box(width=12,title="Quality of representation of variable categories",plotOutput(outputId="cos2P"))
              )      
    ),
    tabItem(tabName = "RF",
            fluidRow(
              box(width = 12,title="Down-sampled training data set",DT::dataTableOutput("Resamp")),
              box(width=12,title="Target variable levels",verbatimTextOutput("tabletarget")),
              box(width=12,title="confusion matrix",plotOutput("FRcm")),
              box(width = 12,title="ROC curve",plotOutput("RFroc")),
              box(width = 12,title="AUC",verbatimTextOutput("RFauc")),
              column(width=4,
                     fluidRow(
                       selectInput("address", label = h4("where are you from ?"), 
                                   choices = list("urban" = "urban", "rural" = "rural"), 
                                   selected = "rural"),
                       selectInput("Medu",label=h4("Mother's education ?"),
                                   choices = list("no education"="no education","primary school"="primary school",
                                                  "middle school"="middle school","high school"="high school",
                                                  "university"="university"),selected="no education"),
                       selectInput("Mjob",label=h4("Mother's job ?"),
                                   choices = list("at home"="at_home","health"="health care",
                                                  "civil services"="services","teaching"="teacher",
                                                  "other"="other"),selected="at_home"),
                       selectInput("reason",label=h4("reason to choose this school"),
                                   choices = list("course preference"="course","close to home"="home",
                                                  "school reputations"="reputation","other"="other"),selected="course"),
                       selectInput("higher",label=h4("wants to take higher education"),
                                   choices=list("yes"="ambitious","no"="lazy"),selected="ambitious"),
                     )),
              column(width=4,
                     fluidRow(
                       selectInput("internet",label=h4("access to internet"),
                                   choices=list("yes"="Internet","no"="no_internet"),selected="ambitious"),
                       selectInput("famrel",label=h4("quality of family relationships "),
                                   choices=list("very bad relationship"="very_bad_famrel","bad relationship"="bad_famrel",
                                                "average relationship"="average_famrel","good relationship"="good_famrel",
                                                "excelent relationship"="excelent_famrel"),selected = "excelent_famrel"),
                       selectInput("freetime",label=h4("free time after school"),
                                   choices=list("very low freetime"="very_low_FT","low freetime"="low_FT",
                                                "average freetime"="average_FT","high freetime"="high_FT",
                                                "very high freetime"="very_high_FT"),selected = "low_FT"),
                       selectInput("goout",label=h4("going out with friends "),
                                   choices=list("very low frequency"="very_low_Gout","low frequency"="low_Gout",
                                                "average frequency"="average_Gout","high frequency"="high_Gout",
                                                "very high frequency"="very_high_Gout"),selected = "low_Gout"),
                       selectInput("Dalc",label=h4("workday alcohol consumption"),
                                   choices=list("very low consumption"="very_low_Dalc","low consumption"="low_Dalc",
                                                "average consumption"="average_Dalc","high consumption"="high_Dalc",
                                                "very high consumption"="very_high_Dalc"),selected = "low_Dalc"),
                       
                     )),
              column(width=4,
                     fluidRow(
                       selectInput("Walc",label=h4("weekend alcohol consumption"),
                                   choices=list("very low consumption"="very_low_Walc","low consumption"="low_Walc",
                                                "average consumption"="average_Walc","high consumption"="high_Walc",
                                                "very high consumption"="very_high_Walc"),selected = "low_Walc"),
                       textInput("age", label = h4("Age ?"), value = "15"),
                       textInput("studytime", label = h4("weekly study time"), value = "6"),
                       textInput("failures", label = h4("number of past class failures"), value = "0"),
                       textInput("absences",label = h4("number of school absences"),value="4")
                     )),
              box(width = 12,title = "Result of predictive analysis",verbatimTextOutput("RFres"))
            )
    ),
    tabItem(tabName = "ANN",
            fluidRow(
              box(width = 12,height = 500, title="Neural network plot",plotOutput("ANNplot")),
              box(width = 12,title="confusion Matrix",plotOutput("ANNcm")),
              box(width = 12,title="ROC curve",plotOutput("ANNroc")),
              box(width = 12,title="AUC",verbatimTextOutput("ANNauc"))
            )
    ),
    tabItem(tabName = "CART",
            fluidRow(
              box(width=12,title="textual presentation",verbatimTextOutput("TR")),
              box(width=12,title="Tree form",plotOutput("TF")),
              box(width = 12,title="confusion Matrix",plotOutput("CARTcm")),
              box(width = 12,title="ROC curve",plotOutput("CARTroc")),
              box(width = 12,title="AUC",verbatimTextOutput("CARTauc"))
            )
    )
  ))

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Social and behavioral study",titleWidth = 380),
  sidebar,
  body
)