#Statistics CA-1
library(shiny)
library(bslib)
library(ggplot2)
library(httr)
library(jsonlite)
library(lubridate)

current_date <- Sys.Date()
date_before_7_days <- current_date - days(7)
enddate <- current_date - days(1)
city <- "Dublin"
base <- 'http://api.weatherapi.com/v1/'
extp <- 'history.json'
key <- '?key=614b297f50c74145aa1145649231206'
q <- "&q="
dt <- "&dt="
end_dt <- "&end_dt="
URL <- paste0(base,extp,key,q,city,dt,date_before_7_days,end_dt,enddate)
raw <- GET(URL)
list1 <- fromJSON(rawToChar(raw$content), flatten = TRUE)
data<-(list1$forecast$forecastday$hour)
data<-do.call(rbind,data)

col_list = c("temp_f","wind_mph","wind_degree","precip_mm","humidity","cloud","feelslike_f","heatindex_f","vis_miles","uv")
col_list_2 = c("is_day","wind_dir","condition","uv")

#Creating the UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel(
    "Weather app",
  ),
  navlistPanel(
    "ITEM 1",
    tabPanel("Describing the data",
             mainPanel(
               "This app is an Api-based app which queries the live hourly data from weatherapi.com for seven days for the selected city and providea visual analysis for the weather conditions",
               h1("Let's see the data using visuals"),
             ),
             sidebarPanel(
               selectInput("apiOption", "Choose a city: ", choices = c("Mumbai","Dublin","Noida","Cork")),
               selectInput("variable_select", "Select a variable for visuals:", choices = col_list),
               side   <- sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30)
             ),
             mainPanel(
               h2("Plotting a histogram"),
               plotOutput("barplot")
             ),
             mainPanel(
               h2("Plotting a scatter plot"),
             ),
             sidebarPanel(
               selectInput("bubble_select1", "Select variable 1 for the x-axis on scatter plot:", choices = col_list),
               selectInput("bubble_select2", "Select variable 2 for the y-axis on scatter plot:", choices = col_list)
             ),
             mainPanel(
               plotOutput("plot")
             ),
             mainPanel(
               h2("Plotting a bar plot"),
             ),
             sidebarPanel(
               selectInput("hist_select", "Select variable for histogram:", choices = col_list)
             ),
             mainPanel(
               plotOutput("hist")
             ),
    ),
    tabPanel("Measures",
             mainPanel(
               h1("Calculating measures")
             ),
             sidebarPanel(
               selectInput("m_inp", "Select a variable for measures:", choices = col_list)
             ),
             mainPanel(
               tableOutput("tab")
             )
    ),
    tabPanel("Chebyshevs rule",
             mainPanel(
               h1("Chebyshev's rule")
             ),
             sidebarPanel(
               selectInput("chebyshev_inp", "Select a variable to apply Chebyshev's rule:", choices = col_list)
             ),
             mainPanel(
               h3("These are the outliers for the selected variable based on one-sigma interval are:"),
               textOutput("chebyshev_out")
             )
    ),
    tabPanel("Box-plot technique",
             mainPanel(
               h1("Detecting outliers using Boxplot")
             ),
             sidebarPanel(
               selectInput("box_inp", "Select a variable to detect outliers:", choices = col_list)
             ),
             mainPanel(
               h5("In the boxplot technique, we identify the quartiles for a given parameter, the lower bound being 25 percentile and the upper bound being 75 percentile"),
               h5("We then calculate the Interquartile range which is the area between the 75th and 25th percentiles"),
               h5("The range for the whiskers is about 1.5 times of the calculated Interquartile range on both the ends"),
               h5("Any values falling beyond this range are considered as Outliers"),
               plotOutput("box_out")
             )
    ),
    "ITEM 2",
    tabPanel(
      "Probability model for day",
      mainPanel(
        h2("Binomial model for the variable is_day"),
        h5("We have selected the Binomial probability model for the variable 'is_day'. The binomial model is a discrete probability model which will result in a YES or NO answer with interations 'n' number of times"),
        h5("These binary experiments were repeated multiple times in the parameter 'is_day' and so we used Binomial distribution model"),
        br(),
        h5("The parameters for this model are n and p, n being the number of samples and p being the probability for each sample"),
        h5("Based on our estimation, the value for n is:"),
        textOutput("binomN_out"),
        h5("and the value for parameter p is:"),
        textOutput("binomP_out"),
        plotOutput("p_plot1")
      )),
    tabPanel(
      "Probability model for temperature",
      mainPanel(
        h2("Normal model for the variable temperature"),
        h5("We have selected the Normal probability model for the variable 'temperature'. This is a type of continuous distribution and has a symmetric probability distribution."),
        h5("The graph represents a bell-shaped curve with the highest frequency in the center"),
        br(),
        h5("The parameters for a Normal distribution model are mean and standard deviation."),
        h5("For this variable, we found the mean to be:"),
        textOutput("NormalM_out"),
        h5("and the standard deviation to be:"),
        textOutput("NormalS_out"),
        h5("PDF"),
        plotOutput("p_plot2"),
        h3("CDF"),
        plotOutput("p_plot3"),
      )),
    tabPanel(
      "Probability model for Heat index",
      mainPanel(
        h2("Exponential model for the variable heat index"),
        h5("An exponential model is a type of function that shows how a quantity grows exponentially over time."),
        br(),
        h5("The parameters for an Exponential distribution model are j, where j is any selected value and lambda."),
        h5("For this variable, we have the lambda as:"),
        textOutput("Exp_l"),
        plotOutput("p_plot4"),
      )),
    tabPanel(
      "Probability model for Humidity",
      mainPanel(
        h2("Poisson model for the variable humidity"),
        h5("The Poisson distribution is a discrete probability distribution that shows the probability of a given number of events occurring in a fixed interval of time or space"),
        br(),
        h5("The parameters for the Poisson distribution model is lambda, where lambda is the mean."),
        h5("For this variable, we have the lambda as:"),
        textOutput("Poisson_l"),
        br(),
        "PDF",
        plotOutput("p_plot5"),
        "CDF",
        plotOutput("p_plot6")
      )),
    "ITEM 3",
    tabPanel(
      "Hypotheses Tests",
      mainPanel(
        h2("Let's check whether 2 variables are independent of each other"),
        "H0: Values uv and is_day are dependent",
        br(),
        "H1: Values uv and is_day are independent",
        h5("The test value calculated is:"),
        textOutput("Tvalue"),
        h5("The critical value calculated is:"),
        textOutput("Cvalue"),
        h5("Our decision"),
        textOutput("Indout")
      ),
    ),
    tabPanel(
      "Goodness of Fit Test",
      mainPanel(
        h2("Performing Goodness of Fit test"),
        "H0: p1=p3=p4=p5=p6 = 1/5",
        br(),
        "H1: p1=p3=p4=p5=p6  NOT= 1/5",
        h5("The test value calculated is:"),
        textOutput("GTvalue"),
        h5("The critical value calculated is:"),
        textOutput("GCvalue"),
        h5("Our decision"),
        textOutput("GTout")
      ),
    ),
    tabPanel(
      "Test of mean",
      mainPanel(
        h2("Calculating Test of Mean"),
        h5("We have used two-sided hypothesis test for the variable wind_mph"),
        "Mu is:",
        textOutput("mu"),
        h5("The test value calculated is:"),
        textOutput("MTvalue"),
        h5("The critical value calculated is:"),
        textOutput("MCvalue"),
        h5("Our decision"),
        textOutput("MTout")
      ))
  )
)

server <- function(input, output,session){
  apiURL <- reactive({
    if (input$apiOption == "Dublin") {
      "Dublin"
    } else if (input$apiOption == "Mumbai") {
      "Mumbai"
    } else if (input$apiOption == "Noida") {
      "Noida"
    } else if (input$apiOption == "Cork") {
      "Cork"
    } else {
      "Dublin"
    }
  })
  
  observeEvent(input$apiOption, {
    current_date <- Sys.Date()
    date_before_7_days <- current_date - days(7)
    enddate <- current_date - days(1)
    city <- apiURL()
    base <- 'http://api.weatherapi.com/v1/'
    extp <- 'history.json'
    key <- '?key=614b297f50c74145aa1145649231206'
    #city <- input$variable_select2
    q <- "&q="
    dt <- "&dt="
    end_dt <- "&end_dt="
    URL <- paste0(base,extp,key,q,city,dt,date_before_7_days,end_dt,enddate)
    raw <- GET(URL)
    list1 <- fromJSON(rawToChar(raw$content), flatten = TRUE)
    data<-(list1$forecast$forecastday$hour)
    data<-do.call(rbind,data)
    data1 = data
    
    #Plotting a bar plot from the selection
    observeEvent(input$variable_select, {
      selected_variable <- input$variable_select
      output$barplot <- renderPlot({
        bins <- seq(min(data1[[selected_variable]]), max(data1[[selected_variable]]), length.out = input$bins + 1)
        hist(data1[[selected_variable]], breaks = bins, col = 'darkgray', border = 'white',
             xlab = selected_variable,
             main = 'Histogram of Selected variable')
      })
    })
    
    observeEvent(c(input$bubble_select1,input$bubble_select2), {
      selected_variable1 <- input$bubble_select2
      selected_variable2 <- input$bubble_select1
      
      output$plot <- renderPlot({
        ggplot(data1, aes_string(x = selected_variable1, y = selected_variable2)) + geom_point() + labs(x = colnames(selected_variable1), y = selected_variable2)
        plot(data1[[selected_variable1]], data1[[selected_variable2]], main = "Plot")
      })
    })
    
    observeEvent(input$hist_select, {
      selected_variable3 <- input$hist_select
      output$hist <- renderPlot({
        barplot(data1[[selected_variable3]])
      })
    })
    
    observeEvent(input$m_inp,{
      s_var4 <- input$m_inp
      
      #Calculating mean
      m1 <- mean(data1[[s_var4]])
      
      #Calculating median
      m2 <- median(data1[[s_var4]])
      
      #Calculating range
      r=max(data1[[s_var4]])-min(data1[[s_var4]])
      
      #Calculating mode
      m3 = as.numeric(names(which.max(table(data1[[s_var4]]))))
      
      #Calculating quantile
      Q=quantile(data1[[s_var4]])
      
      #Calculating IQR
      q1=Q[2];q3=Q[4]
      iqr=q3-q1
      
      #Calculating variance
      v=var(data1[[s_var4]])
      
      #Calculating standard deviation
      s_d=sd(data1[[s_var4]])
      
      #Displaying the measures
      output$tab <- renderTable(
        data.frame(
          Mean = m1,
          Median = m2,
          Mode = m3,
          Range = r,
          IQR = iqr,
          Variance = v,
          Standard_deviation = s_d
        ))
    })
    
    observeEvent(input$chebyshev_inp,{
      selected_variable4 <- input$chebyshev_inp
      # one-sigma interval estimation
      L=mean(data1[[selected_variable4]])-sd(data1[[selected_variable4]]) 
      U=mean(data1[[selected_variable4]])+sd(data1[[selected_variable4]])
      Out=c() 
      for(j in data1[[selected_variable4]]){
        if(j<L|j>U)
        {
          o=j
          Out=c(Out,o)
        }
        else
        {
          o=NULL
        }
      }
      output$chebyshev_out <- renderText(
        {
          paste(unique(Out),",")
        })
    })
    
    observeEvent(input$box_inp,{
      selected_variable5 <- input$box_inp
      Q=quantile(data1[[selected_variable5]])
      Q1=Q[2]; Q3=Q[4]
      IQR=Q3-Q1
      lower=Q1-1.5*IQR ; upper=Q3+1.5*IQR
      outliers=c()
      for(i in data1[[selected_variable5]]){
        if(i<lower || i>upper) {o=i}else{o=NULL}
        outliers=c(outliers, o)
      }
      
      output$box_out <-renderPlot({
        boxplot(data1[[selected_variable5]],data=data1)
      })
    })
    
    #Calculating the probability
    #Item2
    #Variable1 - Binomial model for column is_day
    X1 = data1$is_day
    t = table(X1)
    #parameters - p1, n
    n = length(X1)
    p1 = t/n
    #
    y1=dbinom(0:1,n,p1)
    y2=pbinom(0:1,n,p1)
    
    #Prediction for this attribute
    output$p_plot1 <- renderPlot({
      plot(0:1, y1, xlab='X1',ylab="Binomial", main="pmf of binomial model")
    })
    
    output$binomP_out <- renderText(
      {
        p1
      })
    output$binomN_out <- renderText(
      {
        n
      })
    
    #For variable 2 - temp_f
    X2 = data1$temp_f
    
    #Parameters for this model
    mu = mean(X2)
    s = sd(X2)*sd(X2)
    
    #Normal distribution
    pmf = dnorm(X2, mu, s)
    cdf = pnorm(X2, mu, s)
    pred = rnorm(100, mu, s)
    
    #Output
    output$NormalM_out <- renderText({mu})
    output$NormalS_out <- renderText({s})
    
    #Prediction for this attribute
    output$p_plot2 <- renderPlot({
      hist(pmf, xlab = "pdf", main = "PDF")
    })
    
    output$p_plot3 <- renderPlot({
      plot(ecdf(X2))
    })
    
    #For variable 3- Exponential model - heatindex_f
    X3 = data1$heatindex_f
    #Parameters
    mu = mean(X3)
    lambda = 1/mu
    #
    pmf = dexp(X3, lambda)
    cdf = pexp(X3,lambda)
    r = rexp(100,lambda)
    #Prediction:
    output$p_plot4 <- renderPlot({
      plot(X3, dexp(X3,lambda), xlab='X3',ylab="Exponential", main="pmf of exponential model")
    })
    output$Exp_l <- renderText({lambda})
    
    #For variable 4 - Poisson model - humidity
    X4 = data1$humidity
    #Parameters
    lambda1 = mean(X4)
    #
    d = dpois(X4,lambda1)
    c = ppois(X4, lambda1)
    r = rpois(100, lambda1)
    #
    output$p_plot5 <- renderPlot({
      hist(d, xlab = "pdf", main = "PDF")
    })
    output$p_plot6 <- renderPlot({
      plot.ecdf(X4)
    })
    output$Poisson_l <- renderText({lambda1})
    
    #Item3 - Tab A
    H1=data1$uv
    H2=data1$is_day
    r=length(unique(data1$uv)) #number of classes that we have in H1(uv)
    c=length(unique(data1$is_day)) #number of classes that we have in H2(is_day)
    alpha = 0.01
    O=table(H1,H2)
    n=nrow(data1)
    E=matrix(NA,r,c)
    for (i in 1:r){
      for(j in 1:c){
        Oi0=sum(O[i,]); O0j=sum(O[,j])
        E[i,j] = (Oi0*O0j)/n
      }
    }
    test.value1=sum(((O-E)^2)/E)
    c.value1=qchisq(1-alpha,(r-1)*(c-1))
    if(test.value1 > c.value1)
    {
      HTout = "As Test value is greater than C value, we reject the Null Hypothese and accept the alternative Hypotheses. Therefore, UV and is_day are dependent on each other"
    }
    else
    {
      HTout = "As Test value is less than C value, we accept the Null Hypothese and reject the alternative Hypotheses. Therefore, UV and is_day are independent of each other"
    }
    output$Tvalue <- renderText({paste(test.value1)})
    output$Cvalue <- renderText({paste(c.value1)})
    output$Indout <- renderText({paste(HTout)})
    
    #Item3 - Tab b - Goodness of Fit test
    H3=data1$vis_miles
    alpha=0.05
    O=table(H3)
    r=length(unique(data1$vis_miles))
    p=rep(1/r,r)
    E=r*p
    test.value2=sum((O-E)^2/E)
    c.value2=qchisq(1-alpha, (r-1))
    
    if(test.value2 > c.value2)
    {
      GTout = "As Test value is greater than C value, we reject the Null Hypotheses and accept the alternative Hypotheses. Therefore, the candidate set of probabilities are inappropriate to quantify the uncertainty of class frequency of Vis_miles at the significant level alpha=0.05"
    }
    else
    {
      GTout = "As Test value is less than C value, we accept the Null Hypotheses and reject the alternative Hypotheses. Therefore, the candidate set of probabilities are appropriate to quantify the uncertainty of class frequency of Vis_miles at the significant level alpha=0.05"
    }
    
    output$GTvalue <- renderText({test.value2})
    output$GCvalue <- renderText({c.value2})
    output$GTout <- renderText({GTout})
    
    #Item3 - tab c - Test of mean
    H4=data1$wind_mph
    mu=data1$wind_mph[1]
    n=length(H4)
    alpha=0.05
    sigma=sd(H4)
    xbar=mean(H4)
    test.value3=(xbar-mu)/(sigma/sqrt(n))
    c.value3=qnorm(1-(alpha)/2)
    if(abs(test.value3) > c.value3)
    {
      Meanout = "As test value is greater than c value, Mu is rejected for Wind_mph"
    }
    else
    {
      Meanout = "As test value is less than c value, Mu is accepted for Wind_mph"
    }
    output$mu <- renderText({mu})
    output$MTvalue <- renderText({test.value3})
    output$MCvalue <- renderText({c.value3})
    output$MTout <- renderText({Meanout})
    
  }) #END OF API EVENT
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)

