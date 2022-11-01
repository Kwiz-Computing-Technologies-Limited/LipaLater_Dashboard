library(shiny)
library(shiny.semantic)
library(shiny.fluent)
library(readr)
library(tidyverse)
library(future)
library(rhandsontable)
library(plotly)
library(bslib)
library(chron)
library(wordcloud)
library(tibble)
library(DT)

# read and clean data
source("WWW/data_cleaning_script.R")
dataset_raw = vroom::vroom("WWW/Data Analyst - Items Disbursed Analysis.csv.zip",  show_col_types = F) |>
  filter(`Principal Disbursed Derived` > 0)

dataset = dataset_raw |>
  clean_data()


UI <- fluidPage(
  navs_bar(title = "Data Analysis Dashboard",
           
           # Tab 1
           tabPanel("Overview",
                    fluidRow(
                      style = "height:700px;",
                      
                      column(width = 3,
                             wellPanel(
                               card(
                                 shiny::selectizeInput(inputId = "Item_Type",
                                                       label = "Product", 
                                                       choices = c(NA))),
                               
                               h5(),
                               card(
                                 shiny::selectizeInput(inputId = "Rank_by", 
                                                       label = "Rank Brands By", 
                                                       choices = c("Sales Frequency", "sales revenue"),
                                                       selected = "Sales Frequency")),
                               
                               h5(),
                               card(
                                 shiny::selectizeInput(inputId = "Arrears_By", 
                                                       label = "View Arrears Distribution By", 
                                                       choices = c("Days", "Amount", "Days and Amount"),
                                                       selected = "Days and Amount")),
                               
                               h5(),
                               card(
                                 shiny::sliderInput(inputId = "arrears_axis_extent", 
                                                    label = "Adjust the extent for the Arrears distribution histogram",
                                                    min = 0,
                                                    max = 50,
                                                    value = c(0, 50))),
                               
                               h5(),
                               card(
                                 textOutput("insights_title"),
                                 
                                 h5(),
                                 textOutput("arrears_insight"))),
                             
                             h5(),
                             h4("DISCLAIMER"),
                             h6("All transactions are assumed to be in KES since Currency is not specified in the data")
                             ),
                      
                      column(width = 5,
                             h4("Summary"),
                             wellPanel(
                               textOutput("data_preview")),
                             
                             h4("Top Brands"),
                             wellPanel(
                               card(
                                 width = 12,
                                 dataTableOutput("Top_Brands")
                               ))),
                      
                      
                      column(width = 4,
                             h4("Partner Stores Generating Arrears"),
                             wellPanel(
                               plotOutput("Partners", height = "300px")
                             ),
                             
                             h4("Arrears"),
                             wellPanel(
                               plotOutput("Arrears", height = "230px")
                             ))
                      )),
           
           # Tab 2
           tabPanel("Performance",
                    fluidRow(
                      style = "height:700px;",
                      
                      column(
                        h4("Product Ranking"),
                        width = 4,
                        wellPanel(
                          card(
                            shiny::selectizeInput(inputId = "Rank_Repayments_By", 
                                                  label = "Rank By", 
                                                  choices = c("Item Type", "Item Brand", "Partner Store",
                                                              "Client Location", "Referral Source",
                                                              "Month Submitted"),
                                                  selected = "Item Type")),
                          
                          card(
                            shiny::selectizeInput(inputId = "Arrange_Repayments_By", 
                                                  label = "Arrange By", 
                                                  choices = c("repayment %", "sales Frequency", "Principal Disbursed Derived",
                                                              "Arrears Amount"),
                                                  selected = "repayment %")),
                          
                          textOutput("repayment_comments")),
                        
                        h4("Repeat Customers"),
                        wellPanel(
                          textOutput("repeat_customers_summary")
                        )),
                      
                      column(
                        width = 8,
                        (
                          dataTableOutput("Product_Rank", height = "650px")
                        ))
                      )),
           
           # Tab 3
           tabPanel("Cluster",
                    fluidRow(
                      style = "height:700px;",
                      # col 1
                      column(
                        width = 3,
                        wellPanel(
                          card(
                            shiny::selectizeInput(inputId = "select_cluster_variables", 
                                                  label = "Cluster By", 
                                                  choices = c(NA),
                                                  selected = c(NA),
                                                  multiple = TRUE)),
                          
                          card(
                            shiny::selectizeInput(inputId = "number_of_clusters", 
                                                  label = "Number of Clusters", 
                                                  choices = 1:10,
                                                  selected = c(1)))
                        ),
                        
                        card(
                          shiny::selectizeInput(inputId = "view_cluster", 
                                                label = "View Products in Cluster", 
                                                choices = c(NA),
                                                selected = c(1))),
                        
                        h6("Note that clustering is done using the K-means Algorrithm")),
                      
                      # col 2
                      column(
                        width = 9,
                        h4("Cluster Means for the Variables used"),
                        wellPanel(
                          dataTableOutput("clustering_data_table")
                        ),
                        
                        h4("List of Products in the selected Product Cluster"),
                        
                        wellPanel(
                          textOutput("cluster_products")))
                    ))
  ))


Server <- function(input, output, session) {
  track.values = reactiveValues(
    data = dataset,
    dataset_raw = dataset_raw
  )
  
  
  
  # create data2 by:
  data2 %<-% reactive({
    data = track.values$data
    
    # Given multiple item purchases, find proportion due to each item
    # count number of loan items and update column
    
    data = data |> 
      mutate(`Client Id` = as.character(`Client Id`)) |>
      group_by(`Client Id`, `Submitted On Date`, `Principal Disbursed Derived`) |>
      mutate(proportion = `Item Purchase Price`/sum(`Item Purchase Price`)) |> 
      arrange(`Client Id`) |>
      ungroup(`Client Id`, `Submitted On Date`, `Principal Disbursed Derived`) |>
      distinct() |>
      
      
      # Use this proportion to adjust the item values for (arrears amount, principal/interest/fee disbursed,
      # principal/interest/fee repaid, principal/interest/fee outstanding, total expected repayment, 
      # total repayment derived)
      mutate(`Arrears Amount` = `Arrears Amount` * proportion,
             `Principal Disbursed Derived` = `Principal Disbursed Derived` * proportion,
             `Principal Repaid Derived` = `Principal Repaid Derived` * proportion,
             `Principal Outstanding Derived` = `Principal Outstanding Derived` * proportion,
             
             `Interest Charged Derived` = `Interest Charged Derived` * proportion,
             `Interest Repaid Derived` = `Interest Repaid Derived` * proportion,
             `Interest Outstanding Derived` = `Interest Outstanding Derived` * proportion,
             
             `Fee Charges Charged Derived` = `Fee Charges Charged Derived` * proportion,
             `Fee Charges Repaid Derived` = `Fee Charges Repaid Derived` * proportion,
             `Fee Charges Outstanding Derived` = `Fee Charges Outstanding Derived` * proportion,
             
             `Total Expected Repayment Derived` = `Total Expected Repayment Derived` * proportion,
             `Total Repayment Derived` = `Total Repayment Derived` * proportion,
             
             # find repayment rate as (principal + interest repaid) / (principal disbursed + interest charged)
             `repayment %` = 100 * (`Total Repayment Derived`) / (`Total Expected Repayment Derived`),
             
             `Total Recovered Derived` = `Total Recovered Derived` * proportion)|>
      
      # extract months from date submitted and save as factor
      mutate(`Submitted On Date` = (as.character(gsub("/DD/YYYY", "", `Submitted On Date`))),
             `Submitted On Date` = gsub("01", "Jan", `Submitted On Date`),
             `Submitted On Date` = gsub("02", "Feb", `Submitted On Date`),
             `Submitted On Date` = gsub("03", "March", `Submitted On Date`),
             `Submitted On Date` = gsub("04", "Apr", `Submitted On Date`),
             `Submitted On Date` = gsub("05", "May", `Submitted On Date`),
             `Submitted On Date` = gsub("06", "June", `Submitted On Date`),
             `Submitted On Date` = gsub("07", "July", `Submitted On Date`),
             `Submitted On Date` = gsub("08", "Aug", `Submitted On Date`),
             `Submitted On Date` = gsub("09", "Sep", `Submitted On Date`),
             `Submitted On Date` = gsub("10", "Oct", `Submitted On Date`),
             `Submitted On Date` = gsub("11", "Nov", `Submitted On Date`),
             `Submitted On Date` = gsub("12", "Dec", `Submitted On Date`),) |> 
      rename(`Month Submitted` = `Submitted On Date`) |>
      
      
      # return table
      select(-c(starts_with(" Item Description"),
                starts_with(" IMEI"),
                `Repay Frequency`
      ))
    return(data)
  })
  
  # update UI product selection choices from the data
  observe({
    choice = data2() |>
      select(`Item Type`) |> na.omit() |> unique()
    
    
    shiny::updateSelectizeInput(inputId = "Item_Type", 
                      choices = c("all", choice), selected = "all")
  })
  
  
  # filtered data based on product selection
  filtered_data %<-% reactive({
    if(input$Item_Type == "all") {
      data2()
    } else {
      data2() |>
        filter(`Item Type` == input$Item_Type)
    }
  })
  
  # stores by arrears
  store_arrears %<-% reactive({
    data = filtered_data() |>
      select(`Arrears Amount`, `Partner Store`) |>
      na.omit()
    
    if(nrow(data) > 2){
      data |> 
        aggregate(`Arrears Amount` ~ `Partner Store`, FUN = sum) |> 
        na.omit()
    }
  })
  
  # Partners by product 
  output$Partners <- renderPlot({
    data = store_arrears() 
    
    if(length(data$`Partner Store`) > 0){
      wordcloud(words = data$`Partner Store`, 
                freq = data$`Arrears Amount`, 
                min.freq = 1, max.words = 150, 
                random.order = FALSE, 
                rot.per = 0.32,
                colors = brewer.pal(8, "Dark2"))
    } 
  })
  
  
  # summary stats for filtered table
  summary_data %<-% reactive({
    paste("KES", round(sum(filtered_data()$`Principal Disbursed Derived`, na.rm = T),2), " was disbursed to", 
          length(unique(filtered_data()$`Client Id`)), "clients (Average Loan Amount: KES", 
          round(sum(filtered_data()$`Principal Disbursed Derived`, na.rm = T)/length(unique(filtered_data()$`Client Id`)),0),
          ") for the purchase of ", input$Item_Type, "products from ", length(unique(filtered_data()$`Partner Store`)), 
          " partners (Average Total Payout per Store: KES", 
          round(sum(filtered_data()$`Principal Disbursed Derived`, na.rm = T)/length(unique(filtered_data()$`Partner Store`)), 0) ,
          "). Returns, [Total Repayment / Principal Disbursed] are at ", 
          round(100 * (sum(filtered_data()$`Total Repayment Derived`, na.rm = T)/sum(filtered_data()$`Principal Disbursed Derived`, na.rm = T) - 1), 2),
          "% out of an expected return of ", 
          round(100 * (sum(filtered_data()$`Total Expected Repayment Derived`, na.rm = T)/sum(filtered_data()$`Principal Disbursed Derived`, na.rm = T) - 1), 2),
          "% [Total Expected Repayment / Principal Disbursed]")
  })
  # show summary
  output$data_preview <- renderText({
    summary_data() 
  })
  
  # top brands
  top_brand %<-% reactive({
    
    ## by Sales frequency 
    if(input$Rank_by == "Sales Frequency"){
      filtered_data() |>
        select(`Item Brand`) |>
        table() |>
        data.frame() |>
        rename(`Item Brand` = Item.Brand, Frequency = Freq) |>
        arrange(desc(Frequency))
    } else {
      
      ## by Sales Amount
      filtered_data() |>
        aggregate(`Item Purchase Price` ~ `Item Brand`, FUN = sum) |>
        arrange(desc(`Item Purchase Price`)) |>
        rename(`Total Sales Amount` = `Item Purchase Price`)
    }
    
  })
  
  output$Top_Brands <- renderDataTable(
    top_brand () |> 
      datatable(options = list(pageLength = 8)),
    selection = list(mode = "single", target = "cell")
  )
  
  
  # Arrears
  arrears_plot_data %<-% reactive({
    
    if(nrow(filtered_data()) > 0) {
      ## by days
      if(input$Arrears_By == "Days") {
        data = filtered_data() |>
          select(`Arrears Days`) |> 
          unlist() |>
          as.numeric()
        
      } else {
        
        ## by amount
        data = filtered_data() |>
          select(`Arrears Amount`)|> 
          unlist() |>
          as.numeric() 
        data = data/1000
      }
      data
    }
    
    })
  
  # update slider with data values
  observe({
    shiny::updateSliderInput(
      inputId = "arrears_axis_extent",
      min = min(arrears_plot_data(), na.rm = T),
      max = max(arrears_plot_data(), na.rm = T),
      value = c(min(arrears_plot_data(), na.rm = T),
                max(arrears_plot_data(), na.rm = T)))
  })
  
  arrears_plot %<-% reactive({
    if(sum(arrears_plot_data(), na.rm = T) > 0){
      
      if(input$Arrears_By == "Days and Amount") {
        filtered_data() |> 
          select(`Arrears Days`, `Arrears Amount`) |>
          ggplot(aes(x = `Arrears Days`, y = `Arrears Amount`)) +
          geom_point() +
          theme_classic()
      } else {
        
        arrears_plot_data() |> 
          qplot()+
          xlim(input$arrears_axis_extent[1], 
               input$arrears_axis_extent[2]) + 
          theme_classic() + 
          xlab(x_axis_arrears()) +
          ylab("Frequency")
      }
    } 
  })
  

  ## axis names
  x_axis_arrears %<-% reactive({
    if(input$Arrears_By == "Days"){
      "Arrears Days"
      } else if(input$Arrears_By == "Amount"){
        "Arrears Amount (x 000)"
      }
    
           
  })
  
  output$Arrears <- renderPlot({
    arrears_plot()
  })
  
  # insights on arrears
  output$insights_title <- renderText({
    if(input$Arrears_By == "Days and Amount"){
      paste("INSIGHTS")
    }
  })
  
  insights_text %<-% reactive({
    
    data = filtered_data() |>
      select(`Arrears Days`, `Arrears Amount`) |> na.omit()
    # corelation
    if(nrow(data) > 2){
      correlation = cor.test(data$`Arrears Days`, data$`Arrears Amount`)
      pvalue = correlation$p.value
      statistic = round(correlation$statistic, 1)
      
      
      if(input$Arrears_By == "Days and Amount") {
        if(pvalue > 0.05) {
          paste("There is no relationship between the size of the arrears", 
                " and the duration of arrears for", input$Item_Type, " products") 
        } else if(statistic < 0){
          paste("Smaller loans attract longer periods of arrears for", input$Item_Type, " products (Correlation coefficient: ", statistic,")")
        } else {
          paste("Larger loans attract longer periods of arrears for", input$Item_Type, " products (Correlation coefficient: ", statistic,")")
        }
      }
    }
  })
  
  output$arrears_insight <- renderText({
    insights_text()
  })
  
  
  ## TAB 2
  
  # rank products by repayment rates
  repayments0 %<-% reactive({
    
    # repayments
    repayments = data2() |> 
      select(`repayment %`, all_of(input$Rank_Repayments_By)) |>
      aggregate(`repayment %` ~ . , FUN = mean) |>
      mutate(`repayment %` = round(`repayment %`, 2))
    
    # sales volume
    sales = data2() |>
      select(all_of(input$Rank_Repayments_By))|> 
      table() |> data.frame() |>
      rename(`sales Frequency` = Freq)
    colnames(sales)[1] = input$Rank_Repayments_By
    
    # principal disbursed
    principal = data2() |>
      select(`Principal Disbursed Derived`, all_of(input$Rank_Repayments_By))|> 
      aggregate(`Principal Disbursed Derived` ~ . , FUN = sum) |>
      mutate(`Principal Disbursed Derived` = round(`Principal Disbursed Derived`, 0))
    
    # Arrears
    Arrears = data2() |>
      select(`Arrears Amount`, all_of(input$Rank_Repayments_By))|> 
      aggregate(`Arrears Amount` ~ . , FUN = sum) |>
      mutate(`Arrears Amount` = round(`Arrears Amount`, 0))
    
    data = merge(repayments, sales) |>
      merge(principal) |>
      merge(Arrears) 
  })
  
  # update UI sorting variable
  observe({
    choice = repayments0() |>
      colnames() 
    
    
    shiny::updateSelectizeInput(inputId = "Arrange_Repayments_By", 
                                choices = c(choice[2:length(choice)]), 
                                selected = choice[2])
  })
  
  repayments %<-% reactive({
    sort_var = colnames(repayments0())[colnames(repayments0()) == input$Arrange_Repayments_By] |>
      as.character()
    
    repayments0() |>
      dplyr::arrange(dplyr::desc(!!rlang::sym(sort_var)))
  })
  
  
  output$Product_Rank <- renderDataTable({
    if(nrow(repayments()) > 0) {
        # plot selection
        repayments() |>
        datatable(options = list(pageLength = 15))
    }
    
  })
  
  # comment on data cleaning required for Client Location and referral source
  comment_on_repayment %<-% reactive({
    
    if(input$Rank_Repayments_By %in% c("Client Location", "Referral Source")) {
      paste("These variable (", input$Rank_Repayments_By, ") must be cleaned for this filter to be efficient")
      }
    
  })
  
  output$repayment_comments <- renderText({
    comment_on_repayment()
    })
  
  
  ## TAB 3: REPEAT CUSTOMERS
  
  ### frequency for repeat customers
  repeat_customer_frequency %<-% reactive({
    repeat_customers = data2()$`Client Id` |>
      table() |>
      data.frame() |>
      filter(Freq > 1) |>
      rename(`Customer Id` = Var1, Frequency = Freq) |>
      arrange(desc(Frequency))
  })
  
  
  output$repeat_customers <- renderDataTable({
    repeat_customer_frequency() |>
      datatable(options = list(pageLength = 3))
  })
  
  
  ### filter repeat customers 
  
  repeat_customers_filtered %<-% reactive({
    data2() |> filter(`Client Id` %in% repeat_customer_frequency()$`Customer Id`)
  })
  
  # repeat customers summary
  summary_repeat_customers %<-% reactive({
    data = repeat_customer_frequency() 
    dataset_raw = track.values$dataset_raw 
    
    if(nrow(data) > 0) {
      n = nrow(data)
      N = data2()$`Client Id` |>
        table() |>
        data.frame() |>
        nrow()
      
      principal_ratio = round(100 * sum(repeat_customers_filtered()$`Principal Disbursed Derived`, na.rm = T)/sum(filtered_data()$`Principal Disbursed Derived`, na.rm = T) ,2)
      

    }
    
    paste("There are ", n, " repeat customers representing ", round(100 * n/N, 2), 
          "% of all loan receipients.", "KES", round(sum(repeat_customers_filtered()$`Principal Disbursed Derived`, na.rm = T),2), "(", principal_ratio, "% of Total) was disbursed. (Average Loan Amount: KES",
          
          ## CONTINUE HERE
          
          round(sum(repeat_customers_filtered()$`Principal Disbursed Derived`, na.rm = T)/length(unique(repeat_customers_filtered()$`Client Id`)),0),
          ") from an average of ", round(mean(repeat_customer_frequency()$Frequency, na.rm =T), 2)  , " visits. Purchases were made from ", length(unique(repeat_customers_filtered()$`Partner Store`)), 
          " partners (Average Total Payout per Store: KES", 
          round(sum(repeat_customers_filtered()$`Principal Disbursed Derived`, na.rm = T)/length(unique(repeat_customers_filtered()$`Partner Store`)), 0) ,
          "). Returns, [Total Repayment / Principal Disbursed] are at ", 
          round(100 * (sum(repeat_customers_filtered()$`Total Repayment Derived`, na.rm = T)/sum(repeat_customers_filtered()$`Principal Disbursed Derived`, na.rm = T) - 1), 2),
          "% out of an expected return of ", 
          round(100 * (sum(repeat_customers_filtered()$`Total Expected Repayment Derived`, na.rm = T)/sum(repeat_customers_filtered()$`Principal Disbursed Derived`, na.rm = T) - 1), 2),
          "% [Total Expected Repayment / Principal Disbursed]")
    
    
  })
  
  output$repeat_customers_summary <- renderText({
    summary_repeat_customers()
  })
  
  
  ## TAB 3
  # cluster analysis
  numeric_features %<-% reactive({
    data = data2()
    
    if(nrow(data) > 0) {
      data = data |> 
        select(`Principal Disbursed Derived`, `Principal Outstanding Derived`,
               `Interest Charged Derived`, `Interest Outstanding Derived`, `Fee Charges Charged Derived`,
               `Fee Charges Outstanding Derived`, `repayment %`, `Item Type`)
    }
  })
  
  
  # update UI options for clustering variables from the numeric columns
  observe({
    choice = numeric_features() |>
      colnames()
    choice = choice[choice != "Item Type"]
    
    
    shiny::updateSelectizeInput(inputId = "select_cluster_variables", 
                                choices = c(choice), selected = choice)
  })
  
  
  ## select variables to cluster on based on user selection
  clustering_data %<-% reactive({
    numeric_features() |>
      select(all_of(as.character(input$select_cluster_variables)), `Item Type`)
  })
  
  
 
  # update UI for number of clustering variables 
  observe({
    if(length(all_of(input$select_cluster_variables)) > 5){
      N = length(all_of(input$select_cluster_variables)) + 1
    } else {
      N = length(all_of(input$select_cluster_variables)) + 2
    }
    
    shiny::updateSelectizeInput(inputId = "number_of_clusters", 
                                choices = 2:N, selected = N)
  })
  
  # fit K-means Clustering model
  cluster_model %<-% reactive({
    data = clustering_data() |>
      select(-`Item Type`) |> 
      na.omit()
    
    if(nrow(data) > input$number_of_clusters) {
      set.seed(1272)
      cluster_model <- kmeans(data, input$number_of_clusters, nstart = 20)
    }
    cluster_model
  })
  
  
  # show cluster centers
  output$clustering_data_table <- renderDataTable({
    clusters_n = aggregate(Item.Type ~ Cluster, data = clustered_groups(), 
                           FUN = function(x){
                             unique(x) |> length()
                           }) |> 
      rename(`Number of Products` = Item.Type) 


    data = cluster_model()$centers |>
      as_tibble() |>
      round(0) |>
      mutate(Cluster = as.character(rownames(cluster_model()$centers)))|> 
      merge(clusters_n, by = "Cluster") |>
      select(-Cluster) |> 
      t()
    
    colnames(data) = 1:ncol(data)
    
    data  |>
      datatable(options = list(pageLength = 10))
  })

  
  # assign items to their clusters
  clustered_groups %<-% reactive({
    clustering_data() |>
      na.omit() |>
      mutate(Cluster = as.character(cluster_model()$cluster)) |>
      data.frame()
    
    
  })
  
  # update UI for cluster whose products we want to view 
  observe({
    updateSelectizeInput(inputId = "view_cluster", 
                         choices = 1:input$number_of_clusters) 
    
  })
  
  ## Show the cluster products
  cluster_products_text %<-% reactive({
    products = clustered_groups() |> 
      filter(Cluster == as.character(input$view_cluster)) |>
      select(Item.Type) |> unique() |> as.character()
    
    paste0(all_of(products)) 
  })
  
  
  output$cluster_products <- renderText({
    cluster_products_text()
  })
  
  

}


shinyApp(UI, Server)