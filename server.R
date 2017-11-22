

library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)
library(V8)

#necessary for remote box-collapsing
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

sqlite <- dbConnect(SQLite(), "db.sqlite")


server <- function(input, output, session) {
  
  global_nd1 <- 0
  global_liablity_new <- 0
  global_asset_new <- 0
  
  observeEvent(input$ab_Initial_Pricing, {
    js$collapse("box_Do")
    hide(id = "box_Initial_Pricing", anim = FALSE)
    
    temp_db_Stock_Derivative_Static <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative,
        input$ti_Stock_ISIN,
        input$ti_Exercise_Or_Forward_Price,
        as.character(input$ti_Contracting_Date),
        as.character(input$ti_Expiration_Date),
        input$ti_Contract_Size,
        input$ti_Number_Of_Contracts,
        input$ti_Stock_Volatility,
        input$ti_Interest_Rate,
        input$ti_Mark_To_Model
      )
    names(temp_db_Stock_Derivative_Static) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static,
                 append = TRUE)
  })
  observeEvent(input$button_Do, {
    temp_db_Stock_Pricing_Dynamic <-
      cbind.data.frame(
        input$ti_Stock_ISIN,
        input$ti_Do_Stock_Price,
        as.character(input$ti_Do_timestamp)
      )
    names(temp_db_Stock_Pricing_Dynamic) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic,
                 append = TRUE)
    
    
    js$collapse("box_Plan")
  })
  
  observeEvent(input$button_Plan, {
    
    output$to_Plan <- renderText("N(d1) = 1")
    js$collapse("box_Check")
  })
  
  #https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
  
  
  observeEvent(input$button_Check, {
    output$to_Check <- renderText("Delta N(d1) = 0")
    js$collapse("box_Act")
  })
  
  observeEvent(input$button_Act, {
    output$to_Act <- renderText("Forward: No action possible")
    v$doCalcAndPlot <- input$button_Act #CalcAndPlot
  })
  
  observeEvent(input$button_Act_Continue, {
    js$collapse("box_Act")
    js$collapse("box_Plan")
    js$collapse("box_Check")
    
    output$to_Plan <- renderText("")
    output$to_Check <- renderText("")
    output$to_Act <- renderText("")
    
  })
  
  observeEvent(input$reset_db, {
    dbSendStatement(sqlite, "DELETE from Stock_Derivative_Static")
    dbSendStatement(sqlite, "DELETE from Stock_Pricing_Dynamic")
    dbSendStatement(sqlite, "DELETE from Derivative_Instrument_Dynamic")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Risky_Income")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Fixed_Income")
    dbSendStatement(sqlite, "DELETE from Asset")
    dbSendStatement(sqlite, "DELETE from Liability")
    dbSendStatement(sqlite, "DELETE from Off_Balance")
  })
  
  

  
  v <- reactiveValues(doCalcAndPlot = FALSE) #recalc and redraw
  
  output$timeline <- renderDygraph({
    if (v$doCalcAndPlot == FALSE)
      return()
    isolate({
      temp_db_draw <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
      temp_db_draw$Pricing_Date <-
        as.Date(as.POSIXct(temp_db_draw$timestamp))
      
      #legacy calc
      temp_db_draw$TtM <-
        as.numeric(difftime(
          as.Date(isolate(input$ti_Expiration_Date)),
          as.Date(temp_db_draw$Pricing_Date),
          unit = "weeks"
        )) / 52.1775
      temp_db_draw$Interest_Rate <-
        as.numeric(input$ti_Interest_Rate) / 100
      temp_db_draw$Interest_Rate_Cont <-
        log(1 + temp_db_draw$Interest_Rate)
      temp_db_draw$F_Price <-
        temp_db_draw[1, 3] * (1 + as.numeric(input$ti_Interest_Rate) / 100) ^ (as.numeric(difftime(
          as.Date(input$ti_Expiration_Date),
          as.Date(input$ti_Contracting_Date),
          unit = "weeks"
        )) / 52.1775)
      temp_db_draw$Liability <-
        -temp_db_draw$F_Price * exp(-temp_db_draw$Interest_Rate_Cont * temp_db_draw$TtM)
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      temp_db_draw$'Forward Value' <-
        round(temp_db_draw$Liability + temp_db_draw$Stock_Price, 1)

      #Composing XTS
      temp_xts_draw <-
        xts(x = temp_db_draw[, c("Asset", "Liability", "Forward Value")], order.by =
              temp_db_draw[, 5])
      
      #Derivative_Instrument_Dynamic entry
      temp_Stock_Derivative_Static <-
        dbReadTable(sqlite, "Stock_Derivative_Static")
      temp_db_Derivative_Instrument_Dynamic <-
        cbind.data.frame(
          tail(temp_Stock_Derivative_Static$Stock_Derivative_Static_ID, 1),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Forward Value', 1)
        )
      names(temp_db_Derivative_Instrument_Dynamic) <-
        c("Stock_Derivative_Static_ID",
          "timestamp",
          "Fair_Value")
      dbWriteTable(
        sqlite,
        "Derivative_Instrument_Dynamic",
        temp_db_Derivative_Instrument_Dynamic,
        append = TRUE
      )
      
      #Economic_Resource_Risky_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Risky_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          1,
          tail(temp_db_draw$'Asset', 1),
          1
        )
      names(temp_db_Economic_Resource_Risky_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Nd1t",
          "Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Risky_Income",
        temp_db_Economic_Resource_Risky_Income,
        append = TRUE
      )
      
      #Economic_Resource_Fixed_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Fixed_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Liability', 1),
          1
        )
      names(temp_db_Economic_Resource_Fixed_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Present_Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Fixed_Income",
        temp_db_Economic_Resource_Fixed_Income,
        append = TRUE
      )
      
      #Asset, Liability of Off Balance
      if (tail(temp_db_draw$'Forward Value', 1) > 0) {
        #Asset
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_asset <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_asset) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Asset", temp_db_asset, append = TRUE)
      } else if (tail(temp_db_draw$'Forward Value', 1) < 0) {
        #Liability
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_liability <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_liability) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Liability", temp_db_liability, append = TRUE)
      }
      else {
        # Off_Balance
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_off_balance <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp)
          )
        names(temp_db_off_balance) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp")
        dbWriteTable(sqlite, "Off_Balance", temp_db_off_balance, append = TRUE)
      }
      
      #Plotting XTS
      dygraph(temp_xts_draw) %>%
        dyRangeSelector()
    })
  })
  
  # START 
  
  observeEvent(input$ab_Initial_PricingCO, {
    js$collapse("box_DoCO")
    hide(id = "box_Initial_PricingCO", anim = FALSE)
    
    temp_db_Stock_Derivative_StaticCO <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_DerivativeCO,
        input$ti_Stock_ISINCO,
        input$ti_Exercise_Or_Forward_PriceCO,
        as.character(input$ti_Contracting_DateCO),
        as.character(input$ti_Expiration_DateCO),
        input$ti_Contract_SizeCO,
        input$ti_Number_Of_ContractsCO,
        input$ti_Stock_VolatilityCO,
        input$ti_Interest_RateCO,
        input$ti_Mark_To_ModelCO
      )
    names(temp_db_Stock_Derivative_StaticCO) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_StaticCO,
                 append = TRUE)
  })
  observeEvent(input$button_DoCO, {
    temp_db_Stock_Pricing_DynamicCO <-
      cbind.data.frame(
        input$ti_Stock_ISINCO,
        input$ti_Do_Stock_PriceCO,
        as.character(input$ti_Do_timestampCO)
      )
    names(temp_db_Stock_Pricing_DynamicCO) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_DynamicCO,
                 append = TRUE)
    
    
    js$collapse("box_PlanCO")
  })
  
  ## Button - PlanCO - Start
  
  observeEvent(input$button_PlanCO, {
    
    #lesen aus Datenbank
   spd <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
   sps <- dbReadTable(sqlite, "Stock_Derivative_Static")
   
   
    #Zuweisen der Felder
    #P = Preis -> Stock_Price notwendig, war Stock_Pricing_Dynamic_ID
    P <-as.numeric(spd$Stock_Price[1])
    X <-as.numeric(sps$Exercise_Or_Forward_Price[1])
    r <-as.numeric(sps$Interest_Rate[1])
    sigma <-as.numeric(sps$Stock_Volatility[1])
    c_start<-as.Date(sps$Contracting_Date[1])
    c_end <- as.Date(sps$Expiration_Date[1])
    c_timestamp <- as.Date(spd$timestamp[1])
    
    #Typ Convertierung

    
    #Calculate Time to maturity
    dtse <- as.numeric(difftime(c_end,c_start, units ="days"))
    dtss <- as.numeric(difftime(c_timestamp,c_start, units = "days"))
    tm <- round(1 -dtss/dtse, digits = 2)
  
   
    #Berechnung d1
    r<- r/100 #Angabe in Prozent
    sigma <- sigma/100 # Angabe in Prozent
    if((tm != 0) & 
       (sigma != 0)){
      d1<-(log(P/X)+((r+sigma^2/2)*tm))/(sigma*sqrt(tm))
      d2<-d1-sigma*sqrt(tm)
      Nd1 <- pnorm(d1)
      global_nd1 <- Nd1
      Nd2<- pnorm(d2)
    }else{
      d1 <- 0
      d2 <- 0
      Nd1 <- 1
      Nd2<- 1
    }
    finA<-P*Nd1
    finL<- ((-X*Nd2)*exp(-r*tm))
    fV <- finA+finL
    if(finA > (finL*-1))
    {
      aol <-1
    }
    else{
      aol <-2
    }
    
    #Write to Database
    
    #Derivative_Instrument_Dynamic entry
    temp_Stock_Derivative_Static <-
      dbReadTable(sqlite, "Stock_Derivative_Static")
    temp_db_Derivative_Instrument_DynamicCO<-cbind.data.frame(
        tail(temp_Stock_Derivative_Static$Stock_Derivative_Static_ID, 1),
        as.character(c_timestamp),
      fV
    )
    names(temp_db_Derivative_Instrument_DynamicCO) <-
      c("Stock_Derivative_Static_ID",
        "timestamp",
        "Fair_Value")
    dbWriteTable(
      sqlite,
      "Derivative_Instrument_Dynamic",
      temp_db_Derivative_Instrument_DynamicCO,
      append = TRUE
    )
    #Economic_Resource_Fixed_Income entry
    temp_Derivative_Instrument_Dynamic <-
    dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    temp_db_Economic_Resource_Risky_IncomeCO<-
      cbind.data.frame(
        tail(
          temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
          1
        ),
        as.character(input$ti_Do_timestamp),
        as.numeric(Nd1),
        as.numeric(finA),
        aol
  
      )
    names(temp_db_Economic_Resource_Risky_IncomeCO) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Nd1t",
        "Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(
      sqlite,
      "Economic_Resource_Risky_Income",
      temp_db_Economic_Resource_Risky_IncomeCO,
      append = TRUE
    )
    #Economic_Resource_Fixed_Income entry
    temp_Derivative_Instrument_Dynamic <-
      dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    temp_db_Economic_Resource_Fixed_IncomeCO <-
      cbind.data.frame(
        tail(
          temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
          1
        ),
        as.character(input$ti_Do_timestamp),
        as.numeric(finL),
        aol
      )
    names(temp_db_Economic_Resource_Fixed_IncomeCO) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Present_Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(
      sqlite,
      "Economic_Resource_Fixed_Income",
      temp_db_Economic_Resource_Fixed_IncomeCO,
      append = TRUE
    )
  
    # Output
    
    outText <- sprintf("N(d1) = %f Riski Income = %f Fixed Income = %f",round(Nd1,digits=2),round(finA,digits=2), round(finL, digits=2))
    output$to_PlanCO <- renderText(outText)
    js$collapse("box_CheckCO")
  })
  
  
  ## Button - CheckCO - Start
  
  #retrieve N(d,t-1) from DB and compare to N(d,t)
  #propose rebalance of asset and liability
  #hand over asset, liability
  observeEvent(input$button_CheckCO, {
    
    #read from DB_Risky_Income
    temp_check_risky_income <- dbReadTable(sqlite, "Economic_Resource_Risky_Income")
  
    #TODO - at the moment just 1 value available bec. of Init - change? take 1x global & 1x from DB
    #TODO - save N(d1,t) at Init -> global and comppare it to new N(d1,t+1) and save it 
    
    #retrieve last element of N(d1,t-1)
    nd1_previous = as.numeric(tail(temp_check_risky_income$Nd1t, 1)) 
    #calculate Delta N(d1,t-1) - N(d1,t)
    deltaNd1 = nd1_previous - global_nd1
      
    
    #read from DB / price
    spd <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    #read from DB / liability
    fixed_income <- dbReadTable(sqlite, "Economic_Resource_Fixed_Income")
    
    #extract var
    price     <-as.numeric(spd$Stock_Price[1])
    liability <- as.numeric(fixed_income$Present_Value)
    
    #propose rebalancing of asset and liability
    global_asset_new    =  price * global_nd1
    global_liablity_new =  liability + ( -1 * price * deltaNd1)
    
    #output
    output$to_CheckCO <- renderText(paste("Delta N(d1) t = ",deltaNd1, sep = "")) 
    js$collapse("box_ActCO")
  })
  
  
  ## Button - ActCO - Start
  
  observeEvent(input$button_ActCO, {
    
    ## TODO Act Action ##
    # 1 - perform rebalancing of protfolio - write to DB table fixed and risky
    # 2 - Calculate portfolio fair value 
    # 3 - store in DB tabel derivate_instr*_dynamic
    #   - fv
    #   - N(d1,t) in table risky
    #   - set value for asset/liab/off_balance
    
    #aol =0 -> off_balance, aol = 1 -> asset, aol = 2 -> liability
    aol = 0 
    #check asset or liability
    if(global_asset_new > (global_liablity_new*-1))
    {
      aol <-1
    }
    else{
      aol <-2
    }
    
    #calculate new porfolio fair value
    fV = global_asset_new + global_liablity_new
      
    #1 write asset and liability in DB table Economic_Resource_Fixed_Income
    
    #Derivative_Instrument_Dynamic entry
    spd <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    
    
    #Derivative_Instrument_Dynamic entry
    temp_Stock_Derivative_Static <- dbReadTable(sqlite, "Stock_Derivative_Static")
    
    temp_db_Derivative_Instrument_DynamicCO2<-cbind.data.frame(
      tail(temp_Stock_Derivative_Static$Stock_Derivative_Static_ID, 1),
      spd$timestamp[1],
      fV
    )
    names(temp_db_Derivative_Instrument_DynamicCO2) <-
      c("Stock_Derivative_Static_ID",
        "timestamp",
        "Fair_Value")
    
    dbWriteTable(
      sqlite,
      "Derivative_Instrument_Dynamic",
      temp_db_Derivative_Instrument_DynamicCO2,
      append = TRUE
    )
    
    #Economic_Resource_Risky_Income entry
    temp_Derivative_Instrument_Dynamic <- dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    
    temp_db_Economic_Resource_Risky_IncomeCO<-
      cbind.data.frame(
        tail(
          temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
          1
        ),
        tail(
          temp_Derivative_Instrument_Dynamic$timestamp,
          1
        ),
        as.numeric(global_nd1),
        as.numeric(global_asset_new),
        aol
      )
    
    names(temp_db_Economic_Resource_Risky_IncomeCO) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Nd1t",
        "Value",
        "Asset_Or_Liability"
      )
    
    dbWriteTable(
      sqlite,
      "Economic_Resource_Risky_Income",
      temp_db_Economic_Resource_Risky_IncomeCO,
      append = TRUE
    )
    
    #Economic_Resource_Fixed_Income entry
    temp_Derivative_Instrument_Dynamic <- dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    
    temp_db_Economic_Resource_Fixed_IncomeCO <-
      cbind.data.frame(
        tail(
          temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
          1
        ),
        tail(
          temp_Derivative_Instrument_Dynamic$timestamp,
          1
        ),
        as.numeric(global_liablity_new),
        aol
      )
    names(temp_db_Economic_Resource_Fixed_IncomeCO) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Present_Value",
        "Asset_Or_Liability"
      )
    
    dbWriteTable(
      sqlite,
      "Economic_Resource_Fixed_Income",
      temp_db_Economic_Resource_Fixed_IncomeCO,
      append = TRUE
    )
    
    #Write Table Asset aol > 0  / Liability aol < 0 or Off_Balance aol = 0
    #TODO - check if aol can be 0
    
    temp_Derivative_Instrument_Dynamic <- dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    
    if (aol == !0){ #not off_balance
      
      temp_db_Asset_Liab <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          tail(
            temp_Derivative_Instrument_Dynamic$timestamp,
            1
          ),
          fV
        )
      
      names(temp_db_Asset_Liab) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Fair_Value"
        )
      
      if(aoal > 0){ #write to asset
        dbWriteTable(
          sqlite,
          "Asset",
          temp_db_Asset_Liab,
          append = TRUE
        )
      }
      else{ #write to liability
        dbWriteTable(
          sqlite,
          "Liability",
          temp_db_Asset_Liab,
          append = TRUE)
      }
      
    }
    else{ # off_balance aol = 0 
      
      temp_db_Balance_Off <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          tail(
            temp_Derivative_Instrument_Dynamic$timestamp,
            1
          )
        )
      
      names(temp_db_Balance_Off) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp"
        )
      
      dbWriteTable(
        sqlite,
        "Off_Balance",
        temp_db_Balance_Off,
        append = TRUE)
    }
    
      
    output$to_ActCO <- renderText("Forward: No action possible")
    
    #TODO - fill diagram with values
    v$doCalcAndPlot <- input$button_ActCO #CalcAndPlot
  })
  
  ## Button - ActCO - End
  
  #add Continue Event -> clear for next round
  
  observeEvent(input$button_Act_ContinueCO, {
   js$collapse("box_ActCO")
   js$collapse("box_PlanCO")
   js$collapse("box_CheckCO")
    
   output$to_PlanCO <- renderText("")
   output$to_CheckCO <- renderText("")
   output$to_ActCO <- renderText("")
    
})
  
  
  observeEvent(
    input$load_table_Stock_Pricing_Dynamic,
    output$table_Stock_Pricing_Dynamic <- renderDataTable({
      dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Information_Static,
    output$table_Stock_Information_Static <- renderDataTable({
      dbReadTable(sqlite, "Stock_Information_Static")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Derivative_Static,
    output$table_Stock_Derivative_Static <-
      renderDataTable({
        dbReadTable(sqlite, "Stock_Derivative_Static")
      })
  )
  
  observeEvent(
    input$load_table_Derivative_Instrument_Dynamic,
    output$table_Derivative_Instrument_Dynamic <-
      renderDataTable({
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Risky_Income,
    output$table_Economic_Resource_Risky_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Risky_Income")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Fixed_Income,
    output$table_Economic_Resource_Fixed_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Fixed_Income")
      })
  )
  
  observeEvent(input$load_table_Asset,
               output$table_Asset <- renderDataTable({
                 dbReadTable(sqlite, "Asset")
               }))
  
  observeEvent(input$load_table_Liability,
               output$table_Liability <- renderDataTable({
                 dbReadTable(sqlite, "Liability")
               }))
  
  observeEvent(input$load_table_Off_Balance,
               output$table_Off_Balance <- renderDataTable({
                 dbReadTable(sqlite, "Off_Balance")
               }))
  
  
}

