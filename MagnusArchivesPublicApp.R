# Shiny App for Magnus Archives

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyauthr)
library(data.table)
library(RPostgres)
library(sf)
library(leaflet)
library(ggplot2)
library(lubridate)
library(plotly)
library(htmltools)

# make a temp table of users
user_base <- data.frame('user' = 'guest', password = '1234', stringsAsFactors = F, row.names = F)


# Dashboard UI -----
ui <- dashboardPage(skin = 'blue',
    dashboardHeader(title = 'Magnus Archives', titleWidth = '100%'),
    dashboardSidebar(
        sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            menuItem(text = 'Map', tabName = 'map', icon = icon('map')),
            menuItem(text = 'Time Line', tabName = 'timeline', icon = icon('clock')),
            menuItem(text = 'Help', tabName = 'help', icon = icon('circle-info')),
            menuItemOutput(outputId = 'dbmenuitem')
        )
    ),
    dashboardBody(
        tabItems(
            # HOME
            tabItem(tabName = 'help', 
                    tags$iframe(src = './IntroText.html', # put myMarkdown.html to /www
                                width = '100%', height = '800px', 
                                frameborder = 0, scrolling = 'auto'
                    )),
            # MAP 
            tabItem(tabName = 'map',
                    fluidRow(
                        column(width = 3,
                               selectInput(inputId = 'eventlocs', label = 'Choose Case File(s) to Display on Map', choices = NULL, multiple = T)
                               )
                    ),
                    fluidRow(
                        box(leafletOutput(outputId = 'map', width = '100%', height = 700), width = "100%"),
                    )
                    ),
            # Timeline graph
            tabItem(tabName = 'timeline',
                    h3('Time Line of Cases'),
                    box(plotlyOutput(outputId = "time_graph", height = 800), width = '100%', height = 900)
                    ),
            # Database View and Edit
            tabItem(tabName = 'logindb',
                    # must turn shinyjs on
                    shinyjs::useShinyjs(),
                    # add logout button UI 
                    div(class = "pull-right", logoutUI(id = "logout")),
                    # add login panel UI function
                    loginUI(id = "login"),
                    # setup table output to show user info after login
                    textOutput(outputId = 'loggedin')
                    
                    ),
            # Database From Entry
            tabItem(tabName = 'formdb',
                    h3('Input Case File Information'),
                    fluidRow(
                        actionButton(inputId = 'addbutton_cf', label = 'Add'),
                        actionButton(inputId = 'editbutton_cf', label = 'Edit'),
                        actionButton(inputId = 'deletebutton_cf', label = 'Delete')
                    ),
                    fluidRow(
                        box(DT::DTOutput(outputId = 'formtable'), width = '100%')
                    ),
                    hr(),
                    h3('Input Event Locations'),
                    fluidRow(
                        column(width = 2, 
                               selectInput(inputId = 'casefileselect', label = 'Choose Case File Name to Add Locations', choices = NULL)
                               ),
                        column(width = 2,
                               numericInput(inputId = 'long', label = 'Longitude', value = 0)
                               ),
                        column(width = 2,
                               numericInput(inputId = 'lat', label = 'Latitude', value = 0)
                               ),
                        column(width = 2,
                               textInput(inputId = 'locname', label = 'Location Name')
                               ),
                        column(width = 2,
                               actionButton(inputId = 'addlocs', label = 'Add Location to Database', style = 'position:relative;top:25px')
                               )
                    ),
                    hr(),
                    fluidRow(
                        actionButton(inputId = 'editbutton_loc', label = 'Edit'),
                        actionButton(inputId = 'deletebutton_loc', label = 'Delete')
                    ),
                    fluidRow(
                        box(DT::DTOutput(outputId = 'loctable'), width = '100%')
                    ),
                    hr(),
                    h3('Input for Additional Persons of Interest'),
                    fluidRow(
                        column(width = 2, 
                               selectInput(inputId = 'casefileselectpeople', label = 'Choose Case File Name to Add People', choices = NULL)
                               ),
                        column(width = 2,
                               textInput(inputId = 'personsname', label = 'Persons Name')
                               ),
                        column(width = 2,
                               actionButton(inputId = 'addpeople', label = 'Add People to Database', style = 'position:relative;top:25px')
                               )
                    ),
                    hr(),
                    fluidRow(
                        actionButton(inputId = 'editbutton_people', label = 'Edit'),
                        actionButton(inputId = 'deletebutton_people', label = 'Delete')
                    ),
                    fluidRow(
                        box(DT::DTOutput(outputId = 'peopletable'), width = '100%')
                    ),
                    hr(),
                    h3('Input for Magnus Events'),
                    h5('Magnus events are events that occur during the recordings of the events.'),
                    fluidRow(
                        column(width = 2,
                               selectInput(inputId = 'mgeventid', label = 'Magnus Event', choices = NULL)),
                        column(width = 2,
                               textInput(inputId = 'mgeventname', label = 'Magnus Event Name')),
                        column(width = 2,
                               textAreaInput(inputId = 'mgeventnotes', label = 'Notes about Event')),
                        column(width = 2,
                               actionButton(inputId = 'addmgevent', label = 'Add Magnus Event to Databae', style = 'position:relative;top:25px'))
                    ),
                    hr(),
                    fluidRow(
                        actionButton(inputId = 'editbutton_mgevent', label = 'Edit'),
                        actionButton(inputId = 'deletebutton_mgevent', label = 'Delete')
                    ),
                    fluidRow(
                        box(DT::DTOutput(outputId = 'mgeventtable'), width = '100%')
                    ),
                    hr(),
                    h3('Input for Connected Cases'),
                    h5('Choose Case Files that are Connected to Other Cases.'),
                    fluidRow(
                        column(width = 3,
                               selectInput(inputId = 'con_id', label = 'Choose Case File Name to Add Connections', choices = NULL)),
                        column(width = 2,
                               selectInput(inputId = 'con_cas_numb', label = 'Choose Connecting Case File Number', choices = NULL)),
                        column(width = 2,
                               textInput(inputId = 'con_file_name', label = 'Connected Case Name'),
                               helpText("The Case File Name is here to help choose the correct file number, but not added to the database.")),
                        column(width = 2,
                               selectInput(inputId = 'con_cas_type', label = 'Choose the Type of Connection', choices = c('Strong', 'Weak', 'Possible'))),
                        column(width = 2,
                               actionButton(inputId = 'addconcase', label = 'Add Connected Cases to Database', style = 'position:relative;top:25px'))
                    ),
                    fluidRow(
                        actionButton(inputId = 'editbutton_concases', label = 'Edit'),
                        actionButton(inputId = 'deletebutton_concases', label = 'Delete')
                    ),
                    fluidRow(
                        box(DT::DTOutput(outputId = 'conncasetable'), width = '100%')
                    )
                    )
        )
    )
)


# Define server logic --------
server <- function(input, output, session) {
    
    # call the logout module with reactive trigger to hide/show
    logout_init <- callModule(shinyauthr::logout, 
                              id = "logout", 
                              active = reactive(credentials()$user_auth))
    # call login module supplying data frame, user and password cols and reactive trigger
    credentials <- callModule(shinyauthr::login, 
                              id = "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              log_out = reactive(logout_init()))
    
    # pulls out the user information returned from login module
    user_data <- reactive({credentials()$info})
    
    
    # check if log in was successful, if so activate the DB entry/edit tab
    output$dbmenuitem <- renderMenu({
        # check if user is authorized, if so allow to access database
        if (credentials()$user_auth == T){
            output$loggedin <- renderText("You have succesfully logged into the database")
            menuItem(text = 'Database', icon = icon('database'), startExpanded = F,
                     menuSubItem(text = 'Log In DB', tabName = 'logindb'),
                     menuSubItem(text = 'Data Input', tabName = 'formdb'))
        }else{
            menuItem(text = 'Database', icon = icon('database'), startExpanded = F,
                     menuSubItem(text = 'Log In DB', tabName = 'logindb'))
        }
        
    })
    
    
    
    # make tables global variables
    db_tables <- reactiveValues()
    
    # tables for replacing edits
    db_tables$main_table = fread(input = 'main_table.csv')
    db_tables$loc_table = fread(input = 'loc_table.csv')
    db_tables$people_table = fread(input = 'people_table.csv')
    db_tables$mgevent_table = fread(input = 'mgevent_table.csv')
    db_tables$concase_table = fread(input = 'concase_table.csv')
    
    # create tables
    output$formtable <- DT::renderDataTable(expr = db_tables$main_table, selection = 'single', rownames = F, options = list(scrollX = TRUE, pageLength = 5, autoWidth = T, columnDefs = list(list(width = '600px', targets = c(12)))))
    
    output$loctable <- DT::renderDataTable(expr = db_tables$loc_table, selection = 'single', options = list(scrollX = TRUE, pageLength = 5), rownames = F)
    
    output$peopletable <- DT::renderDataTable(expr = db_tables$people_table, selection = 'single', options = list(scrollX = TRUE, pageLength = 5), rownames = F)
    
    output$mgeventtable <- DT::renderDataTable(expr = db_tables$mgevent_table, options = list(scrollX = TRUE, pageLength = 5), rownames = F, selection = 'single')
    
    output$conncasetable <- DT::renderDataTable(expr = db_tables$concase_table, options = list(scrollX = TRUE, pageLength = 5), rownames = F, selection = 'single')
    
    # Set up Map section ---------
    observe({
        locs <- db_tables$loc_table
        main <- db_tables$main_table
        # merge add information to plot on map
        locs <- merge(x = locs, y = main[,.(Case.File.Number, Case.File.Name, URL, Episode.Number)], by = 'Case.File.Number')
        # add in popup title column
        locs$popuptitle <- locs[,paste(paste('Name:', paste0("<b><a href=","'",URL,"'>",Case.File.Name,"</a></b>")), paste('Number:',Case.File.Number), paste('Episode:', Episode.Number), Location.Name, sep = '<br>')] #
        # make spatial data for map
        if (is.null(input$eventlocs)){
            sp_events <- st_as_sf(x = locs, coords = c("Longitude", "Latitude"), crs = 'WGS84')
        }else{
            sp_events <- st_as_sf(x = locs[Case.File.Name %in% input$eventlocs], coords = c("Longitude", "Latitude"), crs = 'WGS84')
        }
       
        output$map <- renderLeaflet(expr = 
                                        leaflet(options = leafletOptions(zoomControl = F)) %>%
                                        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
                                        addTiles() %>%
                                        addMarkers(data = sp_events, popup = ~popuptitle)
        )
    })
    
    # Set up Timeline section
    observe({
        # check if no data in table first
        if (nrow(db_tables$main_table) == 0){
            output$time_graph <- NULL
        }else{
            # process the data for the timeline
            time_table <- db_tables$main_table
            
            graph <- plot_ly() %>% layout(yaxis = list(dtick = 1, title = "Episode Number"))
            
            for(i in 1:(nrow(time_table))){
                if(i %% 2 == 0){
                    col = 'rgb(88, 212, 237)' # teal
                }else{
                    col = 'rgb(145, 97, 35)' # brown
                }
                if(time_table[i,]$Date.Event.Ended - time_table[i,]$Date.Event.Started <= 10){
                    graph <- add_markers(p = graph, x = time_table[i,]$Date.Event.Started, y = time_table[i,]$Episode.Number, marker = list(color = col), name = time_table[i,]$Case.File.Name)
                }else{
                    graph <- add_trace(p = graph, x = c(time_table[i,]$Date.Event.Started, time_table[i,]$Date.Event.Ended), y = c(time_table[i,]$Episode.Number, time_table[i,]$Episode.Number), mode = 'lines', line = list(width = 20, color  = col), name = time_table[i,]$Case.File.Name) 
                    
                }
            }
            
            output$time_graph <- renderPlotly(expr = graph)
        }
        
    })
    
    # Set up DB global variables and initial tables
    observe({
        table = db_tables$main_table
        
        updateSelectInput(session, inputId = 'casefileselect', choices = table$Case.File.Name)
        
        updateSelectInput(session, inputId = 'eventlocs', choices = table$Case.File.Name, selected = NULL)
        
        updateSelectInput(session, inputId = 'casefileselectpeople', choices = table$Case.File.Name)
        
        updateSelectInput(session, inputId = 'mgeventid', choices = na.omit(table$Magnus.Event))
        
        updateSelectInput(session, inputId = 'con_id', choices = table$Case.File.Name)
        
        updateSelectInput(session, inputId = 'con_cas_numb', choices = table$Case.File.Number)
        
    })
    
    observeEvent(eventExpr = input$con_cas_numb, {
        # update the text to show the connected case
        updateTextInput(session, inputId = 'con_file_name', value = db_tables$main_table[Case.File.Number == input$con_cas_numb]$Case.File.Name)
    })
    
    # this is only used for the popup window box
    observeEvent(eventExpr = input$con_cas_numb_1, {
        # update the text to show the connected case
        updateTextInput(session, inputId = 'con_file_name_1', value = db_tables$main_table[Case.File.Number == input$con_cas_numb_1]$Case.File.Name)
    })

    # Case File Table DB ------
    # Add Button
    observeEvent(eventExpr = input$addbutton_cf, {
        # show popup window of inputs
        showModal(modalDialog(
            textInput(label = 'Case File Number', inputId = 'casenumber'),
            textInput(label = 'Case File Name', inputId = 'casefilename'),
            numericInput(label = 'Episode Number', inputId = 'episodenumber', min = 1, value = NULL, step = 1),
            dateInput(label = 'Date Statement Recorded', inputId = 'statedate'),
            dateInput(inputId = 'eventstartdate', label = 'Date Event Started'),
            dateInput(inputId = 'eventenddate', label = 'Date Event Ended'),
            helpText('If event ended on a single day, then set start and end data to the same day. If exact dates are not know, as in only the month is given, then set for the entire month'),
            checkboxInput(label = 'Exact Dates Known', inputId = 'exactdates'),
            textInput(inputId = 'providername', label = 'Statement Provider Name'),
            checkboxInput(inputId = 'alive', label = 'Statement Provider Alive', value = T),
            helpText('Check box for alive, blank for dead. If not stated, assume alive.'),
            dateInput(label = 'Magnus Date Recorded', inputId = 'magdaterec'),
            helpText('Date when statement was Re-Recorded'),
            numericInput(label = 'Magnus Event', inputId = 'mgevent', min = 0, value = NULL, step = 1),
            helpText('Magnus Event is a real time event occuring during the recording of the statments. One unique number ID will be used per event.'),
            textInput(inputId = 'url', label = 'Podcast URL'),
            textAreaInput(inputId = 'comment', label = 'Story Comments'),
            
            footer = tagList(
                modalButton("Cancel"),
                actionButton("add_cf", "Add")
            )
        ))
            
    })
    
    observeEvent(eventExpr = input$add_cf, {
        temp = data.table("Case.File.Number" = input$casenumber, "Case.File.Name" = input$casefilename, 'Episode.Number' = input$episodenumber, "Date.Statement.Recorded" = input$statedate, 'Date.Event.Started' = input$eventstartdate, 'Date.Event.Ended' = input$eventenddate, 'Exact.Dates.Known' = input$exactdates, "Statement.Provider.Name" = input$providername, "Statement.Provider.Alive" = input$alive, "Magnus.Recorded.Date" = input$magdaterec, "Magnus.Event" = as.numeric(input$mgevent), "URL" = input$url, "Comments" = input$comment)
        # check if table is empty, if so we overwrite, if not we append
        if (nrow(db_tables$main_table) == 0){
            # add data to table
            #dbWriteTable(conn = MADB, name = 'Case Files', value = temp, row.names = F, overwrite = T)
            #db_tables$main_table = as.data.table(dbReadTable(conn = MADB, name = 'Case Files'))
            db_tables$main_table = as.data.table(temp)
            removeModal()
        }else{
            # add data to table
            #dbWriteTable(conn = MADB, name = 'Case Files', value = temp, row.names = F, append = T)
            #db_tables$main_table = as.data.table(dbReadTable(conn = MADB, name = 'Case Files'))
            db_tables$main_table = rbind(db_tables$main_table, temp)
            removeModal()
        }
    })
    
    # Edit Button
    observeEvent(eventExpr = input$editbutton_cf, {
        # check if row is selected or not
        if (is.null(input$formtable_rows_selected)){
            return()
        }else{
            # store current row of data
            temp_store = db_tables$main_table[input$formtable_rows_selected,]
            # show popup window of inputs
            showModal(modalDialog(
                textInput(label = 'Case File Number', inputId = 'casenumber_1', value = temp_store$Case.File.Number),
                textInput(label = 'Case File Name', inputId = 'casefilename_1', value = temp_store$Case.File.Name),
                numericInput(label = 'Episode Number', inputId = 'episodenumber_1', min = 1, value = temp_store$Episode.Number, step = 1),
                dateInput(label = 'Date Statement Recorded', inputId = 'statedate_1', value = temp_store$Date.Statement.Recorded),
                dateInput(inputId = 'eventstartdate_1', label = 'Date Event Started', value = temp_store$Date.Event.Started),
                dateInput(inputId = 'eventenddate_1', label = 'Date Event Ended', value = temp_store$Date.Event.Ended),
                helpText('If event ended on a single day, then set start and end data to the same day. If exact dates are not know, as in only the month is given, then set for the entire month'),
                checkboxInput(label = 'Exact Dates Known', inputId = 'exactdates_1', value = temp_store$Exact.Dates.Known),
                textInput(inputId = 'providername_1', label = 'Statement Provider Name', value = temp_store$Statement.Provider.Name),
                checkboxInput(inputId = 'alive_1', label = 'Statement Provider Alive', value = temp_store$Statement.Provider.Alive),
                helpText('Check box for alive, blank for dead. If not stated, assume alive.'),
                dateInput(label = 'Magnus Date Recorded', inputId = 'magdaterec_1', value = temp_store$Magnus.Date.Recorded),
                helpText('Date when statement was Re-Recorded'),
                numericInput(label = 'Magnus Event', inputId = 'mgevent_1', min = 0, value = temp_store$Magnus.Event, step = 1),
                helpText('Magnus Event is a real time event occuring during the recording of the statments. One unique number ID will be used per event.'),
                textInput(inputId = 'url_1', label = 'Podcast URL', value = temp_store$URL),
                textAreaInput(inputId = 'comment_1', label = 'Story Comments', value = temp_store$Comments),
                
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("save_cf", "Save")
                )
            ))
            
        }
    })
    
    observeEvent(eventExpr = input$save_cf, {
        # overwrite the current table row with edits
        temp = data.table("Case.File.Number" = input$casenumber_1, "Case.File.Name" = input$casefilename_1, 'Episode.Number' = input$episodenumber_1, "Date.Statement.Recorded" = input$statedate_1, 'Date.Event.Started' = input$eventstartdate_1, 'Date.Event.Ended' = input$eventenddate_1, 'Exact.Dates.Known' = input$exactdates_1, "Statement.Provider.Name" = input$providername_1, "Statement.Provider.Alive" = input$alive_1, "Magnus.Recorded.Date" = input$magdaterec_1, "Magnus.Event" = as.numeric(input$mgevent_1), "URL" = input$url_1, "Comments" = input$comment_1)
        # Overwrite current row with new data
        db_tables$main_table[input$formtable_rows_selected,] <- temp
        # save current model to db
        #dbWriteTable(conn = MADB, name = 'Case Files', value = db_tables$main_table, row.names = F, overwrite = T)
        removeModal()
        
    })
    
    # Delete Button
    observeEvent(eventExpr = input$deletebutton_cf, {
        # check if row is selected or not
        if (is.null(input$formtable_rows_selected)){
            return()
        }else{
            # show popup window of inputs 
            showModal(modalDialog(
                title = 'Delete Row',
                "This will delete the Case File and all datasets associated with this case file. Such as locations and persons of interest.  This action can not be reversed, if you wish to proceed click the delete button below.",
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("delete_cf", "Delete")
                    )
                ))
        }
    })
    
    observeEvent(eventExpr = input$delete_cf, {
        # find associated tables with same case number and remove as well
        case_num = db_tables$main_table[input$formtable_rows_selected, Case.File.Number]
        MGEvent_num = db_tables$main_table[input$formtable_rows_selected, Magnus.Event]
        db_tables$loc_table <- db_tables$loc_table[!Case.File.Number == case_num]
        db_tables$people_table <- db_tables$people_table[!Case.File.Number == case_num]
        db_tables$mgeventtable <- db_tables$mgeventtable[!Magnus.Event == MGEvent_num]
        db_tables$concase_table <- db_tables$concase_table[!Case.File.Number == case_num]
        
        #dbWriteTable(conn = MADB, name = 'Locations', value = db_tables$loc_table, row.names = F, overwrite = T)
        #dbWriteTable(conn = MADB, name = 'People', value = db_tables$people_table, row.names = F, overwrite = T)
        #dbWriteTable(conn = MADB, name = 'Magnus Event', value = db_tables$mgeventtable, row.names = F, overwrite = T)
        #dbWriteTable(conn = MADB, name = 'Connected Cases', value = db_tables$concase_table, row.names = F, overwrite = T)
        
        db_tables$main_table <- db_tables$main_table[-input$formtable_rows_selected,]
        #dbWriteTable(conn = MADB, name = 'Case Files', value = db_tables$main_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    # update location table -------
    # Add data
    observeEvent(eventExpr = {input$addlocs},{
                 # first using the case file name, obtain the case file number
                 table = db_tables$main_table
                 case_numb = table[Case.File.Name == input$casefileselect]$Case.File.Number
                 
                 # create table
                 temp = data.table('Case.File.Number' = case_numb, "Latitude" = input$lat, 'Longitude' = input$long, 'Location.Name' = input$locname)
                 # add data to table
                 if (nrow(db_tables$loc_table) == 0){
                     #dbWriteTable(conn = MADB, name = 'Locations', value = temp, row.names = F, overwrite = T)
                     #db_tables$loc_table = as.data.table(dbReadTable(conn = MADB, name = 'Locations'))
                      db_tables$loc_table = temp
                 }else{
                     #dbWriteTable(conn = MADB, name = 'Locations', value = temp, row.names = F, append = T)
                     #db_tables$loc_table = as.data.table(dbReadTable(conn = MADB, name = 'Locations'))
                     db_tables$loc_table = rbind(db_tables$loc_table, temp)
                 }
                 
                 },ignoreNULL = T, ignoreInit = F)
    
    # Edit 
    observeEvent(eventExpr = input$editbutton_loc, {
        # check if row is selected or not
        if (is.null(input$loctable_rows_selected)){
            return()
        }else{
            # store current row of data
            temp_store = db_tables$loc_table[input$loctable_rows_selected,]
            # show popup window of inputs
            showModal(modalDialog(
                numericInput(inputId = 'long_1', label = 'Longitude', value = temp_store$Longitude),
                numericInput(inputId = 'lat_1', label = 'Latitude', value = temp_store$Latitude),
                textInput(inputId = 'locname_1', label = 'Location Name', value = temp_store$Location.Name),
                
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("save_loc", "Save")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$save_loc, {
        temp_store = db_tables$loc_table[input$loctable_rows_selected,]
        # overwrite the current table row with edits
        temp = data.table('Case.File.Number' = temp_store$Case.File.Number, "Latitude" = input$lat_1, 'Longitude' = input$long_1, 'Location.Name' = input$locname_1)
        # Overwrite current row with new data
        db_tables$loc_table[input$loctable_rows_selected,] <- temp
        # save current model to db
        #dbWriteTable(conn = MADB, name = 'Locations', value = db_tables$loc_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    # Delete
    observeEvent(eventExpr = input$deletebutton_loc, {
        # check if row is selected or not
        if (is.null(input$loctable_rows_selected)){
            return()
        }else{
            # show popup window of inputs 
            showModal(modalDialog(
                title = 'Delete Row',
                "This will delete the location. This action can not be reversed, if you wish to proceed click the delete button below.",
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("delete_loc", "Delete")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$delete_loc, {
        db_tables$loc_table <- db_tables$loc_table[-input$loctable_rows_selected,]
        #dbWriteTable(conn = MADB, name = 'Locations', value = db_tables$loc_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    # Update Additional People Tables -----
    # add data
    observeEvent(eventExpr = {input$addpeople}, {
        # get current table
        table = db_tables$main_table
        # get the case number from the case name
        case_numb = table[Case.File.Name == input$casefileselectpeople]$Case.File.Number
        # create table from inputs in form
        temp = data.table('Case.File.Number' = case_numb, 'Name' = input$personsname)
        # add table to database
        if (nrow(db_tables$people_table) == 0){
            #dbWriteTable(conn = MADB, name = 'People', value = temp, row.names = F, overwrite = T)
            #db_tables$people_table = as.data.table(dbReadTable(conn = MADB, name = 'People'))
            db_tables$people_table = temp
        }else{
            #dbWriteTable(conn = MADB, name = 'People', value = temp, row.names = F, append = T)
            #db_tables$people_table = as.data.table(dbReadTable(conn = MADB, name = 'People'))
            db_tables$people_table = rbind(db_tables$people_table, temp)
        }
        
    })
    
    # Edit
    observeEvent(eventExpr = input$editbutton_people, {
        # check if row is selected or not
        if (is.null(input$peopletable_rows_selected)){
            return()
        }else{
            # store current row of data
            temp_store = db_tables$people_table[input$peopletable_rows_selected,]
            # show popup window of inputs
            showModal(modalDialog(
                textInput(inputId = 'personsname_1', label = 'Persons Name', value = temp_store$Name),
                
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("save_people", "Save")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$save_people, {
        temp_store = db_tables$people_table[input$peopletable_rows_selected,]
        # create table from inputs in form
        temp = data.table('Case.File.Number' = temp_store$Case.File.Number, 'Name' = input$personsname_1)
        # Overwrite current row with new data
        db_tables$people_table[input$peopletable_rows_selected,] <- temp
        # save current model to db
        #dbWriteTable(conn = MADB, name = 'People', value = db_tables$people_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    # Delete
    observeEvent(eventExpr = input$deletebutton_people, {
        # check if row is selected or not
        if (is.null(input$peopletable_rows_selected)){
            return()
        }else{
            # show popup window of inputs 
            showModal(modalDialog(
                title = 'Delete Row',
                "This will delete the location. This action can not be reversed, if you wish to proceed click the delete button below.",
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("delete_people", "Delete")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$delete_people, {
        db_tables$people_table <- db_tables$people_table[-input$peopletable_rows_selected,]
        #dbWriteTable(conn = MADB, name = 'People', value = db_tables$people_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    
    # Update Magnus Events -----
    # add data
    observeEvent(eventExpr = {input$addmgevent}, {
        # create table from inputs in form
        temp = data.table('Magnus.Event' = input$mgeventid, 'Event.Name' = input$mgeventname, 'Notes' = input$mgeventnotes)
        # add table to database
        if (nrow(db_tables$mgevent_table) == 0){
            #dbWriteTable(conn = MADB, name = 'Magnus Event', value = temp, row.names = F, overwrite = T)
            #db_tables$mgevent_table = as.data.table(dbReadTable(conn = MADB, name = 'Magnus Event'))
            db_tables$mgevent_table = temp
        }else{
            #dbWriteTable(conn = MADB, name = 'Magnus Event', value = temp, row.names = F, append = T)
            #db_tables$mgevent_table = as.data.table(dbReadTable(conn = MADB, name = 'Magnus Event'))
            db_tables$mgevent_table = rbind(db_tables$mgevent_table, temp)
        }
    })
    
    # Edit
    observeEvent(eventExpr = input$editbutton_mgevent, {
        # check if row is selected or not
        if (is.null(input$mgeventtable_rows_selected)){
            return()
        }else{
            # store current row of data
            temp_store = db_tables$mgevent_table[input$mgeventtable_rows_selected,]
            # show popup window of inputs
            showModal(modalDialog(
                textInput(inputId = 'mgeventname_1', label = 'Magnus Event Name', value = temp_store$Event.Name),
                textInput(inputId = 'mgeventnotes_1', label = 'Notes about Event', value = temp_store$Notes),
                
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("save_mgevent", "Save")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$save_mgevent, {
        temp_store = db_tables$mgevent_table[input$mgeventtable_rows_selected,]
        # create table from inputs in form
        temp = data.table('Magnus.Event' = temp_store$Magnus.Event, 'Event.Name' = input$mgeventname_1, 'Notes' = input$mgeventnotes_1)
        # Overwrite current row with new data
        db_tables$mgevent_table[input$mgeventtable_rows_selected,] <- temp
        # save current model to db
        #dbWriteTable(conn = MADB, name = 'Magnus Event', value = db_tables$mgevent_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    # Delete
    observeEvent(eventExpr = input$deletebutton_mgevent, {
        # check if row is selected or not
        if (is.null(input$mgeventtable_rows_selected)){
            return()
        }else{
            # show popup window of inputs 
            showModal(modalDialog(
                title = 'Delete Row',
                "This will delete the location. This action can not be reversed, if you wish to proceed click the delete button below.",
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("delete_mgevent", "Delete")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$delete_mgevent, {
        db_tables$mgevent_table <- db_tables$mgevent_table[-input$mgeventtable_rows_selected,]
        #dbWriteTable(conn = MADB, name = 'Magnus Event', value = db_tables$mgevent_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    # Update Connected Cases Tables -----
    # add data
    observeEvent(eventExpr = {input$addconcase}, {
        case_numb = db_tables$main_table[Case.File.Name == input$con_id]$Case.File.Number
        # create table from inputs in form
        temp = data.table('Case.File.Number' = case_numb, 'Connected.Case.File.Number' = input$con_cas_numb, 'Connection.Type' = input$con_cas_type)
        # add table to database
        if (nrow(db_tables$concase_table) == 0){
            #dbWriteTable(conn = MADB, name = 'Connected Cases', value = temp, row.names = F, overwrite = T)
            #db_tables$concase_table = as.data.table(dbReadTable(conn = MADB, name = 'Connected Cases'))
            db_tables$concase_table = temp
        }else{
            #dbWriteTable(conn = MADB, name = 'Connected Cases', value = temp, row.names = F, append = T)
            #db_tables$concase_table = as.data.table(dbReadTable(conn = MADB, name = 'Connected Cases'))
            db_tables$concase_table = rbind(db_tables$concase_table, temp)
        }
    })
    
    # Edit
    observeEvent(eventExpr = input$editbutton_concases, {
        # check if row is selected or not
        if (is.null(input$conncasetable_rows_selected)){
            return()
        }else{
            # store current row of data
            temp_store = db_tables$concase_table[input$conncasetable_rows_selected,]
            # show popup window of inputs
            showModal(modalDialog(
                selectInput(inputId = 'con_id_1', label = 'Choose Case File Name to Add Connections', choices = db_tables$main_table$Case.File.Number, selected = temp_store$Case.File.Number),
                selectInput(inputId = 'con_cas_numb_1', label = 'Choose Connecting Case File Number', choices = db_tables$main_table$Case.File.Number, selected = temp_store$Connected.Case.File.Number),
                textInput(inputId = 'con_file_name_1', label = 'Connected Case Name'),
                helpText('The Case File Name is here to help choose the correct file number, but not added to the database.'),
                selectInput(inputId = 'con_cas_type_1', label = 'Choose the Type of Connection', choices = c('Strong', 'Weak', 'Possible'), selected = temp_store$Connection.Type),
                
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("save_concase", "Save")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$save_concase, {
        temp_store = db_tables$concase_table[input$conncasetable_rows_selected,]
        # create table from inputs in form
        temp = data.table('Case.File.Number' = input$con_id_1, 'Connected.Case.File.Number' = input$con_cas_numb_1, 'Connection.Type' = input$con_cas_type_1)
        # Overwrite current row with new data
        db_tables$concase_table[input$conncasetable_rows_selected,] <- temp
        # save current model to db
        #dbWriteTable(conn = MADB, name = 'Connected Cases', value = db_tables$concase_table, row.names = F, overwrite = T)
        removeModal()
    })
    
    # Delete
    observeEvent(eventExpr = input$deletebutton_concases, {
        # check if row is selected or not
        if (is.null(input$conncasetable_rows_selected)){
            return()
        }else{
            # show popup window of inputs 
            showModal(modalDialog(
                title = 'Delete Row',
                "This will permanently delete the selected row. This action can not be reversed, if you wish to proceed click the delete button below.",
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("delete_concases", "Delete")
                )
            ))
        }
    })
    
    observeEvent(eventExpr = input$delete_concases, {
        db_tables$concase_table <- db_tables$concase_table[-input$conncasetable_rows_selected,]
        #dbWriteTable(conn = MADB, name = 'Connected Cases', value = db_tables$concase_table, row.names = F, overwrite = T)
        removeModal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
