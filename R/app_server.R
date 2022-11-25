#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import rintrojs
#' @import shinydashboardPlus
#' @import visNetwork
#' @noRd
#' 
app_server <- function(db){
  server <- function(input, output, session) {
    shinyhelper::observe_helpers(help_dir = app_sys("app/doc/"))
    
    ## data  ===================
    dict.combine <- getData(NULL, "dict", db)
    dict.combine <- dict.combine[, !colnames(dict.combine) %in% c("term_s", "stype_s")]
    ids <- unique(dict.combine$id)
    if(sum(!grepl("\\w", dict.combine$group, perl = TRUE)) > 0){
      dict.combine$group[!grepl("\\w", dict.combine$group, perl = TRUE)] <- "Unknown"
    }
    
    tb_input <- dict.combine[, c("id", "term")]
    
    node_num_cutoff = 500
    
    winsize <- windowSizeServer("win")
    
    headerServer("btn", steps[, -1], app_sys("app/doc/documentation.md"), df_edges_cutted())
    
    directed = FALSE
    
    # center_nodes ====
    center_nodes <- sidebarServer("side", tb_input, tname = "dict", db, type = 2, 
                                  # selected = c(2, 4, 8, 12),
                                  selected = c(4, 5),
                                  # init_nodes = c("C0003873", "C0409637", "PheCode:714.1", "PheCode:714.2"),
                                  init_nodes = c("PheCode:714.1", "PheCode:714.2"),
                                  server = TRUE)
    
    output$centernodes <- renderText({
      center_nodes()
    })
    
    name_input <- reactive({
      paste(gsub("[^\\w]", "_", center_nodes(), perl = TRUE), collapse = "_")
    })
    
    observeEvent(center_nodes(), {
      top_n <- 500
      max_nodes <- min(length(center_nodes()) * top_n, nrow(df_edges_center()))
      thr_cos <- floor(sort(df_edges_center()$weight, decreasing = TRUE)[max_nodes]*100)/100
      max = ceiling(max(df_edges_center()$weight)*100)/100
      min = floor(min(df_edges_center()$weight)*100)/100
      
      df <- df_edges_center()[df_edges_center()$weight >= thr_cos, ]
      categories <- sort(unique(df$category))
      
      # if(input$controlbarMenu == "Network"){
      #   updateControlbarMenu(session = session, id = "controlbarMenu", selected = "Filter Nodes")
      # }
      print("====min====max====value====")
      print(max)
      print(min)
      print(c(thr_cos, max))
      if(is.na(thr_cos)){
        thr_cos <- min
      }
      output$ui_filter <- renderUI({
        tagList(
          sliderInput(
            inputId = paste0(name_input(), "-filter_cos"),
            label = "Filter nodes by cosine similarity",
            min = min, max = max,
            value = c(thr_cos, max),
            step = 0.01,
            width = "100%"
          ),
          shinyWidgets::pickerInput(
            inputId = paste0(name_input(), "-filter_category"),
            label = "Filter nodes by category:",
            choices = categories,
            selected = categories,
            options = list(
              `actions-box` = TRUE),
            multiple = TRUE,
            width = "100%"
          )
        )
      })
    })
    
    categories <- reactive({
      req(input[[paste0(name_input(), "-filter_cos")]])
      thr_cos <- input[[paste0(name_input(), "-filter_cos")]]
      df <- df_edges_center()[df_edges_center()$weight >= thr_cos, ]
      sort(unique(df$category))
    })
    
    observeEvent(categories(), {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = paste0(name_input(), "-filter_category"),
        label = "Filter nodes by category:",
        choices = categories(),
        selected = categories(),
        options = list(
          `actions-box` = TRUE)
      )
    })
    
    df_edges_cutted <- reactive({
      req(input[[paste0(name_input(), "-filter_cos")]])
      req(input[[paste0(name_input(), "-filter_category")]])
      # print("df_edges_cutted")
      # print(nrow(df_edges_center()))
      df <- df_edges_center()[df_edges_center()$weight >= input[[paste0(name_input(), "-filter_cos")]][1], ]
      print(nrow(df))
      # print(nrow(df[df$category %in% input$filter_category, ]))
      df[df$category %in% input[[paste0(name_input(), "-filter_category")]], ]
    })
    
    df_edges_center <- reactive({
      print("df_edges")
      req(center_nodes())
      print(center_nodes())
      df <- Reduce(rbind, lapply(center_nodes(), getData, "df_edges", db))
      df$category <- dict.combine$category[match(df$to, dict.combine$id)]
      print(table(df$category))
      df
    })
    
    
    ## network  ====================================
      output$ui_network <- renderUI({
        req(winsize()[2])
        # req(input$filter_cos)
        # req(input$filter_category)
        if(length(center_nodes()) > 0 & (nrow(df_edges_cutted()) > 0)){
          print("ui_network")
          print(nrow(df_edges_cutted()))
          print(center_nodes())
          shinycssloaders::withSpinner(
            visNetworkOutput("network",
                             height =  paste0((winsize()[2]-100),"px")),
            type = 6
          )
        } else {
          h3("Try select some rows and click on submit")
        }
      })
      
      output$network <- renderVisNetwork({
        print("********************network**************")
        # myconfirmation = input$myconfirmation
        print(center_nodes())
        if(length(center_nodes()) > 0 | (isTruthy(df_edges_cutted()))){
          req(picked_colors())
          req(picked_shapes())
          df <- df_edges_cutted()
          plot_network(center_nodes(), df_edges_cutted(), 
                       dict.combine, attrs, picked_colors(), picked_shapes(),
                       hide_labels = FALSE, 
                       directed = directed,
                       node_num_cutoff = 500, 
                       layout = "layout_nicely")
        }
      })
    
    selected_id = reactive({
      print("selected_id")
      input$current_node_id$nodes[[1]]
    })
    
    # node info ====
    observeEvent(input$current_node_id, {
      if (!is.null(selected_id())){
        toggleModal(session, "selectednode", toggle = "open")
        df <- getData(selected_id(), "df_edges", db)
        v_cos = df$weight
        print("openBS_nodeinfo")
        print(length(v_cos))
        t_cos = sort(v_cos, decreasing = TRUE)[min(length(v_cos), 100)]
        ifelse(is.na(t_cos), min(v_cos), t_cos)
        updateSliderInput(
          inputId = "cutoff_ind",
          # label = "Filter edges by cosine similarity (above):",
          min = 0.1, value = c(t_cos,1), max = 1,
          step = 0.01
        )
      }
    })
    
    # Clicked node text
    output$clicked_node_title <- renderUI({
      h3(dict.combine$term[match(selected_id(), dict.combine$id)])
    })
    output$clicked_node_info <- renderUI({
      print("clicked_node_info")
      clickedNodeText(selected_id(), dict.combine)
    })
  
    ## df plots  ===================================
    df_plots <- reactive({
      print("df_plots")
      if (!is.null(selected_id())){
        df <- getData(selected_id(), "df_edges", db)
        df[df$weight >= input$cutoff_ind[1],]
        # df_edges[df_edges$from == selected_id() | df_edges$to == selected_id(), ]
      }
    })
  
    ## sunburst =======================================
    output$ui_sun <- renderUI({
      if(nrow(df_plots()) > 0){
        shinycssloaders::withSpinner(
          plotly::plotlyOutput("sun",width="auto",
                               height="750px"), type = 6
        )} else {
          ""
        }
    })
  
    output$sun <- plotly::renderPlotly({
      print("sunburst")
      sunburstPlotly(selected_id(), df_plots(),
                     # input$changeline, input$rotatelabel, input$scale_sungh,
                     dict.combine)
    })
    
    ## circular plot  =======================================

    output$circularplot <- renderUI({
      if(nrow(df_plots()) > 0){
        shinycssloaders::withSpinner(
          plotOutput("circular", width = "100%",
                     height = paste0(max(700, winsize()[2] - 450),"px")), type = 6
        )} else {
          ""
        }
    })
    output$circular <- renderPlot({
      print("circular")
      node_now = selected_id()
      circularBar(df_plots(), dict.combine, ColorsCirc)
    })
    
    # table of clicked node  ================================
    # tbClickedServer("tb1", df_plots(), paste0(winsize()[2] - 400,"px"), dict.combine)
    
    output$clicked_node_table <- renderUI({
      if(nrow(df_plots()) > 0){
        shinycssloaders::withSpinner(
          reactable::reactableOutput("tb_clicked_node", width = "100%",
                                     height = paste0(winsize()[2] - 400,"px")), type = 6
        )} else {
          ""
        }
    })
    
    df_clicked_node <- reactive({
      df <- df_plots()
      colnames(df) <- c("center_nodes", "connected_nodes", "cosine_similarity")
      df <- left_join(df,
                      dict.combine[, c("id", "term", "category")],
                      by = c("connected_nodes" = "id"))
      df$cosine_similarity <- round(df$cosine_similarity, 3)
      df[order(df$cosine_similarity, decreasing = TRUE), c(1, 2, 5, 4, 3)]
    })
    
    
    output$tb_clicked_node <- reactable::renderReactable(
      reactable::reactable({ df_clicked_node() },
                           groupBy = c("center_nodes"),
                           columns = list(
                             cosine_similarity = reactable::colDef(name = "cosine similarity"),
                             connected_nodes = reactable::colDef(
                               minWidth = 250,
                               name = "connected_nodes / term",
                               # Show species under character names
                               cell = function(value, index) {
                                 term <- df_clicked_node()$term[index]
                                 term <- if (!is.na(term)) term else "Unknown"
                                 div(
                                   div(style = list(fontWeight = 600), value),
                                   div(style = list(fontSize = 12), term)
                                 )
                               }
                             ),
                             term = reactable::colDef(show = FALSE)
                           ),
                           bordered = TRUE,
                           defaultExpanded = TRUE,
                           pagination = FALSE
      )
    )
    
    
    
    ## ui details  ===================================================
    output$ui_details <- renderUI({
      if(selected_id() %in% phecode$Phecode){
        phe_id <- gsub(".+:", "", selected_id(), perl = TRUE)
        href <- paste0("https://hmsrsc.aws.hms.harvard.edu/content/89/?phecode=", phe_id)
        tags$a(span(icon("hand-point-right"), "Phecode map to ICD"), 
               href = href, target = "_blank", style = "color: darkblue")

      } else {
      outdiv <- tagList()
      tbs <- getData(NULL, "details", db)
      if(!is.null(tbs)) {
      # tbs <- rbind(tbs, data.frame(tname = "synonyms", title = "Synonyms", note = "Synonyms"))
      print(tbs)
      sy <- "synonyms" %in% tbs$tname
      print(sy)
      
        apply(tbs, 1, function(x){
          tname = x[1]
          title = x[2]
          helps = ifelse(!is.null(x) & length(x) == 3, x[3], "")
          print(tname)
          df <- getData(selected_id(), tname, db, field = "id")
          print(head(df))
          if(nrow(df) > 0){
            outdiv <<- detailsTab(tname, df[df$id == selected_id(),], title, outdiv, selected_id(), output, sy, helps)
          }
        })
        if (length(outdiv) > 0){
          outdiv
        } else {
          h4("No more details.")
        } } else {
          h4("No more details.")
        }
      
      }
      
    })
    
    # color picker  =================================
    
    output$ui_color <- renderUI({
      n <- 1
      colors_group <- NULL
      print("ui_color")
      print(ColorsNet$group)
      print(unique(dict.combine$group))
      lapply(sort(unique(dict.combine$group)), function(x){
        if(!x %in% ColorsNet$group){
          c <- sample(setdiff(colors, c(ColorsNet$color.background, colors_group$color.background)), 1)
          colors_group <<- rbind(colors_group, data.frame("group"=x,
                                                          "color.background"=c))
        } else {
          colors_group <<- rbind(colors_group, ColorsNet[ColorsNet$group == x,])
        }
        column(6, colorpickerUI(x, colors_group$color.background[colors_group$group == x]))
      })
    })
    
    picked_colors <- reactive({
      updateControlbarMenu("controlbarMenu", selected = "Network")
      c <- sapply(sort(unique(dict.combine$group)), colorpickerServer)
      req(c[[1]])
      print("picked_colors")
      print(c)
      data.frame("group" = names(c), "color.background" = c)
    })
    
    # shape picker ====
    output$ui_shape <- renderUI({
      n <- 1
      shape_group <- NULL
      print("ui_shape")
      types <- sort(unique(dict.combine$type))
      lapply(1:length(types), function(x){
        shape_group <<- rbind(shape_group, data.frame("type"=types[x],
                                                      "shape"=shapes[x]))
        column(6, shapepickerUI(types[x], shapes[x]))
      })
    })
    
    picked_shapes <- reactive({
      updateControlbarMenu("controlbarMenu", selected = "Network")
      c <- sapply(sort(unique(dict.combine$type)), shapepickerServer)
      req(c[[1]])
      print("picked_shapes")
      print(c)
      data.frame("type" = names(c), "shape" = c)
    })
  }
  return(server)
}