server <- function(input, output) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  
  output$menu <- renderMenu({4
    sidebarMenu(
      menuItem(paste("All data were most recently updated on the release of COSMIC v3.1."), 
               icon = icon("calendar"))
    )
  })
  
  
  observeEvent(input$mysidebar, {
    # for desktop browsers
    addClass(selector = "body", class = "sidebar-collapse")
    # for mobile browsers
    removeClass(selector = "body", class = "sidebar-open")
  })
  
  ###########
  # Page 1-v3
  ###########
  corr_mat_1_v3 <- reactive({
    return(corr_mat_v3[[as.character(input$method_1_v3)]])
  })
  
  index_v3 <- reactive({
    as.character(input$N_F_v3)
  })
  
  indexS_v3 <- reactive({
    as.numeric(stringr::str_extract(input$N_S_D_v3,"\\d+"))
  })
  
  output$selected_sig_1_v3 <- renderPlot({
    visPMS_full_modified(sig_full_v3[, index_v3()], 3, FALSE)
  })
  
  output$corrplot1_2_v3 <- renderPlot({
    corrplot(cosmic_corr_v3[index_v3(), , drop = FALSE],
             is.corr = FALSE, bg = "#F8F8FF",
             col = myCol(200), tl.col = "black",
             tl.cex = 0.8, cl.pos = "n", cl.lim = c(0, 1)
    )
  })
  
  output$mytable1_v3 <- renderDataTable({
    table <- cbind(
      `COSMIC v3` = index_v3(),
      `pmsignature` = paste0("P", 1:dim(corr_mat_1_v3())[1]),
      `Similarity` = round(corr_mat_1_v3()[, index_v3()], 3)
    )
    table <- table[order(table[, 3], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  }) 
  
  output$corrplot1_1_v3 <- renderPlot({
    corrplot(t(corr_mat_1_v3())[index_v3(), , drop = FALSE],
             is.corr = FALSE,
             tl.col = "black", method = "shade", col = col(200),
             p.mat = t(corr_mat_1_v3())[index_v3(), , drop = FALSE] %>% round(1),
             insig = "p-value", tl.srt = 0, tl.offset = 1,
             tl.cex = 1.2, cl.pos = "n"
    )
  })
  
  output$selected1_v3_1 <- renderValueBox({
    valueBox(
      paste0(index_v3()), "COSMIC signature",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$selected1_v3_2 <- renderValueBox({
    valueBox(
      paste0("P", indexS_v3()), "pmsignature input",
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  output$selected_sig_text_1_v3 <- renderText({
    HTML(paste0(
      "<b>Type:</b> COSMIC signature ", index_v3(), "</br>",
      "<b>", "Cancer Membership:</b> ", paste(names(which(cosmic_corr_v3[index_v3(), ] > 0)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_1_v3 <- renderPlot({
    visPMS_full_modified(sig_full_v3[, index_v3()], 3, FALSE)
  })
  
  # the most similar signature
  output$selected_sig_text_1_v3_1 <- renderText({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v3())[index_v3(), ], decreasing = TRUE)[1:1])))
    HTML(paste(
      "<b>Type:</b> pmsignature ", paste0("P", rank), "</br>",
      "<b>Similarity(highest):</b> ", t(corr_mat_1_v3())[index_v3(), rank] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(pm_corr[rank, ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_pm_full_1_v3_1 <- renderPlot({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v3())[index_v3(), ], decreasing = TRUE)[1:1])))
    visPMS_full_modified(convertSignatureMatrixToVector(Fs[[rank]], c(6, 4, 4)), 3, FALSE)
  })
  
  output$selected_sig_full_1_v3_1 <- renderPlot({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v3())[index_v3(), ], decreasing = TRUE)[1:1])))
    pmsignature:::visPMS_ind(Fs[[rank]], 5, isScale = TRUE)
  })
  
  # self-defined signature
  output$highest_v3 <- renderValueBox({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v3())[index_v3(), ], decreasing = TRUE)[1:1])))
    valueBox(
      paste0("P", rank), "Most similar pmsignature", icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$selected_sig_text_1_v3_2 <- renderText({
    HTML(paste(
      "<b>Type:</b> pmsignature ", paste0("P", indexS_v3()), "</br>",
      "<b>Similarity(selected):</b> ", t(corr_mat_1_v3())[index_v3(), indexS_v3()] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(pm_corr[indexS_v3(), ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_pm_full_1_v3_2 <- renderPlot({
    visPMS_full_modified(convertSignatureMatrixToVector(Fs[[indexS_v3()]], c(6, 4, 4)), 3, FALSE)
  })
  
  output$selected_sig_full_1_v3_2 <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[indexS_v3()]], 5, isScale = TRUE)
  })
  
  ###########
  # Page 1-v2
  ###########
  corr_mat_1_v2 <- reactive({
    return(corr_mat_v2[[as.character(input$method_1_v2)]])
  })
  
  index_v2 <- reactive({
    as.numeric(stringr::str_extract(input$N_F_v2,"\\d+"))
  })
  
  indexS_v2 <- reactive({
    as.numeric(stringr::str_extract(input$N_S_D_v2,"\\d+"))
  })
  
  output$selected_sig_1_v2 <- renderPlot({
    visPMS_full_modified(sig_full_v2[, index_v2() + 3], 3, FALSE)
  })
  
  output$corrplot1_2_v2 <- renderPlot({
    corrplot(cosmic_corr_v2[index_v2(), , drop = FALSE],
             is.corr = FALSE, bg = "#F8F8FF",
             col = myCol(200), tl.col = "black",
             tl.cex = 0.9, cl.pos = "n", cl.lim = c(0, 1)
    )
  })
  
  output$mytable1_v2 <- renderDataTable({
    table <- cbind(
      `COSMIC v2` = paste0("C", index_v2()),
      `pmsignature` = paste0("P", 1:dim(corr_mat_1_v2())[1]),
      `Similarity` = round(corr_mat_1_v2()[, index_v2()], 3)
    )
    table <- table[order(table[, 3], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  }) 
  
  output$corrplot1_1_v2 <- renderPlot({
    corrplot(t(corr_mat_1_v2())[index_v2(), , drop = FALSE],
             is.corr = FALSE,
             tl.col = "black", method = "shade", col = col(200),
             p.mat = t(corr_mat_1_v2())[index_v2(), , drop = FALSE] %>% round(1),
             insig = "p-value", tl.srt = 0, tl.offset = 1,
             tl.cex = 1.2, cl.pos = "n"
    )
  })
  
  output$selected1_v2_1 <- renderValueBox({
    valueBox(
      paste0("C", index_v2()), "COSMIC signature",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$selected1_v2_2 <- renderValueBox({
    valueBox(
      paste0("P", indexS_v2()), "pmsignature input",
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  output$selected_sig_text_1_v2 <- renderText({
    HTML(paste0(
      "<b>Type:</b> COSMIC signature C", index_v2(), "</br>",
      "<b>", "Cancer Membership:</b> ", paste(names(which(cosmic_corr_v2[index_v2(), ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_1_v2 <- renderPlot({
    visPMS_full_modified(sig_full_v2[, index_v2() + 3], 3, FALSE)
  })
  
  # the most similar signature
  output$selected_sig_text_1_v2_1 <- renderText({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v2())[index_v2(), ], decreasing = TRUE)[1:1])))
    HTML(paste(
      "<b>Type:</b> pmsignature ", paste0("P", rank), "</br>",
      "<b>Similarity(highest):</b> ", t(corr_mat_1_v2())[index_v2(), rank] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(pm_corr[rank, ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_pm_full_1_v2_1 <- renderPlot({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v2())[index_v2(), ], decreasing = TRUE)[1:1])))
    visPMS_full_modified(convertSignatureMatrixToVector(Fs[[rank]], c(6, 4, 4)), 3, FALSE)
  })
  
  output$selected_sig_full_1_v2_1 <- renderPlot({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v2())[index_v2(), ], decreasing = TRUE)[1:1])))
    pmsignature:::visPMS_ind(Fs[[rank]], 5, isScale = TRUE)
  })
  
  # self-defined signature
  output$highest_v2 <- renderValueBox({
    rank <- as.numeric(gsub("[^0-9.]", "", names(sort(t(corr_mat_1_v2())[index_v2(), ], decreasing = TRUE)[1:1])))
    valueBox(
      paste0("P", rank), "Most similar pmsignature", icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })

  output$selected_sig_text_1_v2_2 <- renderText({
    HTML(paste(
      "<b>Type:</b> pmsignature ", paste0("P", indexS_v2()), "</br>",
      "<b>Similarity(selected):</b> ", t(corr_mat_1_v2())[index_v2(), indexS_v2()] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(pm_corr[indexS_v2(), ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_pm_full_1_v2_2 <- renderPlot({
    visPMS_full_modified(convertSignatureMatrixToVector(Fs[[indexS_v2()]], c(6, 4, 4)), 3, FALSE)
  })
  
  output$selected_sig_full_1_v2_2 <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[indexS_v2()]], 5, isScale = TRUE)
  })
  
  
  ###########
  # Page 2-v2
  ###########
  corr_mat_2_v2 <- reactive({
    return(corr_mat_v2[[as.character(input$method_2_v2)]])
  })
  
  index2_v2 <- reactive({
    as.numeric(stringr::str_extract(input$N_D_v2,"\\d+"))
  })
  
  indexS2_v2 <- reactive({
    as.numeric(stringr::str_extract(input$N_D_S_v2,"\\d+"))
  })
  
  rank1_v2 <- reactive({
    as.numeric(gsub("[^0-9.]", "", names(sort(corr_mat_2_v2()[index2_v2(), ], decreasing = TRUE)[1:1])))
  })
  
  output$selected_sig_2_v2 <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[index2_v2()]], 5, isScale = TRUE)
  })

  output$corrplot2_v2_1 <- renderPlot({
    corrplot(pm_corr[index2_v2(), , drop = FALSE],
             is.corr = FALSE, bg = "#F8F8FF",
             col = myCol(200), tl.col = "black",
             tl.cex = 1.2, cl.pos = "n"
    )
  })
  
  output$mytable2_v2 <- renderDataTable({
    table <- cbind(
      `pmsignature` = paste0("P", index2_v2()),
      `COSMIC v2 Signature` = paste0("C", 1:dim(corr_mat_2_v2())[2]),
      `Similarity` = round(corr_mat_2_v2()[index2_v2(), ], 3)
    )
    rownames(table) <- NULL
    table <- table[order(table[, 3], decreasing = TRUE), ]
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  })

  output$corrplot2_v2_2 <- renderPlot({
    corrplot(corr_mat_2_v2()[index2_v2(), , drop = FALSE],
             is.corr = FALSE,
             tl.col = "black", method = "shade", col = col(200),
             p.mat = corr_mat_2_v2()[index2_v2(), , drop = FALSE] %>% round(1),
             insig = "p-value", tl.srt = 0, tl.offset = 1,
             tl.cex = 1.2, cl.pos = "n"
    )
  })
  
  # Page 1: first two rows
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

  output$selected2_v2_1 <- renderValueBox({
    valueBox(
      paste0("P", index2_v2()), "pmsignature",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$highest2_v2 <- renderValueBox({
    valueBox(
      paste0("C", rank1_v2()), "Most simliar COSMIC", icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$selected2_v2_2 <- renderValueBox({
    valueBox(
      paste0("C", indexS2_v2()), "COSMIC input",
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  output$selected_sig_text_v2_2 <- renderText({
    HTML(paste0(
      "<b>Type:</b> pmsignature P", index2_v2(), "</br>",
      "<b>", "Cancer Membership:</b> ", paste(names(which(pm_corr[index2_v2(), ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_v2_2 <- renderPlot({
    visPMS_full_modified(convertSignatureMatrixToVector(Fs[[index2_v2()]], c(6, 4, 4)), 3, FALSE)
  })
  
  output$selected_sig_2_v2_1 <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[index2_v2()]], 5, isScale = TRUE)
  })
  
  # the most similar signature
  output$selected_sig_text_2_v2_1 <- renderText({
    HTML(paste(
      "<b>Type:</b> COSMIC signature ", paste0("C", rank1_v2()), "</br>",
      "<b>Similarity(highest):</b> ", corr_mat_2_v2()[index2_v2(), rank1_v2()] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(cosmic_corr_v2[rank1_v2(), ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_2_v2_1 <- renderPlot({
    visPMS_full_modified(sig_full_v2[, rank1_v2() + 3], 3, FALSE)
  })

  output$selected_sig_text_2_v2_2 <- renderText({
    HTML(paste(
      "<b>Type:</b> COSMIC signature ", paste0("C", indexS2_v2()), "</br>",
      "<b>Similarity(selected):</b> ", corr_mat_2_v2()[index2_v2(), indexS2_v2()] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(cosmic_corr_v2[indexS2_v2(), ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_2_v2_2 <- renderPlot({
    visPMS_full_modified(sig_full_v2[, indexS2_v2() + 3], 3, FALSE)
  })

  ###########
  # Page 2-v3
  ###########
  corr_mat_2_v3 <- reactive({
    return(corr_mat_v3[[as.character(input$method_2_v3)]])
  })
  
  index2_v3 <- reactive({
    as.numeric(stringr::str_extract(input$N_D_v3,"\\d+"))
  })
  
  indexS2_v3 <- reactive({
    as.character(input$N_D_S_v3)
  })
  
  rank1_v3 <- reactive({
    as.character(names(sort(corr_mat_2_v3()[index2_v3(), ], decreasing = TRUE)[1:1]))
  })
  
  output$selected_sig_2_v3 <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[index2_v3()]], 5, isScale = TRUE)
  })
  
  output$corrplot2_v3_1 <- renderPlot({
    corrplot(pm_corr[index2_v3(), , drop = FALSE],
             is.corr = FALSE, bg = "#F8F8FF",
             col = myCol(200), tl.col = "black",
             tl.cex = 1.2, cl.pos = "n"
    )
  })
  
  output$mytable2_v3 <- renderDataTable({
    table <- cbind(
      `pmsignature` = paste0("P", index2_v3()),
      `COSMIC v3.1 Signature` = colnames(corr_mat_2_v3()),
      `Similarity` = round(corr_mat_2_v3()[index2_v3(), ], 3)
    )
    rownames(table) <- NULL
    table <- table[order(table[, 3], decreasing = TRUE), ]
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  })
  
  output$corrplot2_v3_2 <- renderPlot({
    corrplot(corr_mat_2_v3()[index2_v3(), , drop = FALSE],
             is.corr = FALSE,
             tl.col = "black", method = "shade", col = col(200),
             insig = "p-value", tl.offset = 1,
             tl.cex = 0.8, cl.pos = "n"
    )
  })
  
  # Page 1: first two rows
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  output$selected2_v3_1 <- renderValueBox({
    valueBox(
      paste0("P", index2_v3()), "pmsignature",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$highest2_v3 <- renderValueBox({
    valueBox(
      paste0(rank1_v3()), "Most simliar COSMIC", icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$selected2_v3_2 <- renderValueBox({
    valueBox(
      paste0("C", indexS2_v3()), "COSMIC input",
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  output$selected_sig_text_v3_2 <- renderText({
    HTML(paste0(
      "<b>Type:</b> pmsignature P", index2_v3(), "</br>",
      "<b>", "Cancer Membership:</b> ", paste(names(which(pm_corr[index2_v3(), ] == 1)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_v3_2 <- renderPlot({
    visPMS_full_modified(convertSignatureMatrixToVector(Fs[[index2_v3()]], c(6, 4, 4)), 3, FALSE)
  })
  
  output$selected_sig_2_v3_1 <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[index2_v3()]], 5, isScale = TRUE)
  })
  
  # the most similar signature
  output$selected_sig_text_2_v3_1 <- renderText({
    HTML(paste(
      "<b>Type:</b> COSMIC signature ", paste0(rank1_v3()), "</br>",
      "<b>Similarity(highest):</b> ", corr_mat_2_v3()[index2_v3(), rank1_v3()] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(cosmic_corr_v3[rank1_v3(), ] > 0)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_2_v3_1 <- renderPlot({
    visPMS_full_modified(sig_full_v3[, rank1_v3()], 3, FALSE)
  })
  
  output$selected_sig_text_2_v3_2 <- renderText({
    HTML(paste(
      "<b>Type:</b> COSMIC signature ", paste0(indexS2_v3()), "</br>",
      "<b>Similarity(selected):</b> ", corr_mat_2_v3()[index2_v3(), indexS2_v3()] %>% round(3), "</br>", "</b>",
      "<b>", "Cancer Membership:", "</b>",
      paste(names(which(cosmic_corr_v3[indexS2_v3(), ] > 0)), collapse = ", ")
    ))
  })
  
  output$selected_sig_full_2_v3_2 <- renderPlot({
    visPMS_full_modified(sig_full_v3[, indexS2_v3()], 3, FALSE)
  })
  

  ########
  # Page 3
  ########
  # input file 
  fu_vector <- reactive({
    file1 <- input$file1
    
    if (is.null(file1))
      return(sig_full_v2[,4])
    
    read.table(file=file1$datapath, header = input$header1)[,1]
  })
  
  # input file plot
  output$similar_full <- renderPlot({
    visPMS_full_modified(fu_vector(), 3, FALSE)
  })

  # correlation with v2
  corr_full_v2 <- reactive({
    corr_full_v2 <- rep(NA, dim(sig_full_v2)[2] - 3)
    for (i in 1:(dim(sig_full_v2)[2] - 3)) {
      corr_full_v2[i] <- getCosDistance(fu_vector(), sig_full_v2[, i + 3])
    }
    corr_full_v2
  })

  # correlation with pm
  corr_vec_full_pm<- reactive({
    corr_vec_full_pm_v2 <- rep(NA, length(Fs))
    for (i in 1:length(Fs)) {
      input_sig <- convertSignatureMatrixToVector(Fs[[i]][-6, ], c(6, 4, 4))
      corr_vec_full_pm_v2[i] <- getCosDistance(fu_vector(), input_sig)
    }
    corr_vec_full_pm_v2
  })
  
  # correlation with v3
  corr_full_v3 <- reactive({
    corr_full_v3 <- rep(NA, dim(sig_full_v3)[2] - 2)
    for (i in 1:(dim(sig_full_v3)[2] - 2)) {
      corr_full_v3[i] <- getCosDistance(fu_vector(), sig_full_v3[, i + 2])
    }
    corr_full_v3
  })

  # table with v2
  output$fu_table_v2 <- renderDataTable({
    table <- cbind(
      `COSMIC signature` = paste0("C", 1:length(corr_full_v2())),
      `Cosine Similarity` = round(corr_full_v2(), 3)
    )
    table <- table[order(table[, 2], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  }, server = TRUE)

  # table with pm
  output$fu_table_pm <- renderDataTable({
    table <- cbind(
      `pmsignature` = paste0("P", 1:length(corr_vec_full_pm())),
      `Cosine Similarity` = round(corr_vec_full_pm(), 3)
    )
    table <- table[order(table[, 2], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  }, server = TRUE)
  
  # table with v3
  output$fu_table_v3 <- renderDataTable({
    table <- cbind(
      `COSMIC signature` = colnames(sig_full_v3)[-c(1:2)],
      `Cosine Similarity` = round(corr_full_v3(), 3)
    )
    table <- table[order(table[, 2], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  }, server = TRUE)  
  
  # text with v2
  output$fu_text_v2 <- renderText({
    if (is.null(input$fu_table_rows_selected)){
      paste0("COSMIC signature C", which.max(corr_full_v2()))
    }
  })
  
  # text with pm
  output$fu_text_pm <- renderText({
    paste0("pmsignature P", which.max(corr_vec_full_pm()))
  })
  
  # text with v3
  output$fu_text_v3 <- renderText({
    if (is.null(input$fu_table_rows_selected)){
      paste0("COSMIC signature ", colnames(sig_full_v3)[which.max(corr_full_v3())+2])
    }
  })
  
  # plot with v2
  output$fu_plot_v2<- renderPlot({
    visPMS_full_modified(sig_full_v2[, which.max(corr_full_v2()) + 3], 3, FALSE)
  })
  
  # plot with pm
  output$fu_plot_pm <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[which.max(corr_vec_full_pm())]][-6, ], 5, isScale = TRUE)
  })
  
  # plot with v3
  output$fu_plot_v3<- renderPlot({
    visPMS_full_modified(sig_full_v3[, which.max(corr_full_v3()) + 2], 3, FALSE) 
  })

  # box with v2
  output$fu_box_v2 <- renderValueBox({
    valueBox(
      max(corr_full_v2()) %>% round(3) %>% sprintf("%1.3f", .),
      paste0("Similarity"),
      color = "blue"
    )
  })
  
  # box with pm
  output$fu_box_pm <- renderValueBox({
    valueBox(
      max(corr_vec_full_pm()) %>% round(3) %>% sprintf("%1.3f", .),
      paste0("Similarity"),
      color = "green"
    )
  })
  
  # box with v3
  output$fu_box_v3 <- renderValueBox({
    valueBox(
      max(corr_full_v3()) %>% round(3) %>% sprintf("%1.3f", .),
      paste0("Similarity"),
      color = "aqua"
    )
  })
  
  # sig text 1 (v2 only)
  output$selected_sig_text_fu_v2 <- renderText({
    HTML(paste0(
      names(which(cosmic_corr_v2[which.max(corr_full_v2()), ] == 1)), collapse = ", "
    ))
  })

  # sig text 2 (pm only)
  output$selected_sig_text_fu_pm <- renderText({
    HTML(paste(
      names(which(pm_corr[which.max(corr_vec_full_pm()), ] == 1)), collapse = ", "
    ))
  })
  
  # sig text 3 (v3 only)
  output$selected_sig_text_fu_v3 <- renderText({
    HTML(paste0(
      names(which(cosmic_corr_v3[which.max(corr_full_v3()), ] > 0)), collapse = ", "
    ))
  })

  ##########
  # Page 4
  ##########
  
  # input file
  pm_vector <- reactive({
    file2 <- input$file2
    
    if (is.null(file2))
      return(Fs[[1]][1:5,])
    
    read.table(file=file2$datapath, header = input$header2, sep = ",")
  })
  
  # input file plot
  output$similar_pm <- renderPlot({
    pmsignature:::visPMS_ind(as.matrix(pm_vector()),
      5, isScale = TRUE
    )
  })
  
  # correlation with v2 
  corr_vec_v2 <- reactive({
    corr_vec_v2 <- rep(NA, dim(sig_full_v2)[2] - 3)
    for (i in 1:(dim(sig_full_v2)[2] - 3)) {
      input_sig <- convertSignatureMatrixToVector(as.matrix(pm_vector()), c(6, 4, 4))
      corr_vec_v2[i] <- getCosDistance(input_sig, sig_full_v2[, i + 3])
    }
    corr_vec_v2
  })
  
  # correlation with v2
  corr_vec_pm <- reactive({
    corr_vec_pm <- rep(NA, length(Fs))
    for (i in 1:length(Fs)) {
      full_sig <- convertSignatureMatrixToVector(Fs[[i]], c(6, 4, 4, 4, 4))
      input_sig <- convertSignatureMatrixToVector(as.matrix(pm_vector()), c(6, 4, 4, 4, 4))
      corr_vec_pm[i] <- getCosDistance(input_sig, full_sig)
    }
    corr_vec_pm
  })

  # correlation with v3
  corr_vec_v3 <- reactive({
    corr_vec_v3 <- rep(NA, dim(sig_full_v3)[2] - 2)
    for (i in 1:(dim(sig_full_v3)[2] - 2)) {
      input_sig <- convertSignatureMatrixToVector(as.matrix(pm_vector()), c(6, 4, 4))
      corr_vec_v3[i] <- getCosDistance(input_sig, sig_full_v3[, i + 2])
    }
    corr_vec_v3
  })

  # table with v2
  output$pm_table_v2 <- renderDataTable({
    table <- cbind(
      `COSMIC signature` = paste0("C", 1:length(corr_vec_v2())),
      `Cosine Similarity` = round(corr_vec_v2(), 3)
    )
    table <- table[order(table[, 2], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  })
  
  # table with pm
  output$pm_table_pm <- renderDataTable({
    table <- cbind(
      `pmsignature` = paste0("P", 1:length(corr_vec_pm())),
      `Cosine Similarity` = round(corr_vec_pm(), 3)
    )
    table <- table[order(table[, 2], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  })
  
  # table with v3
  output$pm_table_v3 <- renderDataTable({
    table <- cbind(
      `COSMIC signature` = colnames(sig_full_v3)[-c(1:2)],
      `Cosine Similarity` = round(corr_vec_v3(), 3)
    )
    table <- table[order(table[, 2], decreasing = TRUE), ]
    rownames(table) <- NULL
    datatable(table, options = list(
      pageLength = 3, searching = FALSE,
      pagingType = "simple", lengthChange = FALSE
    )) %>%
      formatStyle(columns = 1:3, "text-align" = "center")
  })
  
  # box with v2
  output$pm_box_v2 <- renderValueBox({
    valueBox(
      max(corr_vec_v2()) %>% round(3) %>% sprintf("%1.3f", .),
      paste0("Similarity"),
      color = "blue"
    )
  })

  # box with pm
  output$pm_box_pm <- renderValueBox({
    valueBox(
      max(corr_vec_pm()) %>% round(3) %>% sprintf("%1.3f", .),
      paste0("Similarity"),
      color = "green"
    )
  })

  # box with v3
  output$pm_box_v3 <- renderValueBox({
    valueBox(
      max(corr_vec_v3()) %>% round(3) %>% sprintf("%1.3f", .),
      paste0("Similarity"),
      color = "aqua"
    )
  })
  
  # text with v2
  output$pm_text_v2 <- renderText({
    paste0("COSMIC signature C", which.max(corr_vec_v2()))
  })

  # text with pm
  output$pm_text_pm <- renderText({
    paste0("pmsignature P", which.max(corr_vec_pm()))
  })
  
  # text with v3
  output$pm_text_v3 <- renderText({
    paste0("COSMIC signature ", colnames(sig_full_v3)[which.max(corr_vec_v3())+2])
  })

  # plot with v2
  output$pm_plot_v2 <- renderPlot({
    visPMS_full_modified(sig_full_v2[, which.max(corr_vec_v2()) + 3], 3, FALSE)
  })
  
  # plot with pm
  output$pm_plot_pm <- renderPlot({
    pmsignature:::visPMS_ind(Fs[[which.max(corr_vec_pm())]][-6, ], 5, isScale = TRUE)
  })

  # plot with v3
  output$pm_plot_v3 <- renderPlot({
    visPMS_full_modified(sig_full_v3[, which.max(corr_vec_v3()) + 2], 3, FALSE)
  })

  # sig text with v2
  output$selected_sig_text_pm_v2 <- renderText({
    HTML(paste(
      names(which(cosmic_corr_v2[which.max(corr_vec_v2()), ] == 1)), collapse = ", "
    ))
  })
  
  # sig text with pm
  output$selected_sig_text_pm_pm <- renderText({
    HTML(paste0(
      names(which(pm_corr[which.max(corr_vec_pm()), ] == 1)), collapse = ", "
    ))
  })
  
  # sig text with v3
  output$selected_sig_text_pm_v3 <- renderText({
    HTML(paste(
      names(which(cosmic_corr_v3[which.max(corr_vec_v3()), ] > 0)), collapse = ", "
    ))
  })
  
  # download
  output$downloadData <- downloadHandler(
    filename = function() {
      "cosmic_signature_sample.csv"
    },
    content = function(file) {
      file.copy("data/cosmic_signature_sample.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "pmsignature_sample.csv"
    },
    content = function(file) {
      file.copy("data/pmsignature_sample.csv", file)
    },
    contentType = "text/csv"
  )
  
  version <- reactive({
    as.character(input$heatmap)
  })
  
  corr_mat_pg5 <- reactive({
    if(version() == "v2"){
      corr_mat <- corr_mat_v2
    } else {
      corr_mat <- corr_mat_v3
    }
    return(corr_mat[[as.character(input$method_5)]])
  })
  
  output$heatmap_v <- d3heatmap::renderD3heatmap({
    d3heatmap::d3heatmap(
      corr_mat_pg5(),
      colors = "Blues",
      Rowv = NA, Colv = NA
    )
  })
  
  output$heatmap_dynamic <- renderUI({
    width <- ifelse(version()=="v2", "600px", "1200px")
    d3heatmap::d3heatmapOutput("heatmap_v", width = width,
                    height = "500px")
  })
  
}

