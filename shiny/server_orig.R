function(input, output, session) {
    get_data_raw <- shiny::reactive({
        daacs.wgu
    })

    get_data <- shiny::reactive({
        # TODO: all for selecting different institutions
        daacs <- get_data_raw() |>
            dplyr::filter(Treat) |>
            dplyr::select(input$variable_selection) %>%
            dplyr::filter(complete.cases(.)) |>
            dplyr::mutate_if(is.numeric, scale_this)
        return(daacs)
    })

    ##### Cluster Analysis #####################################################
    get_cluster_fit <- shiny::reactive({
        req(input$k)
        req(input$variable_selection)

        daacs <- get_data()
        fit <- kmeans(daacs, input$k)
        return(fit)
    })

    output$n_message <- shiny::renderText({
        daacs <- get_data()
        return(paste0(prettyNum(nrow(daacs), big.mark = ','),
                      ' observations used from ',
                      prettyNum(nrow(get_data()), big.mark = ','),
                      ' total available (',
                      round(nrow(daacs) / nrow(get_data()) * 100, 2), '%).'))
    })

    output$variable_selection <- shiny::renderUI({
        daacs <- get_data_raw()
        shiny::selectizeInput(
            inputId = 'variable_selection',
            label = 'Variables to include',
            choices = names(daacs),
            multiple = TRUE,
            selected = default_vars
        )
    })

    output$n_clusters_plot <- shiny::renderPlot({
        req(input$variable_selection)
        daacs <- get_data()
        wss <- (nrow(daacs) - 1) * sum(apply(daacs, 2, var))
        for(i in 2:10) { # Calculate within sum of quares for up to 10 clusters
            wss[i] <- sum(kmeans(daacs, centers = i)$withinss)
        }
        plot(1:10, wss, type = 'b',
             xlab = 'Number of Clusters',
             ylab = 'Within group sum of squares')
    })

    output$cluster_size_bar <- shiny::renderPlot({
        fit <- get_cluster_fit()
        daacs <- get_data()
        daacs$cluster <- factor(fit$cluster, labels = letters[1:input$k])
        ggplot(daacs, aes(x = cluster,
                         label = scales::percent(prop.table(stat(count))))) +
            geom_bar() +
            geom_text(stat = 'count',
                      position = position_dodge(.9),
                      vjust = -0.5,
                      size = 4) +
            ggtitle('Cluster Sizes')
    })

    output$variable_cluster_plot <- shiny::renderPlot({
        fit <- get_cluster_fit()
        daacs <- get_data()
        daacs$cluster <- factor(fit$cluster, labels = letters[1:input$k])

        daacs.melted <- melt(daacs, id.vars = 'cluster')
        tab <- describeBy(daacs.melted$value,
                          group = list(daacs.melted$cluster, daacs.melted$variable),
                          mat = TRUE)
        tab <- tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
        names(tab)[1:2] <- c('Cluster', 'Factor')

        ggplot(tab[order(tab$Factor),],
               aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
            geom_path(alpha = 0.5) +
            geom_point() +
            geom_errorbar(aes(ymin = mean - se_bar_multiplier * se, ymax = mean + se_bar_multiplier * se),
                          width = 0.25, alpha = 0.5) +
            geom_text(aes(label = round(mean, digits = 2)), hjust = -0.5, size = 4) +
            xlab('Scale') + ylab('Mean Standardized Scale Score')

    })

    output$cluster_pairs_plot <- shiny::renderPlot({
        fit <- get_cluster_fit()
        daacs <- get_data()
        daacs$cluster <- factor(fit$cluster, labels = letters[1:input$k])

        ggpairs(daacs,
                columns = names(daacs),
                axisLabels = 'show',
                mapping = ggplot2::aes_string(color='cluster'),
                # upper='blank',
                upper = list(continuous = "cor"),
                diag = list(continuous = "densityDiag", discrete = 'barDiag',
                            aes_string(group = 'cluster', fill = 'cluster')),
                lower = list(continuous="density", combo="box", alpha = 0.2))

    })

    output$bivariate_cluster_plot <- shiny::renderPlot({
        fit <- get_cluster_fit()
        daacs <- get_data()
        daacs$cluster <- factor(fit$cluster, labels = letters[1:input$k])

        cluster::clusplot(daacs,
                          daacs$cluster,
                          color = TRUE,
                          shade = TRUE,
                          labels = 2,
                          lines = 0,
                          main = 'Bivariate Cluster Plot')
    })

    output$discriminant_projection_plot <- shiny::renderPlot({
        fit <- get_cluster_fit()
        daacs <- get_data()
        daacs$cluster <- factor(fit$cluster, labels = letters[1:input$k])

        fpc::plotcluster(daacs[,names(daacs) != 'cluster'],
                         daacs$cluster,
                         main = 'Discriminant projection plot')
    })

    ##### Regression Analysis ##################################################




    ##### Topic Modeling #######################################################
    get_text_data <- shiny::reactive({
        essays <- essays.wgu
        textdata <- essays[,c('DAACS_ID', 'essay')]
        names(textdata) <- c('doc_id', 'text')
        return(textdata)
    })

    get_topic_model <- shiny::reactive({
        DTM <- get_corpus()
        topicModel <- topicmodels::LDA(x = DTM,
                                       k = input$n_topics,
                                       method = "Gibbs",
                                       control = list(iter = 500, verbose = 0))
        return(topicModel)
    })

    get_corpus <- shiny::reactive({
        req(input$min_words)

        textdata <- get_text_data()
        corpus <- tm::Corpus(tm::DataframeSource(textdata))
        # Preprocessing chain
        processedCorpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
        processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, stop_words)
        processedCorpus <- tm::tm_map(processedCorpus, tm::removePunctuation, preserve_intra_word_dashes = TRUE)
        processedCorpus <- tm::tm_map(processedCorpus, tm::removeNumbers)
        processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "en")
        processedCorpus <- tm::tm_map(processedCorpus, tm::stripWhitespace)

        DTM <- tm::DocumentTermMatrix(processedCorpus,
                                      control = list(bounds = list(global = c(input$min_words, Inf))))

        rowTotals <- apply(DTM , 1, sum) #Find the sum of words in each Document
        warning(paste0('Removing ', sum(rowTotals == 0), ' essays since they do not contain any terms.'))
        DTM   <- DTM[rowTotals >  0, ]           #remove all docs without words

        return(DTM)
    })

    output$n_topics_plot <- shiny::renderPlot({
        req(input$n_topics)

        cache_file <- paste0('cache/FindTopicsNumber_results.Rda')
        if(file.exists(cache_file)) {
            message(paste0('Loading ', cache_file, ' from cache...'))
            load(cache_file)
        } else {
            DTM <- get_corpus()
            result <- ldatuning::FindTopicsNumber(
                DTM,
                topics = seq(from = 2, to = 20, by = 1),
                metrics = c("CaoJuan2009", "Deveaud2014"),
                method = "Gibbs",
                control = list(seed = 2112),
                verbose = FALSE
            )
            save(result, file = cache_file)
        }

        ldatuning::FindTopicsNumber_plot(result)
    })

    output$terms_table <- shiny::renderDataTable({
        topicModel <- get_topic_model()
        topicmodels::terms(topicModel, input$n_terms_preview)
    })

    output$topic_selector <- shiny::renderUI({
        topics <- 1:input$n_topics
        names(topics) <- paste0('Topic ', 1:input$n_topics)
        shiny::selectInput(
            inputId = 'topic_to_preview',
            label = 'Topic to visualize:',
            choices = topics
        )
    })

    output$word_cloud <- shiny::renderPlot({
        topicModel <- get_topic_model()
        tmResult <- modeltools::posterior(topicModel)

        topicToViz <- input$topic_to_preview
        top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:input$n_terms_preview]
        words <- names(top40terms)
        # extract the probabilites of each of the 40 terms
        probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:input$n_terms_preview]
        # visualize the terms as wordcloud
        mycolors <- brewer.pal(8, "Dark2")
        wordcloud(words, probabilities, random.order = FALSE, color = mycolors)
    })
}
