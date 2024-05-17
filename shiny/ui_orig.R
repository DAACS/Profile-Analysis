shiny::navbarPage(
    title = 'DAACS Profile Analysis',

    shiny::tabPanel(
        title = 'Cluster Analysis',
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                width = 3,
                shiny::sliderInput(inputId = "k",
                            label = "Number of clusters:",
                            min = 2,
                            max = 10,
                            value = 4),

                shiny::uiOutput('variable_selection')
            ),
            shiny::mainPanel(
                width = 9,
                shiny::textOutput('n_message'),
                shiny::tabsetPanel(
                    shiny::tabPanel(
                        title = 'Clusters',
                        shiny::plotOutput('n_clusters_plot'),
                        shiny::hr(),
                        shiny::plotOutput('cluster_size_bar', height = '300px')

                    ),

                    shiny::tabPanel(
                        title = 'Variable Plots',
                        shiny::plotOutput('variable_cluster_plot', height = '600px')

                    ),

                    shiny::tabPanel(
                        title = 'Pairs Plot',
                        shiny::plotOutput('cluster_pairs_plot', height = '600px')
                    ),

                    shiny::tabPanel(
                        title = 'Cluster Plots',
                        shiny::plotOutput('bivariate_cluster_plot'),
                        shiny::plotOutput('discriminant_projection_plot')
                    )
                )

            )
        )
    )

    # shiny::tabPanel(
    #     title = 'Topic Modeling',
    #     shiny::sidebarLayout(
    #         shiny::sidebarPanel(
    #             width = 3,
    #             shiny::numericInput(
    #                 inputId = 'min_words',
    #                 label = 'Minimum Word Frequency',
    #                 min = 1, max = 20, step = 1, value = 2
    #             ),
    #             shiny::numericInput(
    #                 inputId = 'n_topics',
    #                 label = 'Number of topics:',
    #                 min = 2, max = 100, step = 1, value = 2
    #             ),
    #             shiny::numericInput(
    #                 inputId = 'n_terms_preview',
    #                 label = 'Number of terms to preview:',
    #                 min = 10, max = 1000, step = 10, value = 40
    #             ),
    #             shiny::uiOutput('topic_selector')
    #         ),
    #         shiny::mainPanel(
    #             width = 9,
    #             shiny::tabsetPanel(
    #                 shiny::tabPanel(
    #                     title = 'Topics',
    #                     shiny::plotOutput('n_topics_plot')
    #                 ),
    #                 shiny::tabPanel(
    #                     title = 'Terms',
    #                     shiny::dataTableOutput('terms_table')
    #                 ),
    #                 shiny::tabPanel(
    #                     title = 'Word Clouds',
    #                     shiny::plotOutput('word_cloud', height = '600px')
    #                 )
    #             )
    #         )
    #     )
    # )
)
