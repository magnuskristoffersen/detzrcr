ui <- shiny::fluidPage(shiny::tabsetPanel(

  # Start of data input tab
  shiny::tabPanel('Data Input', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::conditionalPanel(
        condition = 'input.example_data == false',
        shiny::fileInput('file1', 'Select CSV File',
                         accept=c('text/csv',
                                  'text/comma-separated-values,text/plain',
                                  '.csv'), multiple = TRUE),
        shiny::tags$hr(),
        shiny::radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ','),
        shiny::radioButtons('quote', 'Quote',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"'),
        shiny::tags$hr()),
      shiny::checkboxInput('disc',
                           label = 'Remove discordant data',
                           value = FALSE),
      shiny::uiOutput('show_disc_limit'),
      shiny::tags$hr(),
      shiny::checkboxInput('example_data', 'Display example data', value=FALSE)
    ),
    shiny::mainPanel(
      shiny::tableOutput('head')
    )
  )),

  # Start of density distribution tab
  shiny::tabPanel('Density Distribution ', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons('type', 'Density distribution type',
                   c(KDE='kde',
                     PDP='pdd')),
      shiny::checkboxInput('hist', label = 'Histogram', value = TRUE),
      selectInput('dens_type', 'Plot type',
                  c('All samples in one'='dens_facet',
                    'Individual samples'='dens_ind',
                    'Combine samples'='dens_combine')),
      shiny::uiOutput('dens_switch'),
      shiny::numericInput('binwidth', 'Binwidth', 50),
      shiny::numericInput('bw', "Bandwidth", 30),
      shiny::sliderInput("xlim", "X-axis range (Ma)",
                  min = 0, max = 4560, value = c(200, 4000)),
      shiny::numericInput('xstep', 'X step', 200),
      shiny::numericInput('densWidth', 'Image Width (cm)', 15),
      shiny::numericInput('densHeight', 'Image Height (cm)', 15),
      shiny::downloadButton('downloadDensplot', 'Save Image')),
    shiny::mainPanel(
      shiny::plotOutput('dens_plot')
    )
  )),

  # Start of ECDF tab
  shiny::tabPanel('ECDF', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons('ecdf_input_type', 'Type',
                          c('Age'='age',
                            'Model age'='t_dm2')),
      selectInput('ecdf_type', 'Plot type',
                  c('All samples in one'='same_plot',
                    'Individual samples'='ind_plot',
                    'Combine samples'='ecdf_combine_plot')),
      shiny::uiOutput("ecdf_switch"),
      shiny::checkboxInput('ecdf_conf', label='Confidence bands', value=FALSE),
      shiny::sliderInput("ecdf_xlim", "X-axis range (Ma)",
                  min = 0, max = 4560, value = c(200, 4000)),
      shiny::numericInput('ecdf_xstep', 'X step', 200),
      shiny::checkboxInput("ecdf_legend", label = "Show legend", value = TRUE),
      shiny::numericInput('ecdf_width', 'Image Width (cm)', 15),
      shiny::numericInput('ecdf_height', 'Image Height (cm)', 15),
      shiny::downloadButton('download_ecdf_plot', 'Save Image')
    ),
    shiny::mainPanel(
      shiny::plotOutput(('ecdf_plot'))
    )
  )),

  # Start of UQ vs. tLQ tab
  shiny::tabPanel('UQ vs. LQ', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons('uqlq_type', 'UQ vs. LQ type',
                          c('Age'='uqlq_age',
                            'Model age'='uqlq_tdm')),
      shiny::uiOutput('uqlq_samples'),
      shiny::sliderInput('uqlq_xlim', 'X-axis range (Ma)',
                         min = 0, max = 4560, value = c(200, 4000)),
      shiny::sliderInput('uqlq_ylim', 'Y-axis range (Ma)',
                         min = 0, max = 4560, value = c(200, 4000)),
      shiny::numericInput('uqlq_xstep', 'X step', 500),
      shiny::checkboxInput('uqlq_conf',
                           label='Confidence limits',
                           value=FALSE),
      shiny::checkboxInput('mixing_model',
                           label='Add mixing model',
                           value=FALSE),
      shiny::conditionalPanel(
        condition="input.mixing_model == true",
        shiny::numericInput('mu1', 'First mean', value=500),
        shiny::numericInput('sig1', 'First standard deviation',
                            value=50),
        shiny::numericInput('mu2', 'Second mean', value=1000),
        shiny::numericInput('sig2', 'Second standard deviation',
                            value=100)),
      shiny::tags$hr(),
      shiny::checkboxInput("uqlq_legend", label = "Show legend", value = TRUE),
      shiny::numericInput('uqlq_width', 'Image Width (cm)', 15),
      shiny::numericInput('uqlq_height', 'Image Height (cm)', 15),
      shiny::downloadButton('download_uqlq_plot', 'Save Image'),
      shiny::tags$hr()
    ),
    shiny::mainPanel(
      shiny::plotOutput(('uqlq'))
    )
  )),

  # Start of Hf tab
  shiny::tabPanel('Lu-Hf', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons('hf_type', 'Type',
                          c('Epsilon Hf'='ehf_plot',
                            'Hf/Hf'='hfhf_plot')),
      shiny::uiOutput('hf_samples'),
      shiny::sliderInput('hf_xlim', 'X-axis range (Ma)',
                         min = 0, max = 4560, value = c(200, 4000)),
      shiny::numericInput('hf_xstep', 'X step', 200),
      shiny::uiOutput("hf_switch"),
      shiny::tags$hr(),
      shiny::checkboxInput('error_bars',
                           label = 'Add error bars',
                           value = FALSE),
      shiny::conditionalPanel(
        condition = 'input.error_bars == true',
        shiny::checkboxInput('x_error_bars',
                             label = 'X-direction error bars',
                             value = TRUE)
      ),
      shiny::tags$hr(),
      shiny::checkboxInput('add_contours',
                           label = 'Add contours',
                           value = FALSE),
      shiny::uiOutput('hf_bandwidths'),
      shiny::uiOutput('contour_switch'),
      shiny::tags$hr(),
      shiny::checkboxInput("hf_legend", label = "Show legend", value = TRUE),
      shiny::numericInput('hf_width', 'Image Width (cm)', 15),
      shiny::numericInput('hf_height', 'Image Height (cm)', 15),
      shiny::downloadButton('download_hf_plot', 'Save Image'),
      shiny::tags$hr(),
      shiny::downloadButton('download_hf_table', 'Save Lu-Hf Table')
    ),
    shiny::mainPanel(
      shiny::plotOutput(('hf'))
    )
  )),

  # Start of likeness tab
  shiny::tabPanel('Likeness', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons('likeness_type', 'Type',
                          c('1d (age)'='1d',
                            '2d (age and eHf)'='2d',
                            'Combine'='combine')),
      shiny::uiOutput('likeness_samples'),
      shiny::numericInput('likeness_age_bw', 'Age bandwidth', 30),
      shiny::uiOutput('likeness_bw'),
      shiny::downloadButton('download_likeness_table', 'Save Table')
    ),
    shiny::mainPanel(
      shiny::tableOutput('likeness')
    )
  )),

  # Start of O-parameter age tab
  shiny::tabPanel('1-O', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons('o_type', 'Type',
                          c('Age'='age',
                            'Model age'='tdm',
                            'Combine' ='combine')),
      shiny::uiOutput('o_samples'),
      shiny::downloadButton('download_o_table', 'Save Table'),
      shiny::tags$hr(),
      shiny::checkboxInput("o_fig", label = "Graphical", value = FALSE),
      shiny::conditionalPanel(
        condition = 'input.o_fig == true',
        shiny::numericInput('o_width', 'Image Width (cm)', 15),
        shiny::numericInput('o_height', 'Image Height (cm)', 15),
        shiny::downloadButton('download_o_plot', 'Save Image')
      )
    ),
    shiny::mainPanel(
      shiny::uiOutput('o_switch')
    )
  )),

  # Start of Reimink tab
  shiny::tabPanel('Reimink', shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::uiOutput('reimink_samples'),
      shiny::numericInput('reimink_step', 'Chord step (My)', 100),
      shiny::tags$hr(),
      shiny::numericInput('reimink_width', 'Image Width (cm)', 15),
      shiny::numericInput('reimink_height', 'Image Height (cm)', 15),
      shiny::downloadButton('download_reimink_plot', 'Save Image')
    ),
    shiny::mainPanel(
      shiny::plotOutput('reimink_plot'),
      shiny::tags$hr(),
      shiny::tags$p('Lower: '),
      shiny::verbatimTextOutput('reimink_maxima_lower'),
      shiny::tags$p('Upper: '),
      shiny::verbatimTextOutput('reimink_maxima_upper'),
      shiny::tags$hr(),
      shiny::downloadButton('download_reimink_table', 'Save likelihood data')
    )
  )),

  # Start of Constants tab
  shiny::tabPanel('Constants', shiny::fluidPage(
    shiny::fluidRow(
      column(4,
             shiny::numericInput('lambda_lu',
                                 '176Lu decay constant',
                                 lambda_lu),
             shiny::tags$hr(),
             shiny::numericInput('luhf_chur',
                                 '176Lu/177Hf CHUR',
                                 luhf_chur),
             shiny::numericInput('hfhf_chur',
                                 '176Hf/177Hf CHUR',
                                 hfhf_chur),
             shiny::tags$hr(),
             shiny::numericInput('luhf_dm',
                                 '176Lu/177Hf DM',
                                 luhf_dm),
             shiny::numericInput('hfhf_dm',
                                 '176hf/177Hf DM',
                                 hfhf_dm),
             shiny::numericInput('luhf_zrc',
                                 '176Lu/177Hf',
                                 luhf_zrc)
      )
    )
  )),
  # Start of Plot options tab
  shiny::tabPanel('Plot options', shiny::fluidPage(
    shiny::fluidRow(
      column(4,
             shiny::selectInput('font_name', 'Font',
                                c('Helvetica', 'Courier', 'Times')),
             shiny::tags$hr(),
             shiny::sliderInput('title_size', 'Axes title size (pts)',
                                min = 5, max = 20, value = 10),
             shiny::sliderInput('label_size', 'Axes label size (pts)',
                                min = 5, max = 20, value = 7),
             shiny::sliderInput('legend_size', 'Legend text size (pts)',
                                min = 5, max = 20, value = 10),
             shiny::sliderInput('strip_size', 'Panel text size (pts)',
                                min = 5, max = 20, value = 7),
             shiny::tags$hr()
      )
    )
  ))
  ))

server <- shiny::shinyServer(function(input, output) {
  # Reactives
  csv_data <- shiny::reactive({
    if (input$example_data == FALSE) {
      inFile <- input$file1
      if (is.null(inFile)) {
        return(NULL)
      }
      n <- dim(inFile)[1]
      if (n == 1) {
        dat <- utils::read.csv(inFile$datapath, header=TRUE, sep=input$sep,
                               quote=input$quote)
      }
      if (n > 1) {
        dat <- utils::read.csv(inFile[[1, 'datapath']], header=TRUE,
                               sep=input$sep, quote=input$quote)
        for (i in seq(2, n)) {
          csv <- utils::read.csv(inFile[[i, 'datapath']], header=TRUE,
                          sep=input$sep, quote=input$quote)
          dat <- merge(dat, csv, all = TRUE)

        }
      }
      if (input$disc) {
        dat <- check_conc(dat, disc_lim=input$disc_limit)
      }
    } else {
      dat <- utils::read.csv(system.file("extdata", "Natal_group.csv",
                                           package="detzrcr"))
      if (input$disc) {
        dat <- check_conc(dat, disc_lim=input$disc_limit)
      }
    }
    return(dat)
  })
  likeness_table <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
      constants <- c(
        input$lambda_lu,
        input$hfhf_chur,
        input$luhf_chur,
        input$hfhf_dm,
        input$luhf_dm,
        input$luhf_zrc
      )
      if(!is.null(input$likeness_samples)) {
        new_data <- new_data[new_data$sample %in% input$likeness_samples, ]
        new_data$sample <- factor(new_data$sample,
                                  levels=input$likeness_samples)
      }
      if (input$likeness_type == '1d') {
        satkoski_1d_matrix(new_data, bw=input$likeness_age_bw)
      } else {
        if (input$likeness_type == '2d') {
          hf_data <- calc_hf(new_data, constants=constants)
          satkoski_2d_matrix(hf_data, bw=c(input$likeness_age_bw,
                                           input$likeness_ehf_bw))
        } else {
          if (input$likeness_type == 'combine') {
            hf_data <- calc_hf(new_data, constants=constants)
            one_d <- satkoski_1d_matrix(new_data, bw=input$likeness_age_bw)
            two_d <- satkoski_2d_matrix(hf_data, bw=c(input$likeness_age_bw,
                                                   input$likeness_ehf_bw))
            combine_matrices(one_d, two_d)
          }
        }
      }
    }
  })
  o_table <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
      constants <- c(
        input$lambda_lu,
        input$hfhf_chur,
        input$luhf_chur,
        input$hfhf_dm,
        input$luhf_dm,
        input$luhf_zrc
      )
      hf_data <- calc_hf(new_data, constants=constants)
      if(!is.null(input$o_samples)) {
        hf_data <- hf_data[hf_data$sample %in% input$o_samples, ]
        hf_data$sample <- factor(hf_data$sample,
                                  levels=input$o_samples)
      }
      if (input$o_type == 'combine') {
        combine_matrices(o_param_matrix_age(hf_data),
                         o_param_matrix_tdm(hf_data))
      } else {
        if (input$o_type == 'age') {
          o_param_matrix_age(hf_data)
        } else {
          if (input$o_type == 'tdm') {
            o_param_matrix_tdm(hf_data)
          }
        }
      }
    }
  })

  o_plot <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
      constants <- c(
        input$lambda_lu,
        input$hfhf_chur,
        input$luhf_chur,
        input$hfhf_dm,
        input$luhf_dm,
        input$luhf_zrc
      )
      new_data <- calc_hf(new_data, constants=constants)
      if(!is.null(input$o_samples)) {
        new_data <- new_data[new_data$sample %in% input$o_samples, ]
        new_data$sample <- factor(new_data$sample,
                                  levels=input$o_samples)
      }
      plot_tile(new_data, type=input$o_type) +
        plot_text_options(font_name = input$font_name,
                          title_size = input$title_size,
                          label_size = input$label_size,
                          legend_size = input$legend_size,
                          strip_text_y_size = input$strip_size)
    }
  })

  reimink_plot <- shiny::reactive({
    new_data <- reimink_table()
    if (!is.null(new_data)) {
      plot_reimink(new_data) +
        plot_text_options(font_name = input$font_name,
                          title_size = input$title_size,
                          label_size = input$label_size,
                          legend_size = input$legend_size,
                          strip_text_y_size = input$strip_size)
    }
  })

  reimink_table <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
      if(!is.null(input$reimink_samples)) {
        new_data <- new_data[new_data$sample %in% input$reimink_samples, ]
        new_data$sample <- factor(new_data$sample,
                                  levels=input$reimink_samples)
      }
      reimink_data <- reimink(new_data, input$reimink_step)
    }
  })

  hf_table <- shiny::reactive({
    new_data <- csv_data()
    constants <- c(
      input$lambda_lu,
      input$hfhf_chur,
      input$luhf_chur,
      input$hfhf_dm,
      input$luhf_dm,
      input$luhf_zrc
    )
    hf_data <- calc_hf(new_data, constants=constants)
  })
  dens_plot <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
       facet <- NULL
      if (input$dens_type == 'dens_combine') {
        facet <- FALSE
        if (!is.null(input$dens_combine_choice)) {
          new_data <- new_data[(new_data$sample %in%
                                  input$dens_combine_choice) ,]
        }
      }
      if (input$dens_type == 'dens_facet') {
        facet <- TRUE
        if (!is.null(input$dens_facet_choice)) {
          new_data <- new_data[(new_data$sample %in% input$dens_facet_choice), ]
          new_data$sample <- factor(new_data$sample,
                                    levels = input$dens_facet_choice)
        }
      }
      if (input$dens_type == 'dens_ind') {
        facet <- FALSE
        if (!is.null(input$dens_ind_choice)) {
          new_data <- new_data[new_data$sample == input$dens_ind_choice, ]
        }
      }
       if (input$hist == TRUE) {
         p <- plot_dens_hist(new_data, binwidth=input$binwidth, bw=input$bw,
                             type=input$type, age_range=input$xlim,
                             facet=facet, step=input$xstep) +
           plot_text_options(font_name = input$font_name,
                             title_size = input$title_size,
                             label_size = input$label_size,
                             legend_size = input$legend_size,
                             strip_text_y_size = input$strip_size)
       } else {
         p <- plot_dens(new_data, bw=input$bw,
                        type=input$type, age_range=input$xlim,
                        facet=facet, step=input$xstep) +
           plot_text_options(font_name = input$font_name,
                             title_size = input$title_size,
                             label_size = input$label_size,
                             legend_size = input$legend_size,
                             strip_text_y_size = input$strip_size)
       }
    }
  })
  ecdf_plot <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
      mult_ecdf <- NULL
      constants <- c(
        input$lambda_lu,
        input$hfhf_chur,
        input$luhf_chur,
        input$hfhf_dm,
        input$luhf_dm,
        input$luhf_zrc
      )
      if (input$ecdf_input_type == 't_dm2') {
        new_data <- calc_hf(new_data, constants=constants)
      }
      if (input$ecdf_type == 'ind_plot') {
        mult_ecdf <- FALSE
        if (!is.null(input$ecdf_ind_samples)) {
          new_data <- new_data[new_data$sample == input$ecdf_ind_samples, ]
        }
      }
      if (input$ecdf_type == 'same_plot') {
        mult_ecdf <- TRUE
        if (length(input$ecdf_mult_samples) > 0) {
          new_data <- new_data[new_data$sample %in% input$ecdf_mult_samples, ]
        }
      }
      if (input$ecdf_type == 'ecdf_combine_plot') {
        mult_ecdf <- FALSE
        if (!is.null(input$ecdf_comb_samples)) {
          new_data <- new_data[new_data$sample %in% input$ecdf_comb_samples, ]
        }
      }
      p <- plot_ecdf(new_data, mult_ecdf=mult_ecdf, conf=input$ecdf_conf,
                     column=input$ecdf_input_type, guide=input$ecdf_legend) +
        plot_axis_lim(xlim=input$ecdf_xlim, step=input$ecdf_xstep) +
        plot_text_options(font_name = input$font_name,
                            title_size = input$title_size,
                            label_size = input$label_size,
                            legend_size = input$legend_size)
    }
  })
  hf_plot <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
      constants <- c(
        input$lambda_lu,
        input$hfhf_chur,
        input$luhf_chur,
        input$hfhf_dm,
        input$luhf_dm,
        input$luhf_zrc
      )
      new_data <- calc_hf(new_data, constants=constants)
      if (input$hf_type == 'ehf_plot') {
        contour_data <- NULL
        if (input$error_bars) {
          if (input$x_error_bars) {
            x_errors <- data.frame(xmin=new_data$age-(2*new_data$uncert),
                                   xmax=new_data$age+(2*new_data$uncert))
            new_data <- cbind(new_data, x_errors)
          }
          if (input$y_error_bars) {
            y_errors <- data.frame(ymin=new_data$ehf_i-new_data$ehf_2se,
                                   ymax=new_data$ehf_i+new_data$ehf_2se)
            new_data <- cbind(new_data, y_errors)
          }
        }
        if (input$add_contours) {
          contour_data <- new_data[new_data$sample %in% input$contour_choice, ]
        }
        if(!is.null(input$hfhf_samples)) {
          new_data <- new_data[new_data$sample %in% input$hfhf_samples, ]
          new_data$sample <- factor(new_data$sample,
                                    levels=input$hfhf_samples)
        }
        p <- plot_hf(new_data, guide=input$hf_legend,
                     contours=input$add_contours,
                     x_bandwidth=input$contour_x_bandwidth,
                     y_bandwidth=input$contour_y_bandwidth,
                     contour_data=contour_data,
                     combine_contours=input$combine_contours,
                     error_bars=input$error_bars,
                     x_errors=input$x_error_bars, y_errors=input$y_error_bars,
                     constants=constants) +
          plot_axis_lim(xlim=input$hf_xlim,
                        ylim=input$ehf_ylim,
                        step=input$hf_xstep) +
          plot_text_options(font_name = input$font_name,
                            title_size = input$title_size,
                            label_size = input$label_size,
                            legend_size = input$legend_size)
      } else {
        if (input$hf_type == 'hfhf_plot') {
          contour_data <- NULL
          if (input$error_bars) {
            if (input$x_error_bars) {
              x_errors <- data.frame(xmin=new_data$age-(2*new_data$uncert),
                                     xmax=new_data$age+(2*new_data$uncert))
              new_data <- cbind(new_data, x_errors)
            }
            if (input$y_error_bars) {
              y_errors <- data.frame(ymin=new_data$hf_i-new_data$hf_2se,
                                     ymax=new_data$hf_i+new_data$hf_2se)
              new_data <- cbind(new_data, y_errors)
            }
          }
          if (input$add_contours) {
            contour_data <-
              new_data[new_data$sample %in% input$contour_choice, ]
          }
          if(!is.null(input$hfhf_samples)) {
            new_data <- new_data[new_data$sample %in% input$hfhf_samples, ]
            new_data$sample <- factor(new_data$sample, levels=
                                        input$hfhf_samples)
          }
          p <- plot_hf(new_data, plot_type = 'hfhf', guide=input$hf_legend,
                       contours=input$add_contours,
                       x_bandwidth=input$contour_x_bandwidth,
                       y_bandwidth=input$contour_y_bandwidth,
                       contour_data=contour_data,
                       combine_contours=input$combine_contours,
                       error_bars=input$error_bars,
                       x_errors=input$x_error_bars, y_errors=input$y_error_bars,
                       constants=constants) +
            plot_axis_lim(xlim=input$hf_xlim,
                          ylim=input$hf_ylim,
                          step=input$hf_xstep) +
            plot_text_options(font_name = input$font_name,
                                title_size = input$title_size,
                                label_size = input$label_size,
                                legend_size = input$legend_size)
        }
      }
    }
  })

  uqlq_plot <- shiny::reactive({
    new_data <- csv_data()
    if (!is.null(new_data)) {
      constants <- c(
        input$lambda_lu,
        input$hfhf_chur,
        input$luhf_chur,
        input$hfhf_dm,
        input$luhf_dm,
        input$luhf_zrc
      )
      mix_data = NULL
      if (input$mixing_model) {
        mix_data <- dzr_mix(input$mu1, input$sig1, input$mu2, input$sig2)
      }
      if(!is.null(input$quant_samples)) {
        new_data <- new_data[new_data$sample %in% input$quant_samples, ]
        new_data$sample <- factor(new_data$sample,
                                  levels=input$quant_samples)
      }
      if (input$uqlq_type == 'uqlq_age') {
        column = 'age'
      }
      if (input$uqlq_type == 'uqlq_tdm') {
        new_data <- calc_hf(new_data, constants=constants)
        column = 't_dm2'
      }
      p <- plot_quantiles(new_data, column=column, guide=input$uqlq_legend,
                          conf=input$uqlq_conf, mix=input$mixing_model,
                          mix_data=mix_data)
      p <- p + plot_axis_lim(xlim=input$uqlq_xlim, step=input$uqlq_xstep,
                             ylim=input$uqlq_ylim)
      p <- p + plot_text_options(font_name = input$font_name,
                                 title_size = input$title_size,
                                 label_size = input$label_size,
                                 legend_size = input$legend_size)
    }
  })
  # Output
  output$head <- shiny::renderTable({
    new_data <- csv_data()
    utils::head(new_data)
  })
  output$dens_plot <- shiny::renderPlot({
    print(dens_plot())
  })
  output$ecdf_plot <- shiny::renderPlot({
    print(ecdf_plot())
  })
  output$reimink_plot <- shiny::renderPlot({
    print(reimink_plot())
  })
  output$downloadDensplot <- shiny::downloadHandler(
    filename = function() {
      paste('kde', '.pdf', sep='')
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = dens_plot(), width=input$densWidth,
                      height=input$densHeight, colormodel='cmyk', units='cm')
    }
  )
  output$download_ecdf_plot <- shiny::downloadHandler(
    filename = function() {
      paste('ecdf', '.pdf', sep='')
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = ecdf_plot(), width=input$ecdf_width,
                      height=input$ecdf_height, colormodel='cmyk', units='cm')
    }
  )
  output$download_hf_plot <- shiny::downloadHandler(
    filename = function() {
      paste('hf', '.pdf', sep='')
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = hf_plot(), width=input$hf_width,
                      height=input$hf_height, colormodel='cmyk', units='cm',
                      useDingbats=FALSE)
    }
  )
  output$download_uqlq_plot <- shiny::downloadHandler(
    filename = function() {
      paste('uqlq', '.pdf', sep='')
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = uqlq_plot(), width=input$uqlq_width,
                      height=input$uqlq_height, colormodel='cmyk', units='cm',
                      useDingbats=FALSE)
    }
  )
  output$download_likeness_table <- shiny::downloadHandler(
    filename = function() {
      paste('likeness', '.csv', sep='')
    },
    content = function(file) {
      utils::write.csv(likeness_table(), file)
    }
  )
  output$download_satkoski_2d <- shiny::downloadHandler(
    filename = function() {
      paste('L2', '.csv', sep='')
    },
    content = function(file) {
      utils::write.csv(satkoski_2d_table(), file)
    }
  )
  output$download_o_table <- shiny::downloadHandler(
    filename = function() {
      paste('otable', '.csv', sep='')
    },
    content = function(file) {
      utils::write.csv(o_table(), file)
    }
  )
  output$download_o_plot <- shiny::downloadHandler(
    filename = function() {
      paste('oplot', '.pdf', sep='')
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = o_plot(), width=input$o_width,
                      height=input$o_height, colormodel='cmyk', units='cm')
    }

  )
  output$download_reimink_plot <- shiny::downloadHandler(
    filename = function() {
      paste('reimink', '.pdf', sep='')
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = reimink_plot(), width=input$reimink_width,
                      height=input$reimink_height, colormodel='cmyk',
                      units='cm')
    }
  )
  output$download_reimink_table <- shiny::downloadHandler(
    filename = 'likelihood.csv',
    content = function(file) {
      utils::write.csv(reimink_table(), file)
    }
  )
  output$download_hf_table <- shiny::downloadHandler(
    filename = function() {
      paste('hf', '.csv', sep='')
    },
    content = function(file) {
      utils::write.csv(hf_table(), file)
    }
  )
  output$dens_facet_select <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    selectInput('dens_facet_choice', 'Select samples', samples,
                multiple=TRUE, selectize=TRUE)
  })
  output$show_disc_limit <- shiny::renderUI({
    if (input$disc) {
      shiny::numericInput('disc_limit', 'Discordancy limit (%)', 10,
                          min=0, max=100)
      }
  })
  # Dynamic UI density tab
  output$dens_switch <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    if (input$dens_type == 'dens_facet') {
      selectInput('dens_facet_choice', 'Select samples', samples,
                  multiple=TRUE, selectize=TRUE)
    } else {
      if (input$dens_type == 'dens_ind') {
        selectInput('dens_ind_choice', 'Select sample', samples)
      } else {
      if (input$dens_type == 'dens_combine') {
        selectInput('dens_combine_choice', 'Select samples', samples,
                    multiple=TRUE, selectize=TRUE)
      }
      }
    }
  })
  # Dynamic UI ecdf tab
  output$ecdf_switch <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    if (input$ecdf_type == 'same_plot') {
      selectInput('ecdf_mult_samples', 'Select samples', samples,
                  multiple=TRUE, selectize=TRUE)
    } else {
      if (input$ecdf_type == 'ind_plot') {
        selectInput('ecdf_ind_samples', 'Select sample', samples)
      } else {
        if (input$ecdf_type == 'ecdf_combine_plot') {
          selectInput('ecdf_comb_samples', 'Select samples', samples,
                      multiple=TRUE, selectize=TRUE)
        }
      }
    }
  })
  # Dynamic UI Lu-Hf tab
  output$hf_switch <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    if (input$hf_type == 'ehf_plot') {
      shiny::sliderInput('ehf_ylim', 'Y-axis range',
                         min=-50, max=50, value = c(-20, 20))
    } else {
      if (input$hf_type == 'hfhf_plot') {
        shiny::sliderInput('hf_ylim', 'Y-axis range',
                           min=0.279, max=0.283, value = c(0.28, 0.283))
      } else {
        if (input$hf_type == 'quant_plot') {
          shiny::sliderInput('quant_ylim', 'Y-axis range',
                             min=0, max=4560, value=c(200, 4000))
          }
        }
      }
  })
  output$hf_bandwidths <- shiny::renderUI({
    if (input$hf_type == 'ehf_plot') {
      shiny::conditionalPanel(
        condition = 'input.add_contours == true',
        shiny::numericInput('contour_x_bandwidth', 'X bandwidth', 30),
        shiny::numericInput('contour_y_bandwidth', 'Y bandwidth', 2.5),
        shiny::checkboxInput('combine_contours',
                             label = 'Combine contours',
                             value = FALSE)
      )
    } else {
      if (input$hf_type == 'hfhf_plot') {
        shiny::conditionalPanel(
          condition = 'input.add_contours == true',
          shiny::numericInput('contour_x_bandwidth', 'X bandwidth', 30),
          shiny::numericInput('contour_y_bandwidth', 'Y bandwidth', 0.00025),
          shiny::checkboxInput('combine_contours',
                               label = 'Combine contours',
                               value = FALSE)
        )
      }
    }
  })
  # Dynamic UI O-tab
  output$o_switch <- shiny::renderUI({
    if (input$o_fig) {
      shiny::verticalLayout(
        shiny::tableOutput('o_table'),
        shiny::plotOutput('o_plot')
      )
    } else {
      shiny::tableOutput('o_table')
    }
  })
  output$contour_switch <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    if (input$add_contours) {
      selectInput('contour_choice', 'Select samples to contour', samples,
                  multiple=TRUE, selectize=TRUE)
    }
  })
  output$hf_samples <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    shiny::selectInput('hfhf_samples', 'Select samples', samples,
                       multiple=TRUE, selectize=TRUE)
  })
  output$hf <- shiny::renderPlot({
    print(hf_plot())
  })
  output$uqlq_samples <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    shiny::selectInput('quant_samples', 'Select samples', samples,
                       multiple=TRUE, selectize=TRUE)
  })
  output$o_samples <- shiny::renderUI({
    constants <- c(
      input$lambda_lu,
      input$hfhf_chur,
      input$luhf_chur,
      input$hfhf_dm,
      input$luhf_dm,
      input$luhf_zrc
    )
    new_data <- calc_hf(csv_data(), constants=constants)
    samples <- as.vector(unique(new_data$sample))
    shiny::selectInput('o_samples', 'Select samples', samples,
                       multiple=TRUE, selectize=TRUE)
  })
  output$likeness_samples <- shiny::renderUI({
    constants <- c(
      input$lambda_lu,
      input$hfhf_chur,
      input$luhf_chur,
      input$hfhf_dm,
      input$luhf_dm,
      input$luhf_zrc
    )
    new_data <- calc_hf(csv_data(), constants=constants)
    samples <- as.vector(unique(new_data$sample))
    shiny::selectInput('likeness_samples', 'Select samples', samples,
                       multiple=TRUE, selectize=TRUE)
  })
  output$uqlq <- shiny::renderPlot({
    print(uqlq_plot())
  })
  output$likeness <- shiny::renderTable({
    likeness_table()
  }, rownames=TRUE)
  output$likeness_bw <- shiny::renderUI({
    if (input$likeness_type == '2d' | input$likeness_type == 'combine') {
      shiny::numericInput('likeness_ehf_bw', 'Epsilon-Hf bandwidth', 2.5)
    }
  })
  output$o_plot <- shiny::renderPlot({
    print(o_plot())
  })
  output$o_table <- shiny::renderTable({
    o_table()
  }, rownames=TRUE)

  output$reimink_samples <- shiny::renderUI({
    new_data <- csv_data()
    samples <- as.vector(unique(new_data$sample))
    shiny::selectInput('reimink_samples', 'Select samples', samples,
                       multiple=FALSE, selectize=FALSE)
  })
  output$reimink_maxima_lower <- renderText({
    reimink_data <- reimink_table()
    if (!is.null(reimink_data)) {
        lower <- reimink_data[reimink_data$type == 'lower',]
        lower_maxima <- find_maxima(lower$y, 0, 1)
        lower[lower_maxima, ]$x
    }
  })
  output$reimink_maxima_upper <- renderText({
    reimink_data <- reimink_table()
    if (!is.null(reimink_data)) {
        upper <- reimink_data[reimink_data$type == 'upper',]
        upper_maxima <- find_maxima(upper$y, 0, 1)
        upper[upper_maxima, ]$x
    }
  })
})

# Run the application
shiny::shinyApp(ui = ui, server = server)
