#Developed by Avril Metcalfe-Roach
library(shiny)
library(tidyverse)
library(stats)
library(sortable)
library(ggpubr)
library(markdown)
ui <- fluidPage(
    
    # App title ----
    titlePanel("Flow Cytometry Statistical Analysis"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # SIDEBAR PANEL ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "1. Upload Statistics",
                      multiple = T,
                      accept = c(".csv")),
            
            # Input: Reorder samples ----
            checkboxInput("remove", strong("2. Remove Unwanted Samples"), value = F),
            conditionalPanel(
                condition = "input.remove == 1",
                uiOutput("sort")
            ),
            
            # Input: Upload Metadata ----
            checkboxInput('meta',strong('3. Metadata Available?'), value = F),
            conditionalPanel(
                condition = "input.meta == 1",
                h5(strong('Option A:'), " Upload .csv of metadata (must include 'Sample' column)"),
                h5(em('If sample names are duplicated between multiple file uploads, 
                metadata must contain a "Batch" column that identifies files as "File 1", etc.')),
                fileInput("metafile","",
                          multiple = F,
                          accept = c(".csv")),
                uiOutput('select_group'),
                h5(strong('Option B:'), ' Data groupings identifiers in sample names'),
                textAreaInput("metaid", h5(em("List grouping IDs (comma separated)")), "")
            ),
            
            # Input: Identify Isotypes ----
            checkboxInput("isoused", strong("4. Isotype used?"), value = F),
            conditionalPanel(
                condition = 'input.isoused == 1',
                checkboxInput("suffix", "Isotypes identified by suffix?", value = T),
                conditionalPanel(
                    condition = 'input.suffix == 1',
                    textAreaInput("isoid", h5("Isotype Identifier (include spaces)"), " iso", width = "200px")
                ),
                conditionalPanel(
                    condition = 'input.suffix == 0',
                    uiOutput('isosort') # Will have to sort the isotypes themselves
                )
            ),
            
            # UI Placeholders for Gate Selection ----
            h5(strong('5. Gating (Multiple gates will be summed before analysis)')),
            uiOutput("pos_gate"),
            uiOutput("neg_gate"),
            
            # Input and UI Placeholders for Plot Options ----
            h5(strong('6. Plot Options (Optional)')),
            conditionalPanel(
                condition = "input.meta == 1",
                checkboxInput("filter_na", "Filter out NAs", value = T),
                uiOutput('control'),
                conditionalPanel(
                    condition = "input.plot_control == 'None'",
                    selectInput("stat_pair1",h5('Specify statistical test'), c('N/A'), multiple = F)
                ),
                conditionalPanel(
                    condition = "input.plot_control == 'Multiple'",
                    selectInput("stat_pair2",h5('Specify statistical test'), c('kruskal.test','anova'), multiple = F)
                ),
                conditionalPanel(
                    condition = "input.plot_control != 'None' & input.plot_control != 'Multiple'",
                    selectInput("stat_pair3",h5('Specify statistical test'), c('wilcox.test','t.test'), multiple = F)
                )
            ),
            
            uiOutput('facet'),
            uiOutput('group')
            
        ),
        
        # MAIN PANEL ----
        mainPanel(
            tabsetPanel(type = "pills",
                        id = "all_tabs",
                        tabPanel("Plot",
                                 br(),
                                 downloadButton("downloadPlot", "Download plot as .png"),
                                 br(),
                                 br(),
                                 plotOutput("print_plot"),
                                 br(),
                                 h5(strong("Statistical Summary")),
                                 downloadButton("download_sum2", "Download output as .csv"),
                                 br(),
                                 br(),
                                 fluidRow(column(9,dataTableOutput("sum2"))),
                                 h5(strong("Grouped Data")),
                                 downloadButton("download_sum3", "Download output as .csv"),
                                 br(),
                                 br(),
                                 fluidRow(column(9,dataTableOutput("sum3")))#,
                                 # fluidRow(column(9,dataTableOutput("sum4")))
                        ),
                        tabPanel("Stats Table",
                                 br(),
                                 downloadButton("download_pcttable", "Download output as .csv"),
                                 br(),
                                 br(),
                                 fluidRow(column(9,dataTableOutput("pcttable")))
                        ),
                        tabPanel("Metadata Table",
                                 fluidRow(column(9,dataTableOutput("df_metaout")))
                        ),
                        tabPanel("Documentation",
                                 # h5('To be updated.'),
                                 # h5('Please see https://github.com/armetcal/science_tools for sample datasets.'),
                                 includeMarkdown('Documentation.md')
                        )
            )
        )
    )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SERVER LOGIC ----

# Order of events:
# 1. Load files, select relevant columns - df_pretty()
# 2. UI for sorting/filtering samples - output$sort
# 3. Remove filtered samples from #2 - df_chosen()
# 4. UI for manual identification - output$isosort
# 5. Load metadata file (if applicable) - df_metafile()
# 6. Add metadata to #3 - df_meta()
# 7. Categorize isotype controls, if applicable - df_iso()
# 8. UI for selecting gates - output$pos_gate, output$neg_gate
# 9. Subtracts iso samples, renders output table - df_pcttable()
# 10. UIs for plot options - output$control, output$facet, output$group
# 11a. MAIN OUTPUT TABLE - output$pcttable
# 11b. Download - output$download_pcttable
# 12. METADATA TABLE - output$df_metaout
# 13a. CREATE PLOT - plot()
# 13b. Render plot - output$print_plot
# 13c. Download plot - output$downloadPlot
# 14a. Stats Prep
# 14b. STATS TABLE
# 14c. GROUPED DATA TABLE

# To add:
# Documentation

server <- function(input, output) {
    
    # 1. Load files, select relevant columns ----
    df_pretty <- reactive({
        req(input$file1)
        # Upload
        upload <- read_csv(input$file1[[1,'datapath']])
        L = length(input$file1$datapath)
        if(L>1){
            upload$Batch = 'File 1'
            for (i in 2:L) {
                temp <- read_csv(input$file1[[i,'datapath']]) %>% 
                    mutate(Batch = paste('File ',i,sep=''))
                upload <- full_join(upload, temp)
            }
            upload$Batch = factor(upload$Batch)
        } else {
        }
        # Select Columns
        res_list = upload %>% select(any_of('Batch'),Sample, Gate, `%Gated`) %>% unique()
        return(res_list)
    })
    
    # 2. UI for sorting/filtering samples ----
    output$sort <- renderUI({
        res_imp <- df_pretty()
        df_s <- unique(res_imp$Sample)
        bucket_list(
            header = "",
            add_rank_list(
                text = 'Keep', labels = df_s,
                input_id = 'sort_keep',
                options = sortable_options(
                    multiDrag = TRUE)
            ),
            add_rank_list(text = "Remove",
                          input_id = 'sort_remove',
                          options = sortable_options(
                              multiDrag = TRUE))
        )
    })
    
    
    # 3. Remove filtered samples from #2 ----
    df_chosen <- reactive({
        res_imp <- df_pretty()
        # Define order
        if(input$remove == F){
            sample_order = unique(res_imp$Sample) # all samples
        } else {
            sample_order = input$sort_keep # filtered
        }
        # Filter dataset
        res_chosen <- res_imp %>% filter(Sample %in% sample_order)
        res_chosen$Sample = factor(res_chosen$Sample, levels = sample_order)
        return(res_chosen)
    })
    
    # 4. UI for manual identification ----
    # MUST be matching isotype for each sample! Remove all other samples
    output$isosort <- renderUI({
        res_imp <- df_chosen()
        df_s <- unique(res_imp$Sample)
        bucket_list(
            header = "",
            add_rank_list(
                text = 'Regular Sample', labels = df_s,
                input_id = 'sample_reg',
                options = sortable_options(
                    multiDrag = TRUE)
            ),
            add_rank_list(text = "Isotype Control **Order must match!**",
                          input_id = 'sample_iso',
                          options = sortable_options(
                              multiDrag = TRUE))
        )
    })
    
    # 5. Load metadata file, select grouping variable (if applicable) ----
    df_metafile <- reactive({
        req(input$metafile)
        m <- read.csv(input$metafile$datapath) %>% 
            select(Sample, everything())
        return(m)
    })
    output$select_group <- renderUI({
        req(input$metafile)
        res_imp <- df_metafile()
        df_s <- unique(names(res_imp)[names(res_imp) != 'Sample'])
        selectInput("groupname",h5('Choose Variable of Interest'), df_s, multiple = F)
    })
    
    # 6. Add metadata to #3 ----
    df_meta <- reactive({
        df = df_chosen()
        # Metadata exists
        if(input$meta == T){
            # Grouping variable identified by sample names
            if(input$metaid != ''){ # ex. "pd,ctrl"
                L = str_split(input$metaid, ',')[[1]]
                strd.v = Vectorize(str_detect)
                df$GROUPVAR = NA
                for(l in L){
                    df = df %>% mutate(test = strd.v(Sample, l))
                    df$GROUPVAR[df$test == T] = l
                }
                df = df %>% select(-test)
            # Metadata supplied by file
            } else {
                if(isTruthy(input$metafile)){
                    df_meta = df_metafile()
                    names(df_meta)[names(df_meta)==input$groupname] = 'GROUPVAR'
                    df = df %>% left_join(df_meta)
                }
            }
        # No metadata
        } else {
        }
        return(df)
    })
    
    # 7. Categorize isotype controls, if applicable ----
    df_iso <- reactive({
        # No isotypes used
        if(input$isoused == F){
            res_iso = df_meta() %>% mutate(Iso = F)
        # Isotypes used
        } else {
            # Isotypes identified by suffix (ex. '1' and '1 iso')
            if(input$suffix == T){
                strd.v = Vectorize(str_detect)
                strr.v = Vectorize(str_remove)
                res_iso = df_meta() %>% mutate(Iso = strd.v(Sample, input$isoid)) %>% 
                    mutate(Sample = strr.v(Sample, input$isoid))
            # Isotypes must be manually matched
            } else {
                if(length(input$sample_reg) != length(input$sample_reg)){
                    stop('Error - each sample must have a matching isotype control!')
                }
                iso_key = tibble(Sample = input$sample_reg, matching_iso = input$sample_iso)
                res_iso = df_meta() %>% mutate(Iso = Sample %in% input$sample_iso)
                for(i in 1:nrow(res_iso)){
                    if(res_iso$Iso[i] == T){
                        w = which(res_iso$Sample[i] == iso_key$matching_iso)
                        res_iso$Sample[i] = iso_key$Sample[w]
                    }
                }
            }
        }
        return(res_iso %>% unique())
    })
    
    # 8. UI for selecting gates ----
    output$pos_gate <- renderUI({
        res_imp <- df_chosen()
        df_s <- unique(res_imp$Gate)
        selectInput("pos_gate",h5(strong('5a.'),' Choose Gate(s) of Interest'), df_s, multiple = T)
        
    })
    output$neg_gate <- renderUI({
        res_imp <- df_chosen()
        df_s <- c('Parent',unique(res_imp$Gate))
        selectInput("neg_gate",h5(strong('5b.'),' Choose Background Gate(s)',br(),br(),
                                  em('Parent: all events in Parent gate.'),br(),br(),
                                  em('Other background gates: results will be calculated as % Gate/(Gate+Background)')),
                    df_s, multiple = T)
    })
    
    # Selects relevant +/- gate(s), finds % Target
    df_gate <- reactive({
        validate(
            need(input$pos_gate, "Please select a positive gate(s)."),
            need(input$neg_gate, "Please select a negative gate(s).")
        )
        gates = c(input$pos_gate, input$neg_gate)
        gates = gates[gates != 'Parent']
        # Filter dataframe to only include gates of interest.
        # If there are duplicate gate names but they're not the gates of interest,
        # this will stop them from throwing an error.
        df = df_iso() %>% filter(Gate %in% gates)
        # Check that gates are unique
        validate(
            need(df %>% unique() %>% group_by_at(setdiff(names(df), "%Gated")) %>%
                 mutate(test = n()) %>% filter(test>1) %>% nrow() == 0,
                 "One or more selected gates represent multiple gated populations - please inspect input file(s).")
        )
        # Pivot df by gates, initializes new df
        df = df %>% pivot_wider(names_from = Gate, values_from = `%Gated`)
        if(isTruthy(input$metaid)){
            df2 = df %>% select(any_of('Batch'),Sample,GROUPVAR,any_of('Iso'))
        } else if(isTruthy(input$metafile)){
            df2 = df %>% select(any_of('Batch'),Sample,GROUPVAR,any_of(names(df_metafile())),any_of('Iso'))
        } else {
            validate(
                need(input$meta == 0, "Please upload the metadata file.")
            )
            df2 = df %>% select(any_of('Batch'),Sample,any_of('Iso'))
        } 
        
        # Define target pop
        df2$Pos = df %>% select(all_of(input$pos_gate)) %>% rowSums()
        if(input$neg_gate == 'Parent'){ # % Target is simply % parent
            df2$Neg = 100 - df2$Pos
            df2 = df2 %>% mutate(`% Target` = Pos)
        } else { # Specific gate is background
            df2$Neg = df %>% select(any_of(input$neg_gate)) %>% rowSums()
            df2 = df2 %>% mutate(`% Target` = 100*Pos/(Pos+Neg))
        }
        
        return(df2)
    })
    
    # 9. Subtracts iso samples, arranges output table ----
    df_pcttable <- reactive({
        req(input$file1)
        df = df_gate() %>% mutate(Pos = round(Pos, digits = 1), Neg = round(Neg, digits = 1),
                                  `% Target` = round(`% Target`, digits = 1))
        df$Iso[df$Iso == T] = 'Isotype'
        df$Iso[df$Iso == F] = 'Regular'
        if(input$isoused == T){
            validate(
                need(length(df$Iso %>% unique())>1, "Error: Isotypes do not exist or are improperly formatted.")
            )
            df = df %>% 
                pivot_wider(names_from = Iso, 
                            values_from = c(Pos, Neg, `% Target`), 
                            names_sep = ' ') %>%
                mutate(`% Target` = `% Target Regular` - `% Target Isotype`) %>%
                select(any_of('Batch'), Sample, everything(), `% Target Regular`, `% Target Isotype`, `% Target`) %>% 
                select(-`Pos Regular`,-`Pos Isotype`,-`Neg Regular`,-`Neg Isotype`)
        } else {
            df = df %>% select(any_of('Batch'), Sample, everything(), `% Target`)  %>% 
                select(-`Pos`,-`Neg`)
        }
        if(input$meta==1){
            df = df %>% mutate(GROUPVAR = as.factor(as.character(GROUPVAR)))
        }
        return(df)
    })
    
    # 10. UIs for plot options ----
    # Select control group
    output$control <- renderUI({
        req(input$meta==1)
        df_s <- df_pcttable() %>% select(GROUPVAR) %>% mutate_all(as.character) %>% unique()
        df_s = df_s[is.na(df_s)==F]
        selectInput("plot_control",
                    h5('Specify control group for pairwise comparison (select Multiple for multiple comparisons)'), 
                    c(df_s,'None','Multiple'), multiple = F, selected = 'Multiple')
    })
    # Select facet group
    output$facet <- renderUI({
        req(input$file1)
        inputs <- c('None','Batch (multiple input files only)')
        if(input$meta == 1){
            req(input$metafile)
            df <- df_metafile() %>% select(-Sample)
            inputs = c(inputs,names(df))
        }
        selectInput("plot_facet",h5('Facet plot by metadata column'), inputs, multiple = F)
    })
    # Select cluster group
    output$group <- renderUI({
        req(input$file1, input$metafile)
        inputs = c('None')
        if(isTruthy(df_metafile()) & input$plot_control %in% c('None','Multiple')){
            df <- df_metafile()  %>% select(-Sample)
            inputs = c(inputs,names(df))
            selectInput("plot_cluster",h5('Cluster by metadata column'), inputs, multiple = F)
        }
    })
    
    # 11a. MAIN OUTPUT TABLE ----
    output$pcttable <- renderDataTable({
        req(input$file1)
        df = df_pcttable() %>% unique() %>% select(-any_of('Iso'))
        if(input$metaid == ''){
            names(df)[names(df)=='GROUPVAR'] = input$groupname
        } else {
            names(df)[names(df)=='GROUPVAR'] = 'Metadata'
        }
        return(df)
    })
    
    # 11b. Download ----
    output$download_pcttable <- downloadHandler(
        filename = paste(Sys.Date(),'_flow_pcttable', ".csv", sep=''),
        content = function(filename) {
            write.csv(df_pcttable(), filename, row.names = F)
        }
    )
    
    # 12. METADATA TABLE ----
    output$df_metaout <- renderDataTable({
        req(input$metafile)
        df = df_metafile()
        names(df)[names(df)=='GROUPVAR'] = input$groupname
        return(df)
    })
    
    # 13a. CREATE PLOT ----
    plot <- reactive({
        # req(input$file1)
        req(df_pcttable())
        df_plot = df_pcttable() %>% unique()
        max_point = max(df_plot$`% Target`, na.rm = T)
        stat_pos = 1.075*max_point
        max_y = 1.15*max_point
        # No metadata
        if(input$meta == 0){
            p = ggplot(df_plot, aes('All Samples',`% Target`)) +
                geom_boxplot(fill = 'purple1',outlier.shape = NA) +
                geom_jitter(height = 0, width = 0.1) +
                theme_classic(base_size = 20) + ylab('% Target Events') + xlab('')
            # Batch facet
            if(isTruthy(input$plot_facet) & input$plot_facet == 'Batch (multiple input files only)'){
                p = p + facet_wrap("Batch")
            }
        # Metadata
        } else if(input$metaid != ''){
            # Filter out NAs
            if(input$filter_na == T){
                df_plot = df_plot %>% filter(is.na(GROUPVAR)==F & GROUPVAR != '')
            }
            # Plot
            p = ggplot(df_plot, aes(GROUPVAR,`% Target`, fill = GROUPVAR)) +
                geom_boxplot(outlier.shape = NA) +
                geom_jitter(height = 0, width = 0.1) +
                theme_classic(base_size = 20) + ylab('% Target Events') + xlab('Metadata')
            # Add stats
            if(input$plot_control == 'None'){
            } else if(input$plot_control == 'Multiple'){
                m = input$stat_pair2
                p = p + stat_compare_means(method = m, inherit.aes = T, size = 6, label.y = stat_pos)
            } else {
                m = input$stat_pair3
                r = input$plot_control
                p = p + stat_compare_means(label = "p.format", method = m, ref.group = r,
                                           inherit.aes = T, size = 6, label.y = stat_pos)
            }
            #Faceting
            if(isTruthy(input$plot_facet) & input$plot_facet == 'Batch (multiple input files only)'){
                p = p+facet_wrap('Batch')
            }
            
            p=p+labs(fill = 'Metadata')
            
        } else {
            # Filter out NAs
            if(input$filter_na == T){
                df_plot = df_plot %>% filter(is.na(GROUPVAR)==F & GROUPVAR != '')
            }
            # No pairwise comparisons
            if(input$plot_control %in% c('None','Multiple')){
                if(isTruthy(input$plot_cluster) & input$plot_cluster != 'None'){
                    validate(
                        need(input$plot_cluster != input$groupname, 
                             "Clustering variable is already selected as variable of interest - please choose another.")
                    )
                    Group = input$plot_cluster
                    names(df_plot)[names(df_plot)==Group] = 'Group'
                    df_plot$Group = factor(df_plot$Group)
                    p = ggplot(df_plot, aes(GROUPVAR, `% Target`, fill = Group))  + xlab(input$groupname)
                } else {
                    p = ggplot(df_plot, aes(GROUPVAR, `% Target`, fill = GROUPVAR))  + xlab(input$groupname)
                }
            # Pairwise (can't cluster, must facet instead)
            } else {
                p = ggplot(df_plot, aes(GROUPVAR, `% Target`, fill = GROUPVAR))  + xlab(input$groupname)
            }
            # Basic parameters
            p = p + geom_boxplot(outlier.shape = NA) +
                geom_point(position = position_jitterdodge(jitter.height = 0, jitter.width = 0.5))
            # Add stats
            if(input$plot_control == 'None'){
            } else if(input$plot_control == 'Multiple'){
                m = input$stat_pair2
                if(isTruthy(input$plot_cluster) & input$plot_cluster != 'None'){
                    p = p + stat_compare_means(aes(group = Group), method = m,
                                               inherit.aes = T, size = 6, label.y = stat_pos) + labs(fill = input$plot_cluster)
                } else {
                    p = p + stat_compare_means(method = m,
                                               inherit.aes = T, size = 6, label.y = stat_pos) + labs(fill = input$groupname)
                }
            } else {
                m = input$stat_pair3
                r = input$plot_control
                p = p + stat_compare_means(label = "p.format", method = m, ref.group = r,
                                           inherit.aes = T, size = 6, label.y = stat_pos) + labs(fill = input$groupname)
            }
            #Faceting
            if(isTruthy(input$plot_facet) & input$plot_facet != 'None'){
                validate(
                    need(input$plot_facet != input$groupname, 
                         "Faceting variable is already selected as variable of interest - please choose another.")
                )
                p = p + xlab(input$groupname)
                
                f = input$plot_facet
                if(f == 'Batch (multiple input files only)'){
                    f = 'Batch'
                }
                names(df_plot)[names(df_plot)==f] = 'Facet'
                df_plot$Facet = factor(df_plot$Facet)
                names(df_plot)[names(df_plot)=='Facet'] = f
                p = p+facet_wrap(f)
            }
        }
        p=p + theme_classic(base_size = 20) + 
            ylab('% Target Events') 
        
        return(p)
    })
    # 13b. Render plot ----
    output$print_plot <- renderPlot({
        p = plot()
        p
    })
    # 13c. Download plot ----
    output$downloadPlot <- downloadHandler(
        filename = paste(Sys.Date(),'_flow_plot', ".png", sep=''),
        content = function(filename) {
            device <- function(...,width, height) {grDevices::png(..., width = width, height = height, res = 300, units = "in")}
            ggsave(filename = filename, plot = plot(), device = device)
        }
    )
    
    # 14a. Stats Prep ----
    sum1 <- reactive({
        req(input$file1)
        df = df_pcttable() %>% unique() %>% filter(`% Target` != '')
        validate(
            need(input$meta == T, "No metadata available for statistics")
        )
        validate(
            need(input$plot_control != 'None', "No statistical test selected")
        )
        
        #Extract data
        g = ggplot_build(plot())$data[[3]] %>% as.data.frame()
        validate(
            need('p.format' %in% names(g), "Not enough observations to calculate P values")
        )
        if(input$plot_control != 'None'){
            t1 = g %>% rename('P value' = p.format, 'Stat Test' = method)
        } else {
            t1 = NA
        }
        return(t1)
        
    })

    # 14b. Stats Table ----
    sum2 <- reactive({
        df = sum1()
        df = df %>% rename('Facet' = PANEL,
                           'Group' = x) %>% 
            select(Facet,Group,`P value`, `Stat Test`)
        
        if(input$meta == 1 & input$metaid ==''){
            df_a = df_metafile()
            w = which(names(df_a)==input$groupname)
            df_g = df_a[,w] %>% as.character() %>% as.factor()
            df2 = df %>% mutate(Group = levels(df_g)[as.numeric(as.character(Group))]) %>% 
                mutate(`X var` = input$groupname) %>% 
                select(`X var`,Group, everything())
        } else if(input$meta == 1 & input$metaid !=''){
            df_a = df_pcttable()
            df_g = df_a$GROUPVAR %>% as.character() %>% as.factor()
            df2 = df %>% mutate(Group = levels(df_g)[as.numeric(as.character(Group))]) %>% 
                mutate(`X var` = 'Metadata') %>% 
                select(`X var`,Group, everything())
        }
        
        # Faceting
        if(isTruthy(input$plot_facet) & input$plot_facet != 'None'){
            if(input$plot_facet == 'Batch'){
                df_f = df_a$Batch %>% as.character() %>% as.factor()
            } else {
                w = which(names(df_a)==input$plot_facet)
                df_f = df_a[,w] %>% as.character() %>% as.factor()
            }
            df3 <- df2 %>% mutate(Facet = levels(df_f)[as.numeric(as.character(Facet))]) %>% 
                select(Facet, everything())
        } else {
            df3 <- df2 %>% select(-Facet)
        }
        
        # Add Ctrl column
        if(df3$`Stat Test`[1] %in% c('Wilcoxon','T-test')){
            df3$`Ctrl Group` = input$plot_control
        } else if(isTruthy(input$plot_cluster) & input$plot_cluster != 'None'){
        } else {
            df3 = df3 %>% select(-Group)
        }
        
        df3 = df3 %>% select(`X var`,any_of('Facet'),any_of('Group'),any_of('Ctrl Group'),any_of('Cluster'),everything())
        return(df3)
    })
    output$sum2 <- renderDataTable({
        return(sum2())
    })
    output$download_sum2 <- downloadHandler(
        filename = paste(Sys.Date(),'_flow_stattable', ".csv", sep=''),
        content = function(filename) {
            write.csv(sum2(), filename, row.names = F)
        }
    )
    
    # 14c. Grouped Data ----
    sum3 = reactive({
        req(input$file1)
        df = df_pcttable() %>% unique() %>% filter(`% Target` != '')
        
        if(input$meta == 0){
            if(isTruthy(input$plot_facet) & input$plot_facet == 'Batch (multiple input files only)'){
                names(df)[names(df)=='Batch'] = 'Facet'
                t1 = df %>% group_by(Facet)
                t1 = t1 %>% summarize(N = n(),
                                      `Mean Target` = round(mean(`% Target`, na.rm = T),digits=2),
                                      `Median Target` = round(median(`% Target`, na.rm = T),digits=2),
                                      `Std Dev` = round(sd(`% Target`, na.rm = T),digits=2),
                                      .groups = 'keep') %>% arrange(Facet)
            } else {
                t1 = df %>% summarize(N = n(),
                                      `Mean Target` = round(mean(`% Target`, na.rm = T),digits=2),
                                      `Median Target` = round(median(`% Target`, na.rm = T),digits=2),
                                      `Std Dev` = round(sd(`% Target`, na.rm = T),digits=2),
                                      .groups = 'keep')
            }
            return(t1)
        } else { # Metadata
            if(input$metaid ==''){
                if(isTruthy(input$plot_facet) & input$plot_facet != 'None'){
                    if(input$plot_facet=='Batch (multiple input files only)'){
                        names(df)[names(df)=='Batch'] = 'FAC'
                    } else {
                        names(df)[names(df)==input$plot_facet] = 'FAC'
                    }
                    if(isTruthy(input$plot_cluster) & input$plot_cluster != 'None' & input$plot_control == 'Multiple'){
                        names(df)[names(df)==input$plot_cluster] = 'CLUS'
                        t1 = df %>% group_by(GROUPVAR,FAC,CLUS)
                    } else {
                        t1 = df %>% group_by(GROUPVAR,FAC)
                    }
                } else {
                    if(isTruthy(input$plot_cluster) & input$plot_cluster != 'None' & input$plot_control == 'Multiple'){
                        names(df)[names(df)==input$plot_cluster] = 'CLUS'
                        t1 = df %>% group_by(GROUPVAR,CLUS)
                    } else {
                        t1 = df %>% group_by(GROUPVAR)
                    }
                }
            } else {
                t1 = df %>% group_by(GROUPVAR)
            }
            
            t1 = t1 %>% summarize(N = n(),
                                 `Mean Target` = round(mean(`% Target`, na.rm = T),digits=2),
                                 `Median Target` = round(median(`% Target`, na.rm = T),digits=2),
                                 `Std Dev` = round(sd(`% Target`, na.rm = T),digits=2),
                                  .groups = 'keep') %>%
                mutate(GROUPVAR = as.character(GROUPVAR))
            if(input$metaid =='' & isTruthy(input$plot_facet) & input$plot_facet != 'None'){
                t1 = t1 %>% arrange(FAC)
            }
            names(t1)[names(t1)=='GROUPVAR'] = 'Group'
            names(t1)[names(t1)=='CLUS'] = 'Cluster'
            names(t1)[names(t1)=='FAC'] = 'Facet'
            if(input$metaid ==''){
                t1 = t1 %>% 
                    mutate(`X var` = input$groupname) %>%
                    select(`X var`,any_of('Facet'),Group, any_of('Cluster'), everything())
            } else {
                t1 = t1 %>% 
                    select(any_of('Facet'),Group, everything())
            }
            return(t1)
        }
    })
    
    output$sum3 <- renderDataTable({
        return(sum3())
    })
    output$download_sum3 <- downloadHandler(
        filename = paste(Sys.Date(),'_flow_stattable', ".csv", sep=''),
        content = function(filename) {
            write.csv(sum3(), filename, row.names = F)
        }
    )

}
shinyApp(ui = ui, server = server)