#Developed by Avril Metcalfe-Roach
library(shiny)
library(shinythemes)
library(tidyverse)
library(markdown)

ui <- fluidPage(
    theme = shinytheme("slate"),
    
    # App title ----
    titlePanel("BIOL 112 - Transcription and Translation Problem Generator"),
    
        # MAIN PANEL ----
        mainPanel(
            
            fluidRow(
                column(
                    width = 12,
                        actionButton("do", 
                                 label = "Generate Problem", 
                                 icon = icon("sync"), 
                                 style="border-color: #FE5800"),
                        actionButton("button_show", 
                                 label = "  Show All Answers", 
                                 icon = icon("eye"), 
                                 style = "success"),
                        actionButton("button_hide", 
                                 label = "  Hide All Answers", 
                                 icon = icon("eye-slash"), 
                                 style = "success"),)
            ),
            # 
            h4(htmlOutput(outputId = 'parameters')),
            h4(htmlOutput(outputId = "text1")),
            h4(htmlOutput(outputId = "text2")),
            br(),
            h3("Questions:"),
            h4(htmlOutput(outputId = "num_start")),
            actionButton("button1", "Show/Hide"),
            h4(htmlOutput(outputId = "template")),
            actionButton("button2", "Show/Hide"),
            h4(htmlOutput(outputId = "mrna")),
            actionButton("button3", "Show/Hide"),
            h4(htmlOutput(outputId = "pep")),
            actionButton("button4", "Show/Hide"),
           )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SERVER LOGIC ----

server <- function(input, output) {

    button1_value <<- 0
    button2_value <<- 0
    button3_value <<- 0
    button4_value <<- 0
    
output$parameters <- renderText({
    text = "<br/>Press <b>Generate Problem<b/> to get started!<br/>"
    HTML(text)
})
    
data <- eventReactive(input$do, {
        
        # Basic parameters
        ct = read.csv('codontable.csv')
        ct_nostop = ct %>% filter(Name != 'Stop')
        ct_stop = ct %>% filter(Name == 'Stop')
        total_length = 35
        
        # Set random parameters
        direction = sample(c('top','bottom'),1)
        num_codons = sample(c(2,3,4,c(0:4)),1)
        protein = sample(ct_nostop$Codon,num_codons, replace = T) %>% str_flatten()
        
        output$parameters <- renderText({
            text = paste0("<br/>A bacterial gene was sequenced and a small stretch of this double-stranded DNA is shown below. ",
                          "Only the <b>start codon (AUG), ",as.character(num_codons)," amino acid(s), and the stop codon (UAA, UAG, or UGA)</b> of the protein are represented ",
                          "by this DNA sequence (i.e. the DNA downstream of the promoter, after the +1 site).<br/><br/>",sep='')
            HTML(text)
        })
        
        # Assemble coding strand
        final_string = paste0('ATG',protein,sample(ct_stop$Codon,1))
        num_bases_needed = 35-str_length(final_string)
        extra_bases_1 = sample(c(0:num_bases_needed),1)
        extra_bases_2 = num_bases_needed-extra_bases_1
        complete_string = paste0(str_flatten(sample(c('A','T','C','G'),
                                                    extra_bases_1, replace = T)),
                                 final_string,
                                 str_flatten(sample(c('A','T','C','G'),
                                                    extra_bases_2, replace = T)))
        #~~~~~~~~
        # Add wrong start codons
        # All unchangeable bases: start/stop codon, any bases that would run into start/stop
        untouchable = c(c((extra_bases_1-1):(extra_bases_1+3)),
                        c((total_length-extra_bases_2-4):(total_length-extra_bases_2))) %>% unique()
        
        #Determine # of fake start codons
        fake_atg = sample(c(0,1),1)
        fake_cat = sample(c(1,2),1)
        # Add fake atg
        changeable = c(1:total_length)
        changeable = changeable[!(changeable %in% untouchable)]
        if (fake_atg==1){
            temp = sample(changeable,1)
            remove_from_changeable_for_gta = c(temp,temp+1,temp+2)
            changeable = changeable[!(changeable %in% remove_from_changeable_for_gta)]
            complete_string = str_split(complete_string,pattern='') %>% unlist()
            complete_string[temp:(temp+2)] = c('A','T','G')
            complete_string = str_flatten(complete_string)
        }
        # Add fake cat
        while(fake_cat>0){
            temp = sample(changeable,1)
            changeable = changeable[!(changeable %in% c(temp,temp+1,temp+2))]
            complete_string = str_split(complete_string,pattern='') %>% unlist()
            complete_string[temp:(temp+2)] = c('C','A','T')
            complete_string = str_flatten(complete_string)
            fake_cat = fake_cat-1
        }
        
        # Check for multiple correct answers
        num_atg = str_locate_all(complete_string, 'ATG')[[1]][,1]
        num_atg = num_atg[num_atg != (extra_bases_1+1)]
        for (i in num_atg){
            # i=num_atg[1]
            temp = i+3+(3*num_codons)
            temp2 = str_sub(complete_string,temp,temp+2)
            if(temp2 %in% ct_stop$Codon){
                if(temp %in% changeable){
                    complete_string = str_split(complete_string,pattern='') %>% unlist()
                    complete_string[temp] = 'C'
                    complete_string = str_flatten(complete_string)
                    changeable = changeable[changeable != temp]
                } else if((temp+1) %in% changeable){
                    complete_string = str_split(complete_string,pattern='') %>% unlist()
                    complete_string[temp+1] = 'C'
                    complete_string = str_flatten(complete_string)
                    changeable = changeable[changeable != (1+temp)]
                } else if((temp+2) %in% changeable){
                    complete_string = str_split(complete_string,pattern='') %>% unlist()
                    complete_string[temp+2] = 'C'
                    complete_string = str_flatten(complete_string)
                    changeable = changeable[changeable != (2+temp)]
                } else {
                    print('Error - please refresh app')
                }
            }
        }
        num_cat = str_locate_all(complete_string, 'CAT')[[1]][,1]
        for (i in num_cat){
            # i=num_atg[1]
            temp = i-3-(3*num_codons)
            temp2 = str_sub(complete_string,temp,temp+2)
            if(temp2 %in% c('TTA','CTA','TCA')){
                if(temp %in% changeable){
                    complete_string = str_split(complete_string,pattern='') %>% unlist()
                    complete_string[temp] = 'G'
                    complete_string = str_flatten(complete_string)
                    changeable = changeable[changeable != temp]
                } else if((temp+1) %in% changeable){
                    complete_string = str_split(complete_string,pattern='') %>% unlist()
                    complete_string[temp+1] = 'G'
                    complete_string = str_flatten(complete_string)
                    changeable = changeable[changeable != (1+temp)]
                } else if((temp+2) %in% changeable){
                    complete_string = str_split(complete_string,pattern='') %>% unlist()
                    complete_string[temp+2] = 'G'
                    complete_string = str_flatten(complete_string)
                    changeable = changeable[changeable != (2+temp)]
                } else {
                    print('Error - please refresh app')
                }
            }
        }
        #~~~~~~~~
        
        # Reverse if necessary
        if(direction=='bottom'){
            template_strand <- 'Top'
            complete_string = intToUtf8(rev(utf8ToInt(complete_string)))
        } else {
            template_strand <- 'Bottom'
        }
        
        # Create template strand
        df = tibble(coding = str_split(complete_string,pattern='') %>% unlist())
        base_key = tibble(coding = c('A','T','C','G'), template = c('T','A','G','C'), mRNA = c('A','U','C','G'))
        df = df %>% left_join(base_key)
        
        # Combine - will have to print one on top of each other
        if(direction=='top'){
            output_top = paste0('5-',str_flatten(df$coding),'-3')
            output_bottom = paste0('3-',str_flatten(df$template),'-5')
        } else {
            output_top = paste0('5-',str_flatten(df$template),'-3')
            output_bottom = paste0('3-',str_flatten(df$coding),'-5')
        }
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # Correct answers:
        # Assumes everything is after +1 site
        
        num_start = as.character(length(num_atg)+length(num_cat))
        
        if(direction=='bottom'){
            mrna_sequence = paste0('5-',
                                   intToUtf8(rev(utf8ToInt(str_flatten(df$mRNA)))),
                                   '-3')
        } else {
            mrna_sequence = paste0('5-', str_flatten(df$mRNA), '-3')
        }
        
        # Peptide sequence
        pep = c('ATG')
        while(str_length(protein)>0){
            pep = c(pep,str_sub(protein, end=3))
            protein = str_sub(protein,start = 4)
        }
        
        pep_out = tibble(Codon = pep) %>% left_join(ct)
        pep_out = paste0('N-',str_flatten(pep_out$AA, collapse = '-'),'-C')
        
        # Render answers
        output$num_start <- renderText({
            if(is.null(button1_value)) { 
                HTML('1. How many potential start codons are there?') 
            } else if(button1_value%%2==0){
                HTML('1. How many potential start codons are there?')
            } else {
                d = data()
                HTML(paste0('1. How many potential start codons are there?  <b>',num_start,'<b/>',sep=''))
            }
            
        })
        output$template <- renderText({
            if(is.null(button2_value)) { 
                HTML('2. Which strand is the TEMPLATE strand?') 
            } else if(button2_value%%2==0){
                HTML('2. Which strand is the TEMPLATE strand?')
            } else {
                d = data()
                HTML(paste0('2. Which strand is the TEMPLATE strand?  <b>',template_strand,'<b/>',sep=''))
            }
            
        })
        output$mrna <- renderText({
            if(is.null(button3_value)) { 
                HTML('3. What is the corresponding mRNA sequence?') 
            } else if(button3_value%%2==0){
                HTML('3. What is the corresponding mRNA sequence?')
            } else {
                d = data()
                HTML(paste0('3. What is the corresponding mRNA sequence? <br/><b>',mrna_sequence,'<b/>',sep=''))
            }
            
        })
        output$pep <- renderText({
            if(is.null(button4_value)) { 
                HTML('4. What is the corresponding peptide sequence?') 
            } else if(button4_value%%2==0){
                HTML('4. What is the corresponding peptide sequence?')
            } else {
                d = data()
                HTML(paste0('4. What is the corresponding peptide sequence?<br/><b>',pep_out,'<b/>',sep=''))
            }
            
        })
        
        return(c(output_top, output_bottom, num_start, template_strand, mrna_sequence, pep_out))
    })

output$text1 <- renderText({
    d = data()
    HTML(d[1])
})
output$text2 <- renderText({
    d = data()
    HTML(d[2])
})

observeEvent(input$button_show, {
    button1_value <<- 1
    button2_value <<- 1
    button3_value <<- 1
    button4_value <<- 1
    d = data()
    output$num_start <- renderText({
        HTML(paste0('1. How many potential start codons are there?  <b>',d[3],'<b/>',sep=''))
    })
    output$template <- renderText({
        HTML(paste0('2. Which strand is the TEMPLATE strand?  <b>',d[4],'<b/>',sep=''))
    })
    output$mrna <- renderText({
        HTML(paste0('3. What is the corresponding mRNA sequence? <br/><b>',d[5],'<b/>',sep=''))
    })
    output$pep <- renderText({
        HTML(paste0('4. What is the corresponding peptide sequence?<br/><b>',d[6],'<b/>',sep=''))
    })
})
observeEvent(input$button_hide, {
    button1_value <<- 0
    button2_value <<- 0
    button3_value <<- 0
    button4_value <<- 0
    output$num_start <- renderText({
        HTML('1. How many potential start codons are there?')
    })
    output$template <- renderText({
        HTML('2. Which strand is the TEMPLATE strand?')
    })
    output$mrna <- renderText({
        HTML('3. What is the corresponding mRNA sequence?')
    })
    output$pep <- renderText({
        HTML('4. What is the corresponding peptide sequence?')
    })
})

observeEvent(input$button1, {
    button1_value <<- button1_value + 1
    output$num_start <- renderText({
        if(is.null(button1_value)) { 
            HTML('1. How many potential start codons are there?') 
        } else if(button1_value%%2==0){
            HTML('1. How many potential start codons are there?')
        } else {
            d = data()
            HTML(paste0('1. How many potential start codons are there?  <b>',d[3],'<b/>',sep=''))
        }
        
    })
})
observeEvent(input$button2, {
    button2_value <<- button2_value + 1
    output$template <- renderText({
        if(is.null(button2_value)) { 
            HTML('2. Which strand is the TEMPLATE strand?') 
        } else if(button2_value%%2==0){
            HTML('2. Which strand is the TEMPLATE strand?')
        } else {
            d = data()
            HTML(paste0('2. Which strand is the TEMPLATE strand?  <b>',d[4],'<b/>',sep=''))
        }
        
    })
})
observeEvent(input$button3, {
    button3_value <<- button3_value + 1
    output$mrna <- renderText({
        if(is.null(button3_value)) { 
            HTML('3. What is the corresponding mRNA sequence?') 
        } else if(button3_value%%2==0){
            HTML('3. What is the corresponding mRNA sequence?')
        } else {
            d = data()
            HTML(paste0('3. What is the corresponding mRNA sequence? <br/><b>',d[5],'<b/>',sep=''))
        }
        
    })
})
observeEvent(input$button4, {
    button4_value <<- button4_value + 1
    output$pep <- renderText({
        if(is.null(button4_value)) { 
            HTML('4. What is the corresponding peptide sequence?') 
        } else if(button4_value%%2==0){
            HTML('4. What is the corresponding peptide sequence?')
        } else {
            d = data()
            HTML(paste0('4. What is the corresponding peptide sequence?<br/><b>',d[6],'<b/>',sep=''))
        }
        
    })
})

}
shinyApp(ui = ui, server = server)
