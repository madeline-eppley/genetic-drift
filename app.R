library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  titlePanel("Queerness, Reproduction, and Genetic Drift"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pop_size", "Starting Population Size", value = 75, min = 10, max = 100, step = 10),
      numericInput("rounds", "Generations (Simulation Rounds)", value = 5, min = 1, max = 10, step = 1),
      actionButton("simulate", "Run Simulation"), 
      h3("Simulation"),
      p("In this simulation, four haploid alleles represent individuals within a metapopulation. A reproductive trait, which is associated with each individual for the duration of that individuals life, impacts how alleles are passed on from each generation. When an individual reproduces, their offpsring has the same allele. Each generation, after reproduction, a subset of individuals die and are removed from the metapopulation. The simulation plays out over 5-10 generations and then reports resulting allele frequencies and reproductive trait strategy frequencies in the metapopulation."),
      h3("Reproductive Traits"),
      tags$ul(
        tags$li("Reproductive: Alleles paired with this trait will produce one offspring per generation"), 
        tags$li("Non-Reproductive: Alleles paired with this trait will not reproduce in the next generation"), 
        tags$li("Multiple Partners: Alleles paired with this trait will reproduce with multiple partners, producing between 2-3 offspring per generation"),
        tags$li("Disperser: Alleles paired with this trait “migrate” between each round, distributing alleles across the metapopulation each generation"),  
        tags$li("Self-Fertilization: Alleles paired with this trait will produce one offspring in the next generation"), 
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation", 
                 plotOutput("allele_plot"),
                 tableOutput("allele_table"),
                 plotOutput("trait_plot"),
                 tableOutput("trait_table")
        ),
        tabPanel("Statistical Analysis", 
                 h3("Chi-squared Test"),
                 p("Here, a statistical analysis (chi-squared test) has been conducted on the simulated data in our 'Simulation' tab. Any time that the simulation is re-run, the statistical analysis will also be automatically re-run and reflect the calculation on the newest version of the simulation."),
                 verbatimTextOutput("chi_square_allele"),
                 verbatimTextOutput("chi_square_trait"), 
                 
                 h3("Understanding False Positives in Statistical Tests"),
                
                 
                 tags$aside(
                   h4("Calculating an expected rate of false positive tests"),
                   span("A statistically significant p-value (e.g., below 0.05) does not always indicate a true effect. In small populations or when many tests are run, some results may be due to"), strong("random chance"), ("rather than a real biological pattern."), ("Let's simulate false positives in an Evolution class, where each student runs the the chi-squared test once on our simulated data with a significance threshold of p < 0.05. If we have 160 students enrolled in the Evolution class, how many significant chi-squared tests would find by chance?"),
                   
                   tags$br(),
                   tags$br(),
                   strong("false positive result for each student"),
                   verbatimTextOutput("false_positive_sim"),
                   strong("total count of false positives"),
                   verbatimTextOutput("false_positive_count"),
                   span("Given that"), strong("TRUE = a detected false positive test (p < 0.05),"), ("outcomes are present, we can expect to see some significant results due to random chance alone."),
                   
                   tags$br(),
                   tags$br(), 
                   h4("Increasing sample size"),
                   p("Now let's examine the results if we have 160 students in Evolution class and we conduct this study over 20 semesters. How many significant chi-squared tests would we find by chance?"),
                   plotOutput("false_positive_plot"),
                   tags$br(),
                   p("We can interpret the histogram as the frequency of the number of false positives occurring per semester. Generally, we can expect to detect anywhere from 2 - 14 false positives per semester. So, we can conclude that genetic drift is a random process, and one statistically significant result does not necessarily mean there is a significant biological explanation for the outcome."),
                   
                   # Style applies to the aside tag correctly
                   style = "border: 2px solid black; background-color: #e0e0e0; padding: 10px; border-radius: 5px; text-align: center; width: 100%;"
                 ),
                 
                 h3("Evaluate your simulation results"), 
                 tags$ul(
                 tags$li("Did you detect a significant chi-squared test result for reproductive traits?"), 
                 tags$li("If so, how many times did you run the simulation?"),
                 tags$li("Do you think that your results were due to random chance or not?"),
                 ),
                 tags$br(),
                 tags$br()
        ),
        tabPanel("About", 
                 h3("About This Lesson Plan and Simulation Activity"),
                 p("The learning objective of this simulation is to integrate queer theory and fundamental evolutionary concepts to improve understanding of randomness and stochasticity in evolution. Specifically, this activity uses approaches from Queer theory to more inclusively teach the concept of genetic drift. This lesson was developed for the Genetic Drift working group within the Resources for Inclusive Education Working Group (RIE2)."),
                 h4("Author: Madeline G. Eppley"),
                 tags$ul(
                   tags$li(a("Website", href = "https://www.madeline-eppley.com")),
                   tags$li(a("GitHub", href = "https://www.github.com/madeline-eppley"))
                 ),
                 h4("Date Last Updated: 2025-02-18"),
                 h4("Lesson Plan and Resources"),
                 tags$ul(
                   tags$li(a("Lesson Plan on Genetic Drift", href="https://qubeshub.org/community/groups/rie2")),
                   tags$li(a("Resources for Inclusive Education Working Group", href="https://qubeshub.org/community/groups/rie2"))
                 )
        ),
        tabPanel("Pre-Simulation Learning Activity", 
                 h3("Learning about Queerness in Nature through Evolutionary Biology"),
                 h4("Pre-reading List"), 
                 tags$ul(
                   tags$li(a("What Evolutionary Biology Can and Can't Tell Us About Sex, Gender, and Sexuality", href="https://thereader.mitpress.mit.edu/what-evolutionary-biology-can-and-cant-tell-us-about-sex-gender-and-sexuality/")),
                   tags$li(a("Gender-Inclusive Biology Evolution’s Rainbow: A Queer Species Database of 200+ Organisms", href="https://riversuh.com/lesson-plans/2019/11/20/book-report-evolutions-rainbow-diversity-gender-amp-sexuality-in-nature-amp-people"))
                 ),
                 h4("Questions to Answer from Pre-readings"),
                 p("1: Are science concepts black-and-white, or is there ongoing discourse about terminology and definitions, even for seemingly simple concepts? Provide an example."),
                 p("2: Choose one organism from the Queer species database on the Gender-Inclusive Biology website. Answer the following questions:"),
                 p("2a: Did you know that the organism that you chose exhibited queerness before today? Write a few sentences describing the mechanism of queerness (e.g., is it gametic, morphological, behavioral, sexual, etc.)."),
                 p("2b: Open a Google Scholar search page and enter your organism's scientific and/or common name. Browse through the first ~5 papers. Do any of them mention queer behavior when describing the organism?"), 
                 p("2c: Do you think that assuming two sexes that directly correspond to gender gives an accurate representation of the diversity of organisms? Explain.")
        ),
        tabPanel("Post-Simulation Learning Activity", 
                 h3("Queerness and Its Impact on Understanding Genetic Drift"), 
                 h4("Discussion Questions"),
                 p("1. Queer theory proposes that there is no essentialism to sex or gender. Rather, gender and sex are performative actions and genetics are representative, descriptive conditions that create reality (Butler 2004; Prum 2023). Thus, individual genes cannot be the cause of any one trait or identity. Does the simulation activity support or refute essentialism?"),
                 tags$details("Alleles were unassociated with a reproductive approach. Instead, reproduction was aligned with the definition of descriptive condition from Queer theory. The idea of genetic essentialism is upended by the activity."),
                 tags$br(),
                 tags$br(),
                 p("2. Queer theory proposes that identities are not fixed or binary, rather they are fluid and continuous interpretations of self that can change over time. How can this perspective help us better understand biological processes like genetic drift?"),
                 tags$details("Randomness can be expected in many aspects of the natural world and science. Although science is often considered “black-and-white”, this is not often the case. Accepting randomness and  identities that transcend the binary furthers inclusive science and avoids misconceptions around random processes like genetic drift."),
                 tags$br(), 
                 tags$br(),
                 p("3. Did any of the colors seem ‘destined’ to win or lose? How does this challenge the idea that genes determine everything about an organism? How does queer theory’s rejection of genetic determinism help us understand genetic drift?"), 
                 tags$details("No allele colors were “destined” to win or lose. The outcome of the activity was determined by randomness. Genes do not determine every trait an organism has, upending the idea of genetic determinism. Queer theory rejects genetic determinism.")
        )
      )
    )
  )
)

# start server
server <- function(input, output) {
  observeEvent(input$simulate, {
    
    # define alleles & repro traits
    alleles <- c("Z", "X", "Y", "C")
    traits <- c("Reproductive", "Non-Reproductive", "Multiple Partners", "Disperser", "Self-Fertilization")
    
    allele_colors <- c("Z" = "#0080FFFF", "X" = "#99EEFFFF", "Y" = "#FF8000FF", "C" = "#FFC44CFF")
    
    # Custom colors for reproductive strategies
    trait_colors <- c("Reproductive" = "#00496FFF", 
                      "Non-Reproductive" = "#0F85A0FF", 
                      "Multiple Partners" = "#EDD746FF", 
                      "Disperser" = "#ED8B00FF", 
                      "Self-Fertilization" = "#DD4124FF")
    
    # equal weights all traits
    trait_weights <- c(0.25, 0.25, 0.25, 0.25, 0.25)
    
    population <- data.frame(
      Allele = sample(alleles, input$pop_size, replace = TRUE),
      Trait = sample(traits, input$pop_size, replace = TRUE, prob = trait_weights)
    )
    
    results_allele <- list()
    results_trait <- list()
    
    for (round in 1:input$rounds) {
      
      # Step 1: Random allele loss (e.g. genetic drift)
      if (nrow(population) > 0) {
        remove_allele <- sample(1:nrow(population), size = round(nrow(population) * 0.25), replace = FALSE)
        population <- population[-remove_allele, ]
      }
      
      # Step 2: Reproduction
      new_offspring <- population %>%
        rowwise() %>%
        mutate(Offspring = case_when(
          Trait == "Reproductive" ~ 1,
          Trait == "Non-Reproductive" ~ 0,
          Trait == "Multiple Partners" ~ sample(2:3, 1),
          Trait == "Disperser" ~ 1, 
          Trait == "Self-Fertilization" ~ 1
        )) %>%
        ungroup()
      
      new_population <- data.frame(
        Allele = rep(new_offspring$Allele, new_offspring$Offspring),
        Trait = sample(traits, sum(new_offspring$Offspring), replace = TRUE, prob = trait_weights) # each offspring gets own repro strategy
      )
      
      population <- rbind(population, new_population)
      
      # Record results
      results_allele[[round]] <- population %>% count(Allele) %>% mutate(Round = round)
      results_trait[[round]] <- population %>% count(Trait) %>% mutate(Round = round)
    }
    
    results_allele_df <- bind_rows(results_allele)
    results_trait_df <- bind_rows(results_trait)
    
    final_alleles <- results_allele_df %>% filter(Round == input$rounds) %>% complete(Allele = alleles, fill = list(n = 0))
    final_traits <- results_trait_df %>% filter(Round == input$rounds) %>% complete(Trait = traits, fill = list(n = 0))
    
    # stats tests - chi-squared
    chi_sq_allele <- chisq.test(final_alleles$n, p = rep(1 / length(alleles), length(alleles)))
    chi_sq_trait <- chisq.test(final_traits$n, p = rep(1 / length(traits), length(traits)))
    
    # display statistical test results
    output$chi_square_allele <- renderPrint({
      chi_sq_allele
    })
    
    output$chi_square_trait <- renderPrint({
      chi_sq_trait
    })
    
    # display tables
    output$allele_table <- renderTable({
      final_alleles
    })
    
    output$trait_table <- renderTable({
      final_traits
    })
    
    output$allele_plot <- renderPlot({
      ggplot(results_allele_df, aes(x = Round, y = n, color = Allele, group = Allele)) +
        geom_line() + geom_point() +
        scale_color_manual(values = allele_colors) +
        labs(title = "allele frequencies in metapopulation", x = "Round", y = "Count", color = "Allele") +
        theme_minimal()
    })
    
    output$trait_plot <- renderPlot({
      ggplot(results_trait_df, aes(x = Round, y = n, color = Trait, group = Trait)) +
        geom_line() + geom_point() +
        scale_color_manual(values = trait_colors) +
        labs(title = "reproductive strategy frequencies in metapopulation", x = "Round", y = "Count", color = "Reproductive Strategy") +
        theme_minimal()
    })
    
    # false positive chi-squared test
    output$false_positive_sim <- renderPrint({
      false.pos <- (rchisq(160, df = 3) > qchisq(0.95, df = 3))  # TRUE if p < 0.05
      false.pos
    })
    
    # false positive chi-squared test
    output$false_positive_count <- renderPrint({
      false.pos <- (rchisq(160, df = 3) > qchisq(0.95, df = 3))  # TRUE if p < 0.05
      false.pos.counts <- sum(false.pos)  # count false pos
      false.pos.counts
    })
    
    output$false_positive_plot <- renderPlot({
      false.pos.counts <- replicate(20, sum(rchisq(160, df = 3) > qchisq(0.95, df = 3)))
      
      hist(false.pos.counts, 
           xlab = "false positive tests per semester", 
           main = "expected false positives over 20 semesters", 
           col = "#0F85A0FF", 
           border = "black")
    })
    
    
    
  })
}

# Run App
shinyApp(ui, server)