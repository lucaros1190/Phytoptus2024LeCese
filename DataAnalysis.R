
# Data analysis galls different farming systems
# Created by Luca Rossini on 10 April 2024
# Last update 10 August 2024

# E-mail: luca.rossini@unitus.it


# Acquisition of the data - File 'DatasetGalls.csv'

filename <- file.choose()


data_galls = read.csv2(filename, header=T, sep=",", dec=".", na.string="NA")


head(data_galls)


year <- as.factor(data_galls$Year)
management <- as.factor(data_galls$Management)
site <- as.factor(data_galls$Site)
plant <- as.factor(data_galls$plant)
branch <- as.factor(data_galls$branch)
galls <- as.numeric(data_galls$galls)

  # Check the levels

levels(year)
levels(management)
levels(site)
levels(plant)
levels(branch)

  # Create sub-datasets for each year for a deeper analysis

dataset_2023 <- data_galls[data_galls == "2023", ]

    # Assign factors and numeric values

management_2023 <- as.factor(dataset_2023$Management)
site_2023 <- as.factor(dataset_2023$Site)
plant_2023 <- as.factor(dataset_2023$plant)
branch_2023 <- as.factor(dataset_2023$branch)
galls_2023 <- as.numeric(dataset_2023$galls)


dataset_2024 <- data_galls[data_galls$Year == "2024", ]

    # Assign factors and numeric values

management_2024 <- as.factor(dataset_2024$Management)
site_2024 <- as.factor(dataset_2024$Site)
plant_2024 <- as.factor(dataset_2024$plant)
branch_2024 <- as.factor(dataset_2024$branch)
galls_2024 <- as.numeric(dataset_2024$galls)



# GLM - Whole dataset: 'galls' variable

library(lme4)

GenLin_WholeDataset <- glmer.nb(galls ~ year + management + (1 | site) +  
                                  (1 | plant) + (1 | branch) , data=data_galls)

summary(GenLin_WholeDataset)


    # Pairwise comparison - Year + Management:

library(multcompView)
library(emmeans)

marginal_WholeDataset = emmeans(GenLin_WholeDataset, ~ year + management)
pairs(marginal_WholeDataset, adjust="bonferroni")

    # Letters of significance:

library(multcomp)

lettere_WholeDataset <- cld(marginal_WholeDataset, alpha=0.05, 
                                   Letters=letters, adjust="bonferroni")
lettere_WholeDataset

    # Pairwise comparison - Year:

marginal_WholeDataset_Year = emmeans(GenLin_WholeDataset, ~ year)
pairs(marginal_WholeDataset_Year, adjust="bonferroni")

    # Letters of significance:

lettere_WholeDataset_Year <- cld(marginal_WholeDataset_Year, alpha=0.05, 
                                    Letters=letters, adjust="bonferroni")
lettere_WholeDataset_Year

    # Pairwise comparison - Management:

marginal_WholeDataset_Management = emmeans(GenLin_WholeDataset, ~ management)
pairs(marginal_WholeDataset_Management, adjust="bonferroni")

    # Letters of significance:

lettere_WholeDataset_Management <- cld(marginal_WholeDataset_Management, alpha=0.05, 
                                      Letters=letters, adjust="bonferroni")
lettere_WholeDataset_Management



# GLM - Only Year 2023 to check the differences

GenLin_2023 <- glmer.nb(galls_2023 ~ management_2023 + (1 | site_2023) + 
                      (1 | plant_2023) + (1 | branch_2023) , data=dataset_2023)

summary(GenLin_2023)


    # Pairwise comparison - Management:

marginal_2023 = emmeans(GenLin_2023, ~ management_2023)
pairs(marginal_2023, adjust="bonferroni")

    # Letters of significance:

lettere_2023 <- cld(marginal_2023, alpha=0.05, 
                    Letters=letters, adjust="bonferroni")
lettere_2023



# GLM - Only Year 2024 to check the differences

GenLin_2024 <- glmer.nb(galls_2024 ~ management_2024 + (1 | site_2024) + 
                       (1 | plant_2024) + (1 | branch_2024) , data=dataset_2024)

summary(GenLin_2024)


# Pairwise comparison - Management:

marginal_2024 = emmeans(GenLin_2024, ~ management_2024)
pairs(marginal_2024, adjust="bonferroni")

# Letters of significance:

lettere_2024 <- cld(marginal_2024, alpha=0.05, 
                    Letters=letters, adjust="bonferroni")
lettere_2024






# Overall plot of the results

library(ggplot2)

boxPlot_Complete <- ggplot(data_galls, aes(x=year, y=galls,
                                           fill=management)) + 
                          geom_boxplot(width=0.5) + 
                          xlab("Year") + 
                          ylab("Number of big buds per branch") + 
                          ggtitle("Infestation level over the years") +
                          theme(plot.title = element_text(hjust=0.5), 
                                text = element_text(size=21)) + 
                          scale_fill_manual(drop = FALSE, name = "Management", 
                                labels = c("Organic","IPM", "Renaturalised"), 
                                values= alpha(c("green", "blue", "orange"), 0.5))

boxPlot_Complete

boxPlot_years <- ggplot(data_galls, aes(x=year, y=galls, fill=year)) + 
                        geom_boxplot(width=0.5, show.legend = F) + 
                        xlab("Year") + 
                        ylab("Number of big buds per branch") + 
                        ggtitle("Infestation level over the years") +
                        theme(plot.title = element_text(hjust=0.5), 
                        text = element_text(size=21))

boxPlot_years

boxPlot_2023 <- ggplot(dataset_2023, aes(x=management_2023, y=galls_2023,
                                         fill=management_2023)) + 
                          geom_boxplot(width=0.5, show.legend = F) + 
                          xlab("Management") + 
                          ylab("Number of big buds per branch") + 
                          ggtitle("Infestation level - Season 2023") +
                          theme(plot.title = element_text(hjust=0.5), 
                                text = element_text(size=21)) + 
                          scale_x_discrete(labels = c("Organic","IPM", "Renaturalised"))

boxPlot_2023


boxPlot_2024 <- ggplot(dataset_2024, aes(x=management_2024, y=galls_2024,
                                         fill=management_2024)) + 
                          geom_boxplot(width=0.5, show.legend = F) + 
                          xlab("Management") + 
                          ylab("Number of big buds per branch") + 
                          ggtitle("Infestation level - Season 2024") +
                          theme(plot.title = element_text(hjust=0.5), 
                                text = element_text(size=21)) +
                          scale_x_discrete(labels = c("Organic","IPM", "Renaturalised"))

boxPlot_2024

