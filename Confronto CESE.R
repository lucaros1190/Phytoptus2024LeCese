
# Susceptibility of the different cultivar at Le Cese to P. avellanae
# infestations.
#
# Created by Mario Contarini on 17 June 2024
#
# Revised by Luca Rossini on 15 August 2024


# Acquisition of the data - File 'DatasetCese.csv'

filename <- file.choose()


data_mites = read.csv2(filename, header=T, sep=",", dec=".", na.string="NA")


head(data_mites)


year <- as.factor(data_mites$Year)
cultivar <- as.factor(data_mites$CV)
plant <- as.factor(data_mites$plant)
branch <- as.factor(data_mites$branch)
galls <- as.numeric(data_mites$galls)

# Check the levels

levels(year)
levels(cultivar)
levels(plant)
levels(branch)


# GLM - Whole dataset: 'galls' variable

library(lme4)

GenLin_WholeDataset <- glmer.nb(galls ~ year + cultivar + (1 | plant) +
                                  (1 | branch), data=data_mites)

summary(GenLin_WholeDataset)


# Pairwise comparison - Year + Management:

library(multcompView)
library(emmeans)

marginal_WholeDataset = emmeans(GenLin_WholeDataset, ~ year + cultivar)
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

# Pairwise comparison - Cultivar:

marginal_WholeDataset_CV = emmeans(GenLin_WholeDataset, ~ cultivar)
pairs(marginal_WholeDataset_CV, adjust="bonferroni")

# Letters of significance:

lettere_WholeDataset_CV <- cld(marginal_WholeDataset_CV, alpha=0.05, 
                                       Letters=letters, adjust="bonferroni")
lettere_WholeDataset_CV


# Overall plot of the results

library(ggplot2)

boxPlot_Complete <- ggplot(data_mites, aes(x=cultivar, y=galls,
                                           fill=cultivar)) + 
                    geom_boxplot(width=0.5) + 
                    xlab("Cultivar") + 
                    ylab("Number of big buds per branch") + 
                    ggtitle("Infestation level over the cultivars") +
                    theme(plot.title = element_text(hjust=0.5), 
                          text = element_text(size=21), legend.position = "none", 
                          axis.text.x = element_text(angle=90)) +
                    scale_x_discrete(labels = c('Barcelona', 'Ennis',
                                           'Marveille de Bollwiller', 'Negret',
                                           'Nocchione', 'Riccia di Talanico',
                                           'San Giovanni', 'Tombul', 'Tonda di Giffoni', 
                                           'Tonda Gentile', 'Tonda Gentile Romana'))

boxPlot_Complete


