
rm(list = ls())

# Set working directory

#Load relevant packages
library(igraph)
library(RColorBrewer)
library(RSiena)
library(tidyverse) # for ggplot, dplyr and several other stuff
library(forcats) # for sorting within ggplot
library(glue) # easier than paste()
library(ggtext)
library(sysfonts)

# Load in the data
covariates <- read.csv("./1-data/covariates.csv", stringsAsFactors = F)

load("./2-analyses/analyses.RData")


# Respecify the data for plotting
for (i in seq_along(M)) {
  M[[i]][M[[i]] == 10 | is.na(M[[i]])] <- 0
}


############################################################################
#                        COVARIATE DESCIPTIVES                             #
############################################################################

gender <- data.frame(variable = "Gender", n = sum(!is.na(att$female)),
                                          mean = round(mean(att$female),2),
                                          sd = round(sd(att$female),2),
                                          median = round(median(att$female),2),
                                          range = paste(round(min(att$female),2),
                                            "-",round(max(att$female),2), sep = ""))

democrat <- data.frame(variable = "Democrat", n = sum(!is.na(att$democrat)),
                                          mean  = round(mean(att$democrat),2),
                                          sd = round(sd(att$democrat),2),
                                          median = round(median(att$democrat),2),
                                          range = paste(round(min(att$democrat),2),
                                            "-",round(max(att$democrat),2), sep = ""))

prior_years <- data.frame(variable = "Prior years in office", n = sum(!is.na(att$prior_years)),
                                          mean  = round(mean(att$prior_years),2),
                                          sd = round(sd(att$prior_years),2),
                                          median = round(median(att$prior_years),2),
                                          range = paste(round(min(att$prior_years),2),
                                            "-",round(max(att$prior_years),2), sep = ""))

senator1 <- data.frame(variable = "Senators 2005", n = sum(!is.na(att$senator1)),
                                          mean  = round(mean(att$senator1, na.rm = TRUE),2),
                                          sd = round(sd(att$senator1, na.rm = TRUE),2),
                                          median = round(median(att$senator1, na.rm = TRUE),2),
                                          range = paste(round(min(att$senator1, na.rm = TRUE),2),
                                            "-",round(max(att$senator1, na.rm = TRUE),2), sep = ""))

senator2 <- data.frame(variable = "Senators 2006", n = sum(!is.na(att$senator2)),
                                          mean  = round(mean(att$senator2, na.rm = TRUE),2),
                                          sd = round(sd(att$senator2, na.rm = TRUE),2),
                                          median = round(median(att$senator2, na.rm = TRUE),2),
                                          range = paste(round(min(att$senator2, na.rm = TRUE),2),
                                            "-",round(max(att$senator2, na.rm = TRUE),2), sep = ""))

senator3 <- data.frame(variable = "Senators 2007", n = sum(!is.na(att$senator3)),
                                          mean  = round(mean(att$senator3, na.rm = TRUE),2),
                                          sd = round(sd(att$senator3, na.rm = TRUE),2),
                                          median = round(median(att$senator3, na.rm = TRUE),2),
                                          range = paste(round(min(att$senator3, na.rm = TRUE),2),
                                            "-",round(max(att$senator3, na.rm = TRUE),2), sep = ""))

senator4 <- data.frame(variable = "Senators 2008", n = sum(!is.na(att$senator4)),
                                          mean  = round(mean(att$senator4, na.rm = TRUE),2),
                                          sd = round(sd(att$senator4, na.rm = TRUE),2),
                                          median = round(median(att$senator4, na.rm = TRUE),2),
                                          range = paste(round(min(att$senator4, na.rm = TRUE),2),
                                            "-",round(max(att$senator4, na.rm = TRUE),2), sep = ""))

leader1 <- data.frame(variable = "Leaders 2005", n = sum(!is.na(att$leader1)),
                                          mean  = round(mean(att$leader1, na.rm = TRUE),2),
                                          sd = round(sd(att$leader1, na.rm = TRUE),2),
                                          median = round(median(att$leader1, na.rm = TRUE),2),
                                          range = paste(round(min(att$leader1, na.rm = TRUE),2),
                                            "-",round(max(att$leader1, na.rm = TRUE),2), sep = ""))

leader2 <- data.frame(variable = "Leaders 2006", n = sum(!is.na(att$leader2)),
                                          mean  = round(mean(att$leader2, na.rm = TRUE),2),
                                          sd = round(sd(att$leader2, na.rm = TRUE),2),
                                          median = round(median(att$leader2, na.rm = TRUE),2),
                                          range = paste(round(min(att$leader2, na.rm = TRUE),2),
                                            "-",round(max(att$leader2, na.rm = TRUE),2), sep = ""))

leader3 <- data.frame(variable = "Leaders 2007", n = sum(!is.na(att$leader3)),
                                          mean  = round(mean(att$leader3, na.rm = TRUE),2),
                                          sd = round(sd(att$leader3, na.rm = TRUE),2),
                                          median = round(median(att$leader3, na.rm = TRUE),2),
                                          range = paste(round(min(att$leader3, na.rm = TRUE),2),
                                            "-",round(max(att$leader3, na.rm = TRUE),2), sep = ""))

leader4 <- data.frame(variable = "Leaders 2008", n = sum(!is.na(att$leader4)),
                                          mean  = round(mean(att$leader4, na.rm = TRUE),2),
                                          sd = round(sd(att$leader4, na.rm = TRUE),2),
                                          median = round(median(att$leader4, na.rm = TRUE),2),
                                          range = paste(round(min(att$leader4, na.rm = TRUE),2),
                                            "-",round(max(att$leader4, na.rm = TRUE),2), sep = ""))

intro1 <- data.frame(variable = "Bills introduced 2005", n = sum(!is.na(att$intro1)),
                                        mean  = round(mean(att$intro1, na.rm = TRUE),2),
                                        sd = round(sd(att$intro1, na.rm = TRUE),2),
                                        median = round(median(att$intro1, na.rm = TRUE),2),
                                        range = paste(round(min(att$intro1, na.rm = TRUE),2),
                                          "-",round(max(att$intro1, na.rm = TRUE),2), sep = ""))


intro2 <- data.frame(variable = "Bills introduced 2006", n = sum(!is.na(att$intro2)),
                                          mean  = round(mean(att$intro2, na.rm = TRUE),2),
                                          sd = round(sd(att$intro2, na.rm = TRUE),2),
                                          median = round(median(att$intro2, na.rm = TRUE),2),
                                          range = paste(round(min(att$intro2, na.rm = TRUE),2),
                                            "-",round(max(att$intro2, na.rm = TRUE),2), sep = ""))

intro3 <- data.frame(variable = "Bills introduced 2007", n = sum(!is.na(att$intro3)),
                                            mean  = round(mean(att$intro3, na.rm = TRUE),2),
                                          sd = round(sd(att$intro3, na.rm = TRUE),2),
                                          median = round(median(att$intro3, na.rm = TRUE),2),
                                          range = paste(round(min(att$intro3, na.rm = TRUE),2),
                                            "-",round(max(att$intro3, na.rm = TRUE),2), sep = ""))

intro4 <- data.frame(variable = "Bills introduced 2008", n = sum(!is.na(att$intro4)),
                                          mean  = round(mean(att$intro4, na.rm = TRUE),2),
                                          sd = round(sd(att$intro4, na.rm = TRUE),2),
                                          median = round(median(att$intro4, na.rm = TRUE),2),
                                          range = paste(round(min(att$intro4, na.rm = TRUE),2),
                                            "-",round(max(att$intro4, na.rm = TRUE),2), sep = ""))

sponsored1 <- data.frame(variable = "Bills sponsored 2005", n = sum(!is.na(att$sponsored1)),
                                        mean  = round(mean(att$sponsored1, na.rm = TRUE),2),
                                        sd = round(sd(att$sponsored1, na.rm = TRUE),2),
                                        median = round(median(att$sponsored1, na.rm = TRUE),2),
                                        range = paste(round(min(att$sponsored1, na.rm = TRUE),2),
                                          "-",round(max(att$sponsored1, na.rm = TRUE),2), sep = ""))


sponsored2 <- data.frame(variable = "Bills sponsored 2006", n = sum(!is.na(att$sponsored2)),
                                          mean  = round(mean(att$sponsored2, na.rm = TRUE),2),
                                          sd = round(sd(att$sponsored2, na.rm = TRUE),2),
                                          median = round(median(att$sponsored2, na.rm = TRUE),2),
                                          range = paste(round(min(att$sponsored2, na.rm = TRUE),2),
                                            "-",round(max(att$sponsored2, na.rm = TRUE),2), sep = ""))

sponsored3 <- data.frame(variable = "Bills sponsored 2007", n = sum(!is.na(att$sponsored3)),
                                            mean  = round(mean(att$sponsored3, na.rm = TRUE),2),
                                          sd = round(sd(att$sponsored3, na.rm = TRUE),2),
                                          median = round(median(att$sponsored3, na.rm = TRUE),2),
                                          range = paste(round(min(att$sponsored3, na.rm = TRUE),2),
                                            "-",round(max(att$sponsored3, na.rm = TRUE),2), sep = ""))

sponsored4 <- data.frame(variable = "Bills sponsored 2008", n = sum(!is.na(att$sponsored4)),
                                          mean  = round(mean(att$sponsored4, na.rm = TRUE),2),
                                          sd = round(sd(att$sponsored4, na.rm = TRUE),2),
                                          median = round(median(att$sponsored4, na.rm = TRUE),2),
                                          range = paste(round(min(att$sponsored4, na.rm = TRUE),2),
                                            "-",round(max(att$sponsored4, na.rm = TRUE),2), sep = ""))

cov_des <- rbind(gender, democrat, prior_years, senator1, senator2, senator3, senator4,
                  leader1, leader2, leader3, leader4, intro1, intro2, intro3, intro4,
                  sponsored1, sponsored2, sponsored3, sponsored4)
write.csv(cov_des,"./3-manuscript/tables/sm-table-1.csv", row.names = F)

############################################################################
#                 Plot network graphs for all networks                     #
############################################################################

# Specify as igraph object
g1 <- graph_from_adjacency_matrix(M[[1]][2:nrow(M[[1]]),2:ncol(M[[1]])], mode = "directed", diag = F)
V(g1)$name <-att$m_id
g2 <- graph_from_adjacency_matrix(M[[2]][2:nrow(M[[2]]),2:ncol(M[[2]])], mode = "directed", diag = F)
V(g2)$name <-att$m_id
g3 <- graph_from_adjacency_matrix(M[[3]][2:nrow(M[[3]]),2:ncol(M[[3]])], mode = "directed", diag = F)
V(g3)$name <-att$m_id
g4 <- graph_from_adjacency_matrix(M[[4]][2:nrow(M[[4]]),2:ncol(M[[4]])], mode = "directed", diag = F)
V(g4)$name <-att$m_id

# Calculate number of isolates per year
sum(degree(g1) == 0)-sum(is.na(covariates$senator1))
sum(degree(g2) == 0)-sum(is.na(covariates$senator2))
sum(degree(g3) == 0)-sum(is.na(covariates$senator3))
sum(degree(g4) == 0)-sum(is.na(covariates$senator4))

# Remove isolates for plotting
g1 <- delete.vertices(g1 , which(degree(g1)==0))
g2 <- delete.vertices(g2 , which(degree(g2)==0))
g3 <- delete.vertices(g3 , which(degree(g3)==0))
g4 <- delete.vertices(g4 , which(degree(g4)==0))

V(g1)$democrat <- att$democrat[match(V(g1)$name, att$m_id)]
V(g1)$gender <- att$female[match(V(g1)$name, att$m_id)]
V(g1)$color <- ifelse(V(g1)$democrat == 1, "blue", "red")
E(g1)$color <- ifelse(is.mutual(g1), "black", "grey")
V(g1)$shape <- ifelse(V(g1)$gender == 1, "square", "circle")

V(g2)$democrat <- att$democrat[match(V(g2)$name, att$m_id)]
V(g2)$gender <- att$female[match(V(g2)$name, att$m_id)]
V(g2)$color <- ifelse(V(g2)$democrat == 1, "blue", "red")
E(g2)$color <- ifelse(is.mutual(g2), "black", "grey")
V(g2)$shape <- ifelse(V(g2)$gender == 1, "square", "circle")

V(g3)$democrat <- att$democrat[match(V(g3)$name, att$m_id)]
V(g3)$gender <- att$female[match(V(g3)$name, att$m_id)]
V(g3)$color <- ifelse(V(g3)$democrat == 1, "blue", "red")
E(g3)$color <- ifelse(is.mutual(g3), "black", "grey")
V(g3)$shape <- ifelse(V(g3)$gender == 1, "square", "circle")

V(g4)$democrat <- att$democrat[match(V(g4)$name, att$m_id)]
V(g4)$gender <- att$female[match(V(g4)$name, att$m_id)]
V(g4)$color <- ifelse(V(g4)$democrat == 1, "blue", "red")
E(g4)$color <- ifelse(is.mutual(g4), "black", "grey")
V(g4)$shape <- ifelse(V(g4)$gender == 1, "square", "circle")


png("./3-manuscript/figures/figure-1a.png", width = 1500, height = 1500, res = 200)
plot(g1,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     layout = layout_nicely
)
dev.off()

png("./3-manuscript/figures/figure-1b.png", width = 1500, height = 1500, res = 200)
plot(g2,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     layout = layout_nicely
)
dev.off()

png("./3-manuscript/figures/figure-1c.png", width = 1500, height = 1500, res = 200)
plot(g3,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     layout = layout_nicely
)
dev.off()

png("./3-manuscript/figures/figure-1d.png", width = 1500, height = 1500, res = 200)
plot(g4,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     layout = layout_nicely
)
dev.off()

############################################################################
#                         PARAMETER ESTIMATES TABLE                        #
############################################################################

#### Please note that further formatting of table 1 in the main manuscript was done in LaTeX

options(scipen=999)
results <- model1
parameter <- results$effects$effectName
estimate <- results$theta
st_error <- sqrt(diag(results$covtheta))
norm_var <- estimate/st_error
pval <- 2*pnorm(abs(norm_var), lower.tail = FALSE)

results_out <- data.frame(parameter,
                          estimate = round(estimate, 2),
                          SE = round(st_error, 2),
                          p = round(pval, 3),
                          OR = round(exp(estimate), 2),
                          CI = paste(round(exp(estimate -
                                                 qnorm(0.975)*st_error), 2),
                                     round(exp(estimate + qnorm(0.975)*st_error), 2), sep = "-"),
                          CI.low = round(estimate -
                                           qnorm(0.975)*st_error, 2),
                          CI.high = round(estimate + qnorm(0.975)*st_error, 2),
                          stringsAsFactors = FALSE
)

results_out[1:3, 5:8] <- "-"
results_out[2, 5:8] <- "-"
results_out$p[results_out$p == 0] <- "<.001"
results_out

results_out[1 , 1] <- "Rate: 2005-2006"
results_out[2 , 1] <- "Rate: 2006-2007"
results_out[3 , 1] <- "Rate: 2007-2008"
results_out[4 , 1] <- "Outdegree"
results_out[5 , 1] <- "Reciprocity"
results_out[6 , 1] <- "Indegree popularity (sqrt)"
results_out[7 , 1] <- "Outdegree popularity (sqrt)"
results_out[8 , 1] <- "Outdegree activity (sqrt)"

sm_table2a <- results_out[,-c(7,8)]

write.csv(sm_table2a, "./3-manuscript/tables/sm-table-2a.csv", row.names = FALSE)

# Read out table of results for extended model

results <- model2
parameter <- results$effects$effectName
estimate <- results$theta
st_error <- sqrt(diag(results$covtheta))
norm_var <- estimate/st_error
pval <- 2*pnorm(abs(norm_var), lower.tail = FALSE)

results_out <- data.frame(parameter,
                          estimate = round(estimate, 2),
                          SE = round(st_error, 2),
                          p = round(pval, 3),
                          OR = round(exp(estimate), 2),
                          CI = paste(round(exp(estimate -
                                                 qnorm(0.975)*st_error), 2),
                                     round(exp(estimate + qnorm(0.975)*st_error), 2), sep = "-"),
                          CI.low = round(estimate -
                                           qnorm(0.975)*st_error, 2),
                          CI.high = round(estimate + qnorm(0.975)*st_error, 2),
                          stringsAsFactors = FALSE
)

results_out[1:3, 5:8] <- "-"
results_out[2, 5:8] <- "-"
results_out$p[results_out$p == 0] <- "<.001"
results_out

results_out[1 , 1] <- "Rate: 2005-2006"
results_out[2 , 1] <- "Rate: 2006-2007"
results_out[3 , 1] <- "Rate: 2007-2008"
results_out[4 , 1] <- "Outdegree"
results_out[5 , 1] <- "Reciprocity"
results_out[6 , 1] <- "Indegree popularity (sqrt)"
results_out[7 , 1] <- "Outdegree popularity (sqrt)"
results_out[8 , 1] <- "Outdegree activity (sqrt)"
results_out[9 , 1] <- "Prior years alter"
results_out[10 , 1] <- "Prior years ego"
results_out[11 , 1] <- "Prior years similarity"
results_out[12 , 1] <- "Senator ego"
results_out[13 , 1] <- "Leader alter"
results_out[14 , 1] <- "Leader ego"
results_out[15 , 1] <- "Leader same"

results_out <- results_out[-c(16:17), ]
sm_table2b <- results_out[,-c(7,8)]

write.csv(sm_table2b, "./3-manuscript/tables/sm-table-2b.csv", row.names = FALSE)

# Read out table of results for extended model

results <- model3
parameter <- results$effects$effectName
estimate <- results$theta
st_error <- sqrt(diag(results$covtheta))
norm_var <- estimate/st_error
pval <- 2*pnorm(abs(norm_var), lower.tail = FALSE)

results_out <- data.frame(parameter,
                          estimate = round(estimate, 2),
                          SE = round(st_error, 2),
                          p = round(pval, 3),
                          OR = round(exp(estimate), 2),
                          CI = paste(round(exp(estimate -
                                                 qnorm(0.975)*st_error), 2),
                                     round(exp(estimate + qnorm(0.975)*st_error), 2), sep = "-"),
                          CI.low = round(estimate -
                                           qnorm(0.975)*st_error, 2),
                          CI.high = round(estimate + qnorm(0.975)*st_error, 2),
                          stringsAsFactors = FALSE
)

results_out[1:3, 5:8] <- "-"
results_out[2, 5:8] <- "-"
results_out$p[results_out$p == 0] <- "<.001"
results_out

results_out[1 , 1] <- "Rate: 2005-2006"
results_out[2 , 1] <- "Rate: 2006-2007"
results_out[3 , 1] <- "Rate: 2007-2008"
results_out[4 , 1] <- "Outdegree"
results_out[5 , 1] <- "Reciprocity"
results_out[6 , 1] <- "Indegree popularity (sqrt)"
results_out[7 , 1] <- "Outdegree popularity (sqrt)"
results_out[8 , 1] <- "Outdegree activity (sqrt)"
results_out[9 , 1] <- "Female alter"
results_out[10 , 1] <- "Female ego"
results_out[11 , 1] <- "Female same"
results_out[12 , 1] <- "Female same reciprocity"
results_out[13 , 1] <- "democrat alter"
results_out[14 , 1] <- "democrat ego"
results_out[15 , 1] <- "democrat same"
results_out[16 , 1] <- "democrat same reciprocity"
results_out[17 , 1] <- "Prior years alter"
results_out[18 , 1] <- "Prior years ego"
results_out[19 , 1] <- "Prior years similarity"
results_out[20 , 1] <- "Senator ego"
results_out[21 , 1] <- "Leader alter"
results_out[22 , 1] <- "Leader ego"
results_out[23 , 1] <- "Leader same"

results_out <- results_out[-c(24:25), ]
table1 <- results_out[,-c(7,8)]

write.csv(table1, "./3-manuscript/tables/table-1.csv", row.names = FALSE)

############################################################################
#                         PARAMETER ESTIMATES PLOT                         #
############################################################################

results_out <- results_out[-c(1:4, 24:29), ]

figure3_data <- data.frame(
  parameter = results_out$parameter,
  est = results_out$estimate,
  lower = as.numeric(results_out$CI.low),
  upper = as.numeric(results_out$CI.high)
  )

figure3_data$parameter <- factor(figure3_data$parameter, levels = rev(unique(figure3_data$parameter)))


figure3 <- ggplot(data=figure3_data, aes(x=parameter, y=est, ymin=lower, ymax=upper )) +
    geom_pointrange(position=position_dodge(width = 0.5)) +
    geom_hline(yintercept= 0, lty=2) +
    element_text(family = "Lusitana") +
    coord_flip() +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=15, family = "Lusitana"), axis.title=element_text(size=15), legend.text=element_text(size=15))
figure3

<<<<<<< HEAD
ggsave(figure3, filename = "./3-manuscript/figures/figure-4.png", device = "png", dpi = 800)
=======
ggsave(figure3, filename = "./3-manuscript/figures/figure-3.png", device = "png", dpi = 800)
>>>>>>> 0dc0cd6955be4b6060cb35116d0b5b06b21a87eb

############################################################################
#                               GOF PLOT                                   #
############################################################################
load("./2-analyses/GoF.RData")

png("./3-manuscript/figures/sm-m1-indeg.png", width = 1200, height = 1200, res = 200)
plot(m1_indeg, ylab = " ", main = " ")
dev.off()

png("./3-manuscript/figures/sm-m2-indeg.png", width = 1200, height = 1200, res = 200)
plot(m2_indeg, ylab = " ", main = " ")
dev.off()

png("./3-manuscript/figures/sm-m3-indeg.png", width = 1200, height = 1200, res = 200)
plot(m3_indeg, ylab = " ", main = " ")
dev.off()

png("./3-manuscript/figures/sm-m1-outdeg.png", width = 1200, height = 1200, res = 200)
plot(m1_outdeg, ylab = " ", main = " ")
dev.off()

png("./3-manuscript/figures/sm-m2-outdeg.png", width = 1200, height = 1200, res = 200)
plot(m2_outdeg, ylab = " ", main = " ")
dev.off()

png("./3-manuscript/figures/sm-m3-outdeg.png", width = 1200, height = 1200, res = 200)
plot(m3_outdeg, ylab = " ", main = " ")
dev.off()

############################################
# END OF SCRIPT
############################################
