library(grid)
library(tidyverse)
library(reshape2)
library(scales)
library(Cairo)
library(extrafont)

loadfonts()

summary_stats <- function(educ,
                          perwt=rep(1, length(educ))) {
  d11 <- sum((educ == 11) * perwt) / sum(perwt)
  d10 <- d11 + sum((educ == 10) * perwt) / sum(perwt)
  d09 <- d10 + sum((educ == 09) * perwt) / sum(perwt)
  d08 <- d09 + sum((educ == 08) * perwt) / sum(perwt)
  d07 <- d08 + sum((educ == 07) * perwt) / sum(perwt)
  d06 <- d07 + sum((educ == 06) * perwt) / sum(perwt)
  d05 <- d06 + sum((educ == 05) * perwt) / sum(perwt)
  d04 <- d05 + sum((educ == 04) * perwt) / sum(perwt)
  d03 <- d04 + sum((educ == 03) * perwt) / sum(perwt)
  d02 <- d03 + sum((educ == 02) * perwt) / sum(perwt)
  d01 <- d02 + sum((educ == 01) * perwt) / sum(perwt)
  d00 <- d01 + sum((educ == 00) * perwt) / sum(perwt)
  return(data.frame(d00, d01, d02, d03, d04, d05, d06, d07, d08, d09, d10, d11))
}

summary_stats_2 <- function(educ,
                            perwt=rep(1, length(educ)),
                            grades=c(07, 08, 09, 10, 11)) {
  college <- sum((educ %in% grades) * perwt)
  college.prop <- sum((educ %in% grades) * perwt) / sum(perwt)
  return(data.frame(college, college.prop))
}


# Race vars
white.race = 1
black.race = 2

# Race colors
white.col <- "darkgoldenrod1"
black.col <- "dodgerblue2"
fill.col <- "gray70"
text.col <- "gray48"
title.col <- "gray26"
fontfam <- "Courier"


theme_custom <- function() {
  # theme colors
  plot.bg <- "white"
  panel.bg <- plot.bg
  text.col <- text.col
  plot.title.col <- text.col
  axis.title.col <- text.col
  axis.text.col <- text.col
  axis.tick.col <- text.col
  #grid.col <- grid_col


  theme_light() +
  theme(
    plot.margin = margin(0, 0.75, 0.5, 0.75, "cm"),    
    text = element_text(family=fontfam),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    axis.text = element_text(colour = axis.text.col, size=18),
    plot.title = element_text(colour = plot.title.col, face = "bold", size = 18, vjust = 1),
    axis.title = element_text(colour = axis.title.col, face = "bold", size = 18),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    #panel.grid.major.y = element_line(colour = grid.col),
    #panel.grid.minor.y = element_blank(),
    #panel.spacing = margin(1, 2, 1, 2, unit='cm'),
    strip.text = element_text(size=18),
    #strip.background = element_rect(fill = main_text_col),
    axis.ticks = element_line(colour = axis.tick.col)
  )
}


# read data
#data <- read.csv("usa_00002.csv")


#basic_sum_stats = data %>%
#  group_by(YEAR, RACE) %>%
#  do(summary_stats(.$EDUC, .$PERWT))
#basic_sum_stats.melt = melt(basic_sum_stats, id.vars=c("YEAR", "RACE"))
college.years = c("d07", "d08", "d09", "d10", "d11")
college.years.labels = c("1", "2", "3", "4", "5+ years\nof college")

college.basic_sum_stats.melt = filter(basic_sum_stats.melt,
                                      variable %in% college.years)

subyears = c(1950, 1970, 2000, 2015)
subyears.college.basic_sum_stats.melt = filter(college.basic_sum_stats.melt,
                                               YEAR %in% subyears)
college.years.completion <- ggplot(data=subyears.college.basic_sum_stats.melt,
                                   aes(x=factor(variable,
                                                levels=college.years,
                                                labels=college.years.labels),
                                       y=value,
                                       group=factor(RACE),
                                       col=factor(RACE,
                                                  levels=c(white.race, black.race),
                                                  labels=c("White", "Black")))) +
                            geom_line(size=1) +
                            facet_wrap(~YEAR, ncol=1) +
                            scale_y_continuous(breaks=c(0.2,0.4),
                                               minor_breaks=c(),
                                               labels=scales::percent) +
                            xlab("") +
                            ylab("Percent of Population") +
                            #guides(colour=guide_legend(title="Race")) +
                            guides(colour=F) +
                            scale_color_manual(values=c(white.col, black.col)) +
                            theme_custom()




college4.grades = c(10)
#college4.data = data %>%
#  group_by(YEAR, RACE) %>%
#  do(summary_stats_2(.$EDUC, .$PERWT, grades=college4.grades))

black.college4.data = filter(college4.data, RACE == black.race)
white.college4.data = filter(college4.data, RACE == white.race)
ribbon.data = data.frame(YEAR=white.college4.data$YEAR,
                         white=white.college4.data$college.prop,
                         black=black.college4.data$college.prop)

annotation <- c("A much higher percentage of ", "White people", "\nthan ",
                "Black people", " go to college and\ncomplete 4 years, and the gap has been growing dramatically over time. Completing 4 years of college can increase lifetime earnings by nearly a million dollars.")
                          
timeser <-  ggplot(college4.data,
                   aes(x=YEAR,
                       y=college.prop,
                       group=factor(RACE),
                       col=factor(RACE,
                                  levels=c(white.race, black.race),
                                  labels=c("White", "Black")))) +
            geom_ribbon(data=ribbon.data,
                        aes(x=YEAR, ymax=white, ymin=black),
                        fill=fill.col,
                        alpha=0.3,
                        inherit.aes=F) +
            geom_line(size=2) +
            geom_point(size=4) +
            scale_x_continuous(breaks=c(1950, 1970, 1990, 2000,
                                        2008, 2015),
                               minor_breaks=c(2004, 2012)) +
            scale_y_continuous(minor_breaks=c(),
                               labels=scales::percent) +
            xlab("") +
            ylab("Percent Completing 4 Years of College") +
            #guides(colour=guide_legend(title="Race")) +
            guides(colour=F, fill=F, alpha=F) +
            scale_color_manual(values=c(white.col, black.col)) +
            theme_custom() +
            annotate("text", x=1952, y=0.13, label="hello")



### Grid layout stuff
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

pdf("race_college_infographic.pdf", family=fontfam, width = 24, height = 18)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(9, 12)))

grid.text("WHY WE NEED AFFIRMATIVE ACTION",
          vp=vplayout(1, 1:12),
          y=unit(0.5, "npc"),
          gp=gpar(fontfamily=fontfam, fontface="bold", col=title.col, cex=6))

#print(college.years.completion, vp = vplayout(2:4, 1:12))
#print(timeser, vp = vplayout(6:9, 1:12))

print(college.years.completion, vp = vplayout(2:9, 1:4))
print(timeser, vp = vplayout(2:9, 5:12))


## MAIN TEXT

grid.rect(vp=vplayout(3, 7),
          x=unit(-1.05, "npc"),
          y=unit(0.72, "npc"),
          hjust=0,
          width=unit(4.8, "npc"),
          height=unit(0.8, "npc"),
          gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))
grid.rect(vp=vplayout(3, 7),
          x=unit(-1.05, "npc"),
          y=unit(-0.08, "npc"),
          hjust=0,
          width=unit(3.4, "npc"),
          height=unit(0.8, "npc"),
          gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

grid.text(expression("A much higher percentage of " *
                     phantom("White people")),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(0.95, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))
grid.text(expression(phantom("A much higher percentage of ") *
                     "White people"),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(0.95, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=white.col, cex=2.4))

grid.text(expression("than " *
                     phantom("Black people") *
                     " go to college and"),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(0.7, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))
grid.text(expression(phantom("than ") *
                     "Black people" *
                     phantom(" go to college and")),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(0.7, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=black.col, cex=2.4))

grid.text(expression("complete 4 years, and the gap has"),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(0.45, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

grid.text(expression("been growing dramatically over"),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(0.20, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

grid.text(expression("time. Completing 4 years of"),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(-0.05, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

grid.text(expression("college can increase lifetime"),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(-0.30, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

grid.text(expression("earnings by nearly a"),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(-0.55, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

grid.text(expression("million dollars."),
          vp=vplayout(3, 7),
          x=unit(-1, "npc"),
          y=unit(-0.80, "npc"),
          hjust=0,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))



### SMALL TEXT

grid.rect(vp=vplayout(2, 3),
          x=unit(1.72, "npc"),
          y=unit(0.3, "npc"),
          width=unit(2.7, "npc"),
          height=unit(0.5, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

grid.text(expression("A higher percentage of " *
                     phantom("White people")),
          vp=vplayout(2, 3),
          x=unit(1.7, "npc"),
          y=unit(0.45, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))
grid.text(expression(phantom("A higher percentage of ") *
                     "White people"),
          vp=vplayout(2, 3),
          x=unit(1.7, "npc"),
          y=unit(0.45, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=white.col, cex=1.5))

grid.text(expression("than " *
                     phantom("Black people") *
                     " finish each"),
          vp=vplayout(2, 3),
          x=unit(1.7, "npc"),
          y=unit(0.3, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))
grid.text(expression(phantom("than ") *
                     "Black people" *
                     phantom(" finish each")),
          vp=vplayout(2, 3),
          x=unit(1.7, "npc"),
          y=unit(0.3, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=black.col, cex=1.5))

grid.text(expression("year of college."),
          vp=vplayout(2, 3),
          x=unit(1.7, "npc"),
          y=unit(0.15, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))


grid.rect(vp=vplayout(4, 3),
          x=unit(1.72, "npc"),
          y=unit(0.45, "npc"),
          width=unit(2.6, "npc"),
          height=unit(0.35, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

grid.text(expression("Each year completed has measurable"),
          vp=vplayout(4, 3),
          x=unit(1.7, "npc"),
          y=unit(0.5, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))

grid.text(expression("impacts on lifetime income."),
          vp=vplayout(4, 3),
          x=unit(1.7, "npc"),
          y=unit(0.35, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))



grid.rect(vp=vplayout(6, 3),
          x=unit(0.55, "npc"),
          y=unit(0.62, "npc"),
          width=unit(2.4, "npc"),
          height=unit(0.35, "npc"),
          gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

grid.text(expression("Over time, the racial disparity"),
          vp=vplayout(6, 3),
          x=unit(1.7, "npc"),
          y=unit(0.7, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))

grid.text(expression("has increased dramatically."),
          vp=vplayout(6, 3),
          x=unit(1.7, "npc"),
          y=unit(0.55, "npc"),
          hjust=1,
          gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))


dev.off()

embed_fonts("race_college_infographic.pdf")
