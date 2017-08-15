library(tidyverse)
library(scales)
library(ggthemes)

source("configs.R")

theme_custom_2 <- function() {
  # theme colors
  plot.bg <- "white"
  panel.bg <- plot.bg
  text.col <- text.col
  plot.title.col <- text.col
  axis.title.col <- text.col
  axis.text.col <- text.col
  axis.tick.col <- text.col
  grid.col <- "gray80"


  theme_wsj() +
  theme(
    plot.margin = margin(0.75, 0.75, 0.5, 0.75, "cm"),    
    text = element_text(family=fontfam),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    axis.text = element_text(colour = axis.text.col, size=18),
    plot.title = element_text(colour = plot.title.col, face = "bold", size = 18, vjust = 1),
    axis.title = element_text(colour = axis.title.col, face = "bold", size = 18),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid.col),
    #panel.grid.minor.y = element_blank(),
    #panel.spacing = margin(1, 2, 1, 2, unit='cm'),
    strip.text = element_text(size=18),
    #strip.background = element_rect(fill = main_text_col),
    axis.ticks = element_line(colour = axis.tick.col)
  )
}

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
    plot.margin = margin(0.75, 0.75, 0.5, 0.75, "cm"),    
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


college.years.completion <- function(dataset) {
  basic_sum_stats.melt = melt(dataset, id.vars=c("YEAR", "RACE"))
  college.basic_sum_stats.melt = filter(basic_sum_stats.melt,
                                        variable %in% college.years)
  subyears.college.basic_sum_stats.melt = filter(college.basic_sum_stats.melt,
                                                 YEAR %in% subyears)

  ggplot(data=subyears.college.basic_sum_stats.melt,
         aes(x=factor(variable,
                      levels=college.years,
                      labels=college.years.labels),
             y=value,
             group=factor(RACE),
             col=factor(RACE,
                        levels=c(white.race, black.race),
                        labels=c("White", "Black")))) +
    geom_line(size=1) +
    scale_y_continuous(breaks=c(0.2,0.4),
                       minor_breaks=c(),
                       labels=scales::percent) +
    xlab("") +
    ylab("") +
    #guides(colour=guide_legend(title="Race")) +
    guides(colour=F) +
    scale_color_manual(values=c(white.col, black.col)) +
    theme_custom()
}
college.years.completion.poster <- function(dataset) {
  college.years.completion(dataset) +
    facet_wrap(~YEAR, ncol=1)
}
college.years.completion.horiz <- function(dataset) {
  college.years.completion(dataset) +
    facet_wrap(~YEAR, nrow=1)
}


college.years.completion.anim <- function(dataset, yr) {
  basic_sum_stats.melt = melt(dataset, id.vars=c("YEAR", "RACE"))
  college.basic_sum_stats.melt.all = filter(basic_sum_stats.melt,
                                            variable %in% college.years,
                                            YEAR <= yr)
  college.basic_sum_stats.melt.year = filter(college.basic_sum_stats.melt.all,
                                             variable %in% college.years,
                                             YEAR == yr)

  ggplot(data=college.basic_sum_stats.melt.year,
         aes(x=factor(variable,
                      levels=college.years,
                      labels=college.years.labels),
             y=value,
             group=interaction(RACE, YEAR),
             col=factor(RACE,
                        levels=c(white.race, black.race),
                        labels=c("White", "Black")))) +
    geom_line(size=1) +
    geom_line(data=college.basic_sum_stats.melt.all,
              alpha=0.1) +
    scale_y_continuous(limits=c(0,0.43),
                       breaks=c(0.2,0.4),
                       minor_breaks=c(),
                       labels=scales::percent) +
    xlab("") +
    ylab("") +
    guides(colour=F) +
    scale_color_manual(values=c(white.col, black.col)) +
    theme_custom_2() +
    geom_label(aes(x=1, y=0.4, label="completing..."),
               color=text.col, nudge_x=-0.35, label.size=NA, size=4,
               fontface="bold")
}



timeser.poster <- function(dataset) {
  black.college4.data = filter(dataset, RACE == black.race)
  white.college4.data = filter(dataset, RACE == white.race)
  ribbon.data = data.frame(YEAR=white.college4.data$YEAR,
                           white=white.college4.data$college.prop,
                           black=black.college4.data$college.prop)
                            
  ggplot(dataset,
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
    scale_y_continuous(breaks=c(0.05, 0.1, 0.15),
                       minor_breaks=c(),
                       labels=scales::percent) +
    xlab("") +
    ylab("") +
    #guides(colour=guide_legend(title="Race")) +
    guides(colour=F, fill=F, alpha=F) +
    scale_color_manual(values=c(white.col, black.col)) +
    theme_custom_2() +
    geom_label(aes(x=1950, y=0.15, label="with 4 yrs. college"),
               color=text.col, nudge_x=1.5, label.size=NA, size=4,
               fontface="bold")
}
