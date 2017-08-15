library(grid)
library(extrafont)
library(Cairo)

loadfonts()

source("configs.R")
source("graphs.R")
source("data.R")

grid_poster <- function(dataset) {
  #basic_sum_stats <- summarize_stats_1(dataset)
  #college4.data <- summarize_stats_2(dataset)

  years.completed.graph <- college.years.completion.poster(basic_sum_stats)
  four.years.graph <- timeser.poster(college4.data)

  ### Grid layout stuff
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  grid.text("WHY WE NEED AFFIRMATIVE ACTION",
            vp=vplayout(1, 1:12),
            y=unit(0.5, "npc"),
            gp=gpar(fontfamily=fontfam, fontface="bold", col=title.col, cex=6))

  # Graphs
  print(years.completed.graph, vp = vplayout(2:9, 1:4))
  print(four.years.graph, vp = vplayout(2:9, 5:12))

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
            width=unit(3.8, "npc"),
            height=unit(0.8, "npc"),
            gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))
  grid.rect(vp=vplayout(3, 7),
            x=unit(-1.05, "npc"),
            y=unit(-0.68, "npc"),
            hjust=0,
            width=unit(2.5, "npc"),
            height=unit(0.5, "npc"),
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

}


grid_poster_notitle <- function(dataset) {
  #basic_sum_stats <- summarize_stats_1(dataset)
  #college4.data <- summarize_stats_2(dataset)

  years.completed.graph <- college.years.completion.poster(basic_sum_stats)
  four.years.graph <- timeser.poster(college4.data)

  ### Grid layout stuff
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(8, 12)))

  # Graphs
  print(years.completed.graph, vp = vplayout(1:8, 1:4))
  print(four.years.graph, vp = vplayout(1:8, 5:12))

  ## MAIN TEXT

  grid.rect(vp=vplayout(2, 7),
            x=unit(-1.05, "npc"),
            y=unit(0.72, "npc"),
            hjust=0,
            width=unit(4.8, "npc"),
            height=unit(0.8, "npc"),
            gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))
  grid.rect(vp=vplayout(2, 7),
            x=unit(-1.05, "npc"),
            y=unit(-0.08, "npc"),
            hjust=0,
            width=unit(3.8, "npc"),
            height=unit(0.8, "npc"),
            gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))
  grid.rect(vp=vplayout(2, 7),
            x=unit(-1.05, "npc"),
            y=unit(-0.68, "npc"),
            hjust=0,
            width=unit(2.5, "npc"),
            height=unit(0.5, "npc"),
            gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

  grid.text(expression("A much higher percentage of " *
                       phantom("White people")),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(0.95, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))
  grid.text(expression(phantom("A much higher percentage of ") *
                       "White people"),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(0.95, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=white.col, cex=2.4))

  grid.text(expression("than " *
                       phantom("Black people") *
                       " go to college and"),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(0.7, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))
  grid.text(expression(phantom("than ") *
                       "Black people" *
                       phantom(" go to college and")),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(0.7, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=black.col, cex=2.4))

  grid.text(expression("complete 4 years, and the gap has"),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(0.45, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

  grid.text(expression("been growing dramatically over"),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(0.20, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

  grid.text(expression("time. Completing 4 years of"),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(-0.05, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

  grid.text(expression("college can increase lifetime"),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(-0.30, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

  grid.text(expression("earnings by nearly a"),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(-0.55, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))

  grid.text(expression("million dollars."),
            vp=vplayout(2, 7),
            x=unit(-1, "npc"),
            y=unit(-0.80, "npc"),
            hjust=0,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=2.4))



  ### SMALL TEXT

  grid.rect(vp=vplayout(1, 3),
            x=unit(1.72, "npc"),
            y=unit(0.3, "npc"),
            width=unit(2.7, "npc"),
            height=unit(0.5, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

  grid.text(expression("A higher percentage of " *
                       phantom("White people")),
            vp=vplayout(1, 3),
            x=unit(1.7, "npc"),
            y=unit(0.45, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))
  grid.text(expression(phantom("A higher percentage of ") *
                       "White people"),
            vp=vplayout(1, 3),
            x=unit(1.7, "npc"),
            y=unit(0.45, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=white.col, cex=1.5))

  grid.text(expression("than " *
                       phantom("Black people") *
                       " finish each"),
            vp=vplayout(1, 3),
            x=unit(1.7, "npc"),
            y=unit(0.3, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))
  grid.text(expression(phantom("than ") *
                       "Black people" *
                       phantom(" finish each")),
            vp=vplayout(1, 3),
            x=unit(1.7, "npc"),
            y=unit(0.3, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=black.col, cex=1.5))

  grid.text(expression("year of college."),
            vp=vplayout(1, 3),
            x=unit(1.7, "npc"),
            y=unit(0.15, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))


  grid.rect(vp=vplayout(3, 3),
            x=unit(1.72, "npc"),
            y=unit(0.45, "npc"),
            width=unit(2.6, "npc"),
            height=unit(0.35, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

  grid.text(expression("Each year completed has measurable"),
            vp=vplayout(3, 3),
            x=unit(1.7, "npc"),
            y=unit(0.5, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))

  grid.text(expression("impacts on lifetime income."),
            vp=vplayout(3, 3),
            x=unit(1.7, "npc"),
            y=unit(0.35, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))



  grid.rect(vp=vplayout(5, 3),
            x=unit(0.55, "npc"),
            y=unit(0.62, "npc"),
            width=unit(2.4, "npc"),
            height=unit(0.35, "npc"),
            gp=gpar(fontfamily=fontfam, col="white", fill="white", alpha=1))

  grid.text(expression("Over time, the racial disparity"),
            vp=vplayout(5, 3),
            x=unit(1.7, "npc"),
            y=unit(0.7, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))

  grid.text(expression("has increased dramatically."),
            vp=vplayout(5, 3),
            x=unit(1.7, "npc"),
            y=unit(0.55, "npc"),
            hjust=1,
            gp=gpar(fontfamily=fontfam, col=text.col, cex=1.5))

}


make_poster_pdf <- function(dataset.file, title=T) {
  #data <- read_data(dataset.file)

  pdf("race_college_infographic.pdf", family=fontfam, width = 24, height = 18)
  if (title) {
    grid_poster(data)
  } else {
    grid_poster_notitle(data)
  }
  dev.off()
  embed_fonts("race_college_infographic.pdf")
}

make_poster_svg <- function(dataset.file, title=T) {
  #data <- read_data(dataset.file)

  svg("race_college_infographic.svg", family=fontfam, width = 24, height = 18)
  if (title) {
    grid_poster(data)
  } else {
    grid_poster_notitle(data)
  }
  dev.off()
}
