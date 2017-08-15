library(grid)
library(extrafont)
library(Cairo)

loadfonts()

source("configs.R")
source("graphs.R")
source("data.R")

grid_completion_year <- function(dataset, yr) {
  #basic_sum_stats <- summarize_stats_2(dataset)

  completion.graph <- college.years.completion.anim(basic_sum_stats, yr)

  ### Grid layout stuff
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(completion.graph, vp = vplayout(2:9, 1:12))

  # Title
  grid.text("Percentage of Population with\nEach Year of College",
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))

  # Year
  grid.text(yr,
            vp=vplayout(2, 12),
            x=unit(0.1, "npc"),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=fontfam, fontface="bold", col=title.col, cex=2))


  ## MAIN TEXT
  xval <- 0.4
  yval <- 1.5
  line.gap <- 0.33
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.vp <- vplayout(4, 12)
  grid.text(expression("A much higher percentage of " *
                       phantom("white people") *
                       " than " *
                       phantom("black people") *
                       " complete"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("A much higher percentage of ") *
                       "white people" *
                       phantom(" than ") *
                       phantom("black people") *
                       phantom(" complete")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=white.col, cex=block.cex))
  grid.text(expression(phantom("A much higher percentage of ") *
                       phantom("white people") *
                       phantom(" than ") *
                       "black people" *
                       phantom(" complete")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=black.col, cex=block.cex))

  grid.text(expression("each successive year of college. Over time, the percentages"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("of both " *
                       phantom("white") *
                       " and " *
                       phantom("black") *
                       " people attending college have"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("of both ") *
                       "white" *
                       phantom(" and ") *
                       phantom("black") *
                       phantom(" people attending college have")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=white.col, cex=block.cex))
  grid.text(expression(phantom("of both ") *
                       phantom("white") *
                       phantom(" and ") *
                       "black" *
                       phantom(" people attending college have")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=black.col, cex=block.cex))

  grid.text(expression("increased, but the racial disparity remains large."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 3), "npc"),
            hjust=1,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

}


grid_timeseries <- function(dataset) {
  #college4.data <- summarize_stats_2(dataset)

  four.years.graph <- timeser.poster(college4.data)

  ### Grid layout stuff
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(four.years.graph, vp = vplayout(2:9, 1:12))

  # Title
  grid.text("Percentage of Population with\nFour Years of College",
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))

  ## MAIN TEXT
  xval <- 0.1
  yval <- 0.8
  line.gap <- 0.33
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  grid.text(expression("A much higher percentage of " *
                       phantom("white people") *
                       " than " *
                       phantom("black people")),
            vp=vplayout(3, 4),
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=0,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("A much higher percentage of ") *
                       "white people" *
                       phantom(" than ") *
                       phantom("black people")),
            vp=vplayout(3, 4),
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=0,
            gp=gpar(fontfamily=text.fontfam, col=white.col, cex=block.cex))
  grid.text(expression(phantom("A much higher percentage of ") *
                       phantom("white people") *
                       phantom(" than ") *
                       "black people"),
            vp=vplayout(3, 4),
            x=unit(xval, "npc"),
            y=unit(yval, "npc"),
            hjust=0,
            gp=gpar(fontfamily=text.fontfam, col=black.col, cex=block.cex))

  grid.text(expression("go to college and complete 4 years, and the gap has"),
            vp=vplayout(3, 4),
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=0,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("been growing dramatically over time. Completing"),
            vp=vplayout(3, 4),
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=0,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("4 years of college can increase lifetime"),
            vp=vplayout(3, 4),
            x=unit(xval, "npc"),
            y=unit(newline(yval, 3), "npc"),
            hjust=0,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("earnings by nearly a million dollars."),
            vp=vplayout(3, 4),
            x=unit(xval, "npc"),
            y=unit(newline(yval, 4), "npc"),
            hjust=0,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

}


make_timeseries_pdf <- function(dataset.file) {
  #data <- read_data(dataset.file)

  pdf("race_college_timeseries.pdf", family=fontfam, width = 12, height = 8)
  grid_timeseries(data)
  dev.off()
  embed_fonts("race_college_timeseries.pdf")
}

make_timeseries_svg <- function(dataset.file) {
  #data <- read_data(dataset.file)

  svg("race_college_timeseries.svg", family=fontfam, width = 12, height = 8)
  grid_timeseries(data)
  dev.off()
  #embed_fonts("race_college_infographic.pdf")
}


make_completion_year_pdf <- function(dataset.file, yr) {
  data <- read_data(dataset.file)

  fname <- paste(c("completion_year_",
                   yr,
                   ".pdf"),
                 sep="",
                 collapse="")
  pdf(fname, family=fontfam, width = 12, height = 8)
  grid_completion_year(data, yr)
  dev.off()
  embed_fonts(fname)
}

make_completion_year_svg <- function(dataset.file, yr) {
  #data <- read_data(dataset.file)

  fname <- paste(c("completion_year_",
                   yr,
                   ".svg"),
                 sep="",
                 collapse="")
  svg(fname, family=fontfam, width = 12, height = 8)
  grid_completion_year(data, yr)
  dev.off()
  #embed_fonts("race_college_infographic.pdf")
}
