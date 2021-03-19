# Libraries ----
library(tidyverse)
library(report)
# library(ggpubr)
library(patchwork)
library(viridis)
library(ggfortify)
library(pheatmap)
library(ComplexHeatmap)
library(circlize)
library(readxl)
library(plyr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(EnhancedVolcano)
library(SummarizedExperiment)
library(csaw)
library(edgeR)
library(seriation)
library(ggsci)
library(RColorBrewer)
library(rcartocolor)
ht_opt$message <- FALSE


# Colors ----
cols <- rev(inferno(n = 10))[c(4, 8)]
cols <- c(MSUS = cols[1], Control = cols[2])


# Colorbar ----
myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])

myPalette2 <- colorRampPalette(colors = rev(brewer.pal(n = 11, name = "RdBu")))

# Theme ----
th <- theme_bw(base_family = "Helvetica") +
  theme(
    legend.background = element_rect(),

    # title, subtitle, and caption
    plot.title = element_text(
      colour = "black",
      angle = 0,
      size = 10,
      face = "bold",
      vjust = 1
    ),
    plot.subtitle = element_text(
      colour = "black",
      angle = 0,
      size = 8,
      face = "plain",
      vjust = 1
    ),
    plot.caption = element_text(
      colour = "black",
      angle = 0,
      size = 8,
      face = "plain",
      vjust = 1,
      hjust = 0.5
    ),

    # axis text
    axis.text.x = element_text(
      colour = "black",
      angle = 0,
      size = 8,
      # vjust = 0.5,
      # hjust = 0.5
    ),
    axis.text.y = element_text(
      colour = "black",
      angle = 0,
      size = 8,
      # vjust = 0.5,
      # hjust = 0.5
    ),
    axis.title.x = element_text(
      colour = "black",
      size = 10,
      face = "plain"
    ),
    axis.title.y = element_text(
      colour = "black",
      size = 10,
      face = "plain"
    ),

    # legend
    legend.position = "right",
    legend.key = element_blank(),
    # legend.key.size = unit(0, "cm"),
    legend.text = element_text(
      colour = "black",
      size = 8,
      # margin = margin(r = 0, t = 0, b = 2, l = 0, unit = "pt"),
      vjust = 0, hjust = 0
    ),
    legend.title = element_text(size = 10, face = "plain", colour = "black"),
    legend.justification = "top",
    legend.margin = margin(c(0, 0, 0, 0)),
    legend.box.margin = margin(0, -10, -7, -5),
    legend.key.height = unit(4, "mm"),
    legend.key.width = unit(4, "mm"),
    legend.spacing = unit(2, "mm"),
    plot.margin = grid::unit(c(0, 10, 0, 0), "mm"),
    strip.background = element_rect(fill = NA, colour = "black"),
    strip.text.x = element_text(face = "plain", colour = "black", size = 9)
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
    strip.background = element_rect(color = "black", size = 0.7)
  )


hm_cols <- function(x, col = viridis(3)) {
  x[is.na(x)] <- 0
  q <- quantile(data.matrix(x), c(0.01, 0.99))
  ramp <- colorRamp2(c(min(q), 0, max(q)), col)
  return(ramp)
}


#' Title
#'
#' @param x A vector of strings
#' @param minSizeForBreak minimum break size
#' @param lb length break
#' @param nb number of breaks
#'
#' @return
#' @export
#'
#' @examples

breakStrings <- function(x, minSizeForBreak = 20, lb = "\n", nb = 2) {
  sapply(x, minSizeForBreak = minSizeForBreak, lb = lb, FUN = function(x,minSizeForBreak, lb) {
    if (nchar(x) <= minSizeForBreak) {
      return(x)
    }
    
    g <- gregexpr(" ", x)[[1]]
    if (length(g) == 0) {
      return(x)
    }
    if (length(g) == 1 & all(g == -1)) {
      return(x)
    }
    
    if(nb == 2){
      mid <- nchar(x) / 2
      mid <- g[order(abs(g - mid))[1]]
      substr(x, mid, mid) <- lb
    } else if(nb == 3){
      mid1 <- round(nchar(x) / 3)
      mid2 <- mid1*2
      mid1 <- g[order(abs(g - mid1))[1]]
      mid2 <- g[order(abs(g - mid2))[1]]
      substr(x, mid1, mid1) <- lb
      substr(x, mid2, mid2) <- lb
    }
    
    return(x)
  })
}
