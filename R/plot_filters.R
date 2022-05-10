#' Diagnostic plot of compound filtering
#'
#' This function creates a plot that visualizes compound rarity (x-axis),
#' maximum peak area across samples (y-axis), mean nonzero area peak area (size),
#' amounts relative to ambient controls (labels), and whether the compound met
#' the final filtering critera (filter_final column), or the area and frquency filters.
#' Requires filter_area and gilter_freq to be run first.
#' @param chemtable the data frame of the data about the compounds
#' @param yrange the number of order of magnitudes the log10 y-axis will span
#' @param fontsize size of the compound labels
#' @param pointsize size of the largest point
#' @keywords plot filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- make_chemtable(longdata, metadata)
#' chemtable <- filter_area(chemtable, min_maximum = 20000)
#' chemtable <- filter_freq(chemtable, 0.2)
#' chemtable <- combine_filters(chemtable)
#' plot_filters(chemtable)
#' @export
plot_filters <- function(chemtable, yrange = 3, fontsize = 3, pointsize = 15) {
  chemtable <- subset(chemtable, !is.na(mean.nonzero.floral))
  if(!"filter_final" %in% names(chemtable)) {
    stop("Run combine_filters(chemtable) first to create \'filter_final\' column")
  }
  if(all(c("filter_area","filter_freq.floral") %in% names(chemtable))) {
    chemtable <- within(chemtable,
      filter_status <- ifelse(filter_final, "Included in final table",
                              ifelse(filter_area == "OK" & filter_freq.floral == "OK",
                                     "Excluded - pass area & freq. filters", "Excluded")))
  } else {
    chemtable <- within(chemtable,
                        filter_status <- ifelse(filter_final, "Included in final table", "Excluded"))
  }
  if("ambient_ratio" %in% names(chemtable)) {
    chemtable$plot_label <- paste0(chemtable$name, "\n", round(chemtable$ambient_ratio,1),"\u00D7")
  } else {
    chemtable$plot_label <- chemtable$name
  }


  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" must be installed to use this function.", call. = FALSE)
  }
  library(ggplot2)
  ggplot(chemtable, aes(x=freq.floral, y=max.floral)) +
    geom_point(aes(size=mean.nonzero.floral, color=filter_status)) +
    geom_text(aes(label=plot_label, alpha=filter_status), size=fontsize, lineheight = 1) +
    scale_x_continuous(limits=c(0,1), labels = scales::percent)+
    scale_y_log10(limits=c(max(chemtable$max.floral, na.rm=T)/(10^yrange),NA)) +
    scale_size_area(max_size=pointsize, labels=scales::scientific) +
    scale_alpha_manual(values=c("Excluded"=0,
                                "Excluded - pass area & freq. filters"=0.5,
                                "Included in final table"=1)) +
    scale_color_manual(values=c("Excluded"="red",
                                "Excluded - pass area & freq. filters"="orange",
                                "Included in final table"="palegreen")) +
    labs(color="Filtering", alpha="Filtering", size = "Mean nonzero peak area",
         x="Frequency in floral samples", y="Maximum peak area in floral samples",
         title=paste("Filtering diagnostics:",
                     sum(chemtable$filter_final),"of",nrow(chemtable),"compounds in floral samples included"),
         subtitle = "Bottom label: relative amount in floral vs. ambient, Inf = not in ambients")+
    theme_minimal()
}
