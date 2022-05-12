#' Diagnostic plot of compound filtering
#'
#' This function creates several plots that visualizes compounds and filtering criteria. Requires filter_area and filter_freq to be run first, and optionally filter_ambient_ratio. It requires ggplot2.
#' @param chemtable the data frame of the data about the compounds
#' @param method the type of plot to produce, "rarity" or "ambient", see Details
#' @param yrange the number of order of magnitudes the log10 y-axis will span, NA shows all compounds
#' @param fontsize size of the compound labels
#' @param pointsize size of the largest point
#' @details The following plotting options are available. The color of points is set based on whether the compound met
#' the final filtering criteria (filter_final column), or the area and frequency filters.
#' \describe{
#'   \item{rarity}{Plots compound rarity (x-axis),
#' maximum peak area across samples (y-axis), mean nonzero area peak area (size), and
#' amounts relative to ambient controls (labels, if filter_ambient_ratio is run first)}
#'   \item{ambient}{Plots mean peak area in ambient controls (x-axis) versus in floral samples (y-axis),
#'   frequency in floral samples (size), and shows the cutoff used for filter_ambient_ratio (dotted line)}
#' }
#'
#' @keywords plot filter
#' @examples
#' data(GCMSfloral)
#' plot_filters(chemtable, option="rarity")
#' plot_filters(chemtable, option="ambient")
#' @export
plot_filters <- function(chemtable, option="rarity", yrange = 3, fontsize = 3, pointsize = 10) {

  if("ambient_ratio" %in% names(chemtable)) {
    min_ratio <- attr(chemtable,"ambient_ratio")
    chemtable$plot_label <- paste0(chemtable$name, "\n", round(chemtable$ambient_ratio,1),"\u00D7")
  } else {
    min_ratio <- NA
    chemtable$plot_label <- chemtable$name
  }

  if(!"filter_final" %in% names(chemtable)) {
    stop("Run combine_filters(chemtable) first to create \'filter_final\' column")
  }

  if(all(c("filter_area","filter_freq.floral") %in% names(chemtable))) {
    area_min_maximum <- attr(chemtable, "area_min_maximum")
    freq_min <- attr(chemtable, "freq_min")
    chemtable <- within(chemtable,
                        filter_status <- ifelse(filter_final, "Included in final table",
                                                ifelse(filter_area == "OK" & filter_freq.floral == "OK",
                                                       "Excluded - pass area & freq. filters", "Excluded")))
  } else {
    area_min_maximum <- NA
    freq_min <- NA
    chemtable <- within(chemtable,
                        filter_status <- ifelse(filter_final, "Included in final table", "Excluded"))
  }

  chemtable <- subset(chemtable, !is.na(mean.nonzero.floral))

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" must be installed to use this function.", call. = FALSE)
  }
  library(ggplot2)

  filter_status_pal <-c("Excluded"="red",
                        "Excluded - pass area & freq. filters"="orange",
                        "Included in final table"="forestgreen")
  diag_title <- paste0("Filtering diagnostics: ",
                      sum(chemtable$filter_final),"/",nrow(chemtable)," compounds included")

  if(option == "rarity") {
    ggplot(chemtable, aes(x=freq.floral, y=max.floral)) +
      geom_point(aes(size=mean.nonzero.floral, color=filter_status)) +
      geom_text(aes(label=plot_label, alpha=filter_status), size=fontsize, lineheight = 1) +
      geom_hline(yintercept=area_min_maximum, linetype=2)+
      geom_vline(xintercept=freq_min, linetype=2)+
      scale_x_continuous(limits=c(0,1), labels = scales::percent)+
      scale_y_log10(limits=c(max(chemtable$max.floral, na.rm=T)/(10^yrange),NA)) +
      scale_size_area(max_size=pointsize, labels=scales::scientific) +
      scale_alpha_manual(values=setNames(c(0,0.5,1), names(filter_status_pal))) +
      scale_color_manual(values=filter_status_pal)+
      labs(color="Filtering", alpha="Filtering", size = "Mean nonzero peak area",
           x="Frequency in floral samples", y="Maximum peak area in floral samples",
           title=diag_title,
           subtitle = "Bottom label: relative amount in floral vs. ambient, Inf = not in ambients")+
      theme_minimal()

  } else if(option == "ambient") {
    ggplot(chemtable, aes(x=mean.ambient, y=mean.floral)) +
      geom_point(aes(color=filter_status, size=freq.floral)) +
      geom_text(aes(alpha=filter_status, label=name), nudge_y=0.05, nudge_x=0.05, hjust=0, size=fontsize) +
      geom_abline(data=data.frame(yint = c(0, log10(min_ratio)), yslope=c(1,1),
                                  dashes = factor(c("Equal", "Minimum ratio"), levels=c("Minimum ratio", "Equal"))),
                  aes(intercept = yint, slope=yslope, linetype=dashes)) +
      scale_linetype_manual(values=c(2,1))+
      scale_size(range=c(0.2,pointsize), labels=scales::percent) +
      scale_alpha_manual(values=setNames(c(0,0,1), names(filter_status_pal))) +
      scale_color_manual(values=filter_status_pal)+
      scale_x_log10() +
      scale_y_log10(limits=c(max(chemtable$mean.floral, na.rm=T)/(10^yrange),NA)) +
      labs(color="Filtering", alpha="Filtering", size = "Frequency in floral samples",
           x="Mean peak area in ambient controls", y="Mean peak area in floral samples",
           title=diag_title, linetype="Ambient ratio") +
      theme_minimal()

  } else {
    stop("Enter a valid plotting  option")
  }
}
