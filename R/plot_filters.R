#' Diagnostic plot of compound filtering
#'
#' This function creates several plots that visualizes compounds and filtering criteria. Requires filter_area and filter_freq to be run first, and optionally filter_ambient_ratio. It requires ggplot2.
#' @param chemtable the data frame of the data about the compounds
#' @param method the type of plot to produce, "rarity" or "ambient", see Details
#' @param yrange the number of orders of magnitude the log10 y-axis will span (for method "ambient") or the negative left limit of the  log2 x-axis (for method "volcano"), NA means no limit
#' @param fontsize size of the compound labels
#' @param pointsize size of the largest point
#' @param alpha_excluded transparency of text for compounds that passed some test but were excluded from the final filter
#' @details The following plotting options are available. The color of points is set based on whether the compound met
#' the final filtering criteria (filter_final column), or passed some test. See the pipeline vignette for details.
#' \describe{
#'   \item{rarity}{Plots compound rarity (x-axis),
#' maximum peak area across samples (y-axis), mean nonzero area peak area (size), and
#' amounts relative to ambient controls (labels, if filter_ambient_ratio is run first)}
#'   \item{ambient}{Plots mean peak area in ambient controls (x-axis) versus in floral samples (y-axis),
#'   frequency in floral samples (size), and shows the cutoff used for filter_ambient_ratio (dotted line)}
#'   \item{volcano}{Plots fold change (x-axis) and (adjusted) p-value (y-axis) comparing floral to ambient samples,
#'   frequency in floral samples (size), and shows the cutoff used for filter_ambient_ratio and alpha (dotted lines)}
#'   \item{prop}{Plots the proportion of chemicals that passed each test}
#' }
#'
#' @keywords plot filter
#' @examples
#' data(GCMSfloral)
#' plot_filters(chemtable, option="rarity")
#' plot_filters(chemtable, option="ambient")
#' @export
plot_filters <- function(chemtable, option="rarity", yrange = 3, fontsize = 3, pointsize = 10, alpha_excluded=0) {

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

  if(option == "rarity" & all(c("filter_area","filter_freq.floral") %in% names(chemtable))) {
    area_min_maximum <- attr(chemtable, "area_min_maximum")
    freq_min <- attr(chemtable, "freq_min")
    chemtable <- within(chemtable,
                        filter_status <- ifelse(filter_final, "Included in final table",
                                                ifelse(filter_area == "OK" & filter_freq.floral == "OK",
                                                       "Excluded - pass area & freq. filters", "Excluded")))
  } else if(option %in% c("ambient","volcano") & "filter_ambient_ttest" %in% names(chemtable)) {
    ttest_alpha <- attr(chemtable, "alpha")
    chemtable <- within(chemtable,
                        filter_status <- ifelse(filter_final, "Included in final table",
                                                ifelse(filter_ambient_ttest == "OK",
                                                       "Excluded - pass t-test", "Excluded")))
  } else if(option == "prop") {

  } else {
    area_min_maximum <- NA
    freq_min <- NA
    chemtable <- within(chemtable,
                        filter_status <- ifelse(filter_final, "Included in final table", "Excluded"))
  }

  diag_title <- paste0("Filtering diagnostics: ",
                       sum(chemtable$filter_final),"/",nrow(chemtable)," compounds included")

  chemtable <- subset(chemtable, !is.na(mean.nonzero.floral))

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" must be installed to use this function.", call. = FALSE)
  }
  library(ggplot2)

  filter_status_pal <-c("Excluded"="firebrick1",
                        "Exclude - pass"="orange",
                        "Included in final table"="limegreen")
  names(filter_status_pal)[2] <- c(rarity="Excluded - pass area & freq. filters",
                                   ambient="Excluded - pass t-test", volcano="Excluded - pass t-test")[option]

  if(option == "rarity") {
    ggplot(chemtable, aes(x=freq.floral, y=max.floral)) +
      geom_point(aes(size=mean.nonzero.floral, color=filter_status)) +
      geom_text(aes(label=plot_label, alpha=filter_status), size=fontsize, lineheight = 1) +
      geom_hline(yintercept=area_min_maximum, linetype=2)+
      geom_vline(xintercept=freq_min, linetype=2)+
      scale_x_continuous(limits=c(0,1), labels = scales::percent)+
      scale_y_log10(limits=c(max(chemtable$max.floral, na.rm=T)/(10^yrange),NA)) +
      scale_size_area(max_size=pointsize, labels=scales::scientific) +
      scale_alpha_manual(values=setNames(c(0,alpha_excluded,1), names(filter_status_pal))) +
      scale_color_manual(values=filter_status_pal)+
      labs(color="Filtering", alpha="Filtering", size = "Mean nonzero peak area",
           x="Frequency in floral samples", y="Maximum peak area in floral samples",
           title=diag_title,
           subtitle = "Bottom label: relative amount in floral vs. ambient, Inf = not in ambients")+
      theme_minimal()

  } else if(option == "ambient") {
    ggplot(chemtable, aes(x=mean.ambient, y=mean.floral)) +
      geom_point(aes(color=filter_status, size=freq.floral)) +
      geom_text(aes(alpha=filter_status, label=name), show.legend=F) +
      geom_abline(data=data.frame(yint = c(0, log10(min_ratio)), yslope=c(1,1),
                                  dashes = factor(c("Equal", "Minimum ratio"), levels=c("Minimum ratio", "Equal"))),
                  aes(intercept = yint, slope=yslope, linetype=dashes)) +
      scale_linetype_manual(values=c(2,1))+
      scale_size(range=c(0.2,pointsize), labels=scales::percent) +
      scale_alpha_manual(values=setNames(c(0,alpha_excluded,1), names(filter_status_pal))) +
      scale_color_manual(values=filter_status_pal)+
      scale_x_log10() +
      scale_y_log10(limits=c(max(chemtable$mean.floral, na.rm=T)/(10^yrange),NA)) +
      labs(color="Filtering", alpha="Filtering", size = "Frequency in floral samples",
           x="Mean peak area in ambient controls", y="Mean peak area in floral samples",
           title=diag_title, linetype="Ambient ratio") +
      theme_minimal()
  } else if(option == "volcano") {
    chemtable %>%
      ggplot(aes(x=log2(ambient_ratio), y=-log10(ambient_pvalue)))+
      geom_point(aes(color=filter_status, size=freq.floral))+
      geom_text(aes(label=name, alpha=filter_status), size=fontsize, show.legend=F) +
      geom_vline(xintercept=log2(1))+ geom_vline(xintercept=log2(min_ratio), linetype=2)+
      geom_hline(yintercept=-log10(ttest_alpha), linetype=2) +
      scale_size(range=c(0.2,pointsize), labels=scales::percent) +
      scale_alpha_manual(values=setNames(c(0,alpha_excluded,1), names(filter_status_pal))) +
      scale_color_manual(values=filter_status_pal)+
      scale_x_continuous(limits=c(-yrange,NA)) +
      labs(color="Filtering", alpha="Filtering", size = "Frequency in floral samples",
           x="log2(ambient ratio)", y="-log10(P)", title=diag_title) +
      theme_minimal()
  } else if(option == "prop") {
    chemtable[,grepl("name|filter_", colnames(chemtable))] %>%
      transform(filter_final = ifelse(filter_final, "OK", "Excluded")) %>%
      lapply(as.character) %>% as.data.frame() %>%
      reshape2::melt(id.vars="name") %>%
      transform(value = relevel(factor(value), "OK")) %>%
      ggplot(aes(y=variable, fill=value)) + geom_bar(position="fill") +
      scale_x_continuous(labels=scales::percent)+ labs(x="", y="", fill="") +
      theme_minimal() + scale_fill_brewer(type="qual") +
      theme(axis.text.y=element_text(hjust=0))
  } else {
    stop("Enter a valid plotting option")
  }
}

