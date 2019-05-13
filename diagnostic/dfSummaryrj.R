library(summarytools)
dfSummaryrj <- function (x, distributions, round.digits = st_options("round.digits"), varnumbers = st_options("dfSummary.varnumbers"), 
          labels.col = st_options("dfSummary.labels.col"), valid.col = st_options("dfSummary.valid.col"), 
          na.col = st_options("dfSummary.na.col"), graph.col = st_options("dfSummary.graph.col"), 
          graph.magnif = st_options("dfSummary.graph.magnif"), style = st_options("dfSummary.style"), 
          plain.ascii = st_options("plain.ascii"), justify = "left", 
          col.widths = NA, headings = st_options("headings"), display.labels = st_options("display.labels"), 
          max.distinct.values = 10, trim.strings = FALSE, max.string.width = 25, 
          split.cells = 40, split.tables = Inf, tmp.img.dir = st_options("tmp.img.dir"), 
          silent = st_options("dfSummary.silent"), ...) {
  errmsg <- character()
  converted_to_df <- FALSE
  if (!is.data.frame(x)) {
    xnames <- substitute(x)
    x <- try(as.data.frame(x))
    if (inherits(x, "try-error")) {
      errmsg %+=% paste(deparse(xnames), " is not coercible to a data frame")
    }
    else {
      converted_to_df <- TRUE
      df_name <- setdiff(all.names(xnames), c("[", "[[", 
                                              ":", "$"))[1]
      if (!isTRUE(silent)) {
        message(deparse(xnames), " was converted to a data frame")
      }
    }
  }
  errmsg <- c(errmsg, summarytools:::check_arguments(match.call(), list(...)))
  if (length(errmsg) > 0) {
    stop(paste(errmsg, collapse = "\n  "))
  }
  if (isTRUE(labels.col) && length(label(x, all = TRUE)) == 
      0) {
    labels.col <- FALSE
  }
  parse_info <- try(parse_args(sys.calls(), sys.frames(), match.call(), 
                               var_name = converted_to_df, var_label = converted_to_df, 
                               caller = "dfSummary"), silent = TRUE)
  if (inherits(parse_info, "try-error")) {
    parse_info <- list()
  }
  if (!("df_name" %in% names(parse_info)) && exists("df_name")) {
    parse_info$df_name <- df_name
  }
  if (isTRUE(converted_to_df) && identical(colnames(x), "x")) {
    if ("var_name" %in% names(parse_info)) {
      colnames(x) <- parse_info$var_name
    }
    else {
      colnames(x) <- parse_info$df_name
    }
  }
  if (!isTRUE(plain.ascii) && style == "grid" && isTRUE(graph.col)) {
    if (is.na(tmp.img.dir)) {
      store_imgs <- FALSE
      if (!isTRUE(silent)) {
        message("text graphs are displayed; set 'tmp.img.dir' parameter to ", 
                "activate png graphs")
      }
    }
    else {
      store_imgs <- TRUE
      dir.create(tmp.img.dir, showWarnings = FALSE)
      if (Sys.info()[["sysname"]] == "Windows" || tmp.img.dir != 
          "/tmp") {
        if (!isTRUE(silent)) {
          message("temporary images written to '", normalizePath(tmp.img.dir), 
                  "'")
        }
      }
    }
  }
  else {
    store_imgs <- FALSE
  }
  output <- data.frame(no = numeric(), variable = character(), 
                       label = character(), stats.values = character(), freqs.pct.valid = character(), 
                       graph = character(), text.graph = character(), valid = character(), 
                       missing = character(), stringsAsFactors = FALSE, check.names = FALSE)
  n_tot <- nrow(x)
  for (i in seq_len(ncol(x))) {
    column_data <- x[[i]]
    output[i, 1] <- i
    output[i, 2] <- paste0(names(x)[i], "\\\n[", paste(class(column_data), 
                                                       collapse = ", "), "]")
    if (is.factor(column_data)) {
      barcode_type <- summarytools:::detect_barcode(levels(column_data))
    }
    else {
      barcode_type <- summarytools:::detect_barcode(column_data)
    }
    if (is.character(barcode_type)) {
      output[i, 2] <- paste(output[i, 2], paste(barcode_type, 
                                                summarytools:::trs("codes")), sep = "\\\n")
      if (is.numeric(column_data)) {
        column_data <- as.character(column_data)
      }
    }
    if (isTRUE(labels.col)) {
      output[i, 3] <- label(x[[i]])
      if (is.na(output[i, 3])) 
        output[i, 3] <- ""
    }
    n_miss <- sum(is.na(column_data))
    n_valid <- n_tot - n_miss
    if (is.factor(column_data)) {
      output[i, 4:7] <- summarytools:::crunch_factor(column_data)
    }
    else if (is.character(column_data)) {
      output[i, 4:7] <- summarytools:::crunch_character(column_data)
    }
    else if (is.numeric(column_data)) {
      output[i, 4:7] <- crunch_numeric_rj(column_data, is.character(barcode_type))
    }
    else if (inherits(column_data, c("Date", "POSIXct"))) {
      output[i, 4:7] <- summarytools:::crunch_time_date(column_data)
    }
    else {
      output[i, 4:7] <- summarytools:::crunch_other(column_data)
    }
    output[i, 8] <- paste0(n_valid, "\\\n(", round(n_valid/n_tot * 
                                                     100, round.digits), "%)")
    output[i, 9] <- paste0(n_miss, "\\\n(", round(n_miss/n_tot * 
                                                    100, round.digits), "%)")
    ##rj
    output[i, 4] <- distributions[i]
  }
  if (!isTRUE(varnumbers)) {
    output$no <- NULL
  }
  if (!isTRUE(labels.col)) {
    output$label <- NULL
  }
  if (!isTRUE(graph.col)) {
    output$graph <- NULL
    output$text.graph <- NULL
  }
  if (!isTRUE(valid.col)) {
    output$valid <- NULL
  }
  if (!isTRUE(na.col)) {
    output$missing <- NULL
  }
  
  print(names(output))
  output$freqs.pct.valid <- NULL
  for (i in seq_along(output)) {
    if (colnames(output)[i] == "text.graph") 
      next
    colnames(output)[i] <- summarytools:::trs(colnames(output)[i])
  }
  class(output) <- c("summarytools", class(output))
  attr(output, "st_type") <- "dfSummary"
  attr(output, "date") <- Sys.Date()
  attr(output, "fn_call") <- match.call()
  data_info <- list(Data.frame = parse_info$df_name, 
                    Dataf.rame.label = ifelse("df_label" %in% names(parse_info), parse_info$df_label, NA), 
                    Dimensions = paste(n_tot, "x", ncol(x)), 
                    Duplicates = n_tot - n_distinct(x), 
                    Group = ifelse("by_group" %in% names(parse_info), parse_info$by_group, NA), 
                    by_first = ifelse("by_group" %in% names(parse_info), parse_info$by_first, NA), 
                    by_last = ifelse("by_group" %in%  names(parse_info), parse_info$by_last, NA))
  
  attr(output, "data_info") <- data_info[!is.na(data_info)]
  format_info <- list(style = style, round.digits = round.digits, 
                      plain.ascii = plain.ascii, justify = justify, headings = headings, 
                      display.labels = display.labels, labels.col = labels.col, 
                      split.cells = split.cells, split.tables = split.tables, 
                      col.widths = col.widths)
  attr(output, "format_info") <- format_info[!is.na(format_info)]
  attr(output, "user_fmt") <- list(... = ...)
  attr(output, "lang") <- st_options("lang")
  return(output)
}

crunch_numeric_rj <- function (column_data, is_barcode){
  outlist <- list()
  outlist[[1]] <- ""
  outlist[[2]] <- ""
  outlist[[3]] <- ""
  outlist[[4]] <- ""
  max.distinct.values <- parent.frame()$max.distinct.values
  graph.magnif <- parent.frame()$graph.magnif
  round.digits <- parent.frame()$round.digits
  if (parent.frame()$n_miss == parent.frame()$n_tot) {
    outlist[[1]] <- paste0(summarytools:::trs("all.nas"), "\n")
  }
  else {
    counts <- table(column_data, useNA = "no")
    if (length(counts) == 1) {
      outlist[[1]] <- paste(1, summarytools:::trs("distinct.value"))
    }
    else {
      if (isTRUE(is_barcode)) {
        maxchars <- max(nchar(c(summarytools:::trs("min"), summarytools:::trs("max"), 
                                summarytools:::trs("mode"))))
        outlist[[1]] <- paste0(summarytools:::trs("min"), 
                               strrep(" ", maxchars - nchar(summarytools:::trs("min"))), 
                               " : ", min(column_data, na.rm = TRUE), "\\\n", 
                               summarytools:::trs("mode"), 
                               strrep(" ", maxchars - nchar(summarytools:::trs("mode"))), " : ", 
                               names(counts)[which.max(counts)][1], 
                               "\\\n", summarytools:::trs("max"), 
                               strrep(" ", maxchars - nchar(summarytools:::trs("max"))), " : ", 
                               max(column_data, na.rm = TRUE))
      }
      else if (length(counts) == 2) {
        maxchars <- max(nchar(c(summarytools:::trs("min"), summarytools:::trs("max"), 
                                summarytools:::trs("mean"))))
        outlist[[1]] <- paste0(summarytools:::trs("min"), strrep(" ", maxchars - nchar(summarytools:::trs("min"))), 
                               " : ", round(min(column_data, na.rm = TRUE), round.digits - 1), "\\\n", 
                               summarytools:::trs("mean"), strrep(" ", maxchars - nchar(summarytools:::trs("mean"))), 
                               " : ", round(mean(column_data, na.rm = TRUE), round.digits - 1), "\\\n", 
                               summarytools:::trs("max"), strrep(" ", maxchars - nchar(summarytools:::trs("max"))), 
                               " : ", round(max(column_data, na.rm = TRUE), round.digits - 1))
      }
      else {
        outlist[[1]] <- paste(summarytools:::trs("mean"), 
                              paste0(" (", summarytools:::trs("sd"), ") : "), 
                              round(mean(column_data, na.rm = TRUE), round.digits - 1), 
                              " (", round(sd(column_data, na.rm = TRUE), round.digits - 1), ")\\\n", 
                              tolower(paste(summarytools:::trs("min"), "<", 
                                            summarytools:::trs("med.short"), 
                                            "<", summarytools:::trs("max"))), ":\\\n", 
                              round(min(column_data, na.rm = TRUE), round.digits - 1), " < ", 
                              round(median(column_data, na.rm = TRUE), round.digits - 1), " < ", 
                              round(max(column_data, na.rm = TRUE), round.digits - 1), "\\\n", 
                              paste0(summarytools:::trs("iqr"), " (", summarytools:::trs("cv"), ") : "), 
                              round(IQR(column_data, na.rm = TRUE), round.digits - 1), 
                              " (", round(sd(column_data, na.rm = TRUE)/mean(column_data, na.rm = TRUE), 
                                          round.digits - 1), ")", 
                              collapse = "", sep = "")
      }
    }
    extra_space <- FALSE
    if (inherits(column_data, "ts")) {
      maxchars <- max(nchar(c(summarytools:::trs("start"), summarytools:::trs("end"))))
      outlist[[2]] <- paste(length(counts), summarytools:::trs("distinct.values"), 
                            paste0("\\\n", summarytools:::trs("start"), 
                                   strrep(" ", maxchars -  nchar(summarytools:::trs("start"))), ":"), 
                            paste(sprintf("%02d", start(column_data)), collapse = "-"), 
                            paste0("\\\n", summarytools:::trs("end"), 
                                   strrep(" ", maxchars - nchar(summarytools:::trs("end"))), ":"), 
                            paste(sprintf("%02d", end(column_data)), collapse = "-"))
    }
    else if (length(counts) <= max.distinct.values && 
             (all(column_data%%1 == 0, na.rm = TRUE) || identical(names(column_data), "0") || 
              all(abs(as.numeric(names(counts[-which(names(counts) == "0")]))) >= 10^-round.digits))) {
      props <- round(prop.table(counts), round.digits + 
                       2)
      counts_props <- align_numbers_dfs(counts, props)
      rounded_names <- trimws(format(round(as.numeric(names(counts)), 
                                           round.digits), nsmall = round.digits * !all(column_data%%1 == 
                                                                                         0, na.rm = TRUE)))
      maxchars <- max(nchar(rounded_names))
      outlist[[2]] <- paste(paste0(rounded_names, 
                                   strrep(" ", maxchars - nchar(rounded_names)), 
                                   ifelse(as.numeric(names(counts)) != as.numeric(rounded_names), "!", " ")), 
                            counts_props, sep = ": ", collapse = "\\\n")
      if (any(as.numeric(names(counts)) != as.numeric(rounded_names))) {
        extra_space <- TRUE
        outlist[[2]] <- paste(outlist[[2]], paste("!", 
                                                  summarytools:::trs("rounded")), sep = "\\\n")
      }
    }
    else {
      outlist[[2]] <- paste(length(counts), summarytools:::trs("distinct.values"))
      if (parent.frame()$n_miss == 0 && (isTRUE(all.equal(column_data, min(column_data):max(column_data))) || 
                                         isTRUE(all.equal(column_data, max(column_data):min(column_data))))) {
        outlist[[2]] <- paste(outlist[[2]], paste0("(", 
                                                   summarytools:::trs("int.sequence"), ")"), sep = "\\\n")
      }
    }
    if (isTRUE(parent.frame()$graph.col)) {
      if (length(counts) <= max.distinct.values) {
        outlist[[3]] <- encode_graph_rj(counts, "barplot", 
                                        graph.magnif)
        if (isTRUE(parent.frame()$store_imgs)) {
          png_loc <- encode_graph_rj(counts, "barplot", 
                                     graph.magnif, TRUE)
          outlist[[4]] <- paste0("![](", png_loc, ")")
        }
        else {
          outlist[[4]] <- txtbarplot(prop.table(counts))
        }
        if (isTRUE(extra_space)) {
          outlist[[3]] <- paste0(outlist[[3]], "\n\n")
          outlist[[4]] <- paste0(outlist[[4]], " \\ \n \\")
        }
      }
      else {
        outlist[[3]] <- encode_graph_rj(column_data, "histogram", 
                                        graph.magnif)
        if (isTRUE(parent.frame()$store_imgs)) {
          png_loc <- encode_graph_rj(column_data, "histogram", 
                                     graph.magnif, TRUE)
          outlist[[4]] <- paste0("![](", png_loc, ")")
        }
        else {
          outlist[[4]] <- summarytools:::txthist(column_data)
        }
      }
    }
  }
  Encoding(outlist[[1]]) <- "UTF-8"
  Encoding(outlist[[2]]) <- "UTF-8"
  Encoding(outlist[[3]]) <- "UTF-8"
  return(outlist)
}

encode_graph_rj <- function (data, graph_type, graph.magnif = 1, pandoc = FALSE){
  devtype <- switch(Sys.info()[["sysname"]], Windows = "windows", 
                    Linux = "Xlib", Darwin = "quartz")
  if (graph_type == "histogram") {
    rc <- try(png(png_loc <- tempfile(fileext = ".png"), 
                  width = 800 * graph.magnif, height = 150 * graph.magnif, 
                  units = "px", bg = "transparent", type = devtype, 
                  antialias = "none"), silent = TRUE)
    if (!is.null(rc)) {
      png(png_loc <- tempfile(fileext = ".png"), width = 800 * 
            graph.magnif, height = 150 * graph.magnif, units = "px", 
          bg = "transparent", antialias = "none")
    }
    mar <- par(mar = c(2, 0.02, 0.02, 8),oma = c(0, 0.0, 0.0, 4))
    on.exit(par(mar), add = TRUE)
    data <- data[!is.na(data)]
    breaks_x <- pretty(range(data), n = min(nclass.Sturges(data), 
                                            250), min.n = 1)
    cl <- try(suppressWarnings(hist(data, freq = FALSE, breaks = breaks_x, 
                                    axes = FALSE, xlab = NULL, ylab = NULL, main = NULL, 
                                    col = "navyblue", border = "navyblue")), silent = TRUE); 
    axis(1)
    abline(v=mean(data),col='darkorange',lwd=2)
    abline(v=mean(data)+sd(data),col='darkorange',lwd=2,lty=2)
    abline(v=mean(data)-sd(data),col='darkorange',lwd=2,lty=2)
    abline(v=median(data),col='turquoise',lwd=2)
    abline(v=quantile(data,0.75),col='turquoise',lwd=2,lty=2)
    abline(v=quantile(data,0.25),col='turquoise',lwd=2,lty=2)
    legend('topright',lwd=2,lty=c(1,1,2,2),col=c('darkorange','turquoise'),legend=c('mean','median','2 sd','50% range'),
           cex=1.2,bty='n',inset=c(-0.2,0),xpd=NA)
    if (inherits(cl, "try-error")) {
      plot.new()
      text("Graph Not Available", x = 0.5, y = 0.5, cex = 1)
    }
    dev.off()
    ii <- magick:::image_read(png_loc)
    #ii <- magick:::image_border(magick:::image_trim(ii), color = "white", geometry = "6x4")
  }
  else if (graph_type == "barplot") {
    rc <- try(png(png_loc <- tempfile(fileext = ".png"), 
                  width = 150 * graph.magnif, height = 26 * length(data) * 
                    graph.magnif, units = "px", bg = "transparent", 
                  type = devtype, antialias = "none"), silent = TRUE)
    if (!is.null(rc)) {
      png(png_loc <- tempfile(fileext = ".png"), width = 150 * 
            graph.magnif, height = 26 * length(data) * graph.magnif, 
          units = "px", bg = "transparent", antialias = "none")
    }
    mar <- par(mar = c(0.02, 0.02, 0.02, 0.02))
    on.exit(par(mar), add = TRUE)
    data <- rev(data)
    barplot(data, names.arg = "", axes = FALSE, space = 0.22, 
            col = "grey97", border = "grey65", horiz = TRUE, 
            xlim = c(0, sum(data)))
    dev.off()
    ii <- magick:::image_read(png_loc)
    ii <- magick:::image_border(magick:::image_trim(ii), color = "white", geometry = "6x4")
  }
  if (isTRUE(pandoc)) {
    png_path <- generate_png_path(parent.frame(2)$tmp.img.dir)
    magick:::image_write(magick:::image_transparent(ii, "white"), path = png_path)
    return(png_path)
  }
  else {
    magick:::image_write(magick:::image_transparent(ii, "white"), png_loc)
    img_txt <- RCurl:::base64Encode(txt = readBin(con = png_loc, 
                                                  what = "raw", n = file.info(png_loc)[["size"]]), 
                                    mode = "character")
    return(paste0("<img style=\"border:none;background-color:transparent;", 
                  "padding:0\" src=\"data:image/png;base64, ", img_txt, 
                  "\">"))
  }
}

