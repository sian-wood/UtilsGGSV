#' @md
#' @title Biaxial scatter plot with group medians overlaid
#'
#' @description
#' Creates a biaxial scatter plot with observations colored by group assignment.
#' Median group centroids are overlaid as large points and labeled with group names.
#' The plot can use either raw variables (specified by the user) or dimensionality
#' reduction components as axes.
#'
#' ## Colour palette
#'
#' When `point_col` is `NULL`, group colours are assigned automatically
#' based on `palette_group`. The `"auto"` strategy selects a palette by the
#' number of groups:
#'
#' - **1–8 groups**: Okabe-Ito — colorblind-safe 8-colour palette.
#' - **9–12 groups**: ColorBrewer Paired — 12 colours pairing light and dark
#'   versions of 6 hues.
#' - **13–21 groups**: Kelly's palette (optional `Polychrome` package) — 21
#'   colours of maximum perceptual contrast (white excluded). Falls back to
#'   `hue_pal()` with a warning if `Polychrome` is not installed.
#' - **22–31 groups**: Glasbey's palette (optional `Polychrome` package) — 31
#'   algorithmically spaced colours (white excluded). Falls back to `hue_pal()`
#'   with a warning if `Polychrome` is not installed.
#' - **> 31 groups**: `hue_pal()` — evenly spaced hues (a warning is issued).
#'
#' Set `palette_group` explicitly to override the automatic selection (provided
#' the chosen palette supports at least as many colours as there are groups).
#'
#' @param .data data.frame. Rows are observations. Must contain a column identifying group membership and numeric variables.
#' @param group character. Name of the column in `.data` that identifies group membership.
#' @param dim_red character or `NULL`. Dimensionality reduction method: one of `"none"`, `"pca"`, `"tsne"`, `"umap"`. If `NULL`, auto-selects `"none"` when exactly 2 numeric vars are available, otherwise `"pca"`.
#' @param vars character vector or `NULL`. Names of numeric columns in `.data` to use for the plot or reduction. If `NULL`, uses all numeric columns except `group` and `point_col_var`.
#' @param point_col_var character or `NULL`. Column to use for point colour mapping. Default is same as `cluster`.
#' @param point_col named vector or `NULL`. Custom colours for discrete
#'   `point_col_var` (named by level). When `NULL` (default), colours are
#'   chosen automatically by number of groups: Okabe-Ito for up to 8,
#'   ColorBrewer Paired for up to 12, Kelly's palette (requires `Polychrome`)
#'   for up to 21, Glasbey's palette (requires `Polychrome`) for up to 31,
#'   and `hue_pal()` for larger numbers. Ignored for continuous
#'   `point_col_var` (use `col` instead).
#' @param palette_group character. Palette used for automatic colour assignment
#'   when `point_col` is `NULL` for a discrete `point_col_var`. One of
#'   `"auto"` (default), `"okabe_ito"`, `"paired"`, `"kelly"`, `"glasbey"`,
#'   or `"hue_pal"`. See the **Colour palette** section of Details.
#' @param palette character or `NULL`. Named colour palette for the continuous `point_col_var` colour
#'   scale. When not `NULL`, overrides `col` and `col_positions`. Available palettes:
#'   `"bipolar"` (default, blue-white-red), `"alarm"` (green-white-red, good-to-bad),
#'   `"accessible"` (blue-white-orange, colour-blind-safe diverging), `"heat"` (light-yellow to
#'   dark-red, sequential), `"sky"` (white to navy, sequential). Set to `NULL` to use `col` and
#'   `col_positions` directly. Ignored when `point_col_var` is discrete.
#' @param col character vector. Colours used for the continuous `point_col_var` colour scale, ordered
#'   from low to high values. Default is `c("#2166AC", "#F7F7F7", "#B2182B")` (blue, white, red).
#'   Any number of colours (>= 2) is accepted. Ignored when `point_col_var` is discrete or when
#'   `palette` is not `NULL`.
#' @param col_positions numeric vector or `"auto"`. Positions (in \[0, 1\]) at which each colour in
#'   `col` is placed on the colour scale. Must be the same length as `col`, sorted ascending, with
#'   first value `0` and last value `1`. When `"auto"` (default) and `col` has exactly three colours,
#'   the middle colour is stretched over `white_range`. In all other `"auto"` cases the colours are
#'   evenly spaced from 0 to 1. Ignored when `point_col_var` is discrete or when `palette` is not
#'   `NULL`.
#' @param white_range numeric vector of length 2. The range of positions (on a 0-1 scale) over which
#'   the middle colour is stretched. Only used when `col` has exactly three colours and
#'   `col_positions = "auto"`. Also applied to diverging `palette` presets. Default is `c(0.4, 0.6)`.
#'   Ignored when `point_col_var` is discrete.
#' @param na_rm logical. Whether to remove observations with missing values in the variables used
#'   for the plot axes (and dimensionality reduction, when applicable). When `TRUE` (default),
#'   missing observations are removed and a message is issued showing how many. When `FALSE` and
#'   `dim_red` is not `"none"`, an error is raised if any missing values are found (dimensionality
#'   reduction algorithms cannot handle them). When `FALSE` and `dim_red = "none"`, observations
#'   with missing axis values are silently dropped by ggplot2.
#' @param point_size numeric. Size of observation points. Default is `2`.
#' @param point_alpha numeric. Alpha transparency for observation points and legend guide. Default is `0.65`.
#' @param centroid_size numeric. Size of centroid points. Default is `3`.
#' @param label_size numeric. Font size for centroid labels. Default is `4`.
#' @param label_offset numeric. Label repulsion padding for centroid labels in cm. Default is `0.3`.
#' @param ggrepel logical. Use `ggrepel::geom_text_repel` for centroid labels. Default is `TRUE`.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`. Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is `cowplot::theme_cowplot(font_size = font_size)` with white background.
#' @param x_lab character or `NULL`. Label for x axis; default uses reduction variable names.
#' @param y_lab character or `NULL`. Label for y axis; default uses reduction variable names.
#' @param show_legend logical. Whether to show the legend. Default is `TRUE`.
#'   Set to `FALSE` to hide the legend, as centroid labels may suffice.
#' @param dim_red_args named list. Additional arguments passed to the
#'   dimensionality reduction function, overriding any defaults set by
#'   `plot_cluster_scatter`. For `dim_red = "pca"` these are passed to
#'   [stats::prcomp()] (default: `scale. = TRUE`); for `dim_red = "tsne"` to
#'   [Rtsne::Rtsne()] (defaults: `dims = 2`, `perplexity` auto-computed,
#'   `check_duplicates = FALSE`, `pca = FALSE`); for `dim_red = "umap"` to
#'   [umap::umap()]. The data argument is always set internally and cannot be
#'   overridden. Ignored when `dim_red = "none"`. Default is `list()`.
#' @param grid ggplot2 layer or `NULL`. Background grid added to the plot.
#'   Default is `cowplot::background_grid(major = "xy")`. Set to `NULL` to
#'   suppress the grid.
#' @importFrom stats aggregate setNames
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' set.seed(1)
#' .data <- data.frame(
#'   group = rep(paste0("C", 1:3), each = 20),
#'   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#'   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0)),
#'   var3 = c(rnorm(20, 1), rnorm(20, -1), rnorm(20, 0))
#' )
#' plot_group_scatter(.data, group = "group")
#' plot_group_scatter(.data, group = "group", dim_red = "none", vars = c("var1", "var2"))
#' plot_group_scatter(.data, group = "group", show_legend = FALSE)
#' # Pass extra arguments to the dim-red function, e.g. disable scaling in PCA:
#' plot_group_scatter(.data, group = "group", dim_red = "pca",
#'                      dim_red_args = list(scale. = FALSE))
plot_group_scatter <- function(.data,
                                 group,
                                 dim_red = NULL,
                                 vars = NULL,
                                 dim_red_args = list(),
                                 point_col_var = NULL,
                                 point_col = NULL,
                                 palette_group = "auto",
                                 palette = "bipolar",
                                 col = c("#2166AC", "#F7F7F7", "#B2182B"),
                                 col_positions = "auto",
                                 white_range = c(0.4, 0.6),
                                 na_rm = TRUE,
                                 point_size = 2,
                                 point_alpha = 0.65,
                                 centroid_size = 3,
                                 label_size = 4,
                                 label_offset = 0.3,
                                 ggrepel = TRUE,
                                 font_size = 14,
                                 x_lab = NULL,
                                 y_lab = NULL,
                                 show_legend = TRUE,
                                 thm = cowplot::theme_cowplot(
                                   font_size = font_size
                                 ) +
                                   ggplot2::theme(
                                     plot.background = ggplot2::element_rect(
                                       fill = "white", colour = NA
                                     ),
                                     panel.background = ggplot2::element_rect(
                                       fill = "white", colour = NA
                                     )
                                   ),
                                 grid = cowplot::background_grid(
                                   major = "xy"
                                 )) {
  cluster <- group
  .plot_cluster_validate(.data, cluster, vars)

  if (is.null(point_col_var)) {
    point_col_var <- cluster
  }

  if (!is.character(point_col_var) || length(point_col_var) != 1L ||
      is.na(point_col_var)) {
    stop("`point_col_var` must be a single non-NA character string.", call. = FALSE)
  }
  if (!(point_col_var %in% colnames(.data))) {
    stop(
      paste0("`point_col_var` column \"", point_col_var, "\" not found in `.data`."),
      call. = FALSE
    )
  }

  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("`na_rm` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(ggrepel) || length(ggrepel) != 1L || is.na(ggrepel)) {
    stop("`ggrepel` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_legend) || length(show_legend) != 1L || is.na(show_legend)) {
    stop("`show_legend` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(point_size) || length(point_size) != 1L || point_size <= 0) {
    stop("`point_size` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(point_alpha) || length(point_alpha) != 1L ||
      point_alpha < 0 || point_alpha > 1) {
    stop("`point_alpha` must be a single number in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(centroid_size) || length(centroid_size) != 1L ||
      centroid_size <= 0) {
    stop("`centroid_size` must be a single positive number.", call. = FALSE)
  }
  if (!is.list(dim_red_args)) {
    stop("`dim_red_args` must be a list.", call. = FALSE)
  }

  input_vars <- if (is.null(vars)) {
    candidates <- setdiff(colnames(.data), unique(c(cluster, point_col_var)))
    candidates[sapply(.data[, candidates, drop = FALSE], is.numeric)]
  } else {
    vars
  }

  if (length(input_vars) == 0) {
    stop("No numeric variables found for dimensionality reduction.")
  }

  if (is.null(dim_red)) {
    if (length(input_vars) == 2) {
      dim_red <- "none"
      message("dim_red automatically set to 'none' because exactly two numeric variables are available.")
    } else {
      dim_red <- "pca"
      message("dim_red automatically set to 'pca' because more than two numeric variables are available.")
    }
  }

  dim_red <- match.arg(dim_red, c("none", "pca", "tsne", "umap"))

  if (dim_red != "none" && length(input_vars) < 2) {
    stop("dim_red=\"", dim_red, "\" requires at least two variables in vars or numeric variables in `.data`.")
  }
  if (dim_red == "none" && length(input_vars) < 2) {
    stop("dim_red=\"none\" requires at least two variables in vars or numeric variables in `.data`.")
  }

  # Drop as little as possible: for dim_red="none" only check the two axis
  # variables; for reduction methods all input_vars must be complete.
  vars_for_na_check <- if (dim_red == "none") {
    input_vars[1:2]
  } else {
    input_vars
  }
  complete_idx <- stats::complete.cases(.data[, vars_for_na_check, drop = FALSE])
  n_dropped <- sum(!complete_idx)
  if (n_dropped > 0L) {
    if (!na_rm) {
      if (dim_red != "none") {
        stop(
          n_dropped, " observation(s) have missing values in the selected ",
          "variables. Set na_rm = TRUE to remove them, or use dim_red = ",
          "\"none\" with na_rm = FALSE to let ggplot2 handle missing ",
          "coordinates.",
          call. = FALSE
        )
      }
      # dim_red == "none" + na_rm == FALSE: let ggplot2 handle
    } else {
      message(
        "Removing ", n_dropped, " observation(s) with missing values in ",
        if (dim_red == "none") "the axis variables." else "the selected variables."
      )
    }
  }

  if (na_rm && n_dropped > 0L) {
    if (sum(complete_idx) == 0L) {
      stop("No complete cases remain after removing missing values.")
    }
    data_clean <- .data[complete_idx, , drop = FALSE]
  } else {
    data_clean <- .data
  }

  if (dim_red == "none") {
    x_var <- input_vars[1]
    y_var <- input_vars[2]
    coords <- data_clean[, c(x_var, y_var), drop = FALSE]
    x_values <- coords[[1]]
    y_values <- coords[[2]]
  } else if (dim_red == "pca") {
    pca_call_args <- modifyList(list(scale. = TRUE), dim_red_args)
    pca_call_args[["x"]] <- data_clean[, input_vars, drop = FALSE]
    pca_result <- do.call(stats::prcomp, pca_call_args)
    if (ncol(pca_result$x) < 2) {
      stop("PCA did not return at least two components.")
    }
    x_values <- pca_result$x[, 1]
    y_values <- pca_result$x[, 2]
    x_var <- "PC1"
    y_var <- "PC2"
  } else if (dim_red == "tsne") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      if (interactive()) {
        stop("Package 'Rtsne' is required for dim_red = 'tsne'. Please install it with install.packages('Rtsne').")
      }
      stop("Package 'Rtsne' is required for dim_red = 'tsne'.")
    }
    n_samples <- nrow(data_clean)
    perplexity <- max(1, min(30, floor((n_samples - 1)/3)))
    if (perplexity < 1) {
      stop("Not enough samples for t-SNE. Need at least 3 observations.")
    }
    tsne_call_args <- modifyList(
      list(dims = 2, perplexity = perplexity, check_duplicates = FALSE, pca = FALSE),
      dim_red_args
    )
    tsne_call_args[["X"]] <- data_clean[, input_vars, drop = FALSE]
    tsne_res <- do.call(Rtsne::Rtsne, tsne_call_args)
    x_values <- tsne_res$Y[, 1]
    y_values <- tsne_res$Y[, 2]
    x_var <- "tSNE1"
    y_var <- "tSNE2"
  } else {
    if (!requireNamespace("umap", quietly = TRUE)) {
      if (interactive()) {
        stop("Package 'umap' is required for dim_red = 'umap'. Please install it with install.packages('umap').")
      }
      stop("Package 'umap' is required for dim_red = 'umap'.")
    }
    umap_call_args <- modifyList(list(), dim_red_args)
    umap_call_args[["d"]] <- data_clean[, input_vars, drop = FALSE]
    umap_res <- do.call(umap::umap, umap_call_args)
    x_values <- umap_res$layout[, 1]
    y_values <- umap_res$layout[, 2]
    x_var <- "UMAP1"
    y_var <- "UMAP2"
  }

  plot_data <- tibble::tibble(
    x = x_values,
    y = y_values,
    cluster = data_clean[[cluster]],
    point_col = data_clean[[point_col_var]]
  )

  plot_data$cluster <- as.factor(plot_data$cluster)

  if (point_col_var == cluster) {
    plot_data$point_col <- as.factor(plot_data$point_col)
    point_col_discrete <- TRUE
  } else {
    point_col_discrete <- !is.numeric(plot_data$point_col)
    if (point_col_discrete) {
      plot_data$point_col <- as.factor(plot_data$point_col)
    }
  }

  # Resolve palette → col / col_positions for continuous colour
  if (!point_col_discrete) {
    resolved <- .resolve_cluster_palette(palette, col, col_positions)
    col <- resolved$col
    col_positions <- resolved$col_positions
    if (!is.character(col) || length(col) < 2) {
      stop("`col` must be a character vector of length >= 2.", call. = FALSE)
    }
    if (!is.numeric(white_range) || length(white_range) != 2 ||
        any(white_range < 0) || any(white_range > 1) ||
        white_range[1] >= white_range[2]) {
      stop(
        "`white_range` must be a numeric vector of length 2 in [0, 1] with ",
        "`white_range[1] < white_range[2]`.",
        call. = FALSE
      )
    }
  }

  centroid_tbl <- aggregate(cbind(x = x, y = y) ~ cluster, data = plot_data, FUN = stats::median)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x, y = y, colour = point_col)
  ) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha)

  if (point_col_discrete) {
    if (!is.null(point_col) && is.null(names(point_col))) {
      n_unique <- length(unique(plot_data$point_col))
      if (length(point_col) < n_unique) {
        stop("point_col must have length at least the number of unique values in point_col_var.")
      }
      point_col <- setNames(point_col[seq_len(n_unique)], levels(plot_data$point_col))
    }
    p <- p + ggplot2::scale_colour_manual(
      values = .discrete_cluster_colours(levels(plot_data$point_col), point_col, palette_group)
    )
    if (point_col_var == cluster) {
      p <- p + ggplot2::scale_fill_manual(
        values = .discrete_cluster_colours(levels(plot_data$cluster), point_col, palette_group)
      )
    } else {
      p <- p + ggplot2::scale_fill_manual(
        values = .discrete_cluster_colours(levels(plot_data$cluster), NULL, palette_group)
      )
    }
  } else {
    gradientn_args <- .build_gradientn_args(col, col_positions, white_range)
    p <- p + ggplot2::scale_colour_gradientn(
      colours = gradientn_args$colours,
      values  = gradientn_args$values,
      name    = point_col_var
    )
    p <- p + ggplot2::scale_fill_manual(
      values = .discrete_cluster_colours(levels(plot_data$cluster), NULL, palette_group)
    )
  }

  if (!is.null(label_offset)) {
    if (ggrepel) {
      p <- p + ggrepel::geom_text_repel(
        data = centroid_tbl,
        ggplot2::aes(x = x, y = y, label = cluster),
        inherit.aes = FALSE,
        size = label_size,
        fontface = "bold",
        min.segment.length = 0,
        segment.colour = "black",
        point.padding = ggplot2::unit(label_offset, "cm")
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = centroid_tbl,
        ggplot2::aes(x = x, y = y, label = cluster),
        inherit.aes = FALSE,
        colour = "black",
        size = label_size,
        fontface = "bold",
        vjust = -0.5,
        hjust = 0
      )
    }
  }

  p <- p +
    ggplot2::geom_point(
      data = centroid_tbl,
      ggplot2::aes(x = x, y = y, fill = cluster),
      inherit.aes = FALSE,
      size = centroid_size,
      alpha = 0.9,
      colour = "black",
      shape = 21
    ) +
    ggplot2::labs(
      x = if (is.null(x_lab)) x_var else x_lab,
      y = if (is.null(y_lab)) y_var else y_lab,
      colour = if (point_col_var == cluster) "Group" else point_col_var,
      fill = "Group"
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = point_alpha)))

  if (!is.null(thm)) p <- p + thm
  if (!is.null(grid)) p <- p + grid

  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}

#' @rdname plot_group_scatter
#' @param cluster character. Name of the column in `.data` that identifies
#'   group membership. Alias for the `group` parameter.
#' @param col_clusters named character vector or `NULL`. Per-group colours.
#'   Names should match group labels. When `NULL` (default), colours are
#'   chosen automatically by `palette_cluster`. Forwarded to the `point_col`
#'   argument of [plot_group_scatter()].
#' @param palette_cluster character. Alias for `palette_group` in
#'   [plot_group_scatter()]. See the **Colour palette** section of Details.
#' @param palette character or `NULL`. Named colour palette for the continuous
#'   point colour scale. Forwarded to [plot_group_scatter()]. Default is
#'   `"bipolar"`.
#' @param ... Additional arguments passed to [plot_group_scatter()].
#' @export
plot_cluster_scatter <- function(.data, cluster, col_clusters = NULL,
                                  palette_cluster = "auto",
                                  palette = "bipolar", ...) {
  extra <- list(...)
  if (!is.null(col_clusters)) extra[["point_col"]] <- col_clusters
  do.call(
    plot_group_scatter,
    c(list(.data, group = cluster, palette_group = palette_cluster,
           palette = palette), extra)
  )
}