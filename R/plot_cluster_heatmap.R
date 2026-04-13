#' @md
#' @title Plot heat map of scaled variable values per group
#'
#' @description
#' Creates a heat map where each tile shows a scaled summary of a variable for
#' a group. The scaling method is controlled by the `scale_method` parameter.
#' By default (`scale_method = "ecdf"`), each tile shows the percentile of the
#' group's median value compared against the empirical cumulative distribution
#' function (ECDF) of that variable across all observations not belonging to the
#' group. Groups and variables are ordered along the axes via hierarchical
#' clustering.
#'
#' @param .data data.frame. Rows are observations. Must contain a column
#'   identifying group membership and columns for variable values.
#' @param group character. Name of the column in `.data` that identifies
#'   group membership.
#' @param vars character vector or `NULL`. Names of columns in `.data` to
#'   use as variables. If `NULL`, all columns except `group` are used.
#'   Default is `NULL`.
#' @param scale_method character. Method used to scale variable values for
#'   colouring cells. One of `"ecdf"` (default), `"zscore"`, `"raw"`,
#'   `"minmax"`, or `"minmax_var"`.
#'
#'   - `"ecdf"`: Each cell shows the percentile of the cluster's median value
#'     compared to all observations outside the cluster (empirical CDF).
#'     Fill values are in \[0, 1\] and the legend uses percent labels.
#'   - `"zscore"`: Each cell shows the z-score of the cluster's median
#'     relative to all observations of that variable
#'     (`(median - mean) / sd`). Fill values are unbounded.
#'   - `"raw"`: Each cell shows the raw median value. Fill values are
#'     unbounded.
#'   - `"minmax"`: Each cell shows the cluster median scaled to \[0, 1\]
#'     using the global minimum and maximum across all observations of all
#'     variables. Fill values are in \[0, 1\] and the legend uses percent
#'     labels.
#'   - `"minmax_var"`: Each cell shows the cluster median scaled to \[0, 1\]
#'     using the minimum and maximum of all observations within each variable
#'     separately. Fill values are in \[0, 1\] and the legend uses percent
#'     labels.
#' @param palette character or `NULL`. Named colour palette for the continuous
#'   fill scale. When not `NULL`, overrides `col` and `col_positions`. Available
#'   palettes: `"bipolar"` (default, blue-white-red), `"alarm"`
#'   (green-white-red, good-to-bad), `"accessible"` (blue-white-orange,
#'   colour-blind-safe diverging), `"heat"` (light-yellow to dark-red,
#'   sequential), `"sky"` (white to navy, sequential). Set to `NULL` to use
#'   `col` and `col_positions` directly.
#' @param col character vector. Colours used to fill tiles, ordered from low
#'   to high values. Default is `c("#2166AC", "#F7F7F7", "#B2182B")` (blue,
#'   white, red). Any number of colours (>= 2) is accepted. Ignored when
#'   `palette` is not `NULL`.
#' @param col_positions numeric vector or `"auto"`. Positions (in \[0, 1\]) at
#'   which each colour in `col` is placed on the fill scale. Must be the same
#'   length as `col`, sorted in ascending order, with the first value `0` and
#'   the last value `1`. When `"auto"` (default) and `col` has exactly three
#'   colours and `scale_method = "ecdf"`, the middle colour is stretched over
#'   `white_range` (the current default behaviour). In all other `"auto"` cases
#'   the colours are evenly spaced from 0 to 1. Ignored when `palette` is not
#'   `NULL`.
#' @param white_range numeric vector of length 2. The range of positions (on a
#'   0-1 scale) over which the middle colour is stretched. Only used when `col`
#'   has exactly three colours, `scale_method = "ecdf"`, and `col_positions =
#'   "auto"`. Also applied to diverging `palette` presets (those with
#'   `col_positions = "auto"`). Default is `c(0.4, 0.6)`.
#' @param na_rm logical. Whether to remove `NA` values before computing
#'   per-cluster statistics. When `TRUE` (default), `NA` values are removed
#'   and a message is issued showing how many were removed per variable. When
#'   `FALSE`, `NA` values are passed through: tile fill values will be `NA`
#'   (rendered as grey by default) where a variable has no non-missing
#'   observations in a cluster.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
#' @param grid ggplot2 panel grid or `NULL`. Default is
#'   `cowplot::background_grid(major = "xy")`. Set to `NULL` for no grid.
#' @param show_values logical. Whether to overlay the median value for each
#'   cluster-variable combination as a text label on each tile. Default is
#'   `FALSE`.
#' @param values_format function or `NULL`. A function that takes a numeric
#'   vector and returns a character vector of formatted labels. Applied to
#'   the per-cluster median values when `show_values = TRUE`. When `NULL`,
#'   values are formatted to three significant figures without scientific
#'   notation using `trimws(format(x, digits = 3, scientific = FALSE))`.
#'   Default is `NULL`.
#' @param values_col character. Colour for the overlaid text labels.
#'   Default is `"black"`.
#' @param values_size numeric. Font size (in `ggplot2` units) for the
#'   overlaid text labels. Default is `3`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' set.seed(1)
#' .data <- data.frame(
#'   group = rep(paste0("C", 1:3), each = 20),
#'   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#'   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
#' )
#' plot_group_heatmap(.data, group = "group")
#' plot_group_heatmap(.data, group = "group", show_values = TRUE)
#' plot_group_heatmap(.data, group = "group", palette = "alarm")
plot_group_heatmap <- function(.data,
                                 group,
                                 vars = NULL,
                                 scale_method = "ecdf",
                                 palette = "bipolar",
                                 col = c("#2166AC", "#F7F7F7", "#B2182B"),
                                 col_positions = "auto",
                                 white_range = c(0.4, 0.6),
                                 na_rm = TRUE,
                                 font_size = 14,
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
                                 ),
                                 show_values = FALSE,
                                 values_format = NULL,
                                 values_col = "black",
                                 values_size = 3) {
  cluster <- group
  .plot_cluster_validate(.data, cluster, vars)

  if (is.null(vars)) {
    vars <- setdiff(colnames(.data), cluster)
  }

  scale_method <- match.arg(
    scale_method, c("ecdf", "zscore", "raw", "minmax", "minmax_var")
  )

  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("`na_rm` must be TRUE or FALSE.", call. = FALSE)
  }

  # Resolve palette → col / col_positions
  resolved <- .resolve_cluster_palette(palette, col, col_positions)
  col <- resolved$col
  col_positions <- resolved$col_positions

  if (!is.character(col) || length(col) < 2) {
    stop("`col` must be a character vector of length >= 2.", call. = FALSE)
  }
  if (!identical(col_positions, "auto")) {
    if (!is.numeric(col_positions) || length(col_positions) != length(col)) {
      stop(
        "`col_positions` must be \"auto\" or a numeric vector the same length as `col`.",
        call. = FALSE
      )
    }
    if (!isTRUE(all.equal(col_positions[1], 0)) ||
        !isTRUE(all.equal(col_positions[length(col_positions)], 1))) {
      stop(
        "`col_positions` must start at 0 and end at 1.",
        call. = FALSE
      )
    }
    if (is.unsorted(col_positions)) {
      stop("`col_positions` must be sorted in ascending order.", call. = FALSE)
    }
  }
  if (!is.numeric(white_range) || length(white_range) != 2 ||
      any(white_range < 0) || any(white_range > 1) ||
      white_range[1] >= white_range[2]) {
    stop(
      paste0(
        "`white_range` must be a numeric vector of length 2 with values in",
        " [0, 1] and `white_range[1] < white_range[2]`."
      ),
      call. = FALSE
    )
  }
  if (!is.logical(show_values) || length(show_values) != 1L ||
      is.na(show_values)) {
    stop("`show_values` must be TRUE or FALSE.", call. = FALSE)
  }

  cluster_vec <- unique(.data[[cluster]])

  plot_tbl <- .plot_cluster_heatmap_calc(
    .data, cluster, vars, cluster_vec, scale_method, na_rm
  )
  order_list <- .plot_cluster_heatmap_order(plot_tbl)

  .plot_cluster_heatmap_plot(
    plot_tbl = plot_tbl,
    order_list = order_list,
    col = col,
    col_positions = col_positions,
    white_range = white_range,
    thm = thm,
    grid = grid,
    show_values = show_values,
    values_format = values_format,
    values_col = values_col,
    values_size = values_size,
    scale_method = scale_method
  )
}

.plot_cluster_heatmap_calc <- function(.data, cluster, vars, cluster_vec, scale_method, na_rm) {
  # Alias to avoid rlang .data pronoun masking inside purrr closures
  data <- .data

  # Helper: return values for var in cluster cl, filtering NAs if na_rm = TRUE
  # and messaging about any removed.  Returns a named list(in = <values>,
  # out = <values>) where "in" is the in-cluster subset and "out" is the rest.
  .get_vals <- function(var, clust) {
    obs_in  <- data[[cluster]] == clust
    v_in    <- data[[var]][obs_in]
    v_out   <- data[[var]][!obs_in]
    if (na_rm) {
      n_in  <- sum(is.na(v_in))
      n_out <- sum(is.na(v_out))
      if (n_in > 0L || n_out > 0L) {
        msg_parts <- character(0L)
        if (n_in  > 0L) msg_parts <- c(msg_parts,
          paste0(n_in,  " in cluster '",  clust, "'"))
        if (n_out > 0L) msg_parts <- c(msg_parts,
          paste0(n_out, " outside cluster '", clust, "'"))
        message(
          "Removing NA value(s) from variable '", var, "': ",
          paste(msg_parts, collapse = ", "), "."
        )
      }
      v_in  <- v_in[!is.na(v_in)]
      v_out <- v_out[!is.na(v_out)]
    }
    list(in_vals = v_in, out_vals = v_out)
  }

  if (scale_method == "ecdf") {
    return(purrr::map_df(cluster_vec, function(clust) {
      purrr::map_df(vars, function(var) {
        vals    <- .get_vals(var, clust)
        med     <- stats::median(vals$in_vals, na.rm = FALSE)
        ecdf_fn <- stats::ecdf(vals$out_vals)
        tibble::tibble(
          cluster  = clust,
          variable = var,
          perc     = ecdf_fn(med),
          med      = med
        )
      })
    }))
  }

  med_tbl <- purrr::map_df(cluster_vec, function(clust) {
    purrr::map_df(vars, function(var) {
      vals <- .get_vals(var, clust)
      tibble::tibble(
        cluster  = clust,
        variable = var,
        med      = stats::median(vals$in_vals, na.rm = FALSE)
      )
    })
  })

  if (scale_method == "zscore") {
    perc_vec <- numeric(nrow(med_tbl))
    for (var in vars) {
      all_vals <- data[[var]]
      if (na_rm) all_vals <- all_vals[!is.na(all_vals)]
      mu    <- mean(all_vals, na.rm = FALSE)
      sigma <- stats::sd(all_vals, na.rm = FALSE)
      idx   <- med_tbl$variable == var
      perc_vec[idx] <- if (is.na(sigma) || sigma == 0) 0 else
        (med_tbl$med[idx] - mu) / sigma
    }
    med_tbl$perc <- perc_vec
  } else if (scale_method == "raw") {
    med_tbl$perc <- med_tbl$med
  } else if (scale_method == "minmax") {
    all_vals   <- unlist(lapply(vars, function(v) data[[v]]))
    if (na_rm) all_vals <- all_vals[!is.na(all_vals)]
    global_min <- min(all_vals, na.rm = FALSE)
    global_max <- max(all_vals, na.rm = FALSE)
    med_tbl$perc <- if (global_max == global_min) 0.5 else
      (med_tbl$med - global_min) / (global_max - global_min)
  } else if (scale_method == "minmax_var") {
    perc_vec <- numeric(nrow(med_tbl))
    for (var in vars) {
      all_vals <- data[[var]]
      if (na_rm) all_vals <- all_vals[!is.na(all_vals)]
      var_min <- min(all_vals, na.rm = FALSE)
      var_max <- max(all_vals, na.rm = FALSE)
      idx     <- med_tbl$variable == var
      perc_vec[idx] <- if (var_max == var_min) 0.5 else
        (med_tbl$med[idx] - var_min) / (var_max - var_min)
    }
    med_tbl$perc <- perc_vec
  }

  med_tbl
}

.plot_cluster_heatmap_order <- function(plot_tbl) {
  cluster_vec <- unique(plot_tbl$cluster)
  var_vec <- unique(plot_tbl$variable)

  if (length(cluster_vec) <= 1 || length(var_vec) <= 1) {
    return(list(cluster = cluster_vec, variable = var_vec))
  }

  expr_mat <- .plot_cluster_heatmap_order_mat(plot_tbl, cluster_vec, var_vec)

  list(
    cluster = cluster_vec[stats::hclust(stats::dist(expr_mat))$order],
    variable = var_vec[stats::hclust(stats::dist(t(expr_mat)))$order]
  )
}

.plot_cluster_heatmap_order_mat <- function(plot_tbl, cluster_vec, var_vec) {
  expr_mat <- matrix(
    NA_real_,
    nrow = length(cluster_vec),
    ncol = length(var_vec),
    dimnames = list(cluster_vec, var_vec)
  )
  for (clust in cluster_vec) {
    for (var in var_vec) {
      expr_mat[clust, var] <- plot_tbl$perc[
        plot_tbl$cluster == clust & plot_tbl$variable == var
      ]
    }
  }
  expr_mat
}

.plot_cluster_heatmap_plot <- function(plot_tbl,
                                       order_list,
                                       col,
                                       col_positions,
                                       white_range,
                                       thm,
                                       grid,
                                       show_values,
                                       values_format,
                                       values_col,
                                       values_size,
                                       scale_method) {
  plot_tbl <- plot_tbl %>%
    dplyr::mutate(
      cluster = factor(.data$cluster, levels = order_list$cluster),
      variable = factor(.data$variable, levels = order_list$variable)
    )

  # For ecdf, stretch the middle colour over white_range; other methods use
  # the shared gradientn args builder (auto = evenly spaced for 3 colours).
  if (identical(col_positions, "auto") && length(col) == 3L &&
      scale_method == "ecdf") {
    gradientn_args <- list(
      colours = c(col[1], col[2], col[2], col[3]),
      values  = c(0, white_range[1], white_range[2], 1)
    )
  } else {
    gradientn_args <- .build_gradientn_args(col, col_positions, white_range)
  }

  bounded <- scale_method %in% c("ecdf", "minmax", "minmax_var")
  fill_scale <- if (bounded) {
    ggplot2::scale_fill_gradientn(
      colours = gradientn_args$colours,
      values  = gradientn_args$values,
      limits  = c(0, 1),
      name    = "Relative\nvalue",
      labels  = scales::percent
    )
  } else {
    legend_name <- if (scale_method == "zscore") "Z-score" else "Median"
    ggplot2::scale_fill_gradientn(
      colours = gradientn_args$colours,
      values  = gradientn_args$values,
      name    = legend_name
    )
  }

  p <- ggplot2::ggplot(
    plot_tbl,
    ggplot2::aes(x = .data$cluster, y = .data$variable, fill = .data$perc)
  ) +
    ggplot2::geom_raster() +
    fill_scale +
    ggplot2::labs(x = "Group", y = "Variable")

  if (!is.null(thm)) {
    p <- p + thm
  }
  if (!is.null(grid)) {
    p <- p + grid
  }
  if (show_values) {
    if (is.null(values_format)) {
      values_format <- function(x) trimws(format(x, digits = 3, scientific = FALSE))
    }
    plot_tbl <- plot_tbl %>%
      dplyr::mutate(label = values_format(.data$med))
    p <- p + ggplot2::geom_text(
      data = plot_tbl,
      mapping = ggplot2::aes(
        x = .data$cluster,
        y = .data$variable,
        label = .data$label
      ),
      colour = values_col,
      size = values_size,
      inherit.aes = FALSE
    )
  }

  p
}

#' @rdname plot_group_heatmap
#' @param cluster character. Name of the column in `.data` that identifies
#'   group membership. Alias for the `group` parameter.
#' @param ... Additional arguments passed to [plot_group_heatmap()].
#' @export
plot_cluster_heatmap <- function(.data, cluster, ...) {
  plot_group_heatmap(.data, group = cluster, ...)
}
