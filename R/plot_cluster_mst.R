#' @md
#' @title Plot minimum-spanning tree of groups with per-variable node colouring
#'
#' @description
#' Computes the minimum-spanning tree (MST) over groups, where the distance
#' between two groups is the Euclidean distance between their median variable
#' profiles. The MST is built from the full pairwise Euclidean distance matrix
#' (i.e. a fully connected undirected weighted graph), matching the approach
#' used by FlowSOM (`BuildMST`). The node layout is determined once and shared
#' across all per-variable plots.
#'
#' Two layout algorithms are supported via `layout_algorithm`:
#'
#' - `"kamada-kawai"` (default): uses the Kamada-Kawai force-directed algorithm
#'   (`igraph::layout_with_kk`) on the MST graph, matching the FlowSOM
#'   visualisation style.
#' - `"mds"`: uses classical multidimensional scaling (`stats::cmdscale`) of
#'   the full Euclidean distance matrix.
#'
#' For each variable, a separate plot is produced in which each group node
#' is **filled** according to the ECDF-standardised percentile of that
#' group's median value for the variable — the same scaling used by
#' [plot_group_heatmap()]. The node border and label colour encode group
#' identity and can be overridden via `col_clusters`.
#'
#' By default the function returns a **named list of ggplot2 objects**, one
#' per variable. If `n_col` or `n_row` is supplied the plots are combined into
#' a single figure using `cowplot::plot_grid`, with variable names as labels.
#'
#' ## Colour palette
#'
#' When `col_clusters` is `NULL`, group colours are assigned automatically
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
#' ## Node fill
#'
#' By default (`node_fill_by = "variable"`) each variable produces a separate
#' plot in which node fill encodes the ECDF-standardised percentile of that
#' cluster's median — a continuous gradient from low (blue) to high (red) using
#' the palette controlled by `palette` / `col` / `col_positions`.
#'
#' Set `node_fill_by = "cluster"` to instead fill nodes by cluster identity
#' using the same discrete palette chosen by `palette_group` / `col_clusters`.
#' In this mode the function returns a **single ggplot2 object** (not a list)
#' because the fill is the same regardless of variable.
#'
#' @param .data data.frame. Rows are observations. Must contain a column
#'   identifying group membership and columns for variable values.
#' @param group character. Name of the column in `.data` that identifies
#'   group membership.
#' @param vars character vector or `NULL`. Names of columns in `.data` to
#'   use as variables. If `NULL`, all columns except `group` are used.
#'   Default is `NULL`.
#' @param layout_algorithm character. Layout algorithm for positioning nodes.
#'   One of `"kamada-kawai"` (default) or `"mds"`. `"kamada-kawai"` uses the
#'   Kamada-Kawai force-directed algorithm on the MST graph via
#'   `igraph::layout_with_kk`, matching the FlowSOM visualisation style.
#'   `"mds"` uses classical multidimensional scaling of the full distance
#'   matrix via `stats::cmdscale`.
#' @param coord_equal logical. Whether to enforce equal visual scaling on both
#'   axes (one unit on the x-axis equals one unit on the y-axis) via
#'   `ggplot2::coord_equal()`. Default is `TRUE`.
#' @param suppress_axes logical or `NULL`. Whether to suppress axis text,
#'   ticks, lines, and titles. When `NULL` (default), the value is inherited
#'   from `coord_equal` — axes are suppressed when equal scaling is active.
#' @param col_clusters named character vector or `NULL`. Per-cluster colours
#'   applied to node borders and text labels. Names should match cluster
#'   labels. When `NULL` (default), colours are chosen automatically by number
#'   of groups: Okabe-Ito for up to 8, ColorBrewer Paired for up to 12,
#'   Kelly's palette (requires `Polychrome`) for up to 21, Glasbey's palette
#'   (requires `Polychrome`) for up to 31, and `hue_pal()` for larger
#'   numbers.
#' @param palette_group character. Palette used for automatic colour assignment
#'   when `col_clusters` is `NULL`. One of `"auto"` (default), `"okabe_ito"`,
#'   `"paired"`, `"kelly"`, `"glasbey"`, or `"hue_pal"`. See the **Colour
#'   palette** section of Details.
#' @param node_fill_by character. Controls what the node fill encodes. One of
#'   `"variable"` (default) or `"cluster"`. See the **Node fill** section of
#'   Details.
#' @param palette character or `NULL`. Named colour palette for the continuous
#'   node fill scale. When not `NULL`, overrides `col` and `col_positions`.
#'   Available palettes: `"bipolar"` (default, blue-white-red), `"alarm"`
#'   (green-white-red, good-to-bad), `"accessible"` (blue-white-orange,
#'   colour-blind-safe diverging), `"heat"` (light-yellow to dark-red,
#'   sequential), `"sky"` (white to navy, sequential). Set to `NULL` to use
#'   `col` and `col_positions` directly.
#' @param col character vector. Colours used to fill nodes, ordered from low
#'   to high values. Default is `c("#2166AC", "#F7F7F7", "#B2182B")` (blue,
#'   white, red). Any number of colours (>= 2) is accepted. Ignored when
#'   `palette` is not `NULL`.
#' @param col_positions numeric vector or `"auto"`. Positions (in \[0, 1\]) at
#'   which each colour in `col` is placed on the fill scale. Must be the same
#'   length as `col`, sorted in ascending order, with the first value `0` and
#'   the last value `1`. When `"auto"` (default) and `col` has exactly three
#'   colours, the middle colour is stretched over `white_range`. In all other
#'   `"auto"` cases the colours are evenly spaced from 0 to 1. Ignored when
#'   `palette` is not `NULL`.
#' @param white_range numeric vector of length 2. The range of positions (on a
#'   0-1 scale) over which the middle colour is stretched. Only used when `col`
#'   has exactly three colours and `col_positions = "auto"`. Also applied to
#'   diverging `palette` presets. Default is `c(0.4, 0.6)`.
#' @param na_rm logical. Whether to remove `NA` values before computing
#'   per-cluster medians and ECDF percentiles. When `TRUE` (default), `NA`
#'   values are removed and a message is issued showing how many were removed
#'   per variable. When `FALSE`, `NA` values are passed through: node fill
#'   values will be `NA` (rendered as grey by default) where a variable has no
#'   non-missing observations in a cluster.
#' @param n_col integer or `NULL`. Number of columns passed to
#'   `cowplot::plot_grid`. If supplied (or if `n_row` is supplied) a single
#'   combined figure is returned instead of a list. Default is `NULL`.
#' @param n_row integer or `NULL`. Number of rows passed to
#'   `cowplot::plot_grid`. If supplied (or if `n_col` is supplied) a single
#'   combined figure is returned instead of a list. Default is `NULL`.
#' @param label_x numeric. x position of the plot labels within each panel
#'   in grid mode. Passed to `cowplot::plot_grid`. Default is `0`.
#' @param label_y numeric. y position of the plot labels within each panel
#'   in grid mode. Passed to `cowplot::plot_grid`. Default is `1`.
#' @param hjust numeric. Horizontal justification of the plot labels in grid
#'   mode. Passed to `cowplot::plot_grid`. Default is `-0.5`.
#' @param vjust numeric. Vertical justification of the plot labels in grid
#'   mode. Passed to `cowplot::plot_grid`. Default is `1.5`.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
#' @param grid ggplot2 panel grid or `NULL`. Default is
#'   `cowplot::background_grid(major = "xy")`. Set to `NULL` for no grid.
#'
#' @return A named list of ggplot2 objects (one per variable) when neither
#'   `n_col` nor `n_row` is specified. A `cowplot::plot_grid` figure when
#'   `n_col` or `n_row` is specified.
#' @export
#'
#' @examples
#' set.seed(1)
#' .data <- data.frame(
#'   group = rep(paste0("C", 1:3), each = 20),
#'   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#'   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
#' )
#' # Default: Kamada-Kawai layout, returns a named list of plots
#' plot_list <- plot_group_mst(.data, group = "group")
#'
#' # MDS layout
#' plot_group_mst(.data, group = "group", layout_algorithm = "mds")
#'
#' # Combined grid with 2 columns
#' plot_group_mst(.data, group = "group", n_col = 2)
plot_group_mst <- function(.data,
                              group,
                              vars = NULL,
                              layout_algorithm = c("kamada-kawai", "mds"),
                              coord_equal = TRUE,
                              suppress_axes = NULL,
                              col_clusters = NULL,
                              node_fill_by = "variable",
                              palette_group = "auto",
                              palette = "bipolar",
                              col = c("#2166AC", "#F7F7F7", "#B2182B"),
                              col_positions = "auto",
                              white_range = c(0.4, 0.6),
                              na_rm = TRUE,
                              n_col = NULL,
                              n_row = NULL,
                              label_x = 0,
                              label_y = 1,
                              hjust = -0.5,
                              vjust = 1.5,
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
                              )) {
  cluster <- group
  layout_algorithm <- match.arg(layout_algorithm)

  .plot_cluster_validate(.data, cluster, vars)

  if (!is.logical(coord_equal) || length(coord_equal) != 1L || is.na(coord_equal)) {
    stop("`coord_equal` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(suppress_axes) &&
      (!is.logical(suppress_axes) || length(suppress_axes) != 1L ||
       is.na(suppress_axes))) {
    stop("`suppress_axes` must be TRUE, FALSE, or NULL.", call. = FALSE)
  }
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

  if (is.null(suppress_axes)) suppress_axes <- coord_equal

  if (is.null(vars)) {
    vars <- setdiff(colnames(.data), cluster)
  }

  cluster_vec <- unique(.data[[cluster]])
  node_fill_by <- match.arg(node_fill_by, c("variable", "cluster"))

  med_mat  <- .plot_cluster_mst_medians(.data, cluster, vars, cluster_vec, na_rm)
  dist_mat <- as.matrix(stats::dist(med_mat))
  node_tbl_base <- .plot_cluster_mst_layout(
    dist_mat, cluster_vec, layout_algorithm
  )
  edge_tbl <- .plot_cluster_mst_edges(dist_mat, node_tbl_base)
  if (node_fill_by == "cluster") {
    node_tbl_c <- node_tbl_base
    node_tbl_c$fill <- node_tbl_c$cluster
    return(.plot_cluster_mst_plot_one(
      node_tbl      = node_tbl_c,
      edge_tbl      = edge_tbl,
      col_clusters  = col_clusters,
      palette_group = palette_group,
      col           = col,
      col_positions = col_positions,
      white_range   = white_range,
      layout_algorithm = layout_algorithm,
      coord_equal   = coord_equal,
      suppress_axes = suppress_axes,
      thm           = thm,
      grid          = grid,
      node_fill_by  = node_fill_by
    ))
  }

  perc_tbl <- .plot_cluster_mst_percentiles(.data, cluster, vars, cluster_vec, na_rm)
  plot_list <- stats::setNames(
    lapply(vars, function(v) {
      perc_v   <- perc_tbl[perc_tbl$variable == v, ]
      node_tbl <- node_tbl_base
      node_tbl$fill <- perc_v$perc[match(node_tbl$cluster, perc_v$cluster)]
      .plot_cluster_mst_plot_one(
        node_tbl      = node_tbl,
        edge_tbl      = edge_tbl,
        col_clusters  = col_clusters,
        palette_group = palette_group,
        col           = col,
        col_positions = col_positions,
        white_range   = white_range,
        layout_algorithm = layout_algorithm,
        coord_equal   = coord_equal,
        suppress_axes = suppress_axes,
        thm           = thm,
        grid          = grid,
        node_fill_by  = node_fill_by
      )
    }),
    vars
  )

  if (is.null(n_col) && is.null(n_row)) return(plot_list)

  cowplot::plot_grid(
    plotlist = plot_list,
    ncol = n_col,
    nrow = n_row,
    labels = vars,
    label_x = label_x,
    label_y = label_y,
    hjust = hjust,
    vjust = vjust
  )
}

.plot_cluster_mst_medians <- function(.data, cluster, vars, cluster_vec, na_rm) {
  data <- .data  # alias to avoid rlang .data pronoun masking in lapply
  med_mat <- do.call(rbind, lapply(cluster_vec, function(cl) {
    cl_mask <- data[[cluster]] == cl
    vapply(vars, function(v) {
      vals <- data[[v]][cl_mask]
      n_na <- sum(is.na(vals))
      if (n_na > 0L && na_rm) {
        message("Removing ", n_na, " NA value(s) from variable '", v,
                "' in cluster '", cl, "'.")
        vals <- vals[!is.na(vals)]
      }
      stats::median(vals, na.rm = FALSE)
    }, numeric(1L))
  }))
  rownames(med_mat) <- cluster_vec
  colnames(med_mat) <- vars
  med_mat
}

.plot_cluster_mst_layout <- function(dist_mat, cluster_vec, layout_algorithm) {
  if (layout_algorithm == "kamada-kawai") {
    mst_tbl <- .plot_cluster_mst_kruskal(dist_mat)
    g <- igraph::graph_from_data_frame(
      mst_tbl,
      directed = FALSE,
      vertices = data.frame(name = cluster_vec)
    )
    coords <- igraph::layout_with_kk(g)
  } else {
    k <- min(2L, length(cluster_vec) - 1L)
    coords <- stats::cmdscale(stats::as.dist(dist_mat), k = k)
    if (!is.matrix(coords)) {
      coords <- matrix(coords, ncol = 1L)
    }
  }
  tibble::tibble(
    cluster = cluster_vec,
    x = coords[, 1L],
    y = if (ncol(coords) >= 2L) coords[, 2L] else rep(0, length(cluster_vec))
  )
}

.plot_cluster_mst_kruskal <- function(dist_mat) {
  cluster_vec <- rownames(dist_mat)
  n <- length(cluster_vec)

  from_vec <- character(0L)
  to_vec <- character(0L)
  weight_vec <- numeric(0L)
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      from_vec <- c(from_vec, cluster_vec[i])
      to_vec <- c(to_vec, cluster_vec[j])
      weight_vec <- c(weight_vec, dist_mat[i, j])
    }
  }
  ord <- order(weight_vec)
  from_sorted <- from_vec[ord]
  to_sorted <- to_vec[ord]

  parent <- seq_len(n)
  names(parent) <- cluster_vec

  find_root <- function(i) {
    while (parent[i] != i) {
      parent[i] <<- parent[parent[i]]
      i <- parent[i]
    }
    i
  }

  mst_from <- character(0L)
  mst_to <- character(0L)

  for (k in seq_along(from_sorted)) {
    ri <- find_root(match(from_sorted[k], cluster_vec))
    rj <- find_root(match(to_sorted[k], cluster_vec))
    if (ri != rj) {
      parent[ri] <- rj
      mst_from <- c(mst_from, from_sorted[k])
      mst_to <- c(mst_to, to_sorted[k])
    }
    if (length(mst_from) == n - 1L) break
  }

  tibble::tibble(from = mst_from, to = mst_to)
}

.plot_cluster_mst_edges <- function(dist_mat, node_tbl) {
  mst_tbl <- .plot_cluster_mst_kruskal(dist_mat)
  tibble::tibble(
    x    = node_tbl$x[match(mst_tbl$from, node_tbl$cluster)],
    xend = node_tbl$x[match(mst_tbl$to,   node_tbl$cluster)],
    y    = node_tbl$y[match(mst_tbl$from, node_tbl$cluster)],
    yend = node_tbl$y[match(mst_tbl$to,   node_tbl$cluster)]
  )
}

.plot_cluster_mst_percentiles <- function(.data, cluster, vars, cluster_vec, na_rm) {
  data <- .data  # alias to avoid rlang .data pronoun masking in purrr
  purrr::map_df(cluster_vec, function(clust) {
    obs_in   <- data[[cluster]] == clust
    v_in_all  <- data[obs_in, , drop = FALSE]
    v_out_all <- data[!obs_in, , drop = FALSE]
    purrr::map_df(vars, function(var) {
      v_in  <- v_in_all[[var]]
      v_out <- v_out_all[[var]]
      if (na_rm) {
        n_in  <- sum(is.na(v_in))
        n_out <- sum(is.na(v_out))
        if (n_in > 0L) {
          message("Removing ", n_in, " NA value(s) from variable '", var,
                  "' in cluster '", clust, "'.")
          v_in <- v_in[!is.na(v_in)]
        }
        if (n_out > 0L) {
          message("Removing ", n_out, " NA value(s) from variable '", var,
                  "' outside cluster '", clust, "'.")
          v_out <- v_out[!is.na(v_out)]
        }
      }
      med     <- stats::median(v_in,  na.rm = FALSE)
      ecdf_fn <- stats::ecdf(v_out)
      tibble::tibble(
        cluster  = clust,
        variable = var,
        perc     = ecdf_fn(med)
      )
    })
  })
}

.plot_cluster_mst_plot_one <- function(node_tbl, edge_tbl, col_clusters,
                                        palette_group,
                                        col, col_positions, white_range,
                                        layout_algorithm,
                                        coord_equal, suppress_axes,
                                        thm, grid,
                                        node_fill_by = "variable") {
  gradientn_args <- if (node_fill_by == "variable") {
    .build_gradientn_args(col, col_positions, white_range)
  } else {
    NULL
  }
  fill_scale <- if (node_fill_by == "variable") {
    ggplot2::scale_fill_gradientn(
      colours = gradientn_args$colours,
      values  = gradientn_args$values,
      limits  = c(0, 1),
      name    = "Relative\nvalue",
      labels  = scales::percent
    )
  } else {
    ggplot2::scale_fill_manual(
      values = .discrete_cluster_colours(
        unique(node_tbl$cluster), col_clusters, palette_group
      ),
      name = "Group"
    )
  }

  axis_labels <- if (layout_algorithm == "kamada-kawai") {
    list(x = "KK 1", y = "KK 2")
  } else {
    list(x = "MDS 1", y = "MDS 2")
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_tbl,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend
      )
    ) +
    ggplot2::geom_point(
      data = node_tbl,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        fill = .data$fill, colour = .data$cluster
      ),
      shape = 21L,
      size = 4
    ) +
    ggplot2::geom_text(
      data = node_tbl,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        label = .data$cluster, colour = .data$cluster
      ),
      vjust = -1
    ) +
    fill_scale +
    ggplot2::labs(x = axis_labels$x, y = axis_labels$y, colour = "Group")

  if (coord_equal) p <- p + ggplot2::coord_equal()

  p <- p + ggplot2::scale_colour_manual(
    values = .discrete_cluster_colours(unique(node_tbl$cluster), col_clusters, palette_group)
  )

  if (!is.null(thm)) p <- p + thm
  if (!is.null(grid)) p <- p + grid

  if (suppress_axes) {
    p <- p + ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
  }

  p
}

#' @rdname plot_group_mst
#' @param cluster character. Name of the column in `.data` that identifies
#'   group membership. Alias for the `group` parameter.
#' @param palette_cluster character. Alias for `palette_group` in
#'   [plot_group_mst()]. See the **Colour palette** section of Details.
#' @param palette character or `NULL`. Named colour palette for the continuous
#'   node fill scale. Forwarded to [plot_group_mst()]. Default is `"bipolar"`.
#' @param ... Additional arguments passed to [plot_group_mst()].
#' @export
plot_cluster_mst <- function(.data, cluster, palette_cluster = "auto",
                              palette = "bipolar", ...) {
  plot_group_mst(.data, group = cluster, palette_group = palette_cluster,
                  palette = palette, ...)
}