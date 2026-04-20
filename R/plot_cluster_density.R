#' @md
#' @title Plot density of variable values with per-group overlays
#'
#' @description
#' For each variable, plots kernel density estimates with per-group overlays
#' (density curves and/or median lines). Returns a named list of ggplot2
#' objects or a single faceted plot.
#'
#' @details
#' ## Density modes
#'
#' The `density` argument controls what is shown:
#'
#' - `"both"` (default): the overall density curve plus one density curve per
#'   group, coloured by group. Group curves are scaled according to the
#'   `scale` argument.
#' - `"overall"`: the overall density of all observations with a vertical line
#'   per group at that group's median value.
#' - `"cluster"`: one density curve per group, coloured by group.
#'
#' ## Rug
#'
#' A rug is added by default. The `rug` argument controls which data it shows:
#'
#' - `NULL` (default): per-group rug when `density` is `"cluster"` or
#'   `"both"`, overall rug when `density` is `"overall"`.
#' - `"cluster"`: per-group rug, coloured by group.
#' - `"overall"`: overall rug (no group colouring).
#'
#' ## Bandwidth
#'
#' The `bandwidth` argument controls per-group bandwidth selection (used for
#' per-group density curves and for the even-weighted overall density):
#'
#' - `"hpi_1"` (default): `ks::hpi(x, deriv.order = 1)` --- plug-in bandwidth
#'   based on the first derivative, less sensitive to group size than SJ.
#' - `"hpi_0"`: `ks::hpi(x, deriv.order = 0)` --- plug-in bandwidth based on
#'   the density itself.
#' - `"SJ"`: Sheather-Jones bandwidth via `stats::bw.SJ()`.
#' - A positive numeric value: use that value as the bandwidth directly.
#'
#' If bandwidth estimation fails, a warning is issued and the default
#' (`stats::bw.nrd0`) bandwidth is used as a fallback.
#'
#' ## Layout
#'
#' By default the function returns a **named list of ggplot2 objects**, one per
#' variable. If `n_col` or `n_row` is supplied the plots are instead combined
#' into a **single faceted ggplot2 object** via `facet_wrap`.
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
#' @param .data data.frame. Rows are observations. Must contain a column
#'   identifying group membership and columns for variable values.
#' @param group character. Name of the column in `.data` that identifies
#'   group membership.
#' @param vars character vector or `NULL`. Names of columns in `.data` to
#'   use as variables. If `NULL`, all columns except `group` are used.
#'   Default is `NULL`.
#' @param col_clusters named character vector or `NULL`. Per-group colours.
#'   Names should match group labels. When `NULL` (default), colours are
#'   chosen automatically by `palette_group`.
#' @param palette_group character. Palette used for automatic colour assignment
#'   when `col_clusters` is `NULL`. One of `"auto"` (default), `"okabe_ito"`,
#'   `"paired"`, `"kelly"`, `"glasbey"`, or `"hue_pal"`. See the **Colour
#'   palette** section of Details.
#' @param n_col integer or `NULL`. Number of columns passed to
#'   `ggplot2::facet_wrap`. If supplied (or if `n_row` is supplied) a single
#'   faceted plot is returned instead of a list. Default is `NULL`.
#' @param n_row integer or `NULL`. Number of rows passed to
#'   `ggplot2::facet_wrap`. If supplied (or if `n_col` is supplied) a single
#'   faceted plot is returned instead of a list. Default is `NULL`.
#' @param density character. What density to display. One of `"both"` (default:
#'   overall density curve plus per-cluster density curves), `"overall"`
#'   (overall density curve plus cluster median lines), or `"cluster"` (one
#'   density curve per cluster, coloured by cluster). See **Details**.
#' @param scale character. How to scale per-cluster density curves. Only
#'   relevant when `density` is `"cluster"` or `"both"`. One of
#'   `"max_overall"` (default: each cluster density is rescaled so that its
#'   maximum equals the maximum of the overall density, keeping y-axis values
#'   comparable to the overall density), `"max_cluster"` (no rescaling;
#'   y-axis is determined by the tallest curve), or `"free"` (no rescaling;
#'   equivalent to `"max_cluster"`).
#' @param scales character. The `scales` argument passed to
#'   `ggplot2::facet_wrap` when a faceted plot is requested. Default is
#'   `"free_y"` so that the x-axis is shared across panels.
#' @param expand_coord numeric vector or named list or `NULL`. Expands the
#'   x-axis limits to include the given values. A plain numeric vector is
#'   applied to every variable. A named list (names = variable names, values =
#'   numeric vectors) applies expansion per variable. When a faceted plot is
#'   requested via `n_col`/`n_row` and a named list is provided, a warning is
#'   issued and `expand_coord` is ignored (incompatible with faceting).
#'   Default is `NULL`.
#' @param exclude_min character. Whether to exclude observations whose value
#'   equals the minimum from the density and median calculations. Options are:
#'   `"no"` (default, no exclusion), `"overall"` (exclude observations whose
#'   value equals the global minimum across all variables), or `"variable"`
#'   (for each variable, exclude observations whose value equals that
#'   variable's minimum).
#' @param rug character or `NULL`. Controls the rug added below the density.
#'   `NULL` (default): per-cluster rug when `density` is `"cluster"` or
#'   `"both"`, overall rug when `density` is `"overall"`. `"cluster"`:
#'   per-cluster rug, coloured by cluster. `"overall"`: overall rug with no
#'   cluster colouring. See **Details**.
#' @param density_overall_weight character or `NULL`. Controls weighting of the
#'   overall density when `density` is `"overall"` or `"both"`. `NULL`
#'   (default): the overall density is estimated from all observations pooled
#'   together. `"even"`: the overall density is computed as an equal-weight
#'   average of per-cluster kernel densities, preventing larger clusters from
#'   dominating the density estimate. Ignored when `density` is `"cluster"`.
#' @param bandwidth character or positive numeric. Bandwidth used for
#'   per-cluster kernel density estimation. One of `"hpi_1"` (default),
#'   `"hpi_0"`, `"SJ"`, or a positive number. See **Details**.
#' @param na_rm logical. Whether to remove `NA` values from each variable
#'   before computing densities and medians. When `TRUE` (default), `NA`
#'   values are removed and a message is issued showing how many were removed
#'   per variable. When `FALSE`, `NA` values are passed directly to
#'   `stats::density()`, which will strip them with its own warning.
#' @param alpha numeric. Transparency applied to density curves (both overall
#'   and per-group lines). Must be between 0 (fully transparent) and 1 (fully
#'   opaque). Default is `0.75`.
#' @param pop_ref numeric vector, named list of numeric vectors, or `NULL`.
#'   An additional reference population rendered as a filled light-grey density
#'   area behind all other layers. A plain numeric vector is used for every
#'   variable. A named list (names = variable names, values = numeric vectors)
#'   applies a per-variable reference. `NULL` (default) adds no reference
#'   layer. The density bandwidth, NA handling, and (when `scale =
#'   "max_overall"`) y-axis scaling all follow the same settings as the main
#'   data.
#' @param pop_ref_label character or `NULL`. Label for the `pop_ref` fill
#'   legend entry. When `NULL` (default) and `pop_ref` is supplied, the label
#'   is derived automatically from the name of the object passed to `pop_ref`;
#'   if no simple name can be determined, `"Reference"` is used. Set to `""`
#'   to show the reference area with no legend entry.
#' @param linewidth_overall numeric. Line width for the overall (pooled)
#'   population density curve. Default is `1`.
#' @param linewidth_cluster numeric. Line width for per-group density curves.
#'   Default is `0.5`.
#' @param label logical. Whether to add on-plot labels at the highest-density
#'   peak of each group using `ggrepel::geom_text_repel`. When `density` is
#'   `"overall"`, labels are placed at the overall-density value at each
#'   group's median. Default is `FALSE`.
#' @param show_legend logical or `NULL`. Whether to display the legend. `NULL`
#'   (default): the legend is shown when the number of groups is 15 or fewer
#'   and hidden otherwise. `TRUE`/`FALSE`: always show/hide the legend,
#'   overriding the default behaviour. When both `show_legend` and the
#'   deprecated `legend` are supplied, `show_legend` takes precedence.
#' @param legend logical or `NULL`. Deprecated alias for `show_legend`.
#'   Retained for backwards compatibility.
#' @param font_size numeric. Font size passed to `cowplot::theme_cowplot`.
#'   Default is `14`.
#' @param thm ggplot2 theme object or `NULL`. Default is
#'   `cowplot::theme_cowplot(font_size = font_size)` with a white plot
#'   background. Set to `NULL` to apply no theme adjustment.
#' @param grid ggplot2 panel grid or `NULL`. Default is
#'   `cowplot::background_grid(major = "xy")`. Set to `NULL` for no grid.
#'
#' @return A named list of ggplot2 objects (one per variable) when neither
#'   `n_col` nor `n_row` is specified. A single ggplot2 object with
#'   `facet_wrap` panels when `n_col` or `n_row` is specified.
#' @export
#'
#' @examples
#' set.seed(1)
#' .data <- data.frame(
#'   group = rep(paste0("C", 1:3), each = 20),
#'   var1 = c(rnorm(20, 2), rnorm(20, 0), rnorm(20, -2)),
#'   var2 = c(rnorm(20, -1), rnorm(20, 1), rnorm(20, 0))
#' )
#' # Default: overall + per-group density curves
#' plot_list <- plot_group_density(.data, group = "group")
#'
#' # Overall density with group median lines only
#' plot_group_density(.data, group = "group", density = "overall")
#'
#' # Per-group density curves only
#' plot_group_density(.data, group = "group", density = "cluster")
#'
#' # Even-weighted overall density
#' plot_group_density(
#'   .data, group = "group", density_overall_weight = "even"
#' )
#'
#' # Faceted plot with 2 columns
#' plot_group_density(.data, group = "group", n_col = 2)
#'
#' # On-plot labels at density peaks
#' plot_group_density(.data, group = "group", density = "cluster", label = TRUE)
#'
#' # Always show the legend regardless of group count
#' plot_group_density(.data, group = "group", legend = TRUE)
plot_group_density <- function(.data,
                                 group,
                                 vars = NULL,
                                 col_clusters = NULL,
                                 palette_group = "auto",
                                 n_col = NULL,
                                 n_row = NULL,
                                 density = "both",
                                 scale = "max_overall",
                                 scales = "free_y",
                                 expand_coord = NULL,
                                 exclude_min = "no",
                                 rug = NULL,
                                 density_overall_weight = NULL,
                                 bandwidth = "hpi_1",
                                 na_rm = TRUE,
                                 alpha = 0.75,
                                 pop_ref = NULL,
                                 pop_ref_label = NULL,
                                 linewidth_overall = 1,
                                 linewidth_cluster = 0.5,
                                 label = FALSE,
                                 show_legend = NULL,
                                 legend = NULL,
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
  # Capture the expression passed to pop_ref before any evaluation modifies it,
  # so that we can auto-derive a legend label from the variable name.
  if (is.null(pop_ref_label) && !missing(pop_ref) && !is.null(pop_ref)) {
    pop_ref_nm <- deparse(substitute(pop_ref))
    pop_ref_label <- if (grepl("^[A-Za-z._][A-Za-z0-9._]*$", pop_ref_nm)) {
      pop_ref_nm
    } else {
      "Reference"
    }
  }

  cluster <- group
  density <- match.arg(density, c("both", "overall", "cluster"))
  scale <- match.arg(scale, c("max_overall", "max_cluster", "free"))
  exclude_min <- match.arg(exclude_min, c("no", "overall", "variable"))
  if (!is.null(rug)) rug <- match.arg(rug, c("overall", "cluster"))
  if (!is.null(density_overall_weight)) {
    density_overall_weight <- match.arg(
      density_overall_weight, c("even")
    )
  }
  if (!is.numeric(bandwidth)) {
    bandwidth <- match.arg(
      as.character(bandwidth), c("hpi_1", "hpi_0", "SJ")
    )
  } else if (bandwidth <= 0) {
    stop("`bandwidth` must be a positive number.", call. = FALSE)
  }
  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("`na_rm` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
        alpha < 0 || alpha > 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.null(pop_ref)) {
    if (is.list(pop_ref)) {
      if (!all(vapply(pop_ref, is.numeric, logical(1L)))) {
        stop("Each element of `pop_ref` must be a numeric vector.", call. = FALSE)
      }
    } else if (!is.numeric(pop_ref)) {
      stop(
        "`pop_ref` must be a numeric vector or a named list of numeric vectors.",
        call. = FALSE
      )
    }
    # Strip NAs from pop_ref following the same na_rm logic as .data.
    if (is.list(pop_ref)) {
      pop_ref <- lapply(pop_ref, function(vals) {
        n_na <- sum(is.na(vals))
        if (n_na > 0L && na_rm) {
          message("Removing ", n_na, " NA value(s) from `pop_ref`.")
          vals[!is.na(vals)]
        } else {
          vals
        }
      })
    } else {
      n_na <- sum(is.na(pop_ref))
      if (n_na > 0L && na_rm) {
        message("Removing ", n_na, " NA value(s) from `pop_ref`.")
        pop_ref <- pop_ref[!is.na(pop_ref)]
      }
    }
  }
  if (!is.null(pop_ref_label) &&
      (!is.character(pop_ref_label) || length(pop_ref_label) != 1L)) {
    stop("`pop_ref_label` must be a single character string or NULL.", call. = FALSE)
  }
  if (!is.numeric(linewidth_overall) || length(linewidth_overall) != 1L ||
      is.na(linewidth_overall) || linewidth_overall <= 0) {
    stop("`linewidth_overall` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(linewidth_cluster) || length(linewidth_cluster) != 1L ||
      is.na(linewidth_cluster) || linewidth_cluster <= 0) {
    stop("`linewidth_cluster` must be a single positive number.", call. = FALSE)
  }
  if (!is.logical(label) || length(label) != 1L || is.na(label)) {
    stop("`label` must be TRUE or FALSE.", call. = FALSE)
  }
  # Resolve show_legend vs deprecated legend alias
  if (!is.null(show_legend) && !is.null(legend)) {
    warning(
      "Both `show_legend` and `legend` were supplied; `show_legend` takes ",
      "precedence. The `legend` argument is deprecated.",
      call. = FALSE
    )
  }
  if (is.null(show_legend) && !is.null(legend)) {
    show_legend <- legend
  }
  if (!is.null(show_legend) &&
      (!is.logical(show_legend) || length(show_legend) != 1L || is.na(show_legend))) {
    stop("`show_legend` must be TRUE, FALSE, or NULL.", call. = FALSE)
  }

  .plot_cluster_validate(.data, cluster, vars)

  # Alias to avoid rlang .data pronoun masking inside purrr closures
  data <- .data

  # Coerce cluster column to character unless it is already a factor,
  # ensuring ggplot2 always treats cluster as a discrete variable.
  if (!is.factor(data[[cluster]])) {
    data[[cluster]] <- as.character(data[[cluster]])
  }

  if (is.null(vars)) {
    vars <- setdiff(colnames(data), cluster)
  }

  use_facet <- !is.null(n_col) || !is.null(n_row)

  if (use_facet && is.list(expand_coord) && !is.null(names(expand_coord))) {
    warning(
      "`expand_coord` is a named list, which is incompatible with facet_wrap ",
      "mode (n_col/n_row specified). Ignoring `expand_coord`."
    )
    expand_coord <- NULL
  }

  global_min <- if (exclude_min == "overall") {
    min(unlist(lapply(vars, function(v) data[[v]])), na.rm = TRUE)
  } else {
    NULL
  }

  var_mins <- if (exclude_min == "variable") {
    stats::setNames(
      lapply(vars, function(v) min(data[[v]], na.rm = TRUE)),
      vars
    )
  } else {
    NULL
  }

  # Helper: apply exclude_min filter (does NOT handle NAs).
  .filter_vals <- function(vals, v) {
    if (exclude_min == "no") return(vals)
    min_val <- if (exclude_min == "overall") global_min else var_mins[[v]]
    vals[vals != min_val]
  }

  # Helper: strip NAs from a vector, messaging if any removed (na_rm = TRUE)
  # or returning the vector unchanged (na_rm = FALSE, left to density()).
  .strip_na <- function(vals, v, context = NULL) {
    n_na <- sum(is.na(vals))
    if (n_na == 0L) return(vals)
    if (na_rm) {
      ctx <- if (!is.null(context)) paste0(" (", context, ")") else ""
      message("Removing ", n_na, " NA value(s) from variable '", v, "'", ctx, ".")
      return(vals[!is.na(vals)])
    }
    vals
  }

  cluster_vec <- unique(data[[cluster]])
  n_groups <- length(cluster_vec)
  show_legend <- if (is.null(show_legend)) n_groups <= 15L else isTRUE(show_legend)

  # Helper: resolve per-cluster bandwidth from the `bandwidth` argument.
  .resolve_bw <- function(vals) {
    if (is.numeric(bandwidth)) return(bandwidth)
    tryCatch(
      switch(bandwidth,
        hpi_1 = ks::hpi(vals, deriv.order = 1),
        hpi_0 = ks::hpi(vals, deriv.order = 0),
        SJ    = stats::bw.SJ(vals)
      ),
      error = function(e) {
        warning(
          "'", bandwidth, "' bandwidth estimation failed; ",
          "falling back to default bandwidth. Original error: ",
          conditionMessage(e),
          call. = FALSE
        )
        stats::bw.nrd0(vals)
      }
    )
  }

  # Helper: compute a density tibble from a vector of values (pooled,
  # default bandwidth -- used for overall reference density).
  .dens_tbl <- function(vals) {
    if (length(vals) < 2) return(NULL)
    d <- stats::density(vals)
    tibble::tibble(x = d$x, y = d$y)
  }

  # Helper: compute the overall density as an equal-weight average of
  # per-cluster kernel densities using the resolved bandwidth.
  .even_weight_dens_tbl <- function(dens_vals, all_vals, v) {
    if (length(dens_vals) < 2) return(NULL)
    from <- min(dens_vals, na.rm = TRUE)
    to <- max(dens_vals, na.rm = TRUE)
    n_grid <- 512L
    x_grid <- seq(from, to, length.out = n_grid)

    y_list <- lapply(cluster_vec, function(cl) {
      cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
      cl_vals <- .strip_na(cl_vals, v, paste0("cluster '", cl, "'"))
      if (length(cl_vals) < 2) return(NULL)
      bw <- .resolve_bw(cl_vals)
      d <- stats::density(cl_vals, from = from, to = to, n = n_grid, bw = bw)
      d$y
    })

    valid_y <- Filter(Negate(is.null), y_list)
    if (length(valid_y) == 0) return(.dens_tbl(dens_vals))

    y_mat <- do.call(cbind, valid_y)
    tibble::tibble(x = x_grid, y = rowMeans(y_mat))
  }

  # Helper: compute per-cluster density tibbles using the resolved bandwidth,
  # optionally scaled.
  .cluster_dens_tbl <- function(all_vals, v, max_y_ref = NULL) {
    purrr::map_df(cluster_vec, function(cl) {
      cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
      cl_vals <- .strip_na(cl_vals, v, paste0("cluster '", cl, "'"))
      if (length(cl_vals) < 2) return(tibble::tibble())
      bw <- .resolve_bw(cl_vals)
      d <- stats::density(cl_vals, bw = bw)
      d_tbl <- tibble::tibble(x = d$x, y = d$y)
      if (!is.null(max_y_ref)) {
        max_cl_y <- max(d_tbl$y)
        if (max_cl_y > 0) d_tbl$y <- d_tbl$y * max_y_ref / max_cl_y
      }
      d_tbl$cluster <- cl
      d_tbl
    })
  }

  # Helper: choose the effective rug mode.
  .rug_mode <- function() {
    if (!is.null(rug)) return(rug)
    if (density %in% c("cluster", "both")) "cluster" else "overall"
  }

  # Helper: add a rug layer to a plot (non-faceted).
  .add_rug <- function(p, all_vals, dens_vals, v) {
    mode <- .rug_mode()
    if (mode == "overall") {
      rug_tbl <- tibble::tibble(rug_x = dens_vals)
      p + ggplot2::geom_rug(
        data = rug_tbl,
        ggplot2::aes(x = .data$rug_x),
        sides = "b",
        inherit.aes = FALSE
      )
    } else {
      rug_cl_tbl <- purrr::map_df(cluster_vec, function(cl) {
        cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
        tibble::tibble(rug_x = cl_vals, cluster = cl)
      })
      p + ggplot2::geom_rug(
        data = rug_cl_tbl,
        ggplot2::aes(x = .data$rug_x, colour = .data$cluster),
        sides = "b",
        inherit.aes = FALSE
      )
    }
  }

  # Helper: compute density tibble for the additional reference population,
  # using the same bandwidth strategy as the per-cluster densities.
  .pop_ref_dens_tbl <- function(v) {
    vals <- if (is.list(pop_ref)) pop_ref[[v]] else pop_ref
    if (is.null(vals) || length(vals) < 2) return(NULL)
    bw <- .resolve_bw(vals)
    d <- stats::density(vals, bw = bw)
    tibble::tibble(x = d$x, y = d$y)
  }

  # Helper: add a rug layer to a faceted plot.
  .add_rug_facet <- function(p) {
    mode <- .rug_mode()
    if (mode == "overall") {
      rug_long_tbl <- purrr::map_df(vars, function(v) {
        tibble::tibble(
          variable = v,
          rug_x = .filter_vals(data[[v]], v)
        )
      })
      p + ggplot2::geom_rug(
        data = rug_long_tbl,
        ggplot2::aes(x = .data$rug_x),
        sides = "b",
        inherit.aes = FALSE
      )
    } else {
      rug_long_tbl <- purrr::map_df(vars, function(v) {
        purrr::map_df(cluster_vec, function(cl) {
          cl_vals <- .filter_vals(data[[v]][data[[cluster]] == cl], v)
          tibble::tibble(variable = v, rug_x = cl_vals, cluster = cl)
        })
      })
      p + ggplot2::geom_rug(
        data = rug_long_tbl,
        ggplot2::aes(x = .data$rug_x, colour = .data$cluster),
        sides = "b",
        inherit.aes = FALSE
      )
    }
  }

  # TRUE when pop_ref is provided and a non-empty label was derived/supplied.
  use_pop_ref_label <- !is.null(pop_ref_label) && nzchar(pop_ref_label)

  if (!use_facet) {
    plot_list <- stats::setNames(
      lapply(vars, function(v) {
        all_vals  <- data[[v]]
        all_vals  <- .strip_na(all_vals, v)
        dens_vals <- .filter_vals(all_vals, v)

        pop_ref_d <- if (!is.null(pop_ref)) .pop_ref_dens_tbl(v) else NULL
        p <- ggplot2::ggplot()
        if (!is.null(pop_ref_d)) {
          pop_ref_d$pop_ref_group <- if (use_pop_ref_label) pop_ref_label else ""
          p <- p + ggplot2::geom_area(
            data = pop_ref_d,
            ggplot2::aes(x = .data$x, y = .data$y, fill = .data$pop_ref_group),
            colour = NA,
            alpha = alpha,
            inherit.aes = FALSE
          )
        }

        if (density == "overall") {
          med_tbl <- purrr::map_df(cluster_vec, function(cl) {
            cl_vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
            tibble::tibble(
              cluster = cl,
              median  = stats::median(cl_vals, na.rm = FALSE)
            )
          })

          if (!is.null(density_overall_weight)) {
            overall_d <- .even_weight_dens_tbl(dens_vals, all_vals, v)
            p <- p +
              ggplot2::geom_line(
                data = overall_d,
                ggplot2::aes(x = .data$x, y = .data$y),
                alpha = alpha,
                linewidth = linewidth_overall
              ) +
              ggplot2::geom_vline(
                data = med_tbl,
                ggplot2::aes(
                  xintercept = .data$median, colour = .data$cluster
                )
              ) +
              ggplot2::labs(x = v, y = "Density", colour = "Group")
          } else {
            p <- p +
              ggplot2::geom_density(
                data = tibble::tibble(value = dens_vals),
                ggplot2::aes(x = .data$value),
                alpha = alpha,
                linewidth = linewidth_overall
              ) +
              ggplot2::geom_vline(
                data = med_tbl,
                ggplot2::aes(
                  xintercept = .data$median, colour = .data$cluster
                )
              ) +
              ggplot2::labs(x = v, y = "Density", colour = "Group")
          }
        } else {
          if (!is.null(density_overall_weight)) {
            overall_d <- .even_weight_dens_tbl(dens_vals, all_vals, v)
          } else {
            overall_d <- .dens_tbl(dens_vals)
          }
          max_y_ref <- if (scale == "max_overall" && !is.null(overall_d)) {
            max(overall_d$y)
          } else {
            NULL
          }
          if (!is.null(pop_ref_d) && !is.null(max_y_ref)) {
            max_pr_y <- max(pop_ref_d$y)
            if (max_pr_y > 0) pop_ref_d$y <- pop_ref_d$y * max_y_ref / max_pr_y
          }
          cl_dens <- .cluster_dens_tbl(all_vals, v, max_y_ref)

          if (density == "cluster") {
            p <- p +
              ggplot2::geom_line(
                data = cl_dens,
                ggplot2::aes(x = .data$x, y = .data$y, colour = .data$cluster),
                alpha = alpha,
                linewidth = linewidth_cluster
              ) +
              ggplot2::labs(x = v, y = "Density", colour = "Group")
          } else {
            p <- p +
              ggplot2::geom_line(
                data = cl_dens,
                ggplot2::aes(
                  x = .data$x, y = .data$y, colour = .data$cluster
                ),
                alpha = alpha,
                linewidth = linewidth_cluster
              ) +
              ggplot2::geom_line(
                data = overall_d,
                ggplot2::aes(x = .data$x, y = .data$y),
                alpha = alpha,
                linewidth = linewidth_overall
              ) +
              ggplot2::labs(x = v, y = "Density", colour = "Group")
          }
        }

        p <- .add_rug(p, all_vals, dens_vals, v)

        if (isTRUE(label)) {
          if (density == "overall") {
            od_lbl <- if (!is.null(density_overall_weight)) {
              overall_d
            } else {
              .dens_tbl(dens_vals)
            }
            label_tbl <- purrr::map_df(cluster_vec, function(cl) {
              x_lbl <- med_tbl$median[med_tbl$cluster == cl]
              y_lbl <- if (!is.null(od_lbl) && length(x_lbl) == 1L) {
                y <- stats::approx(od_lbl$x, od_lbl$y, xout = x_lbl)$y
                if (is.na(y)) 0 else y
              } else {
                0
              }
              tibble::tibble(cluster = cl, x = x_lbl, y = y_lbl)
            })
          } else {
            label_tbl <- purrr::map_df(cluster_vec, function(cl) {
              cl_d <- cl_dens[cl_dens$cluster == cl, , drop = FALSE]
              if (nrow(cl_d) == 0L) return(tibble::tibble())
              peak_idx <- which.max(cl_d$y)
              tibble::tibble(
                cluster = cl, x = cl_d$x[peak_idx], y = cl_d$y[peak_idx]
              )
            })
          }
          p <- p + ggrepel::geom_text_repel(
            data = label_tbl,
            ggplot2::aes(
              x = .data$x, y = .data$y, label = .data$cluster,
              colour = .data$cluster
            ),
            inherit.aes = FALSE,
            show.legend = FALSE
          )
        }

        if (!is.null(expand_coord)) {
          ec <- if (is.list(expand_coord)) expand_coord[[v]] else expand_coord
          if (!is.null(ec)) p <- p + ggplot2::expand_limits(x = ec)
        }

        p <- p + ggplot2::scale_colour_manual(
          values = .discrete_cluster_colours(cluster_vec, col_clusters, palette_group)
        )
        if (!is.null(pop_ref_d)) {
          p <- p + ggplot2::scale_fill_manual(
            values = stats::setNames("grey85", if (use_pop_ref_label) pop_ref_label else ""),
            name = NULL,
            guide = if (use_pop_ref_label) ggplot2::guide_legend() else "none"
          )
        }
        if (!is.null(thm)) p <- p + thm
        if (!is.null(grid)) p <- p + grid
        if (!show_legend) p <- p + ggplot2::theme(legend.position = "none")

        p
      }),
      vars
    )
    return(plot_list)
  }

  pop_ref_facet_tbl <- if (!is.null(pop_ref)) {
    purrr::map_df(vars, function(v) {
      d <- .pop_ref_dens_tbl(v)
      if (is.null(d)) return(tibble::tibble())
      d$variable <- v
      d$pop_ref_group <- if (use_pop_ref_label) pop_ref_label else ""
      d
    })
  } else {
    NULL
  }
  p <- ggplot2::ggplot()
  if (!is.null(pop_ref_facet_tbl) && nrow(pop_ref_facet_tbl) > 0) {
    p <- p + ggplot2::geom_area(
      data = pop_ref_facet_tbl,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$pop_ref_group),
      colour = NA,
      alpha = alpha,
      inherit.aes = FALSE
    )
  }

  if (density == "overall") {
    long_tbl <- purrr::map_df(vars, function(v) {
      all_vals <- .strip_na(data[[v]], v)
      tibble::tibble(variable = v, value = .filter_vals(all_vals, v))
    })

    median_tbl <- purrr::map_df(vars, function(v) {
      all_vals <- .strip_na(data[[v]], v)
      purrr::map_df(cluster_vec, function(cl) {
        vals <- .filter_vals(all_vals[data[[cluster]] == cl], v)
        tibble::tibble(
          variable = v,
          cluster  = cl,
          median   = stats::median(vals, na.rm = FALSE)
        )
      })
    })

    if (!is.null(density_overall_weight)) {
      overall_dens_tbl <- purrr::map_df(vars, function(v) {
        all_vals  <- .strip_na(data[[v]], v)
        dens_vals <- .filter_vals(all_vals, v)
        d <- .even_weight_dens_tbl(dens_vals, all_vals, v)
        if (is.null(d)) return(tibble::tibble())
        d$variable <- v
        d
      })
      p <- p +
        ggplot2::geom_line(
          data = overall_dens_tbl,
          ggplot2::aes(x = .data$x, y = .data$y),
          alpha = alpha,
          linewidth = linewidth_overall
        ) +
        ggplot2::geom_vline(
          data = median_tbl,
          ggplot2::aes(xintercept = .data$median, colour = .data$cluster)
        ) +
        ggplot2::facet_wrap(
          ~ .data$variable,
          scales = scales,
          ncol = n_col,
          nrow = n_row
        ) +
        ggplot2::labs(x = "Value", y = "Density", colour = "Group")
    } else {
      p <- p +
        ggplot2::geom_density(
          data = long_tbl,
          ggplot2::aes(x = .data$value),
          alpha = alpha,
          linewidth = linewidth_overall
        ) +
        ggplot2::geom_vline(
          data = median_tbl,
          ggplot2::aes(xintercept = .data$median, colour = .data$cluster)
        ) +
        ggplot2::facet_wrap(
          ~ .data$variable,
          scales = scales,
          ncol = n_col,
          nrow = n_row
        ) +
        ggplot2::labs(x = "Value", y = "Density", colour = "Group")
    }
  } else {
    overall_dens_tbl <- purrr::map_df(vars, function(v) {
      all_vals  <- .strip_na(data[[v]], v)
      dens_vals <- .filter_vals(all_vals, v)
      d <- if (!is.null(density_overall_weight)) {
        .even_weight_dens_tbl(dens_vals, all_vals, v)
      } else {
        .dens_tbl(dens_vals)
      }
      if (is.null(d)) return(tibble::tibble())
      d$variable <- v
      d
    })

    cluster_dens_tbl <- purrr::map_df(vars, function(v) {
      all_vals  <- .strip_na(data[[v]], v)
      dens_vals <- .filter_vals(all_vals, v)
      max_y_ref <- if (scale == "max_overall") {
        od <- if (!is.null(density_overall_weight)) {
          .even_weight_dens_tbl(dens_vals, all_vals, v)
        } else {
          .dens_tbl(dens_vals)
        }
        if (!is.null(od)) max(od$y) else NULL
      } else {
        NULL
      }
      d <- .cluster_dens_tbl(all_vals, v, max_y_ref)
      if (nrow(d) == 0) return(tibble::tibble())
      d$variable <- v
      d
    })

    # Rescale pop_ref densities to the overall peak, per variable.
    if (!is.null(pop_ref_facet_tbl) && nrow(pop_ref_facet_tbl) > 0 &&
        scale == "max_overall") {
      pop_ref_facet_tbl <- purrr::map_df(vars, function(v) {
        sub <- pop_ref_facet_tbl[pop_ref_facet_tbl$variable == v, , drop = FALSE]
        if (nrow(sub) == 0L) return(sub)
        od_sub <- overall_dens_tbl[overall_dens_tbl$variable == v, , drop = FALSE]
        if (nrow(od_sub) == 0L) return(sub)
        max_pr <- max(sub$y)
        if (max_pr > 0) sub$y <- sub$y * max(od_sub$y) / max_pr
        sub
      })
    }

    if (density == "cluster") {
      p <- p +
        ggplot2::geom_line(
          data = cluster_dens_tbl,
          ggplot2::aes(x = .data$x, y = .data$y, colour = .data$cluster),
          alpha = alpha,
          linewidth = linewidth_cluster
        ) +
        ggplot2::facet_wrap(
          ~ .data$variable,
          scales = scales,
          ncol = n_col,
          nrow = n_row
        ) +
        ggplot2::labs(x = "Value", y = "Density", colour = "Group")
    } else {
      p <- p +
        ggplot2::geom_line(
          data = cluster_dens_tbl,
          ggplot2::aes(
            x = .data$x, y = .data$y, colour = .data$cluster
          ),
          alpha = alpha,
          linewidth = linewidth_cluster
        ) +
        ggplot2::geom_line(
          data = overall_dens_tbl,
          ggplot2::aes(x = .data$x, y = .data$y),
          alpha = alpha,
          linewidth = linewidth_overall
        ) +
        ggplot2::facet_wrap(
          ~ .data$variable,
          scales = scales,
          ncol = n_col,
          nrow = n_row
        ) +
        ggplot2::labs(x = "Value", y = "Density", colour = "Group")
    }
  }

  p <- .add_rug_facet(p)

  if (isTRUE(label)) {
    if (density == "overall") {
      label_tbl <- purrr::map_df(vars, function(v) {
        all_vals_lbl  <- .strip_na(data[[v]], v)
        dens_vals_lbl <- .filter_vals(all_vals_lbl, v)
        od_lbl <- if (!is.null(density_overall_weight)) {
          overall_dens_tbl[overall_dens_tbl$variable == v, , drop = FALSE]
        } else {
          .dens_tbl(dens_vals_lbl)
        }
        purrr::map_df(cluster_vec, function(cl) {
          x_lbl <- median_tbl$median[
            median_tbl$variable == v & median_tbl$cluster == cl
          ]
          y_lbl <- if (!is.null(od_lbl) && nrow(od_lbl) > 0L &&
                       length(x_lbl) == 1L) {
            y <- stats::approx(od_lbl$x, od_lbl$y, xout = x_lbl)$y
            if (is.na(y)) 0 else y
          } else {
            0
          }
          tibble::tibble(variable = v, cluster = cl, x = x_lbl, y = y_lbl)
        })
      })
    } else {
      label_tbl <- dplyr::group_by(
        cluster_dens_tbl, .data$variable, .data$cluster
      )
      label_tbl <- dplyr::slice_max(
        label_tbl, order_by = .data$y, n = 1L, with_ties = FALSE
      )
      label_tbl <- dplyr::ungroup(label_tbl)
      label_tbl <- dplyr::select(
        label_tbl, .data$variable, .data$cluster, .data$x, .data$y
      )
    }
    p <- p + ggrepel::geom_text_repel(
      data = label_tbl,
      ggplot2::aes(
        x = .data$x, y = .data$y, label = .data$cluster,
        colour = .data$cluster
      ),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  }

  if (!is.null(expand_coord) && is.numeric(expand_coord)) {
    p <- p + ggplot2::expand_limits(x = expand_coord)
  }

  p <- p + ggplot2::scale_colour_manual(
    values = .discrete_cluster_colours(cluster_vec, col_clusters, palette_group)
  )
  if (!is.null(pop_ref_facet_tbl) && nrow(pop_ref_facet_tbl) > 0) {
    p <- p + ggplot2::scale_fill_manual(
      values = stats::setNames("grey85", if (use_pop_ref_label) pop_ref_label else ""),
      name = NULL,
      guide = if (use_pop_ref_label) ggplot2::guide_legend() else "none"
    )
  }
  if (!is.null(thm)) p <- p + thm
  if (!is.null(grid)) p <- p + grid
  if (!show_legend) p <- p + ggplot2::theme(legend.position = "none")

  p
}

#' @rdname plot_group_density
#' @param cluster character. Name of the column in `.data` that identifies
#'   group membership. Alias for the `group` parameter.
#' @param palette_cluster character. Alias for `palette_group` in
#'   [plot_group_density()]. See the **Colour palette** section of Details.
#' @param ... Additional arguments passed to [plot_group_density()].
#' @export
plot_cluster_density <- function(.data, cluster, palette_cluster = "auto", ...) {
  plot_group_density(.data, group = cluster, palette_group = palette_cluster, ...)
}