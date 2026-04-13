# In R/utils.R or a similar file in your package
# declare these variables as global
# as they are column names in dataframes manipulated
# by dplyr
utils::globalVariables(c(
  'x', 'y', 'grp_y', '.id', '.grp', 'grp_x',
  'txt', 'lb', 'ub', 'est', 'data', 'pval', 'g2',
  'g1', '.y',
  'cluster', 'variable', 'perc'
  ))

# Internal named colour palettes for continuous fill/colour scales used by
# plot_cluster_heatmap, plot_cluster_mst, and plot_cluster_scatter.
#
# Each entry is a list with:
#   col          - character vector of colours, low to high
#   col_positions - "auto" or a numeric vector; "auto" uses the white_range
#                  argument of the calling function to stretch the middle colour
#                  of 3-colour palettes.
#
# Diverging palettes have col_positions = "auto" so the neutral midpoint is
# stretched over white_range.  Sequential palettes use evenly-spaced
# col_positions to avoid that stretching.
.cluster_colour_palettes <- function() {
  list(
    # Neutral blue-white-red. Default. Good for any pos/neg or high/low data.
    bipolar = list(
      col = c("#2166AC", "#F7F7F7", "#B2182B"),
      col_positions = "auto"
    ),
    # Green-white-red. Intuitive "good/safe/present" vs "bad/danger/absent".
    # Note: the red-green pairing is not ideal for red-green colour-blind
    # viewers; use "accessible" for a fully colour-blind-safe alternative.
    alarm = list(
      col = c("#1A9641", "#F7F7F7", "#D7191C"),
      col_positions = "auto"
    ),
    # Blue-white-burnt-orange. Fully colour-blind-safe diverging alternative
    # to "alarm" (uses the Okabe-Ito blue and vermillion).
    accessible = list(
      col = c("#0072B2", "#F7F7F7", "#D55E00"),
      col_positions = "auto"
    ),
    # Light-yellow to dark-red. Sequential warm scale for expression,
    # intensity, or activation.
    heat = list(
      col = c("#FFFFD4", "#FD8D3C", "#800026"),
      col_positions = c(0, 0.5, 1)
    ),
    # White to navy. Sequential cool scale for coverage, frequency, or
    # presence probability.
    sky = list(
      col = c("#F7FBFF", "#6BAED6", "#08306B"),
      col_positions = c(0, 0.5, 1)
    )
  )
}

# Internal helper: generate a named discrete colour vector for cluster/group
# labels. When user_colours is not NULL it is returned unchanged. Otherwise,
# colours are chosen from the palette specified by palette_group.
#
# palette_group choices:
#   "auto"      - pick automatically by n (see below)
#   "okabe_ito" - Okabe-Ito (max 8; colorblind-safe)
#   "paired"    - ColorBrewer Paired (max 12)
#   "kelly"     - Kelly max-contrast palette from Polychrome (max 21, white excluded)
#   "glasbey"   - Glasbey palette from Polychrome (max 31, white excluded)
#   "hue_pal"   - scales::hue_pal() (any n, may be indistinguishable at large n)
#
# "auto" tiers: <=8 okabe_ito, <=12 paired, <=21 kelly, <=31 glasbey, else hue_pal.
# In auto mode, missing Polychrome triggers a warning and falls back to hue_pal.
# When a palette is requested explicitly and is unsupported it stops with an error.
.discrete_cluster_colours <- function(groups, user_colours = NULL,
                                       palette_group = "auto") {
  if (!is.null(user_colours)) return(user_colours)
  n <- length(groups)

  pal <- match.arg(
    palette_group,
    c("auto", "okabe_ito", "paired", "kelly", "glasbey", "hue_pal")
  )

  .okabe_ito <- function() {
    c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#999999"
    )
  }
  .need_polychrome <- function(palette_name) {
    if (!requireNamespace("Polychrome", quietly = TRUE)) {
      stop(
        sprintf(
          "'%s' palette requires the 'Polychrome' package. ",
          palette_name
        ),
        "Install it with: install.packages('Polychrome').",
        call. = FALSE
      )
    }
  }
  .polychrome_warn_fallback <- function(palette_name) {
    warning(
      sprintf(
        "Package 'Polychrome' is recommended for > 12 groups ('%s' palette) ",
        palette_name
      ),
      "but is not installed. Falling back to hue_pal(). ",
      "Install it with: install.packages('Polychrome').",
      call. = FALSE
    )
  }

  if (pal == "auto") {
    pal <- if (n <= 8L) {
      "okabe_ito"
    } else if (n <= 12L) {
      "paired"
    } else if (n <= 21L) {
      if (!requireNamespace("Polychrome", quietly = TRUE)) {
        .polychrome_warn_fallback("kelly")
        "hue_pal"
      } else {
        "kelly"
      }
    } else if (n <= 31L) {
      if (!requireNamespace("Polychrome", quietly = TRUE)) {
        .polychrome_warn_fallback("glasbey")
        "hue_pal"
      } else {
        "glasbey"
      }
    } else {
      warning(
        n, " groups exceeds the recommended maximum of 31 for distinct ",
        "colours. Falling back to hue_pal(), which may produce ",
        "indistinguishable colours at this scale.",
        call. = FALSE
      )
      "hue_pal"
    }
  }

  cols <- switch(pal,
    okabe_ito = {
      ok <- .okabe_ito()
      if (n > length(ok)) {
        stop(
          sprintf("'okabe_ito' supports at most %d groups; got %d.", length(ok), n),
          call. = FALSE
        )
      }
      ok[seq_len(n)]
    },
    paired = {
      if (n > 12L) {
        stop(
          sprintf("'paired' supports at most 12 groups; got %d.", n),
          call. = FALSE
        )
      }
      scales::brewer_pal(palette = "Paired")(n)
    },
    kelly = {
      .need_polychrome("kelly")
      if (n > 21L) {
        stop(
          sprintf("'kelly' supports at most 21 groups; got %d.", n),
          call. = FALSE
        )
      }
      unname(Polychrome::kelly.colors(22L))[-1L][seq_len(n)]
    },
    glasbey = {
      .need_polychrome("glasbey")
      if (n > 31L) {
        stop(
          sprintf("'glasbey' supports at most 31 groups; got %d.", n),
          call. = FALSE
        )
      }
      unname(Polychrome::glasbey.colors(32L))[-1L][seq_len(n)]
    },
    hue_pal = scales::hue_pal()(n)
  )

  stats::setNames(cols, groups)
}

# Internal helper: resolve palette → col + col_positions, with validation.
# Returns a list(col, col_positions).  If palette is NULL, returns the
# supplied col/col_positions unchanged.
.resolve_cluster_palette <- function(palette, col, col_positions) {
  if (is.null(palette)) return(list(col = col, col_positions = col_positions))
  pals <- .cluster_colour_palettes()
  if (!is.character(palette) || length(palette) != 1L || is.na(palette)) {
    stop("`palette` must be a single character string or NULL.", call. = FALSE)
  }
  if (!palette %in% names(pals)) {
    stop(
      paste0(
        "`palette` must be one of: ",
        paste(paste0('"', names(pals), '"'), collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  list(col = pals[[palette]]$col, col_positions = pals[[palette]]$col_positions)
}

# Internal helper: build colour/fill scale arguments from col + col_positions
# + white_range, applying the same stretching logic used by heatmap and MST.
# Returns a list(colours, values) suitable for scale_*_gradientn.
.build_gradientn_args <- function(col, col_positions, white_range) {
  n_col <- length(col)
  if (identical(col_positions, "auto")) {
    if (n_col == 3L) {
      list(
        colours = c(col[1], col[2], col[2], col[3]),
        values  = c(0, white_range[1], white_range[2], 1)
      )
    } else {
      list(
        colours = col,
        values  = seq(0, 1, length.out = n_col)
      )
    }
  } else {
    list(colours = col, values = col_positions)
  }
}
