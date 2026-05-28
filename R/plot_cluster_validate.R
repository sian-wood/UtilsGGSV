#' @title Validate inputs shared by plot_cluster functions
#'
#' @description
#' Internal helper that validates the `.data`, `cluster`, and `vars` arguments
#' shared by `plot_cluster_heatmap` and `plot_cluster_density`. Stops with an
#' informative error message if any check fails.
#'
#' @param .data data.frame. The input data.
#' @param cluster character(1). Name of the cluster column.
#' @param vars character vector or `NULL`. Variable column names.
#'
#' @return Invisibly returns `NULL`. Called for its side-effect (errors).
#' @keywords internal
.plot_cluster_validate <- function(.data, cluster, vars) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(cluster) || length(cluster) != 1L || is.na(cluster)) {
    stop("`cluster` must be a single non-NA character string.", call. = FALSE)
  }

  if (!cluster %in% colnames(.data)) {
    stop(
      paste0(
        "`cluster` column \"", cluster, "\" not found in `.data`."
      ),
      call. = FALSE
    )
  }

  if (is.numeric(.data[[cluster]])) {
    stop(
      paste0(
        "The `cluster` column (\"", cluster, "\") is numeric. ",
        "Cluster membership must be discrete: convert it to a character or ",
        "factor vector before calling this function ",
        "(e.g. `.data[[\"", cluster, "\"]] <- as.character(.data[[\"",
        cluster, "\"]])`)."),
      call. = FALSE
    )
  }

  cluster_vals <- .data[[cluster]][!is.na(.data[[cluster]])]
  if (length(unique(cluster_vals)) < 1L) {
    stop(
      paste0(
        "The `cluster` column (\"", cluster, "\") must have at least ",
        "1 unique non-NA value."
      ),
      call. = FALSE
    )
  }

  if (!is.null(vars)) {
    if (!is.character(vars) || length(vars) == 0L) {
      stop("`vars` must be a non-empty character vector or NULL.", call. = FALSE)
    }
    missing_vars <- setdiff(vars, colnames(.data))
    if (length(missing_vars) > 0L) {
      stop(
        paste0(
          "The following columns in `vars` were not found in `.data`: ",
          paste(missing_vars, collapse = ", "), "."
        ),
        call. = FALSE
      )
    }
    non_numeric_vars <- vars[!vapply(vars, function(v) is.numeric(.data[[v]]),
                                     logical(1L))]
    if (length(non_numeric_vars) > 0L) {
      stop(
        paste0(
          "The following `vars` columns are not numeric: ",
          paste(non_numeric_vars, collapse = ", "), "."
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}
