#' Synthetic QC data sample: hole in flow cell
#'
#' Creates a \code{flowFrame} with artificially generated fluorescence channels, for QC evaluation and benchmarking.
#'
#' @param out.flow_rate if not \code{NULL}, the function will asign an object with flow rate signal data to this.
#' @param out.fluorescence if not \code{NULL}, the function will asign an object with fluorescence signal data to this.
#'
#' @return \code{flowFrame} object.
#'
#' @seealso See other functions from the \code{qctoy} package. Start with \code{sim.baseline}. For plotting, use \code{sim.plot}.
#'
#' @export
sim.sample.hole_in_flow_cell <- function(out.flow_rate = NULL,
                                         out.fluorescence = NULL) {
  flow_rate <- sim.baseline(N = 3000, mu = 150, sigma = 3, distort = "sin", distort_par = c(5, 500)) %>%
    sim.prune(i = 19, N = 20) %>%
    sim.round()

  fluo1 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 4000, sigma = 200)
  fluo2 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 0, sigma = 50)
  fluo <- sim.concatenate(fluo1, fluo2)

  ff <- sim.make_flowFrame(fluo, fluo,
                           channel_names = c("channel1", "channel2"),
                           marker_names = c("marker1", "marker2"))

  if (!is.null(out.flow_rate)) {
    eval.parent(substitute(out.flow_rate <- flow_rate))
  }

  if (!is.null(out.fluorescence)) {
    eval.parent(substitute(out.fluorescence <- fluo))
  }

  return(ff)
}

#' Synthetic QC data sample: linear increase in fluorescence signal over time
#'
#' Creates a \code{flowFrame} with artificially generated fluorescence channels, for QC evaluation and benchmarking.
#'
#' @param out.flow_rate if not \code{NULL}, the function will asign an object with flow rate signal data to this.
#' @param out.fluorescence if not \code{NULL}, the function will asign an object with fluorescence signal data to this.
#'
#' @return \code{flowFrame} object.
#'
#' @seealso See other functions from the \code{qctoy} package. Start with \code{sim.baseline}. For plotting, use \code{sim.plot}.
#'
#' @export
sim.sample.linear_increase_in_signal <- function(out.flow_rate = NULL,
                                                 out.fluorescence = NULL) {
  flow_rate <- sim.baseline(N = 3000, mu = 150, sigma = 1.5) %>%
    sim.prune(i = 19, N = 20, reindex = TRUE) %>%
    sim.round()

  fluo1 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 4000, sigma = 200, distort = "slope", distort_par = c(.2))
  fluo2 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 0, sigma = 500, distort = "slope", distort_par = c(.2))
  fluo <- sim.concatenate(fluo1, fluo2)

  ff <- sim.make_flowFrame(fluo, fluo,
                           channel_names = c("channel1", "channel2"),
                           marker_names = c("marker1", "marker2"))

  if (!is.null(out.flow_rate)) {
    eval.parent(substitute(out.flow_rate <- flow_rate))
  }

  if (!is.null(out.fluorescence)) {
    eval.parent(substitute(out.fluorescence <- fluo))
  }

  return(ff)
}

#' Synthetic QC data sample: measuring air in flow cell
#'
#' Creates a \code{flowFrame} with artificially generated fluorescence channels, for QC evaluation and benchmarking.
#'
#' @param out.flow_rate if not \code{NULL}, the function will asign an object with flow rate signal data to this.
#' @param out.fluorescence if not \code{NULL}, the function will asign an object with fluorescence signal data to this.
#'
#' @return \code{flowFrame} object.
#'
#' @seealso See other functions from the \code{qctoy} package. Start with \code{sim.baseline}. For plotting, use \code{sim.plot}.
#'
#' @export
sim.sample.measuring_air <- function(out.flow_rate = NULL,
                                     out.fluorescence = NULL) {
  flow_rate <- sim.baseline(N = 3000, mu = 150, sigma = 3) %>%
    sim.exponential(range = 1500:2000, strength = 4) %>%
    sim.sharp_flex(range = 2000:3000, strength = 4) %>%
    sim.prune(i = 19, N = 20) %>%
    sim.round()

  fluo1 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 4000, sigma = 200) %>%
    sim.sharp_push(range = c(2000,Inf), strength = -3000)
  fluo2 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 0, sigma = 50) %>%
    sim.sharp_push(range = c(2000,Inf), strength = -2000)
  fluo <- sim.concatenate(fluo1, fluo2)

  ff <- sim.make_flowFrame(fluo, fluo,
                           channel_names = c("channel1", "channel2"),
                           marker_names = c("marker1", "marker2"))

  if (!is.null(out.flow_rate)) {
    eval.parent(substitute(out.flow_rate <- flow_rate))
  }

  if (!is.null(out.fluorescence)) {
    eval.parent(substitute(out.fluorescence <- fluo))
  }

  return(ff)
}

#' Synthetic QC data sample: permanent flow rate change
#'
#' Creates a \code{flowFrame} with artificially generated fluorescence channels, for QC evaluation and benchmarking.
#'
#' @param out.flow_rate if not \code{NULL}, the function will asign an object with flow rate signal data to this.
#' @param out.fluorescence if not \code{NULL}, the function will asign an object with fluorescence signal data to this.
#'
#' @return \code{flowFrame} object.
#'
#' @seealso See other functions from the \code{qctoy} package. Start with \code{sim.baseline}. For plotting, use \code{sim.plot}.
#'
#' @export
sim.sample.permanent_rate_change <- function(out.flow_rate = NULL,
                                             out.fluorescence = NULL) {
  flow_rate <- sim.baseline(N = 3000, mu = 150, sigma = 1.5) %>%
    sim.sigmoid(range = 1500:2000, shift = 30) %>%
    sim.prune(i = 19, N = 20) %>%
    sim.round()

  fluo1 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 4000, sigma = 200) %>%
    sim.smooth_push(range = 1500:2000, strength = -3000, spikiness = .8, D = "gauss")
  fluo2 <- sim.baseline(flow_rate_baseline = flow_rate, mu = 0, sigma = 50) %>%
    sim.smooth_push(range = 1500:2000, strength = -2000, spikiness = .8, D = "gauss")
  fluo <- sim.concatenate(fluo1, fluo2)

  if (!is.null(out.flow_rate)) {
    eval.parent(substitute(out.flow_rate <- flow_rate))
  }

  if (!is.null(out.fluorescence)) {
    eval.parent(substitute(out.fluorescence <- fluo))
  }

  ff <- sim.make_flowFrame(fluo, fluo,
                           channel_names = c("channel1", "channel2"),
                           marker_names = c("marker1", "marker2"))
}
