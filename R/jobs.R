# Background jobs ####
#
# Heavy tools (wavelets with significance, eCOCO, eTimeOpt, ...) run in a separate R process (callr), so the
# window stays responsive and a run can be cancelled.

# Runs in the background process (callr keeps the package namespace as its environment: package = TRUE)
cd_job_worker <- function(tool_id, d, p, d2, file, file2, dev_opt) {
  options(waverider.dev_new = dev_opt)
  grDevices::pdf(NULL)
  cd_run_tool(tool_id, d = d, p = p, d2 = d2, file = file, file2 = file2)
}

cd_job_start <- function(tool_id, d, p, d2, file, file2) {
  callr::r_bg(cd_job_worker, args = list(tool_id = tool_id, d = d, p = p, d2 = d2, file = file, file2 = file2,
      dev_opt = FALSE), supervise = TRUE, package = TRUE)
}

cd_job_done <- function(job) {
  !job$is_alive()
}

cd_job_result <- function(job) {
  out <- try(job$get_result(), silent = TRUE)
  if (inherits(out, "try-error")) {
    msg <- conditionMessage(attr(out, "condition"))
    pe <- attr(out, "condition")$parent
    if (!is.null(pe)) {
      msg <- conditionMessage(pe)
    }
    stop(msg, call. = FALSE)
  }
  out
}
