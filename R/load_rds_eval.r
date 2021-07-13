load_rds_eval <- function(filename,
                          dir_save = NULL) {

  filename <- ifelse(
    grepl(".rds$", filename),
    filename,
    paste0(filename, ".rds")
  )

  if (!is.null(dir_save)) {
    filename <- file.path(dir_save, filename)
  }

  obj_out <- readRDS(filename)

  if (class(obj_out) != "saver_uneval") {
    return(obj_out)
  }

  .save_rds_eval(
    fn_or_call = obj_out$fn_or_call,
    p_dots = obj_out$p_dots,
    filename = filename,
    return_obj = TRUE
  )
}