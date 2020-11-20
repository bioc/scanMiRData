#' getKdModels
#'
#' Returns a collection of miRNA KdModels for the requested species.
#'
#' @param species Either "hsa" (human), "mmu" (mouse) or "rno" (rat)
#'
#' @return An object of class \code{\link[scanMiR]{KdModelList}}
#' @export
#' @import scanMiR
#' @examples
#' mods <- getKdModels("rno")
#' summary(mods)
getKdModels <- function(species=c("hsa","mmu","rno")){
  species <- match.arg(species)
  get(data(species))
}
