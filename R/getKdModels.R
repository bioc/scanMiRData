#' getKdModels
#'
#' Returns a collection of miRNA KdModels for the requested species. 
#' \underline{Note that by default, low-confidence miRNAs are not returned} 
#' (see `categories`).
#'
#' @param species Either "hsa" (human), "mmu" (mouse) or "rno" (rat)
#' @param categories The (mirbase) categories of miRNAs to return. Any 
#' combination of "Low-confidence", "Poorly conserved", 
#' "Conserved across mammals", "Conserved across vertebrates". If NULL, all
#' categories are returned. miRNAs with NA as conservation are not returned
#' unless `categories==NULL`.
#'
#' @return An object of class \code{\link[scanMiR]{KdModelList}}
#' @export
#' @import scanMiR
#' @examples
#' mods <- getKdModels("rno")
#' summary(mods)
getKdModels <- function( species=c("hsa","mmu","rno"), 
                         categories=c( "Poorly conserved", 
                                       "Conserved across mammals", 
                                       "Conserved across vertebrates") ){
  cats <- c("Low-confidence","Poorly conserved","Conserved across mammals",
            "Conserved across vertebrates")
  if(!is.null(categories))
    categories <- match.arg(categories, cats, several.ok=TRUE)
  species <- match.arg(species)
  mods <- get(data(species))
  if(is.null(categories)) return(mods)
  con <- conservation(mods)
  if(all(is.na(con))){
    warning("The collection has no conservation information; the `categories`",
            " argument will be ignored.")
    return(mods)
  }
  mods[con %in% categories]
}
