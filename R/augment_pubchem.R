#' Lookup data from PubChem
#'
#'
#' @param chemtable the data frame of the data about the compounds
#' @param CAS Optional, a vector of CAS numbers
#' @keywords
#' @examples
#' @export
augment_pubchem<-function(chemtable, properties=NULL){
  if(is.na(properties)) {
    properties = c('MolecularFormula', 'MolecularWeight', 'IsomericSMILES', 'InChI','InChIKey', 'IUPACName')
  }
  if(!require(webchem)) {
    install.packages("webchem")
  }
  require(httr)
  httr::set_config(httr::config(http_version = 0)) #fixes connection bug

  CID <- sapply(chemtable$name, webchem::get_cid, verbose=FALSE)
  prop <- webchem::pc_prop(CID, properties = properties, verbose=FALSE)
  chemtable <- cbind(chemtable, prop)
  return(chemtable)
}


