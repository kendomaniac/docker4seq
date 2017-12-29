#' @title Testing if Docker is installed
#' @description This function check that docker is installed
#'
#' @return a character string indicating the version of the docker installed in the system
#' @examples
#'  dockerTest()
#' @export

dockerTest <- function()
{
  test <- system("docker -v", intern = TRUE)

  if (length(test)==0)
  {
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return(FALSE)
  }else{
    cat(paste("\n In your system the following version of Docker is installed:\n",test,sep=""))
    return(TRUE)
  }
}
