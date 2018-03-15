## The two functions in R Script file were found in one of the answers to this question on stackoverflow.com:
## http://stackoverflow.com/questions/30127019/check-whether-file-is-binary
## The functions were written by the stackoverflow user MrFlick, a direct link to his answer is here:
## http://stackoverflow.com/a/30128809/3020822
## This answer was provided on May 8th 2015, which means as can be read here:
## http://meta.stackexchange.com/questions/271080/the-mit-license-clarity-on-using-code-on-stack-overflow-and-stack-exchange
## the code falls under the CC-BY-SA license available here:
## https://creativecommons.org/licenses/by-sa/3.0/

#' is_compressed
#'
#' @description Helper function for is_rdata, checks if the file is a compressed (gzip) file. Does not (yet) check for bzip2 or xz compression.
#'
#' @param filename The filename of the file to test for magic compression codes
#' @param magic.number The magic numbers in as a vector of strings with the hexidecimal numbers (e.g. "0x1f")
#'
#' @return boolean, TRUE if the file is compressed
#'
is_compressed <- function(filename, magic.number=as.raw(c("0x1f","0x8b"))) {
  fh<-file(filename, "rb")
  on.exit(close(fh))
  magic <- readBin(fh, "raw", length(magic.number))
  if(length(magic) != length(magic.number)) return(FALSE)
  if(all(magic == magic.number)) return(TRUE)
  return (FALSE)
}

#' is_rdata
#'
#' @description Checks a file is a rdata file by inspecting the file for so called magic bytes
#'
#' @param filename The filename of the file to test if it is an rdata file
#'
#' @return boolean, TRUE if the file is an rdata file
#' @export
#'
is_rdata <- function(filename) {
  #check for magic number
  #https://github.com/wch/r-source/blob/b99d403f4b7337553acb2d2108c7a00e6c19f908/src/main/saveload.c#L1786

  fh <- if(!is_compressed(filename))
    file(filename, "rb")
  else {
    gzfile(filename, "rb")
  }
  on.exit(close(fh))


  magic <- rawToChar(readBin(fh, "raw", 5))
  if(nchar(magic)<5) return(FALSE)
  if(magic %in% c("RDA1\n","RDB1\n","RDX1\n","RDA2\n","RDB2\n","RDX2\n")) return(TRUE)
  return (FALSE)
}
