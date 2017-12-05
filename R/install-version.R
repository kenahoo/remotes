
#' Install specific version of a package.
#'
#' This function knows how to look in multiple CRAN-like package repositories, and in their
#' \code{archive} directories, in order to find specific versions of the requested package.
#'
#' The repositories are searched in the order specified by the \code{repos} argument.  This enables
#' teams to maintain multiple in-house repositories with different policies - for instance, one repo
#' for development snapshots and one for official releases.  A common setup would be to first search
#' the official release repo, then the dev snapshot repo, then a public CRAN mirror.
#'
#' Older versions of packages on CRAN are usually only available in source form.  If your requested
#' package contains compiled code, you will need to have an R development environment installed. You
#' can check whether you do by running \code{\link[devtools]{has_devel}}.
#'
#' @export
#' @family package installation
#' @param package Name of the package to install.
#' @param version Version of the package to install.  Can either be a string giving the exact
#'   version required, or a specification in the same format as the parenthesized expressions used
#'   in package dependencies (see \code{\link{parse_deps}} and/or
#'   \url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies}).
#' @param ... Other arguments passed on to \code{\link[utils]{install.packages}}.
#' @inheritParams utils::install.packages
#' @author Jeremy Stephens and Ken Williams
#' @importFrom utils available.packages contrib.url install.packages
#' @examples
#' \dontrun{
#' install_version('devtools', '1.11.0')
#' install_version('devtools', '>= 1.12.0')
#'
#' ## Specify search order (e.g. in ~/.Rprofile)
#' options(repos=c(prod = 'http://mycompany.example.com/r-repo',
#'                 dev  = 'http://mycompany.example.com/r-repo-dev',
#'                 CRAN = 'https://cran.revolutionanalytics.com'))
#' install_version('mypackage', '1.15')        # finds in 'prod'
#' install_version('mypackage', '1.16-39487')  # finds in 'dev'
#' }

install_version <- function(package, version = NULL, repos = getOption("repos"), type = getOption("pkgType"), ...) {

  url <- download_version_url(package, version, repos, type)
  install_url(url, ...)
}

package_find_repo <- function(package, repos) {
  for (repo in repos) {
    if (length(repos) > 1)
      message("Trying ", repo)

    archive <-
      tryCatch({
        con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", repo), "rb"))
        on.exit(close(con))
        readRDS(con)
      },
      warning = function(e) list(),
      error = function(e) list())

    info <- archive[[package]]
    if (!is.null(info)) {
      info$repo <- repo
      return(info)
    }
  }

  stop(sprintf("couldn't find package '%s'", package))
}


#' Download a specified version of a CRAN package
#'
#' It downloads the package to a temporary file, and
#' returns the name of the file.
#'
#' @inheritParams install_version
#' @return Name of the downloaded file.
#'
#' @export

download_version <- function(package, version = NULL,
                             repos = getOption("repos"),
                             type = getOption("pkgType"), ...) {

  url <- download_version_url(package, version, repos, type)
  download(path = tempfile(), url = url)
}

download_version_url <- function(package, version, repos, type) {

  if (length(package) < 1) return()
  if (length(package) > 1)
    stop("download_version_url() must be called with a single 'package' argument - multiple packages given")

  ## returns TRUE if version 'to.check' satisfies all version criteria 'criteria'
  satisfies <- function(to.check, criteria) {
    to.check <- package_version(to.check)
    result <- apply(criteria, 1, function(r) {
      if(is.na(r['compare'])) TRUE
      else get(r['compare'], mode='function')(to.check, r['version'])
    })
    all(result)
  }

  ## returns TRUE if 'pkg' is already installed and satisfies all version criteria 'criteria'
  have <- function(pkg, criteria) {
    v <- suppressWarnings(packageDescription(pkg, fields = "Version"))
    !is.na(v) && satisfies(v, criteria)
  }

  install_version_deps <- function(deps) {
    ## TODO How to exclude 'base', 'stats', etc.?
    for (dep in unique(deps$package)) {
      lines <- subset(deps, package==dep)
      if (!have(dep, lines))
        install_version(dep, paste(lines$compare, lines$version), repos, type, ...)
    }
  }

  numeric_ver <- .standard_regexps()$valid_numeric_version
  package_ver <- .standard_regexps()$valid_package_version

  spec <- if(is.null(version) || is.na(version)) package else
    ifelse(grepl(paste0("^", numeric_ver, "$"), version),
           paste0(package, "(== ", version, ")"),
           paste0(package, "(", version, ")"))

  required <- parse_deps(paste(spec, collapse=", "))

  ## TODO should we do for(r in repos) { for (t in c('published','archive')) {...}}, or
  ## for (t in c('published','archive')) { for(r in repos) {...}} ? Right now it's the latter.  It
  ## only matters if required version is satisfied by both an early repo in archive/ and a late repo

  ## First search for currently-published package
  for (repo in repos) {
    contriburl <- contrib.url(repo, type)
    available <- available.packages(contriburl)

    if (package %in% row.names(available) && satisfies(available[package, 'Version'], required)) {
      row <- available[package, ]
      return(paste0(
        row[["Repository"]],
        "/",
        row[["Package"]],
        "_",
        row[["Version"]],
        ".tar.gz"
      ))
    }
  }

  ## Next search for archived package
  for (repo in repos) {
    info <- package_find_repo(package, repo)

    package.path <- NULL
    if (is.null(version)) {
      package.path <- info$path[NROW(info)] # Grab the latest one
    } else {
      for (i in seq_len(nrow(info))) {
        r <- info[i,]
        archive.version <- sub(paste0(".+/.+_(", package_ver, ")\\.tar\\.gz$"), "\\1", r$path)
        if (satisfies(archive.version, required)) {
          package.path <- r$path
          break
        }
      }
    }
    if (!is.null(package.path))
      return(url_remote(file.path(r$repo, "/src/contrib/Archive/", package.path)))
  }

  stop(sprintf("Couldn't find appropriate version of '%s' package", package))
}
