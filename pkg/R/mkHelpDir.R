fileSymLinkRecursive <-function(files,to){
  isDir <- file.info(files)$isdir
  exist <- is.finite(isDir)
  files <- files[exist]
  isDir <- isDir[exist]
  dirs <- files[isDir]
  files <- files[!isDir]
  if(length(files)) file.symlink(files,to)
  if(length(dirs))
    for(d in dirs){
      bn <- basename(d)
      nto <- file.path(to,bn)
      unlink(nto)
      suppressWarnings(dir.create(nto))
      nfiles <- file.path(d,dir(d))
      Recall(nfiles,nto)
    }
}

fileCopyRecursive <-function(files,to){
  isDir <- file.info(files)$isdir
  exist <- is.finite(isDir)
  files <- files[exist]
  isDir <- isDir[exist]
  dirs <- files[isDir]
  files <- files[!isDir]
  if(length(files)) file.copy(files,to,overwrite=TRUE)
  if(length(dirs))
    for(d in dirs){
      bn <- basename(d)
      nto <- file.path(to,bn)
      unlink(nto)
      suppressWarnings(dir.create(nto))
      nfiles <- file.path(d,dir(d))
      Recall(nfiles,nto)
    }
}


make.packages.html <- function (lib.loc = .libPaths())
{
    f.tg <- file.path(tempdir(), ".R/doc/html/packages.html")
    if (!file.create(f.tg)) {
        warning("cannot create HTML package index")
        return(FALSE)
    }
    searchindex <- file.path(tempdir(), ".R/doc/html/search/index.txt")
    if (!file.create(searchindex)) {
        warning("cannot create HTML search index")
        return(FALSE)
    }
    useUTF8 <- capabilities("iconv")
    file.append(f.tg, file.path(R.home("doc"), "html", if (useUTF8)
        "packages-head-utf8.html"
    else "packages-head.html"))
    out <- file(f.tg, open = "a")
    search <- file(searchindex, open = "w")
    known <- character(0)
    for (lib in lib.loc) {
        cat("<p><h3>Packages in ", lib, "</h3>\n<p><table width=\"100%\" summary=\"R Package list\">\n",
            sep = "", file = out)
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        for (i in pg) {
            before <- sum(i %in% known)
            link <- if (before == 0)
                i
            else paste(i, before, sep = ".")
            from <- file.path(lib, i)
            to <- file.path(tempdir(), ".R", "library", link)
            file.symlink(from, to)
            title <- packageDescription(i, lib.loc = lib, field = "Title",
                encoding = ifelse(useUTF8, "UTF-8", ""))
            if (is.na(title))
                title <- "-- Title is missing --"
            cat("<tr align=\"left\" valign=\"top\">\n", "<td width=\"25%\"><a href=\"../../library/",
                link, "/html/00Index.html\">", i, "</a></td><td>",
                title, "</td></tr>\n", file = out, sep = "")
            contentsfile <- file.path(from, "CONTENTS")
            if (!file.exists(contentsfile))
                next
            contents <- readLines(contentsfile)
            isURL <- grep("URL:", contents, fixed = TRUE, useBytes = TRUE)
            if (length(isURL) && link != i)
                contents[isURL] <- gsub(paste("/library/", i,
                  sep = ""), paste("/library/", link, sep = ""),
                  contents[isURL], fixed = TRUE, useBytes = TRUE)
            if(max(nchar(contents)) < 100)
                writeLines(c(contents, ""), search)
            else {
               #cat("\nmax(nchar)=",max(nchar(contents))," ")
               for(jj in 1:length(contents)){
                if(nchar(contents[jj]) < 100)
                  writeLines(contents[jj], search)
                else
                  writeLines(strwrap(contents[jj],width=100,exdent=3), search)
                #cat(".")
               }
               writeLines("",search)
            }
        }
        cat("</table>\n\n", file = out)
        known <- c(known, pg)
    }
    cat("</body></html>\n", file = out)
    close(out)
    close(search)
    invisible(TRUE)
}

mkHelpDir <- function (path="~/R")
{
    #cat(gettext("Making links in per-session dir ...\n"))
    .Script("sh", "help-links.sh", paste(tempdir(), paste(.libPaths(),
        collapse = " ")))
    cat("Making package html pages ...\n")
    make.packages.html()
    tmpdir <- file.path(tempdir(), ".R")
    tmpdocdir <- file.path(tmpdir, "doc")
    targetdocdir <- file.path(path, "doc")
    unlink(targetdocdir,recursive=TRUE)
    suppressWarnings(dir.create(targetdocdir,recursive=TRUE))
    #system(paste("cp -av",tmpdocdir,path))
    fileCopyRecursive(tmpdocdir,path)
    for(libpath in .libPaths()){
      libs <- dir(libpath)
      for(lib in libs)
        if(file.info(file.path(libpath,lib))$isdir){
          ll <- file.path(libpath,lib)
          from <- file.path(ll,"html")
          to <- file.path(targetdocdir,lib)
          unlink(to,recursive=TRUE)
          suppressWarnings(dir.create(to,recursive=TRUE))
          #to <- paste(to,"html",sep="/")
          file.symlink(from,to)
          if(file.exists(file.path(ll,"doc"))){
              file.symlink(file.path(ll,"doc"),to)
          }
        }
        else 
          file.symlink(
            file.path(libpath,lib),
            file.path(targetdocdir,lib)
            )
        
    }
    
    tmpsearchdir <- file.path(tmpdocdir,"html","search")
    targetsearchdir <- file.path(targetdocdir,"html","search")
    fileCopyRecursive(tmpdocdir,path)

    package.index.file <- file.path(tmpdocdir,"html","packages.html")
    #print(package.index.file)
    package.index.html <- readLines(package.index.file)
    package.index.html <- gsub("../../library/","../../doc/",package.index.html,fixed=TRUE)
    #print(package.index.html)
    package.index.file <- file.path(targetdocdir,"html","packages.html")
    file.remove(package.index.file)
    #print(package.index.file)
    writeLines(package.index.html,package.index.file)
    #cat(readLines(package.index.file))
    
    index.file <- file.path(tmpdocdir,"html","index.html")
    #print(index.file)
    index.html <- readLines(index.file)
    index.file <- file.path(targetdocdir,"html","index.html")
    file.remove(index.file)
    writeLines(index.html,index.file)
    #cat(readLines(index.file))
    index.txt.file <- file.path(tmpsearchdir,"index.txt")
    index.txt <- readLines(index.txt.file)
    index.txt <- gsub("../../../library","../../../doc",index.txt,fixed=TRUE)
    index.txt.file <- file.path(targetsearchdir,"index.txt")
    writeLines(index.txt,index.txt.file)
}
