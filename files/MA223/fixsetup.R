# fixsetup
# fix a mistake in the setup files.

pkgs <- utils::installed.packages()
dir <- pkgs[which(pkgs[, "Package"]=="IntroAnalysis"), "LibPath"]

list.files(paste0(dir, "/IntroAnalysis/exec/"))

download.file("https://raw.githubusercontent.com/reyesem/reyesem.github.io/master/files/MA223/ma223setup.R",
              paste0(dir, "/IntroAnalysis/exec/ma223setup.R"),
              quiet = TRUE)

message("That's it!  Things should work now.")