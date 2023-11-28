getOrUpdatePkg <- function(p, minVer, repo) {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    if (missing(repo)) repo = c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("Require", "0.3.1.9015")
getOrUpdatePkg("SpaDES.project", "0.0.8.9023")

terra::terraOptions(progress = 3, memfrac = 0.8, 
                    tempdir = "~/scratch/terra", 
                    memmax = 600)
################### RUNAME

if (SpaDES.project::user("tmichele")) setwd("~/projects/attribution/")

################ SPADES CALL
out <- SpaDES.project::setupProject(
  runName = "attribution",
  paths = list(projectPath = runName,
               scratchPath = "~/scratch"),
  modules =
    file.path("tati-micheletti",
              paste0(
                c("attributionCovars"),
                "@main")
    ),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                 SpaDES.project.fast = TRUE,
                 spades.recoveryMode = 2,
                 future.globals.maxSize = if (pemisc::user("tmichele")) 6000*1024^2 else 6000*1024^2,
                 reproducible.qsPreset = "fast",
                 reproducible.inputPaths = if (any(pemisc::user("emcintir"), 
                                                   pemisc::user("tmichele"))) "~/data" else SpaDES.core::Paths$inputPath,
                 reproducible.overwrite = TRUE,
                 reproducible.cachePath = "~/cache",
                 reproducible.tempPath = "~/scratch/tmp",
                 reproducible.showSimilar = TRUE,
                 spades.moduleCodeChecks = FALSE, # Turn off all module's code checking
                 pemisc.useParallel = TRUE),
  times = list(start = 1985,
               end = 2015),
  params = list(.globals = list(),
                attributionCovars = list(
                  "cleanScratch" = paths$scratchPath)
  ),
  packages = c("PredictiveEcology/reproducible@development (>= 2.0.10)",
               "PredictiveEcology/pemisc@development"),
  useGit = "sub"
)

snippsim <- do.call(SpaDES.core::simInitAndSpades, out)
