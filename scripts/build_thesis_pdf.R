# ============================================================================
# build_thesis_pdf.R
# Builds both deliverables from the single source, blp_thesis_h.Rmd:
#
#   Demand_Estimation_with_Unstructured_Product_Data_UzunBurak_2026.pdf
#       submission version, University of Oldenburg template
#       (thesis/template.tex) — title page, Erklaerung, bilingual
#       Zusammenfassung, TOC/LoF/LoT, chapter-level sections.
#
#   Demand_Estimation_with_Unstructured_Product_Data_UzunBurak_2026_article.pdf
#       article version, the layout defined in the Rmd's own YAML header.
#
# Usage (from the repo root):
#   Rscript scripts/build_thesis_pdf.R                 # both, from blp_thesis_h.Rmd
#   Rscript scripts/build_thesis_pdf.R --thesis        # submission version only
#   Rscript scripts/build_thesis_pdf.R --article       # article version only
#
# Knitting blp_thesis_h.Rmd in RStudio runs this same script through the
# `knit:` hook in the YAML header, so the Knit button produces both PDFs
# under exactly these names.
#
# The submission version runs pdflatex twice more over the kept .tex so that
# the table of contents and the figure/table lists are populated.
# ============================================================================

THESIS_STEM  <- "Demand_Estimation_with_Unstructured_Product_Data_UzunBurak_2026"
ARTICLE_STEM <- paste0(THESIS_STEM, "_article")

build_thesis <- function(input = "blp_thesis_h.Rmd",
                         what  = c("both", "thesis", "article")) {
  what  <- match.arg(what)
  input <- normalizePath(input, mustWork = TRUE)
  repo  <- dirname(input)

  if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !nzchar(Sys.which("pandoc"))) {
    p <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
    if (dir.exists(p)) Sys.setenv(RSTUDIO_PANDOC = p)
  }

  owd <- setwd(repo); on.exit(setwd(owd), add = TRUE)
  made <- character(0)

  # ── Submission version (Oldenburg template) ────────────────────────────────
  if (what %in% c("both", "thesis")) {
    fmt <- bookdown::pdf_document2(
      latex_engine    = "pdflatex",
      template        = "thesis/template.tex",
      number_sections = TRUE,
      toc             = FALSE,        # the template sets its own TOC/LoF/LoT
      fig_caption     = TRUE,
      keep_tex        = TRUE,
      pandoc_args     = c("--top-level-division=chapter")
    )
    rmarkdown::render(input, output_format = fmt,
                      output_file = paste0(THESIS_STEM, ".pdf"),
                      clean = FALSE)

    texfile <- paste0(THESIS_STEM, ".tex")
    if (file.exists(texfile)) {
      for (i in 1:2)
        system2("pdflatex", c("-interaction=nonstopmode", texfile),
                stdout = FALSE, stderr = FALSE)
    }
    unlink(paste0(THESIS_STEM, ".", c("tex", "aux", "log", "toc", "lof", "lot", "out")))
    made <- c(made, paste0(THESIS_STEM, ".pdf"))
  }

  # ── Article version (the Rmd's own YAML layout) ────────────────────────────
  if (what %in% c("both", "article")) {
    rmarkdown::render(input, output_format = "bookdown::pdf_document2",
                      output_file = paste0(ARTICLE_STEM, ".pdf"))
    made <- c(made, paste0(ARTICLE_STEM, ".pdf"))
  }

  stem <- sub("\\.Rmd$", "", basename(input))
  unlink(c(paste0(stem, ".knit.md"), paste0(stem, ".log")))

  for (f in made)
    cat("Written:", f, sprintf("(%.0f KB)\n", file.size(file.path(repo, f)) / 1024))
  invisible(file.path(repo, made))
}

# Auto-run only when this file is the Rscript entry point. Sourcing it from
# elsewhere (the Rmd's `knit:` hook, or an interactive session) just defines
# build_thesis() without building anything.
local({
  a    <- commandArgs(trailingOnly = FALSE)
  self <- sub("^--file=", "", a[grep("^--file=", a)])
  if (!length(self) || basename(self[1]) != "build_thesis_pdf.R") return(invisible())
  args  <- commandArgs(trailingOnly = TRUE)
  what  <- if ("--thesis" %in% args) "thesis" else
           if ("--article" %in% args) "article" else "both"
  files <- setdiff(args, c("--thesis", "--article"))
  build_thesis(if (length(files)) files[1] else "blp_thesis_h.Rmd", what)
})
