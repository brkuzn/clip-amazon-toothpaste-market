# ============================================================================
# build_thesis_pdf.R
# Renders the thesis directly from the R Markdown source into the University
# of Oldenburg MA/BA template (thesis/template.tex) — title page, Erklärung,
# bilingual Zusammenfassung, TOC/LoF/LoT, chapter-level sections.
#
# Usage (from the repo root):
#   Rscript scripts/build_thesis_pdf.R                # blp_thesis.Rmd
#   Rscript scripts/build_thesis_pdf.R blp_thesis_h.Rmd
#
# Output: thesis/<input-stem>_oldenburg.pdf
# Runs pdflatex twice (via a second render of the kept .tex) so that the
# table of contents and the figure/table lists are populated.
# ============================================================================
args  <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1) args[1] else "blp_thesis.Rmd"
stopifnot(file.exists(input))
stem  <- sub("\\.Rmd$", "", basename(input))

if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !nzchar(Sys.which("pandoc"))) {
  p <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
  if (dir.exists(p)) Sys.setenv(RSTUDIO_PANDOC = p)
}

fmt <- bookdown::pdf_document2(
  latex_engine    = "pdflatex",
  template        = "thesis/template.tex",
  number_sections = TRUE,
  toc             = FALSE,          # the template sets its own TOC/LoF/LoT
  fig_caption     = TRUE,
  keep_tex        = TRUE,
  pandoc_args     = c("--top-level-division=chapter")
)

out <- rmarkdown::render(input, output_format = fmt,
                         output_file = paste0(stem, "_oldenburg.pdf"),
                         clean = FALSE)

# second pdflatex pass for TOC / lists
texfile <- paste0(stem, "_oldenburg.tex")
if (file.exists(texfile)) {
  system2("pdflatex", c("-interaction=nonstopmode", texfile),
          stdout = FALSE, stderr = FALSE)
  system2("pdflatex", c("-interaction=nonstopmode", texfile),
          stdout = FALSE, stderr = FALSE)
}

final <- file.path("thesis", paste0(stem, "_oldenburg.pdf"))
file.copy(paste0(stem, "_oldenburg.pdf"), final, overwrite = TRUE)
# tidy intermediates at repo root
unlink(c(paste0(stem, "_oldenburg.", c("pdf", "tex", "aux", "log", "toc", "lof", "lot", "out")),
         paste0(stem, ".knit.md"), paste0(stem, ".log")))
cat("Thesis PDF written to:", final, "\n")
