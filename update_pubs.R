library(rorcid)
library(rcrossref)
library(RefManageR)
library(dplyr)
library(stringr)
library(jsonlite)

# Configuration
orcid_id <- "0009-0004-8754-2108"
markdown_file <- "research/index.qmd"
typst_file <- "cv/resume.typ"

# Name configuration for underlining
MY_NAME <- "Yates S" # Adjust this to match how your name appears
MY_NAME_PATTERNS <- c("Yates S", "Yates, S", "S Yates", "S. Yates")

# Marker configuration - customize these for your files
MARKERS <- list(
  markdown = list(
    start = "<!-- PUBLICATIONS_START -->",
    end = "<!-- PUBLICATIONS_END -->"
  ),
  typst = list(
    start = "// PUBLICATIONS_START",
    end = "// PUBLICATIONS_END"
  )
)

# Function to extract DOIs from the ORCID structure
extract_dois_from_works <- function(works_data) {
  dois <- c()

  if (is.list(works_data) && length(works_data) > 0) {
    works_info <- works_data[[1]] # Get the first (and likely only) element

    if (!is.null(works_info$works)) {
      works_df <- works_info$works

      # Extract DOIs from external-ids.external-id column
      if ("external-ids.external-id" %in% names(works_df)) {
        for (i in 1:nrow(works_df)) {
          ext_ids <- works_df$"external-ids.external-id"[i]

          if (!is.na(ext_ids) && length(ext_ids) > 0 && nchar(ext_ids) > 0) {
            ext_ids_clean <- gsub('["\']', '', ext_ids) # Remove quotes

            # Extract all DOI patterns
            doi_matches <- regmatches(
              ext_ids_clean,
              gregexpr("10\\.[0-9]+/[^,\\s]+", ext_ids_clean)
            )[[1]]
            if (length(doi_matches) > 0) {
              doi_matches <- gsub("[,\\s]*$", "", doi_matches) # Clean up any trailing characters
              dois <- c(dois, doi_matches)
            }
          }
        }
      } else {
        cat("Warning: Column 'external-ids.external-id' not found in ORCID works data.\n")
      }

      # Also check URL column for DOI URLs
      if ("url.value" %in% names(works_df)) {
        for (url in works_df$url.value) {
          if (!is.na(url) && grepl("doi.org", url)) {
            doi_match <- regmatches(url, regexpr("10\\.[0-9]+/[^\\s/]+$", url))
            if (length(doi_match) > 0) {
              dois <- c(dois, doi_match)
            }
          }
        }
      }
    }
  }

  if (length(dois) > 0) {
    # Trim leading/trailing whitespace
    dois <- str_trim(dois)

    # Remove common unwanted trailing characters
    dois <- sub("[\\.,\\);:]+$", "", dois)
    dois <- sub("\\)$", "", dois)

    # Standardize to lowercase and get unique DOIs
    dois <- unique(tolower(dois))

    # Filter out any NAs, empty strings, ensure it starts with "10."
    dois <- dois[
      !is.na(dois) &
        dois != "" &
        grepl("^10\\.", dois) &
        nchar(dois) > 5
    ]
  }

  return(dois)
}

# Function to get full metadata from Crossref
get_crossref_metadata <- function(doi) {
  tryCatch(
    {
      # Use cr_works to get full metadata
      metadata <- cr_works(doi)
      if (!is.null(metadata) && !is.null(metadata$data)) {
        return(metadata$data)
      }
      return(NULL)
    },
    error = function(e) {
      cat("Error fetching Crossref metadata for", doi, ":", e$message, "\n")
      return(NULL)
    }
  )
}

# Function to extract authors from Crossref metadata
extract_authors_from_crossref <- function(metadata) {
  if (is.null(metadata) || !("author" %in% names(metadata))) {
    return("Unknown Author")
  }

  authors <- metadata$author[[1]] # author is usually a list with one data frame

  if (is.null(authors) || nrow(authors) == 0) return("Unknown Author")

  # Extract author names
  author_names <- c()
  for (i in 1:nrow(authors)) {
    given <- authors$given[i]
    family <- authors$family[i]

    if (!is.na(family)) {
      if (!is.na(given)) {
        # Extract first initial
        initial <- substr(given, 1, 1)
        author_name <- paste0(family, " ", initial)
      } else {
        author_name <- family
      }
      author_names <- c(author_names, author_name)
    }
  }

  if (length(author_names) > 0) {
    return(paste(author_names, collapse = ", "))
  } else {
    return("Unknown Author")
  }
}

# Function to create manual preprint entries with Crossref authors
create_manual_preprint_entry_with_authors <- function(doi, works_df) {
  # Find the work in ORCID data
  work_row <- NULL
  for (i in 1:nrow(works_df)) {
    if (
      "external-ids.external-id" %in%
        names(works_df) &&
        !is.na(works_df$"external-ids.external-id"[i]) &&
        grepl(doi, works_df$"external-ids.external-id"[i], fixed = TRUE)
    ) {
      work_row <- works_df[i, , drop = FALSE]
      break
    }
    if (
      is.null(work_row) &&
        "url.value" %in% names(works_df) &&
        !is.na(works_df$url.value[i]) &&
        grepl(doi, works_df$url.value[i], fixed = TRUE)
    ) {
      work_row <- works_df[i, , drop = FALSE]
      break
    }
  }

  if (is.null(work_row) || nrow(work_row) == 0) return(NULL)

  # Get basic info from ORCID
  title <- work_row$title.title.value
  year <- work_row$"publication-date.year.value"[1]
  url <- work_row$url.value[1]

  # Get authors from Crossref
  cat("Fetching authors from Crossref for DOI:", doi, "\n")
  crossref_metadata <- get_crossref_metadata(doi)
  authors_str <- extract_authors_from_crossref(crossref_metadata)

  # Determine publication venue
  venue <- "Preprint"
  if (!is.null(crossref_metadata) && "container-title" %in% names(crossref_metadata)) {
    container_titles <- crossref_metadata$`container-title`[[1]]
    if (length(container_titles) > 0 && !is.na(container_titles[1])) {
      venue <- container_titles[1]
    }
  }

  entry <- list(
    title = title,
    year = if (!is.na(year)) year else "n.d.",
    authors = authors_str,
    doi = doi,
    url = url,
    venue = venue
  )

  return(entry)
}

# Function to underline your name
underline_my_name <- function(text, format = "markdown") {
  if (is.null(text) || is.na(text) || nchar(text) == 0) {
    return(text)
  }

  result <- text
  for (pattern in MY_NAME_PATTERNS) {
    if (format == "markdown") {
      result <- gsub(pattern, paste0("[", pattern, "]{.underline}"), result, fixed = TRUE)
    } else if (format == "typst") {
      result <- gsub(pattern, paste0("#underline[", pattern, "]"), result, fixed = TRUE)
    }

    if (result != text) break # Found and replaced
  }

  return(result)
}

# Function to get bibliographic data with error handling
get_bib_data <- function(dois, works_df = NULL) {
  bib_list <- list()
  failed_dois_bibtex_path <- c()
  manual_entries <- list()

  cat("Fetching bibliographic data...\n")
  if (length(dois) == 0) {
    cat("No DOIs to fetch.\n")
    return(list(bib_entries = NULL, manual_entries = list()))
  }
  pb <- txtProgressBar(min = 0, max = length(dois), style = 3)

  for (i in seq_along(dois)) {
    doi <- dois[i]
    bib_entry_successfully_processed_via_bibtex <- FALSE

    tryCatch(
      {
        bibtex_str <- cr_cn(doi, "bibtex")

        if (!is.null(bibtex_str) && is.character(bibtex_str) && nchar(bibtex_str) > 0) {
          temp_file <- tempfile(fileext = ".bib")
          writeLines(bibtex_str, temp_file)

          current_bib_entry_list <- ReadBib(temp_file)
          unlink(temp_file)

          if (
            length(current_bib_entry_list) > 0 && inherits(current_bib_entry_list[[1]], "BibEntry")
          ) {
            bib_list[[length(bib_list) + 1]] <- current_bib_entry_list
            bib_entry_successfully_processed_via_bibtex <- TRUE
          } else {
            cat(paste(
              "\nReadBib resulted in an empty/invalid entry for DOI:",
              doi,
              ". BibTeX received (first 200 chars): \n",
              substr(bibtex_str, 1, 200),
              "...\n"
            ))
          }
        } else {
          cat(paste("\nCrossref returned empty or non-character BibTeX for DOI:", doi, "\n"))
        }
      },
      error = function(e) {
        cat(paste(
          "\nAn R error occurred while attempting to get/parse BibTeX for DOI:",
          doi,
          "-",
          e$message,
          "\n"
        ))
      }
    )

    # If BibTeX failed, create manual entry with Crossref authors
    if (!bib_entry_successfully_processed_via_bibtex) {
      failed_dois_bibtex_path <- c(failed_dois_bibtex_path, doi)

      # Check if it's a preprint and attempt manual creation
      is_preprint_doi <- (!is.null(works_df) &&
        (grepl("^10\\.1101/", doi) || # bioRxiv/medRxiv
          grepl("^10\\.20944/", doi) || # Preprints.org
          grepl("arxiv", doi, ignore.case = TRUE) ||
          grepl("osf\\.io", doi, ignore.case = TRUE) ||
          grepl("researchsquare", doi, ignore.case = TRUE) ||
          grepl("chemrxiv", doi, ignore.case = TRUE)))

      if (is_preprint_doi) {
        cat(paste(
          "--> BibTeX processing failed for DOI:",
          doi,
          ". Creating manual entry with Crossref authors.\n"
        ))
        manual_entry <- create_manual_preprint_entry_with_authors(doi, works_df)
        if (!is.null(manual_entry)) {
          manual_entries[[length(manual_entries) + 1]] <- manual_entry
          cat(paste(
            "----> Successfully created manual entry with authors:",
            substr(manual_entry$authors, 1, 50),
            "...\n"
          ))
        } else {
          cat(paste("----> Failed to create manual entry for DOI:", doi, "\n"))
        }
      } else {
        # For non-preprints, still try to create manual entry
        cat(paste(
          "--> BibTeX processing failed for DOI:",
          doi,
          ". Attempting manual entry creation.\n"
        ))
        manual_entry <- create_manual_preprint_entry_with_authors(doi, works_df)
        if (!is.null(manual_entry)) {
          manual_entries[[length(manual_entries) + 1]] <- manual_entry
          cat(paste(
            "----> Successfully created manual entry with authors:",
            substr(manual_entry$authors, 1, 50),
            "...\n"
          ))
        } else {
          cat(paste("----> Failed to create manual entry for DOI:", doi, "\n"))
        }
      }
    }

    setTxtProgressBar(pb, i)
    Sys.sleep(0.2) # Be nice to the API
  }

  close(pb)

  if (length(failed_dois_bibtex_path) > 0) {
    cat(paste(
      "\nEncountered issues (failed BibTeX fetch/parse) for",
      length(failed_dois_bibtex_path),
      "DOIs via Crossref. Manual creation attempted for these.\n"
    ))
  }

  return(list(
    bib_entries = if (length(bib_list) > 0) do.call("c", bib_list) else NULL,
    manual_entries = manual_entries
  ))
}

# Function to format references in APA style
format_apa_references <- function(bib_data_list) {
  formatted_refs <- c()

  bib_db <- bib_data_list$bib_entries
  manual_entries_list <- bib_data_list$manual_entries

  # Format standard bibliography entries from Crossref
  if (!is.null(bib_db) && inherits(bib_db, "BibEntry") && length(bib_db) > 0) {
    bib_db <- sort(bib_db, sorting = "ydnt") # Sort by year (desc), name, title

    for (i in seq_along(bib_db)) {
      entry <- bib_db[[i]]

      authors <- if (!is.null(entry$author)) {
        # Basic author formatting
        sapply(entry$author, function(p) {
          fam <- paste(p$family, collapse = " ")
          giv <- paste(substr(p$given, 1, 1), ".", sep = "", collapse = "")
          paste0(fam, ", ", giv) # Produces "Lastname, F."
        }) |>
          paste(collapse = ", ")
      } else "Unknown Author"

      year <- if (!is.null(entry$year)) entry$year else "n.d."
      title <- if (!is.null(entry$title)) entry$title else "Untitled"
      # Remove potential double curlies from BibTeX titles
      title <- gsub("\\{\\{([^}]+)\\}\\}", "\\1", title)
      title <- gsub("\\{([^}]+)\\}", "\\1", title)

      journal <- if (!is.null(entry$journal)) entry$journal else entry$booktitle

      ref <- paste0(authors, " (", year, "). ", title, ".")

      if (!is.null(journal)) {
        ref <- paste0(ref, " *", journal, "*")
        if (!is.null(entry$volume)) {
          ref <- paste0(ref, ", ", entry$volume)
          if (!is.null(entry$number)) {
            ref <- paste0(ref, "(", entry$number, ")")
          }
        }
        if (!is.null(entry$pages)) {
          ref <- paste0(ref, ", ", entry$pages)
        }
      }

      if (!is.null(entry$doi)) {
        ref <- paste0(ref, ". https://doi.org/", entry$doi)
      }
      formatted_refs <- c(formatted_refs, ref)
    }
  }

  # Format manual entries with full author lists
  if (!is.null(manual_entries_list) && length(manual_entries_list) > 0) {
    for (entry in manual_entries_list) {
      authors_display <- if (!is.null(entry$authors)) entry$authors else "Unknown Author"
      ref <- paste0(authors_display, " (", entry$year, "). ", entry$title, ". *", entry$venue, "*.")

      if (!is.null(entry$doi) && nzchar(entry$doi)) {
        ref <- paste0(ref, " https://doi.org/", entry$doi)
      } else if (!is.null(entry$url) && nzchar(entry$url)) {
        ref <- paste0(ref, " ", entry$url)
      }
      formatted_refs <- c(formatted_refs, ref)
    }
  }

  return(formatted_refs)
}

# Function to insert content between markers
insert_between_markers <- function(
  file_path,
  new_content,
  start_marker,
  end_marker,
  backup = FALSE
) {
  if (!file.exists(file_path)) {
    cat("ERROR: File does not exist:", file_path, "\n")
    return(FALSE)
  }

  lines <- readLines(file_path, warn = FALSE)

  if (backup) {
    backup_path <- paste0(file_path, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    writeLines(lines, backup_path)
    cat("Backup created:", backup_path, "\n")
  }

  start_pos <- which(grepl(start_marker, lines, fixed = TRUE))
  end_pos <- which(grepl(end_marker, lines, fixed = TRUE))

  if (length(start_pos) == 0 || length(end_pos) == 0) {
    cat("ERROR: Could not find both start and end markers in", file_path, "\n")
    cat("Looking for:", start_marker, "and", end_marker, "\n")
    return(FALSE)
  }

  if (length(start_pos) > 1 || length(end_pos) > 1) {
    cat("WARNING: Multiple markers found. Using the first occurrence.\n")
  }
  start_pos <- start_pos[1]
  end_pos <- end_pos[1]

  if (start_pos >= end_pos) {
    cat("ERROR: Start marker must come before end marker\n")
    return(FALSE)
  }

  new_lines <- c(
    lines[1:start_pos],
    new_content,
    lines[end_pos:length(lines)]
  )

  writeLines(new_lines, file_path)
  cat("Successfully updated:", file_path, "\n")
  return(TRUE)
}

# Function to generate markdown bibliography content
generate_markdown_content <- function(formatted_refs) {
  content <- c("")
  if (length(formatted_refs) > 0) {
    for (i in seq_along(formatted_refs)) {
      ref_with_underline <- underline_my_name(formatted_refs[i], "markdown")

      # transform bare DOIs to full links
      ref_with_underline <- gsub(
        "(?<!https://doi\\.org/)(10\\.\\d{4,9}/[-._;()/:A-Z0-9]+)",
        "https://doi.org/\\1",
        ref_with_underline,
        ignore.case = TRUE,
        perl = TRUE
      )

      # wrap full DOI links in markdown
      ref_with_underline <- gsub(
        "(https://doi\\.org/10\\.\\d{4,9}/[-._;()/:A-Z0-9]+)",
        "[\\1](\\1)",
        ref_with_underline,
        ignore.case = TRUE
      )

      content <- c(content, paste0(i, ". ", ref_with_underline), "")
    }
  } else {
    content <- c(content, "No publications found or processed.")
  }
  return(content)
}

# Function to generate typst bibliography content
generate_typst_content <- function(formatted_refs) {
  content <- c("")
  if (length(formatted_refs) > 0) {
    for (i in seq_along(formatted_refs)) {
      ref_with_underline <- underline_my_name(formatted_refs[i], "typst")
      content <- c(content, paste0("+ ", ref_with_underline))
    }
  } else {
    content <- c(content, "No publications found or processed.")
  }
  return(content)
}

# Main execution
cat("Starting ORCID bibliography generation...\n")
cat("ORCID ID:", orcid_id, "\n")

markdown_exists <- file.exists(markdown_file)
typst_exists <- file.exists(typst_file)

if (!markdown_exists && !typst_exists) {
  stop(
    "ERROR: Neither ",
    markdown_file,
    " nor ",
    typst_file,
    " exist. Please create these files first, including the necessary start/end markers."
  )
}
if (!markdown_exists) cat("WARNING: ", markdown_file, " does not exist and will be skipped.\n")
if (!typst_exists) cat("WARNING: ", typst_file, " does not exist and will be skipped.\n")

cat("\nFetching works from ORCID...\n")
works_data <- tryCatch(
  {
    orcid_works(orcid_id)
  },
  error = function(e) {
    cat("Error fetching works from ORCID:", e$message, "\n")
    return(NULL)
  }
)

if (
  is.null(works_data) ||
    length(works_data) == 0 ||
    is.null(works_data[[1]]$works) ||
    nrow(works_data[[1]]$works) == 0
) {
  stop("No works found for ORCID ID: ", orcid_id, " or data structure is unexpected.")
}
cat("Works data structure received from ORCID.\n")

cat("\nExtracting DOIs...\n")
dois <- extract_dois_from_works(works_data)
cat(
  "Found",
  length(dois),
  "unique DOIs after initial extraction:",
  if (length(dois) > 0) paste(dois, collapse = ", ") else "None",
  "\n"
)

if (length(dois) == 0) {
  cat("No DOIs found in ORCID works. No publications to process.\n")
  # Generate empty content if files exist, to clear previous entries
  if (markdown_exists) {
    insert_between_markers(
      markdown_file,
      generate_markdown_content(c()),
      MARKERS$markdown$start,
      MARKERS$markdown$end
    )
  }
  if (typst_exists) {
    insert_between_markers(
      typst_file,
      generate_typst_content(c()),
      MARKERS$typst$start,
      MARKERS$typst$end
    )
  }
  cat("Done (no DOIs).\n")
} else {
  bib_data_list <- get_bib_data(dois, works_data[[1]]$works) # works_df for manual entries

  num_bib_entries <- if (!is.null(bib_data_list$bib_entries)) length(bib_data_list$bib_entries) else
    0
  num_manual_entries <- if (!is.null(bib_data_list$manual_entries))
    length(bib_data_list$manual_entries) else 0
  total_processed_entries <- num_bib_entries + num_manual_entries

  cat(paste(
    "\nRetrieved",
    num_bib_entries,
    "entries via Crossref BibTeX and created",
    num_manual_entries,
    "manual entries with Crossref authors.\n"
  ))

  if (total_processed_entries == 0) {
    stop("No bibliographic data could be retrieved or processed successfully.")
  }

  cat("\nFormatting references in APA style...\n")
  formatted_refs <- format_apa_references(bib_data_list)

  if (length(formatted_refs) > 0) {
    cat(length(formatted_refs), "references formatted.\n")
  } else {
    cat("No references were successfully formatted.\n")
  }

  markdown_content <- generate_markdown_content(formatted_refs)
  typst_content <- generate_typst_content(formatted_refs)

  cat("\nUpdating existing files...\n")
  if (markdown_exists) {
    insert_between_markers(
      markdown_file,
      markdown_content,
      MARKERS$markdown$start,
      MARKERS$markdown$end
    )
  }
  if (typst_exists) {
    insert_between_markers(typst_file, typst_content, MARKERS$typst$start, MARKERS$typst$end)
  }

  cat("\n=== Summary ===\n")
  cat("ORCID ID:", orcid_id, "\n")
  cat("DOIs initially extracted:", length(dois), "\n")
  cat("Crossref BibTeX entries processed:", num_bib_entries, "\n")
  cat("Manual entries with Crossref authors created:", num_manual_entries, "\n")
  cat("Total references formatted:", length(formatted_refs), "\n")
  cat("\nFiles processed:\n")
  if (markdown_exists) cat("✓ ", markdown_file, "\n")
  if (typst_exists) cat("✓ ", typst_file, "\n")
  cat("\nDone!\n")
}
