library(tidyverse)

# Create a synthetic version of a data frame. Every column keeps the shape of
# the original (its values, ranges, proportions, and share of missing values),
# but no row is a real record and no real name, phone number, or note is reused.
#
# Columns are synthesized independently, so relationships between columns are
# deliberately broken. That is what keeps the result safe to share.
#
# Column types are guessed from names and values, and the guesses are printed.
# Override any of them with `types`, e.g. types = c(zip = "category").
# Valid types: id, first_name, last_name, full_name, email, phone, date,
# numeric, category, text, drop.
synthesize_data <- function(
  data,
  n = nrow(data),
  types = NULL,
  seed = NULL,
  text_templates = default_text_templates
) {
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }

  check_types(types, data)
  column_types <- imap_chr(data, detect_column_type)
  column_types[names(types)] <- types

  cli::cli_inform(c(
    "Column types used (override with {.arg types}):",
    set_names(
      str_c(names(column_types), ": ", column_types),
      rep("*", length(column_types))
    )
  ))

  kept_types <- column_types[column_types != "drop"]

  data[names(kept_types)] |>
    imap(\(x, name) synthesize_column(x, kept_types[[name]], n, text_templates)) |>
    as_tibble()
}

# Detecting column types -------------------------------------------------------

valid_types <- c(
  "id", "first_name", "last_name", "full_name", "email", "phone",
  "date", "numeric", "category", "text", "drop"
)

check_types <- function(types, data) {
  unknown_columns <- setdiff(names(types), names(data))
  unknown_types <- setdiff(types, valid_types)

  if (length(unknown_columns) > 0) {
    cli::cli_abort("{.arg types} names columns not in the data: {.val {unknown_columns}}.")
  }
  if (length(unknown_types) > 0) {
    cli::cli_abort(c(
      "{.arg types} contains unknown types: {.val {unknown_types}}.",
      "i" = "Valid types are {.val {valid_types}}."
    ))
  }
}

detect_column_type <- function(x, name) {
  name <- str_to_lower(name)
  values <- x[!is.na(x)]

  if (str_detect(name, "(^|_)id$")) return("id")
  if (str_detect(name, "^(first|given|f)_?name$|forename")) return("first_name")
  if (str_detect(name, "^(last|sur|family|l)_?name$")) return("last_name")
  if (str_detect(name, "^(full_?)?name$")) return("full_name")
  if (str_detect(name, "e_?mail") || looks_like_email(values)) return("email")
  if (str_detect(name, "phone|mobile|cell") || looks_like_phone(values)) return("phone")
  if (str_detect(name, "zip|postal|postcode")) return("category")
  if (inherits(x, c("Date", "POSIXt"))) return("date")
  if (is.logical(x) || is.factor(x)) return("category")
  if (n_distinct(values) <= 20) return("category")
  if (is.numeric(x)) return("numeric")

  # Many distinct strings: treat as free text so no real values are reused
  "text"
}

looks_like_email <- function(values) {
  is.character(values) &&
    length(values) > 0 &&
    mean(str_detect(values, "^[^@\\s]+@[^@\\s]+\\.[A-Za-z]+$")) > 0.9
}

looks_like_phone <- function(values) {
  is.character(values) &&
    length(values) > 0 &&
    mean(
      str_detect(values, "^[+\\d\\s().-]+$") &
        between(str_count(values, "\\d"), 7, 15)
    ) > 0.9
}

# Synthesizing columns ---------------------------------------------------------

synthesize_column <- function(x, type, n, text_templates) {
  values <- x[!is.na(x)]

  if (length(values) == 0) {
    return(x[rep(NA_integer_, n)])
  }

  fake <- switch(
    type,
    id = fake_id(values, n),
    first_name = fake_name(values, n, first_name_pool),
    last_name = fake_name(values, n, last_name_pool),
    full_name = fake_full_name(values, n),
    email = fake_email(n),
    phone = fake_phone(values, n),
    date = fake_date(values, n),
    numeric = fake_numeric(values, n),
    category = resample(values, n),
    text = fake_text(n, text_templates)
  )

  add_missing(fake, share = mean(is.na(x)))
}

# Sample with replacement. Safer than sample(), which treats a single number
# as a range (sample(5) returns 1:5 in random order).
resample <- function(x, n) {
  x[sample.int(length(x), n, replace = TRUE)]
}

add_missing <- function(fake, share) {
  fake[runif(length(fake)) < share] <- NA
  fake
}

# New IDs in the same format (e.g. C0001), numbered from 1. Repeated IDs stay
# repeated at the same rate, so a column of household IDs keeps its grouping.
fake_id <- function(values, n) {
  n_ids <- max(1, round(n * n_distinct(values) / length(values)))
  ids <- seq_len(n_ids)

  if (is.numeric(values)) {
    ids <- vctrs::vec_cast(ids, values[0])
  } else {
    prefix <- unique(str_remove(values, "\\d+$"))
    digits <- str_extract(values, "\\d+$")

    if (length(prefix) == 1 && !anyNA(digits)) {
      ids <- str_c(prefix, str_pad(ids, max(str_length(digits)), pad = "0"))
    } else {
      ids <- str_c("ID", str_pad(ids, str_length(n_ids), pad = "0"))
    }
  }

  if (n_ids == n) ids else resample(ids, n)
}

# Names come from built-in pools, minus any name that appears in the real data
fake_name <- function(values, n, pool) {
  candidates <- setdiff(pool, values)
  if (length(candidates) < 10) {
    candidates <- pool
  }
  resample(candidates, n)
}

fake_full_name <- function(values, n) {
  real_parts <- unlist(str_split(values, "\\s+"))
  str_c(
    fake_name(real_parts, n, first_name_pool),
    " ",
    fake_name(real_parts, n, last_name_pool)
  )
}

# example.com is reserved for documentation, so these addresses can't reach anyone
fake_email <- function(n) {
  str_c(
    str_to_lower(resample(first_name_pool, n)),
    ".",
    str_to_lower(resample(last_name_pool, n)),
    "@example.com"
  )
}

# Same format as the most common real format. Area codes are kept (they work
# like a zip code), and the rest is 555-01XX, which is reserved for fiction.
fake_phone <- function(values, n) {
  shapes <- str_replace_all(values, "\\d", "#")
  shape <- names(which.max(table(shapes)))
  n_digits <- str_count(shape, "#")
  line_numbers <- str_c("55501", str_pad(sample(0:99, n, replace = TRUE), 2, pad = "0"))

  if (n_digits > 7) {
    area_codes <- values[shapes == shape] |>
      str_remove_all("\\D") |>
      str_sub(1, n_digits - 7)
    digits <- str_c(resample(area_codes, n), line_numbers)
  } else {
    digits <- map_chr(seq_len(n), \(i) str_flatten(sample(0:9, n_digits, replace = TRUE)))
  }

  fill_shape(shape, digits)
}

# Put digits into a format like "(###) ###-####"
fill_shape <- function(shape, digits) {
  characters <- str_split_1(shape, "")
  slots <- which(characters == "#")

  map_chr(digits, \(number) {
    characters[slots] <- str_split_1(number, "")
    str_flatten(characters)
  })
}

fake_date <- function(values, n) {
  draws <- fake_numeric(as.numeric(values), n)

  if (inherits(values, "Date")) {
    as.Date(draws)
  } else {
    as.POSIXct(draws, tz = attr(values, "tzone") %||% "")
  }
}

# Smoothed bootstrap: draw real values and add noise, so the distribution's
# shape survives but the values themselves are new. Draws outside the real
# range are redrawn, and results are rounded like the original
# (e.g. incomes recorded to the nearest 10 stay that way). Zeros get no noise,
# because zero usually means something (no income, no visits).
fake_numeric <- function(values, n) {
  if (n_distinct(values) == 1) {
    return(rep(values[1], n))
  }

  lowest <- min(values)
  highest <- max(values)
  bandwidth <- bw.nrd0(values)
  draw <- \(k) {
    picked <- resample(values, k)
    picked + rnorm(k, sd = bandwidth) * (picked != 0)
  }

  draws <- draw(n)
  out_of_range <- draws < lowest | draws > highest
  while (any(out_of_range)) {
    draws[out_of_range] <- draw(sum(out_of_range))
    out_of_range <- draws < lowest | draws > highest
  }

  unit <- rounding_unit(values)
  if (!is.na(unit)) {
    draws <- round(draws / unit) * unit
  }

  if (is.integer(values)) as.integer(draws) else draws
}

# The largest of 1000, 100, ..., 0.001 that every value is a multiple of
rounding_unit <- function(values) {
  units <- 10^(3:-3)
  is_multiple <- map_lgl(units, \(unit) all(abs(values / unit - round(values / unit)) < 1e-6))

  if (any(is_multiple)) units[which(is_multiple)[1]] else NA
}

fake_text <- function(n, templates) {
  fake_phones <- str_c("(555) 555-01", str_pad(sample(0:99, n, replace = TRUE), 2, pad = "0"))
  fake_numbers <- as.character(sample(2:30, n, replace = TRUE))

  resample(templates, n) |>
    str_replace_all(fixed("{phone}"), fake_phones) |>
    str_replace_all(fixed("{number}"), fake_numbers)
}

# Built-in values ---------------------------------------------------------------

# {phone} and {number} are filled with fake values
default_text_templates <- c(
  "Called to confirm appointment.",
  "Left a voicemail.",
  "Follow up next week.",
  "Follow up in {number} days.",
  "Updated contact information.",
  "Call back at {phone}.",
  "Requested a callback at {phone}.",
  "Referred to partner program.",
  "No changes since last visit.",
  "Asked for more information.",
  "Sent reminder by text.",
  "Paperwork received.",
  "Waiting on documents.",
  "Visited {number} times this year."
)

first_name_pool <- c(
  "Aaliyah", "Adrian", "Aisha", "Alejandro", "Amara", "Andre", "Anika",
  "Arjun", "Beatriz", "Benjamin", "Camila", "Carlos", "Chloe", "Darius",
  "Deepa", "Diego", "Elena", "Elijah", "Esther", "Fatima", "Felix", "Gabriel",
  "Hana", "Hector", "Imani", "Isaac", "Jamal", "Jasmine", "Javier", "Keiko",
  "Kwame", "Layla", "Leon", "Luis", "Malik", "Marisol", "Mateo", "Maya",
  "Mei", "Miguel", "Nadia", "Naomi", "Omar", "Owen", "Priya", "Rafael",
  "Rosa", "Ruth", "Samir", "Sofia", "Talia", "Tariq", "Valeria", "Victor",
  "Wei", "Xavier", "Yara", "Yusuf", "Zainab", "Zoe"
)

last_name_pool <- c(
  "Abbott", "Acosta", "Adeyemi", "Alvarez", "Banerjee", "Barnes", "Bauer",
  "Castillo", "Chavez", "Delgado", "Dubois", "Ellis", "Espinoza", "Fischer",
  "Fleming", "Gonzales", "Grant", "Gupta", "Haddad", "Hoang", "Holloway",
  "Ibrahim", "Iyer", "Jensen", "Kaplan", "Kowalski", "Larsen", "Lindqvist",
  "Mahmoud", "Medina", "Mensah", "Morales", "Nakamura", "Ndiaye", "Novak",
  "Okafor", "Ortega", "Park", "Patel", "Quinn", "Ramos", "Reyes", "Rossi",
  "Santos", "Schmidt", "Shapiro", "Sullivan", "Tanaka", "Torres", "Tran",
  "Ueda", "Vargas", "Vasquez", "Walsh", "Watanabe", "Weber", "Yilmaz",
  "Young", "Zamora", "Zhang"
)
