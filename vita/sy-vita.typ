#import "@preview/modern-cv:0.10.0": *

#show: resume.with(
  author: (
    firstname: "Sol",
    lastname: "Yates",
    email: "contact@solyates.uk",
    homepage: "https://solyates.uk/",
    github: "oddish3",
    orcid: "0009-0004-8754-2108",
    positions: (
      "Research Associate",
      "Honorary Researcher",
      "Research Assistant",
    ),
  ),
  profile-picture: none,
  date: datetime.today().display(),
  language: "en",
  colored-headers: true,
  show-footer: false,
  paper-size: "us-letter",
)

#let now = datetime.today()

#align(right)[
  #text(gray)[_#now.display("[month repr:long] [year]")_]
]

= Experience

#resume-entry(
  title: "Research Associate in Health Economics",
  location: "Manchester, UK",
  date: "Oct 2024 - Present",
  description: "Manchester Centre for Health Economics, University of Manchester",
  title-link: "https://github.com/oddish3",
)

#resume-item[
  - Conducted a trial-based economic evaluation for #link("https://sites.manchester.ac.uk/equity/")[#strong("EQUITy WP4")]
  - Led regression-based analyses on large-scale rheumatology registry data #link("https://cumming.ucalgary.ca/departments/medicine/division/rheumatology/rheum4u/about")[(#strong("Rheum4U"))]
  - Led a project from inception to first-author publication in a peer-reviewed journal #link("https://doi.org/10.1016/j.xops.2026.101180")[(#strong("EYEGEN"))]
  - Provided technical guidance and *informal R training* to academic colleagues
  - Chaired meetings and supervised research interns, including conducting interviews
  - *Accredited Researcher (ONS, 2025–2030):* authorised to access secure microdata under the Digital Economy Act 2017
]

#resume-entry(
  title: "Honorary Researcher",
  location: "Manchester, UK",
  date: "Nov 2024 - Present",
  description: "Manchester University NHS Foundation Trust",
)

#resume-item[
  - NHS governance credential enabling collection of primary care genetic testing data in collaboration with clinicians and geneticists (#strong("EYEGEN"))
]

#resume-entry(
  title: "Research Assistant",
  location: "Manchester, UK",
  date: "Aug 2023",
  description: "Economics Department, University of Manchester",
)

#resume-item[
  - Implemented support vector machines in Python to predict poverty lines; covered data preprocessing, feature engineering, model training, and evaluation.
]

= Education

#resume-entry(
  title: "University of Manchester",
  location: "Manchester, UK",
  date: "Sep 2023 - Aug 2024",
  description: "MSc in Economics",
)

#resume-item[
  - Grade: *Distinction*
  - *Dissertation:* Differences-in-Differences with a Continuous Treatment: New Evidence and Applications
  - *Key Modules:* Econometric Theory, Microeconometrics, Health Economics
]

#resume-entry(
  title: "University of Manchester",
  location: "Manchester, UK",
  date: "Sep 2020 - Jul 2023",
  description: "BA in Economics",
)

#resume-item[
  - Grade: *First Class*
  - *Key Modules:* Econometrics and Data Science, Quantitative Methods, Microeconomics
]

= Teaching

#resume-entry(
  title: "Teaching Assistant",
  location: "Manchester, UK",
  date: "2024–2026",
  description: "POPH60092 Economic Evaluation in Healthcare, University of Manchester",
)

#resume-item[
  - Supported postgraduate teaching in cost-effectiveness analysis and health technology assessment methods
  - Responsibilities included facilitating discussion boards and contributing to assessment preparation
]

= Skills

#resume-skill-item(
  "Statistical Methods",
  ("Cost-effectiveness analysis", "Multistate models", "Panel econometrics", "Network meta-analysis", "Difference-in-differences"),
)
#resume-skill-item(
  "Languages",
  (strong("R"), strong("Stata"), "Python", "Matlab", "C++"),
)
#resume-skill-item(
  "Tools",
  (strong("R: tidyverse"), "Quarto", "Git", "Excel"),
)

#pagebreak()

= Publications

// PUBLICATIONS_START

+ #underline[Yates, S]., Whittaker, W., Harrison, M., Bayliss, S., Barton, S., Sergouniotis, P.I., Payne, K., Black, G. (2026). Patterns of X-Linked Retinitis Pigmentosa Genetic Testing in England and Implications for Service Provision. *Ophthalmology Science*, 6(6), 101180. https://doi.org/10.1016/j.xops.2026.101180
+ Rogers G, Landi S, Purssell H, Momoh T, #underline[Yates S], Street O, Hanley K, Hanley N, Athwal V, Payne K (2025). Proactive case-finding and risk-stratification in people at risk of chronic liver disease in Greater Manchester: a cost-effectiveness analysis. *Preprint*. https://doi.org/10.1101/2025.06.01.25328671

// PUBLICATIONS_END

\
= Research in Progress

#resume-item[
  - *EYEGENE:* Cost effectiveness of a hypothetical RPGR-XLRP gene therapy.
  - *EQUITy:* Health economic analysis of a cluster randomised trial of a mental health training intervention.
  - *Rheum4U:* A longitudinal study of the presenteeism–health status relationship in a rheumatoid arthritis disease registry.
  - *IMATCH:* A scoping review of benefit measures for increasing the output of clinical trials.
  // - *ID-Liver WP1:* Multistate model of disease progression using patient-level data in alcohol-related liver disease.
  // - *ID-Liver WP2:* Evidence synthesis on early diagnosis of liver disease.
  - *Breast Cancer:* A NMA of medicines that reduce breast cancer risk.
  - *Pharmacy First:* An Economic evaluation of the Pharmacy First programme.
]

= Outside of work

I enjoy time with family and friends, good coffee, food, cooking, music, and movies, as well as the outdoors and travelling.
