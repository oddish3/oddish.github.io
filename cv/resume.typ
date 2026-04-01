#import "@preview/modern-cv:0.9.0": *

#show: resume.with(
  author: (
    firstname: "Sol",
    lastname: "Yates",
    email: "contact@solyates.uk",
    homepage: "https://solyates.uk/",
    github: "oddish3",
    orcid: "0009-0004-8754-2108",
    // birth: "January 1, 1990",
    // linkedin: "Example",
    // address: "111 Example St. Example City, EX 11111",
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
  - Conducted trial-based economic evaluations for #link("https://sites.manchester.ac.uk/equity/")[
  #strong("EQUITy WP4")
]
  - Experience conducting **regression-based analyses** on large-scale healthcare datasets #link("https://cumming.ucalgary.ca/departments/medicine/division/rheumatology/rheum4u/about")[(#strong("Rheum4U"))]
  // - Collaborate with multidisciplinary teams to collect and analyse data for large-scale projects such as RECOLLECT, IMPRINT, and gene therapy studies, producing high-quality research output.
  - Experience drafting peer-reviewed manuscripts
  - Contributed to academic publications, presentations, and grant applications while actively supporting the development of the Manchester Centre for Health Economics. 
  - Provided technical guidance and **informal R training** to academic colleagues
  - Manage administrative leadership duties, including **chairing meetings** and supervising/interviewing research interns
  - Accredited Researcher (ONS, 2025–2030): authorised to access secure microdata under the Digital Economy Act 2017 for research in the public interest]

#resume-entry(
  title: "Honorary Researcher",
  location: "Manchester, UK",
  date: "Nov 2024 - Present",
  description: "Manchester University NHS Foundation Trust",
)

#resume-item[
  // content doesn't have to be bullet points
  - Experience collaborating with clinicians and multidisciplinary teams to collect primary care data for genetic testing study (EYEGEN)
]

#resume-entry(
  title: "Research Assistant",
  location: "Manchester, UK",
  date: "Aug 2023",
  description: "Economics Department, University of Manchester",
)

#resume-item[
  // content doesn't have to be bullet points
  - Implemented support vector machines to predict poverty lines in Python. Performed study replication, data preprocessing, feature engineering, model training, and evaluation.
- Collaborated with team members and independently contributed to various aspects of the project.
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
  - *Dissertation:* Differences-in-Differences with a Continuous Treatment : New Evidence and Applications
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


= Skills

#resume-skill-item(
  "Languages",
  (strong("R"), strong("Stata"), "Python", "Matlab", "C++"),
)
// #resume-skill-item("Spoken Languages", (strong("English"), "Spanish"))
#resume-skill-item(
  "Programs",
  (strong("Excel"), strong("Github")),
)

#pagebreak()

= Publications

// PUBLICATIONS_START

+ #underline[Yates, S]., Whittaker, W., Harrison, M., Bayliss, S., Barton, S., Sergouniotis, P.I., Payne, K., Black, G. (2026). Patterns of X-linked Retinitis Pigmentosa Genetic Testing in England and Implications for Service Provision. *Ophthalmology Science*, 101180. https://doi.org/10.1016/j.xops.2026.101180
+ Rogers G, Landi S, Purssell H, Momoh T, #underline[Yates S], Street O, Hanley K, Hanley N, Athwal V, Payne K (2025). Proactive case-finding and risk-stratification in people at risk of chronic liver disease in Greater Manchester: a cost-effectiveness analysis. *Preprint*. https://doi.org/10.1101/2025.06.01.25328671
// PUBLICATIONS_END

\
= Research in Progress

#resume-item[
  - *EYEGEN:* Mortality-adjusted XLRP prevalence estimation using a single national testing centre. 
]
#resume-item[
  - *Equity RCT:* Health economic analysis of a cluster randomised trial of a mental health training intervention.
]
#resume-item[
  - *Rheum4U:* A longitudinal study of the Presenteeism-Health status relationship in Rheumatoid Arthritis disease registry.
]
#resume-item[
  - *IMATCH Scoping Review* of benefit measures for increasing the output of clinical trials.
]
#resume-item[
  - *ID-Liver WP1:* Built a **multistate model** to estimate disease progression using patient-level data in ARLD.
]

#resume-item[
  - *ID-Liver WP2:* Further Developing the Evidence on the early diagnosis of Liver Disease.
]
#resume-item[
  - *EYEGENE:* Cost effectiveness of a hypothetical RPGR-XLRP therapy under a test–treat framework.
]
#resume-item[
  - *Breast Cancer Medicine NMA:* Systematic review and **network meta-analysis** of medicines that reduce the risk of breast cancer.
]


