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
  // title-link: "https://github.com/DeveloperPaul123",
)

#resume-item[
  - Conduct health economic evaluations, including cost-effectiveness and utility analysis, using regression-based methods and decision-analytic models on key healthcare interventions.
  - Collaborate with multidisciplinary teams to collect and analyse data for large-scale projects such as RECOLLECT, IMPRINT, and gene therapy studies, producing high-quality research output.
  - Contribute to academic publications, presentations, and grant applications while actively supporting the development of the Manchester Centre for Health Economics. 
  - Chair meetings and interview potential interns
  - Accredited Researcher (ONS, 2025–2030) – authorised to access secure microdata under the Digital Economy Act 2017 for research in the public interest]

#resume-entry(
  title: "Honorary Researcher",
  location: "Manchester, UK",
  date: "Nov 2024 - Present",
  description: "Manchester University NHS Foundation Trust",
)

#resume-item[
  // content doesn't have to be bullet points
  Worked in an inter-disciplinary between Academia and Healthcare
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

+ Rogers G, Landi S, Purssell H, Momoh T, #underline[Yates S], Street O, Hanley K, Hanley N, Athwal V, Payne K (2025). Proactive case-finding and risk-stratification in people at risk of chronic liver disease in Greater Manchester: a cost-effectiveness analysis. *Preprint*. https://doi.org/10.1101/2025.06.01.25328671
// PUBLICATIONS_END

\
= Research in Progress

#resume-item[
  - *EYEGEN:* Mortality-adjusted prevalence estimation using a single national testing centre, with partial national coverage.
]
#resume-item[
  - *Equity RCT:* Health economic analysis of a cluster randomised trial of a mental health training intervention.
]
#resume-item[
  - *Rheum4U:* A longitudinal fixed-effects model of a strictly decreasing polynomial relationship in disease registry data for Rheumatoid Arthritis.
]
#resume-item[
  - *IMATCH Scoping Review* of benefit measures for increasing the output of clinical trials.
]
#resume-item[
  - *ID-Liver WP2:* Further Developing the Evidence on the early diagnosis of Liver Disease.
]
#resume-item[
  - *EYEGENE:* A position piece on the challenges of modelling eye disease and building a Markov model.
]
#resume-item[
  - *Breast Cancer Medicine NMA:* A network meta-analysis for risk-reducing breast cancer medicines.
]


