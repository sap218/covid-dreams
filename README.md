# README

Repository for my small investigation into Google trends and UK COVID case counts.

Code is stored here, but information can be read in my [blog post](https://sap218.uk/posts/covid-dreams/).

## `01_covid_data`

+ Data from [OWID](https://github.com/owid/covid-19-data/tree/master/public/data/).
+ Download `COVID-2019 - ECDC (2020).csv` and put in `input/`.

## `02_google_trends`

+ Code information: [`PyTrends`](https://github.com/GeneralMills/pytrends) Version: 4.9.2.

## `03_merging`

+ How I organised the searches:

```
dream_groups = {
    "Positive": ["Happy","Great","Good","Nice"],
    "Negative": ["Sad","Bad","Scary","Nightmare"],
    "Oddity": ["Wild","Crazy","Strange","Weird"],
    "Intensity": ["Vivid","Lucid"],
    }
```

## `R` scripts

+ `04` Poll results
+ `05` Analysing data
+ `06` Comparing to baseline/other years

## AOB

+ Information for `input/events.tsv` were curated form various resourcs, e.g. https://www.instituteforgovernment.org.uk/sites/default/files/timeline-lockdown-web.pdf
