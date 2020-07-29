# StravaStats

A Power BI report aiming to improve the analysis of activities recorded in Strava. Data extraction takes place in R. 
The main questions that the report currently sets out to answer are:
* Which segements do I ride the most?
* Are my segment times improving?
* How are my segment times compared to the KOM/QOM across a whole activity?

![Segments page](screenshot_segments.png)
![KOM comparison page](screenshot_kom.png)

## Setup

1. To use the report on your own account you'll need the following:
	* A [Strava API application](https://developers.strava.com/docs/getting-started/)
	* An R installation ([R Studio](https://rstudio.com/products/rstudio/download/) is recommended)
	* A [Power BI installation](https://www.microsoft.com/en-us/download/details.aspx?id=58494)

2. Create `credentials.yaml` in the repo's top-level directory, and include the API application's client ID and secret as follows:
  ```yaml
  client_id: xxxxx
  secret: xxxxx
  ```

3. Run `scrape_efforts.R`

4. Open the `Strava Stats.pbit` Power BI template and analyse away! (To adjust which activity types are being displayed, open the 'filters' pane; by default this is set to 'not Run')

5. If you would like to refresh your efforts, simply re-run `scrape_efforts.R` then refresh your Power BI report. If you would also like KOM times to be refreshed, delete `koms.csv`; note that will take a fair amount of time, so it's recommened that you only do this once every so often.

## Acknowledgments

Some R code taken from [bldavies/stravadata](https://github.com/bldavies/stravadata)
