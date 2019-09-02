<p align="center"><a href="http://193.146.75.235/sample-apps/final_apps/portada"><img src="https://github.com/ordanovich/images/blob/master/webportal_overview.gif?raw=true"></a></p>
<p align="center"><a href="http://longpop-itn.eu/"><img src="https://github.com/ordanovich/images/blob/master/logo3inline_small.png?raw=true"></a></p>

## Interactive application for programmatic data retrieval from [OECD](https://data.oecd.org/)

This appication is based on the [**OECD** package](https://github.com/expersso/OECD). To learn more about the functionality of the package please refer to the [tutorial](https://cran.r-project.org/web/packages/OECD/vignettes/OECD.html).

### Nomenclature

- :rocket: [app.R](https://github.com/ordanovich/downloadOECD/blob/master/app.R): Shiny app combining **UI** and **server** parts.
- :bar_chart: [report.Rmd](https://github.com/ordanovich/downloadOECD/blob/master/report.Rmd): template markdown file to generate and download an HTML report through the application interface.

The application itself (use `shiny::runApp()` from the cloned repository or open the <a href="http://193.146.75.235/sample-apps/final_apps/oecd_download/"  rel="noopener noreferrer" target="_blank">online version</a> to preview the app) pursues the goal to allow users to consult the contents of the data base, retrieve and visualize the desired datasets in a quick and easy-to-manipulate manner. 


