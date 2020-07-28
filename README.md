# Flexible Lithic Recording Tool [FLiRT]
FLiRT is a database interface designed to facilitate the irregular, iterative, and non-linear workflows that are commonly employed in lithic analysis. Specifically, it is designed to facilitate workflows that tack back and forth between recording assemblage-level and artefact-level data – often beginning with the former and subsequently adding the latter – and that may involve addition of particular kinds of data for limited subsets of artefacts.

[link to demo instance on shinyapps.io]

## Installation and setup
### Connecting with a database
FLiRT is a database interface, not a database. It establishes a connection with your database, which must be set up independently.

FLiRT is designed to interface with a specific schema, which has been provided in `FLiRT-template.sql`. The tables specified in `FLiRT-template.sql` can be integrated into or expanded to form broader archaeological project databases. Changing the names of tables and indexed columns would necessitate parallel changes to be made in the R script to match the new names.

MariaDB and MySQL databases, hosted either locally or on a remote server, are supported. If using a remote database, the database server must be configured to allow remote access, and a user must be created with permission to access the database remotely. Refer to the [MariaDB documentation](https://mariadb.com/kb/en/configuring-mariadb-for-remote-client-access/) for details on how to set this up.

To connect with the database, `keys.R` must be updated with credentials to access the database. Replace the default values (on the right side of the `<-`) with values that correspond with your own database.

* `dbnamex` refers to the name of the database.
* `hostx` refers to the ip address where the database is hosted.
* `portx` refers to the server port. This is read as an integer and not as a string, and therefore should not be framed by quotes (`""`).
* `userx` refers to the MariaDB/MySQL user.
* `passwordx` refers to the password corresponding with the user.

### Running the Shiny app
FLiRT is an R Shiny app, and is therefore run using an R console. This can be accomplished by either running the script using an RStudio Desktop client, or by setting up an R Shiny Server.

#### Using RStudio
Once the script has been configured to access your database and/or customized to suit your project's database schema, simply click 'Run App' located in the top-right corner of RStudio's Source pane.

![](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/images/run-app.png)

When using RStudio's default settings for running Shiny apps, FLiRT will open in a new tab of your operating system's default web browser.

The app might take longer to initiate the first time it is launched if the script needs to download and install depenendent packages.

FLiRT has not been tested for use with the RStudio Server client.

#### Using an R Shiny Server instance
FLiRT can also be hosted on an R Shiny Server instance.

Follow the [Shiny Server Adminstrator's Guide](https://docs.rstudio.com/shiny-server/) or [this much more succinct guide](https://www.linode.com/docs/development/r/how-to-deploy-rshiny-server-on-ubuntu-and-debian/) for installation and setup instructions.

It may be necessary to manually install packages that FLiRT depends upon as well, depending on the memory specifications of the server. To do this, in the command line run the following commands in sequence:
```bash
sudo su - -c "R -q -e "install.packages('shiny', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('xtable', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('DT', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('dplyr', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('reshape2', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('DBI', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('RMariaDB', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('splitstackshape', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('devtools', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('pool', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('purrr', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')""
sudo su - -c "R -q -e "install.packages('stringr', repos='http://cran.rstudio.com/')""
```

Once R and Shiny Server are installed and setup, create a directory for FLiRT and then place `FLiRT.R` (while renaming to `app.R`, as per Shiny Server's default configuration) and `keys.r` within that directory.
```bash
sudo mkdir /srv/shiny-server/FLiRT/
mv FLiRT.R /srv/shiny-server/FLiRT/app.R
mv keys.R /srv/shiny-server/FLiRT
```

Then restart Shiny Server:
```bash
sudo systemctl restart shiny-server
```

Your instance of FLiRT can then be accessed at: `http://YOUR_IP_ADDRESS_OR_DOMAIN:3838/FLiRT/`
