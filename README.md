# Queryable Artifact Record Interface [QuARI]
QuARI is a database interface designed to facilitate the irregular, iterative, and non-linear workflows that are commonly employed in lithic analysis. Specifically, it is designed to facilitate workflows that tack back and forth between recording assemblage-level and artefact-level data – often beginning with the former and subsequently adding the latter – and that may involve addition of particular kinds of data for limited subsets of artefacts.

A demo is available at [https://stelida.mcmaster.ca/QuARIDemo](https://stelida.mcmaster.ca/QuARIDemo/). It is for demonstration purposes only, and all new data will be wiped daily.

See the [user manual](https://github.com/zackbatist/QuARI/blob/master/user-manual.md) for detailed instructions on interact with the app.

## Installation and setup
### Connecting with a database
QuARI is a database interface, not a database. It establishes a connection with your database, which must be set up independently.

QuARI is designed to interface with a specific schema, which has been provided in `QuARI-template.sql`. The tables specified in `QuARI-template.sql` can be integrated into or expanded to form broader archaeological project databases. Changing the names of tables and indexed columns would necessitate parallel changes to be made in the R script to match the new names. We recognize that this schema is not necessarily how other projects organize their data, and encourage that care be taken when incorporating it into your own systems.

MariaDB and MySQL databases, hosted either locally or on a remote server, are supported. See the official [RMariaDB](https://mariadb.com/kb/en/documentation/) and [MySQL](https://dev.mysql.com/doc/refman/8.0/en/) documentation for detailed instructions on how to install and use RMariaDB and MySQL respectively. A practical guide for setting up MariaDB on a virtual machine can be found [here](https://www.digitalocean.com/community/tutorials/how-to-install-mariadb-on-ubuntu-18-04).

If using a remote database, the database server must be configured to allow remote access, and a user must be created with permission to access the database remotely. Refer to the [MariaDB documentation](https://mariadb.com/kb/en/configuring-mariadb-for-remote-client-access/) for details on how to set this up.

To connect with the database, `keys.R` must be updated with credentials to access the database. Replace the default values (on the right side of the `<-`) with values that correspond with your own database.

* `dbnamex` refers to the name of the database.
* `hostx` refers to the ip address where the database is hosted.
* `portx` refers to the server port. This is read as an integer and not as a string, and therefore should not be framed by quotes (`""`).
* `userx` refers to the MariaDB/MySQL user.
* `passwordx` refers to the password corresponding with the user.

### Running the Shiny app
QuARI is an R Shiny app, and is therefore run using an R console. This can be accomplished by either running the script using an RStudio Desktop client, or by setting up an R Shiny Server.

#### Using RStudio
Open the RStudio Project (`QuARI.RProj`). Once the script has been configured to access your database and/or customized to suit your project's database schema, simply click 'Run App' located in the top-right corner of RStudio's Source pane. Optionally, use the dropdown to select where the app should appear (either in a new RStudio window, in the Viewer pane, or in a new tab of your default web browser).

![](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/images/run-app.png)

Select the using RStudio's default settings for running Shiny apps, QuARI will open in a new tab of your operating system's default web browser.

The app might take longer to initiate the first time it is launched if the script needs to download and install package dependencies.

QuARI has not been tested for use with the RStudio Server client.

#### Using Shiny Server
QuARI can also be hosted on a Shiny Server instance.

Follow the [Shiny Server Adminstrator's Guide](https://docs.rstudio.com/shiny-server/) or [this much more succinct guide](https://www.linode.com/docs/development/r/how-to-deploy-rshiny-server-on-ubuntu-and-debian/) for installation and setup instructions.

Once R and Shiny Server are installed and setup, move the QuARI project directory to Shiny Server.
```bash
sudo mvdir QuARI /srv/shiny-server/QuARI/
```

Then restart Shiny Server:
```bash
sudo systemctl restart shiny-server
```

Your instance of QuARI can then be accessed at: `http://YOUR_IP_ADDRESS_OR_DOMAIN:3838/QuARI/`

### Restoring a functional R environment using renv
We use [renv](https://rstudio.github.io/renv/) to record the R environment that supports functional use of this app. Running `renv::restore` in the R console will download the appropriate versions of each R package and their dependencies into a cache folder named "renv" within the RStudio Project folder, which the QuARI RStudio Project will access instead of the system-wide directory where packages are typically stored and called from. Installing certain packages and completing the `restore` may require further action, including installation of system components outside R. This may vary from system to system, see the relevant warnings and caveats that appear in the R console for further instructions on how to resolve such issues as they occur.

It may be necessary to restart the R session after successfully completing `renv::restore` by selecting "Restart R" from the "Session" tab in the RStudio menu bar, or by using the Control+Shift+F10 keyboard shortcut.
