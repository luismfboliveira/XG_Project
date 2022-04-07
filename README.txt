Scopus API key - "a37e8a7500807b7438eb438c1324c85a"

In the same directory of the R project create a folder called input_data.
The script will check for the following files:

	* initial_dataset.xlsx: Mandatory!!! Final dataset (without records to filter out because they are not XG related).
	* API_info.RData: Optional!!! This variable might be present if you already retrieved API data (by running the script gets_api_data.R)
	* VOSviewer.jar: Mandatory!!! This file is needed to write network information in the output directories

RStudio might crash sometimes. Restarting a new session and running again usually solves the issue.
I am still investigating to know what can i do, since it is not caused by errors.

####################### TO RUN ########################

FULL Analysis (including downloading data from Scopus API)

	* Have a VPN connection to Nova IMS.

	* Open the project file (XG.Rproj).

	* Source the script Workflow.R