#### Online voting protocol

##### Introduction

The process of collecting surveys is divided into the following phases
	* Developing the survey, questionnaires and participant profiles.
	* Registration
	* Voting
	* Counting -- We want to allow users to change their minds till the polls last.
	* Tallying - auditing results


##### Developing a survey
	The poller creates and manages questions for the survey to be published. Before publishing 
	a survey the poller needs to specify
		* Start time
		* End time
		* Total number of votes
		* Max Total number of votes per registered voter (default = 1)
		* Total cost of the survey
		* Participant profile
		* Display results before the polls close (some pollers want to influence the vote!)

###### Survey registration 
	An invitation to participate in the survey is send to all the participants (randomly) selected
	and have signed up to participate in these kinds of surveys. 

###### Registration process
	Users register to vote with some preferences for example the time zone and a tentative 
	voting time. Each voter is sent a reminder (reminders will always be shown on login). 
	Successful registration results in a wallet with a pre-filled voting amount along 
	with a voting ticket that will be used during the counting process.

###### Voting process
	The voter can only vote during the poll times and if they have a valid ticket. Voting process
	registers the vote as many times as the client would like to vote during the open polling period.

##### Counting process
	Votes are assigned in decreasing chronological order to allow for voters to change their mind 
	and reduce the voters wallet by the the voting amount. Voting amount has to be greater than 
	zero to register a vote.

##### Results and audit
	Tally the results and the process will be audited using normal auditing processes using the 
	votes transactions.


##### Supporting datamodel
	* Zones (postal zones, carrier zones, such as ups)
	* Languages : Spoken, written, font.
	* Country : Country api
	* Geolocation : latitude/longitude pair - either maintain the database
				-- or make an api call.
	* Contentful API to manage content for the user.


##### Formulae:
	Voting amount = Total cost of the survey / (Total number of votes * Max total allowed )

##### Random numbers : Use crypto random numbers and not system.random numbers.