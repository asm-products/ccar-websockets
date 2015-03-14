#### Online voting protocol

##### Introduction

The general process for conducting surveys or voting is described. The process of collecting surveys is 
divided into the following phases
	* Developing the survey, questionnaires and participant profiles.
	* Registration
	* Voting
	* Counting
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

###### Survey registration 
	An invitation to participate in the survey is send to all the participants (randomly) selected
	and have signed up to participate in these kinds of surveys. This requires some form of 
	a matching process so that the surveys have some added level of integrity.

###### Registration process
	Users register to vote with some preferences for example the time zone and a tentative voting time.
	Each voter is sent a reminder (reminders will always be shown on login). 
	Successful registration results in a wallet with a pre-filled voting amount along with a voting ticket.

###### Voting process
	The voter can only vote during the poll times and if they have a valid ticket. Voting registers the vote 
	as many times as the client would like to vote during the open polling period.

##### Counting process
	Here the votes are assigned in decreasing chronological order to allow for voters to change their mind and
	reduce the voters wallet by the the voting amount. Voting amount has to be greater than zero to register 
	vote.
##### Results and audit
	Here we tally the results and the process will be audited using normal auditing processes using the 
	votes transactions.



##### Formulae:
	Voting amount = Total cost of the survey / (Total number of votes * Max total allowed )

##### Random numbers : Use crypto random numbers and not system.random numbers.