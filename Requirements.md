ccar-websockets
===============
### Objective
### Architecture
A haxe based websocket client interacting with a yesod websockets server.

### Core functions

### Managing the chat through a reseller is not clear to me
### Login (Regular User  - Not through a reseller)
##### Login (Returning user)
	* Connect to the server 
	* nickName is the only field that is displayed to the user
	* Validate the password - Should search for the user in company user directory.
	* Success: broadcast the rest of the community 
	* Server should send users online list to populate
	* Send conversation history
	* Begin processing server events


##### Login (First time user)
	Display the registration form.
	Register the user.
	Notify the rest of the community of a new user (perhaps if they would like to welcome the user).
	Begin processing events	

##### Send messages (private)
	@nickName should send a request for a private message to the user. If the user
	accepts the request for a private message, then : create a display area for the users to 
	have a private chat.
##### Send messages (broadcast)
	WHen no nick name is mentioned, the message is intended to be sent to all the users that 
	have set their preferences. 

##### List of functions available to the user
	Client needs to process this list to set up the tabs, message preferences 
	etc. Some of the functions that we can start with are:
	* Can the user send broadcast messages.
	* Reputation and Messaging tabs are default for every user.
##### Login (Through a reseller)
	* Connect to the server
	* Send nickName, emailId (derived from the reseller db), resellerId
	* Login the user depending on the profile of the user.
		* Reseller may not require guests to enter an email id, only nick name.
		* If the email_id is present, then the reseller must have created a profile
			for the user: which could be one of the predefined roles. 

##### CCAR Workbench
	Query notifications --------------------------------> 
						<-------------------------------  Sends all notifications
	Get user design templates --------------------------> 
						<-------------------------------  Sends all the design templates for the user

	Get all messages ----------------------------------->
					<----------------------------------- Return all off line messages
	Get all scenarios ------------------------------->
					<-----------------------------------  Return all scenarios



##### Send a gift
	Send a request to send a gift and and amount
	Giftee accepts the requset for the gift.
	Sender receives a notification.
	Sender and Giftee receive a gift cerficate for records.
##### List a product
	Create a product, description and the market place to participate

##### List a service

##### Marketplace
	Create a marketplace : description, (optional) barrier to entry in SWBench 
	Some marketplaces have no barrier to entry or cover charge. 

##### Create surveys
	Survey name
	Survey parameters (loaded, unbiased or somewhere in between)
	Survey cost per user in SWBench
	Total survay cost


##### Survey Tally
	Individual transactions that made up the result maintaining anonymity and integrity.

##### Load rscripts.
	Load a set of scripts that the user would like to add to his portfolio. 
	Passphrase used to encode the script - this could get cumbersome to the user.
	A user will be provided with a passphrase that was used to save the file.
	Cost of the script for sharing.



##### Load rjobs
	Load a set of jobs using the pdbr library. The user can specify the script, the source of the data
	and the number of processors desirable to complete the task.

#### Share workbench
	This is sort of the core functionality, for the system as we would like to be able to provide the 
	ability to a user to share a workbench. Sharing will have a cost associated with it. The sharer 
	gets a sharing initiative that is equally divided by the number of members in the sharing list. If 
	the sharing is individual, then the script has the price set by the creator.	



#### Create Ballot
	Ballot name
	Ballot Start time
	Ballot End time
	Ballot Size 
	Ballot quorum 
	Ballot Bias (number of votes allowed per user, *merican idol : looking at you)

#### Send invite to user (send an invite to a user to participate in the ballot)

#### Confirm invitation into the ballot

#### The voting process is as follows:
	Once an invitation in confirmed, each response is recorded as a vote.
	The vote is confirmed after a successful confirmation.
	During this time, the same user cannot vote more than the number of votes allowed and this number
	can be more than the number of votes totally allowed per voter. Which is ok, because only
	confirmed votes are counted. For example, if a voter is allowed 5 votes, the client is a bot
	sending about 100 votes, before we receive any confirmations, is allowed and we will count it as such.
	Once we have confirmations, only votes that are left will counted ensuring that each user cannot vote 
	more than the number of votes allowed in the ballot per voter.





#### User roles
A guest user will have limited priveleges in posting messages on the chat 
and they can be banned at anytime.
Returning user should be able to reach out to customer support for any issues 
Admin : Will have access to an admin ui.
Support : These are special users who help users out. 

#### Support user story
Upon log support user is presented with all the issues that have not been
assigned yet. Issues could be messages that were left when a guest did not 
receive any assistance. When a guest logs in, the support can invite the guest to a 
private message and most conversation from then on, will take 
place a separate tab. At any given time, a supporter may be online with at least
6 concurrent guests/clients etc. This limit needs to be set by the reseller.
The user interface, needs to allow the user to navigate the support needs with 
bells and alarms to ensure that no message is lost.



