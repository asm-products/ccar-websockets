ccar-websockets
===============
### Objective
To provide users a way to evaluate and manage portfolio risks.
### Architecture
A haxe based websocket client interacting with a yesod websockets server.

### Core functions

#### Allow a user to load/modify scenarios. 
        ##### Create audit trails of all modification for the scenarios created.
	##### Compare scenarios 
	##### Upload trades.
	##### Apply a scenario to a trade
	##### Create groups for traders - allow group/p2p communication of all entities
	##### Share scenarios, entities with groups.
	#####


### UI 
##### Login process (First time user)
	User leaves nickName field ---------------> Server checks for nickname
						<--------------------------- User not Found
	Register user 		----------------------> Save the user in the database with a success or error
						<----------------------- User saved successfully
	Show the dashboard  -----------------------> User registered and logged in successfully

##### Login (User registered) 
	User leaves nickname field ------------------------> Server checks for the nickname
						<------------------------------- User found
	Show password form  -------------------------------> Validate user (three attempts at login)
						<-------------------------------
	User valid (display dashboard)


##### Dashboard  - The core workbench
	Query notifications --------------------------------> 
						<-------------------------------  Sends all notifications
	Get user design templates --------------------------> 
						<-------------------------------  Sends all the design templates for the user

	Get all messages ----------------------------------->
					<----------------------------------- Return all off line messages
	Get all scenarios ------------------------------->
					<-----------------------------------  Return all scenarios


#### Send private messages - Send private messages to the each other.
	
	###### Parameters
	Message : Text
	From : The Sender
	To : THe recepient
	###### Issues/exceptions
	If the sender is offline after the message is sent, the user who sent the message will get an error.
	If the user is offline before the message is sent, the user will be provided with a private store of 
	messages that will be replayed when the user log back in. Offline messages should only be sent by users
	who are in the trusted circle of the user. 

#### Send a gift
	Send a request to send a gift and and amount
	Giftee accepts the requset for the gift.
	Sender receives a notification.
	Sender and Giftee receive a gift cerficate for records.
#### List a product
	Create a product, description and the market place to participate

#### List a service

#### Marketplace
	Create a marketplace : description, (optional) barrier to entry in SWBench 
	Some marketplaces have no barrier to entry or cover charge. 

#### Create surveys
	Survey name
	Survey parameters (loaded, unbiased or somewhere in between)
	Survey cost per user in SWBench
	Total survay cost

#### Survey Tally
	Individual transactions that made up the result maintaining anonymity and integrity.

#### Load rscripts.
	Load a set of scripts that the user would like to add to his portfolio. 
	Passphrase used to encode the script - this could get cumbersome to the user.
	A user will be provided with a passphrase that was used to save the file.
	Cost of the script for sharing.

#### Load rjobs
	Load a set of jobs using the pdbr library. The user can specify the script, the source of the data
	and the number of processors desirable to complete the task.

#### Share workbench
	This is sort of the core functionality, for the system as we would like to be able to provide the 
	ability to a user to share a workbench. Sharing will have a cost associated with it. The sharer 
	gets a sharing initiative that is equally divided by the number of members in the sharing list. If 
	the sharing is individual, then the script has the price set by the creator.	
### General requirements 
##### If the app is launched as part of assembly, tie the validation to assembly's oauth (one screen less for the  user).
##### Each user receives some currency as part of signup with a wallet.
##### How does a user ask for more currency? By simply going to a bank or some such entity: I am not sure. At this point, I dont see a
	  need for a bank. As long as coins exist, we issue them. But each request for coins will be a transaction, loan of some sort. 
##### Each user also gets an account created with the above wallet.
##### A single user can have multiple accounts to conduct transactions with the community.
##### User can indicate if they would like to sell products. If yes, then they will be allowed to add products to the marketplace.

##### Products, currently, are defined as scenarios, r analyses files or stata files, socrata filters etc. Although there is no
	  real restriction of the kind of products one can manage in the market place.

##### Market auction. 




#### Data model
	