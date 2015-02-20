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
	