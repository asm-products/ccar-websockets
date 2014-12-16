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


##### Dashboard 
	Query notifications --------------------------------> 
						<-------------------------------  Sends all notifications
	Get user design templates --------------------------> 
						<-------------------------------  Sends all the design templates for the user

	Get all messages ----------------------------------->
					<----------------------------------- Return all off line messages
	Get all scenarios ------------------------------->
					<-----------------------------------  Return all scenarios
