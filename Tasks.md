## Todo list

### Check for the password at the server
Server is currently sending the password as cleartext (which wont be an issue once the server is wss enabled), nevertheless, the server should do the validation of the password and send a result back to the client.

### Add admin profile to a user and other entitlements
Kick user text box should only be visible to users with appropriate permissions.

### Enable oauth2 login so user doesnt have to create a new account.
Support oauth2


### Setup an R server.
The current process is not ideal, where the call to the r script happens in the same vm, although, the 
pBD component takes care of distributing the load. We should look into creating an R server accepting 
parameters to perform specific computation.

### Enable wss/https for nginx/yesod reverse proxy setup.
Done using self-signed certificates.

### Add group support
Users can join/leave groups.

### Group profiles:
Allow users to create temporary rooms. Nothing is truly temporary, because these rooms will only be cleaned up
after an interval : 24 hours and when no user is logged in to the room.

### Integration with sphinx
Each record needs to have a searchable attribute. Also, password field should never be displayed in search results.

### Returning large datasets and cursors
Implement a cursor that is generic across various entities. Integration with sphinx should also allow for time based queries. There could be many more queries that will be needed to make the site usable.

### Implement portfolio analysis scripts and entities.
This task is divided into following parts
	##### Implement/save the actual R script that performs portfolio analysis.
	##### Implement market data entities for each symbol.
	##### Execute/save the script for each portfolio and append the output to the portfolio element.

### Implement Symbol analysis scripts and entities (along the similar lines as the portfolio analysis tasks)
This task will finally be able to show some graphs on the site. The intent is to using ggplot to start with.

### Implemment elementary machine learning in collaboration with Trenton R Users group
This overlaps with a different project. This task is to integrate with the ml utility. The utility, plans to
decode news and rank analysts for each symbol to establish a ranking for the analysts.
	##### Implement the analysts rank entity and allow users to view it. This is a read-only entity for 
		users.
	##### Admin users should be able to tweak the ranks depending so input with an audit trail.
	
