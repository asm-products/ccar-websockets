## Todo list

### Check for the password at the server
Done : Server is currently sending the password as cleartext (which wont be an issue once the server is wss enabled), nevertheless, the server should do the validation of the password and send a result back to the client.

### Add a crypto hash
To prevent saving clear text passwords in the database or filesystems.

### Add admin profile to a user and other entitlements
Kick user text box should only be visible to users with appropriate permissions.

### Enable oauth2 login
Support oauth2.

### Integrate with JIRA (from the client)
Create a jira account to allow users to reduce friction in reporting bugs/issues.

### Add lambda support to the dsl
Create simple lambda functions to allow users greater control.

### Setup an R server
The current process is not ideal, where the call to the r script happens in the same vm, although, the 
pBD component takes care of distributing the load. We should look into creating an R server accepting 
parameters to perform specific computation.

### Setup Julia server
Use json commands to drive a julia server running to complete computations.

### Setup a generic job server
Setup a generic server to drive jobs. There are some tools that do similar 
to what the current scheduler is trying to implement. Need to evaluate this.

### Enable wss/https for nginx/yesod reverse proxy setup.
Done: using self-signed certificates.

### Add group support
Users can join/leave groups.

### User profile
Create a profile page for each user. The standard public/private profile contact information, qualifications.
### Peer review
A page to allow users to get their work reviewed. 

### Peer review transactions
Create a peer review transaction wherein, the reviewee (could be code, papers) credits a review amount to get the work reviewed. In general, if we dont tire ourselves out of getting the reviews, only scripts that have been reviewed and approved can run on the site. Each script has a sunset clause so that scripts
dont run beyond that time without being re-reviewed.

### Group profiles
Allow users to create temporary rooms. Nothing is truly temporary, because these rooms will only be cleaned up when it is obvious that the room is not being used.

### Integration with sphinx
Each record needs to have a searchable attribute. Also, password field should never be displayed in search results.


### Returning large datasets and cursors
Implement a cursor that is generic across various entities. Integration with sphinx should also allow for time based queries. There could be many more queries that will be needed to make the site usable.

### Implement portfolio analysis scripts and entities.
This task is divided into following parts
	##### Implement/save the actual R script that performs portfolio analysis.
	##### Implement market data entities for each symbol.
	##### Execute/save the script for each portfolio and append the output to the portfolio element.

### Implement market data adaptors
Enable market data and trading api to allow users to integrate their trading account.

### Implement Symbol analysis scripts and entities (along the similar lines as the portfolio analysis tasks)
This task will finally be able to show some graphs on the site. The intent is to using ggplot to start with.

### Implemment elementary machine learning in collaboration with Trenton R Users group
This overlaps with a different project. This task is to integrate with the ml utility. The utility, plans to decode news and rank analysts for each symbol to establish a ranking for the analysts.
	##### Implement the analysts rank entity and allow users to view it. This is a read-only entity for 
		users.
	##### Admin users should be able to tweak the ranks depending so input with an audit trail.


### Add selenium test cases
* Get a sense of the current state of the ui.
* Create expected test cases.
* Fix ui code to pass the tests.