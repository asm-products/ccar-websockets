User interaction elements

### User logged in
All elements that need to monitor a successful login (for example status message)

### User joined 

### User left

### User kicked 

### Entity added (Broadcast or reply)
### Entity deleted (Broadcast or reply)
### Entity updated (Broadcast or Reply)
### Entity queried (Reply only)



### UI Guidelines

#### Selecting of a parent entity should return the details, for example, when 
the drop down list of companies in the project ui is selected, retrieve the 
company details.

### Retrieving details that are not visible
As the model can be spread across multiple elements, charts etc, only the visible 
elements need to trigger a model refresh from the server.



#### Notes
Security, permissions, ownership for data need to be controlled. This can only be done 
as mix of server and client interaction: server and client need to conclude that an interaction
is allowed for the user.