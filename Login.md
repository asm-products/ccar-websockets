### Login 

## Current process
### Check availability (this needs to account for multiple client connections)
### Server sends Login information with the password - This needs to be changed.
### Client now validates password, if all goes well sends UserLogged in with a nick name.


## Proposed initial login process

### Check availability including checks for multiple client connections.
### Server sends response without password in all cases. 
### Client sends Validate password with an encrypted key.
### Upon successful login, server sends a user logged in message
### Server send a client session key for subsequent client connections from the same client


## Multiple login process (new)
### Client checks if already connected, sends a multiple login request.
### Server checks if client can login multiple times.
### Server allows the client to connect.


#### Notes
##### Client connection header needs to send a client key for all subsequent messages.
##### Client/server will need to perforrm some form of synchronization so that the view
 		is consistent.





