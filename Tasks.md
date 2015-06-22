## Todo list

### Check for the password at the server
Server is currently sending the password as cleartext (which wont be an issue once the server is wss enabled), nevertheless,
the server should do the validation of the password and send a result back to the client.

### Add admin profile to a user.
Kick user text box should only be visible to users with appropriate permissions.

### Enable oauth2 login so user doesnt have to create a new account.
This is controversial, as security experts dont seem to agree upon the utility of a concentrated small set of identity providers.

### Enable wss/https for nginx/yesod reverse proxy setup.
This is a blocker.

### Add group support
Users can join/leave groups.

### Group profiles:
Allow users to create temporary rooms. Nothing is truly temporary, because these rooms will only be cleaned up
after an interval : 24 hours and when no user is logged in to the room.

