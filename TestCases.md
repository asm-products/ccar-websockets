#### Test cases

Note: Everytime tests are run, the database is cleaned up so that there is no residual data for tests. Server needs 
to prevent for such a function.
##### User login and password 
User id exists and password test.

##### User registration.
Enter a user that doesnt exist, the registration screen should show up.

##### Password tests
Ensure that there is an upper limit for the number of incorrect password attempts. The system should come out of 
invalid password loop after no more than n attempts (n will be configurable. Assume 3 for initial tests.)
##### Select a company

###### Ensure that a company is selected


###### Cycle through each portfolio for a company

###### Test market data 
Test that the updated by you timestamp keeps changing.

###### Test scenarios
Create a scenario and apply the stress to the portfolio.

###### Manage portfolio
Create a portfolio using file upload.
Create a portfolio using symbol entry screen

###### Error handling scenarios
If the portfolio is not inserted correctly, user should see error messages.

###### Test messaging interface.
Send messages to the message board. Ensure that the messages are appended to the text area. 

###### Test project insert/update/delete/retrieve cycle.
First see if its working, test whats broken. Implement a test case to capture the interaction.

###### Test scripts upload 
The drop downs for supported scripts should not appear when no project is created/selected in the 
ui. Select a newly created project, pick different scripts and update sample scripts for that flavor. 
These sample scripts will be provided.

####### Approve/run scripts - TBD.
All scripts that run for a user need a form of review before they can run. Ensure that no rogue script can be run on behalf of a user.

###### Test inserting portfolios using file upload.

###### Test inserting portfolio using single file entry.


###### Test apply stress
* Create stresses using the ui to add stresses, for example 
"Create Equity Shock for TEVA pct 12 % 100;" (both with or without newline)
* Test for a portfolio that has TEVA and see the stress change by the percentage in the stress above.
* Test for a portfolio that doesnt have TEVA or any symbol of your choice and that the stress doesnt apply.
* Check for the time interval between updates of the stress values. This time interval should be consistent
over the run time of the test (this needs some design).

