#### Issues with version 1

##### Handle close request -- Need to mask interrupts, cancel the other threads.
##### Use Control.Monad.Logger and remove putStrLn
##### History configuration should be read from database preferences
##### Make the message area bigger for the ui.
##### Tab out doesnt send a validation request.
##### Send messages to offline users.
##### Allow for an admin user to have admin privileges (special tabs etc.).
##### Clear field after a send.



#### Features for version 2
##### Manage companies and their slideshows.
##### Manage projects for the company.
##### Manage project workbench, slideshows for a project.
##### Execute scripts 
##### Handle runtime exceptions in yesod.
##### Manage user porfolios (equity, options etc) - portfolio analyses
##### Manage symbol lists - symbol analyses

#### Features for version X

##### Create one time wallets for a user with a configurable initial amount.
##### Manage surveys (publish them)
##### Poll registration
##### Manage colored coins


#### Suffix T is being used as Transient objects, this could create some confusion. -- Need to fix this.
For example PortfolioT is not a Portfolio Monad Transformer. It is a dto<->dao pattern borrowed from hibernate.

#### UI design issues
Responses need to be managed in a view and the queries in a model. Is that reasonable?
