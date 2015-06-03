Information system
===============
### Objective
Manage information and needs for a small business.

### Architecture
Haxe based websocket client interacting with a yesod websockets server.

### Core functions


### Login (Regular User  - Not through a reseller)
##### Login (Returning user)
	* Connect to the server 
	* nickName is the only field that is displayed to the user
	* Validate the password - Should search for the user in company user directory.
	* Success: broadcast the rest of the community 
	* Server should send users online list to populate
	* Send conversation history
	* Begin processing server events


##### Login (First time user)
	Display the registration form.
	Register the user.
	Notify the rest of the community of a new user (perhaps if they would like to welcome the user).
	Begin processing events	


##### Allow a given user to open multiple connections
	Once a user has successfully logged in, the user must be allowed to
	open multiple connections. This is to enable user to load large data
	sets on a different channel. 


##### Send messages (private)
	@nickName should send a request for a private message to the user. If the user
	accepts the request for a private message, then : create a display area for the users to 
	have a private chat.
##### Send messages (broadcast)
	WHen no nick name is mentioned, the message is intended to be sent to all the users that 
	have set their preferences. 

##### List of functions available to the user
	Client needs to process this list to set up the tabs, message preferences 
	etc. Some of the functions that we can start with are:
	* Can the user send broadcast messages.
	* Reputation and Messaging tabs are default for every user.
##### Login (Through a reseller)
	* Connect to the server
	* Send nickName, emailId (derived from the reseller db), resellerId
	* Login the user depending on the profile of the user.
		* Reseller may not require guests to enter an email id, only nick name.
		* If the email_id is present, then the reseller must have created a profile
			for the user: which could be one of the predefined roles. 


##### Send a gift
	Send a request to send a gift and and amount
	Giftee accepts the requset for the gift.
	Sender receives a notification.
	Sender and Giftee receive a gift cerficate for records.
##### List a product
	Create a product, description and the market place to participate

##### List a service

##### Marketplace
	Create a marketplace : description, (optional) barrier to entry in SWBench 
	Some marketplaces have no barrier to entry or cover charge. 

##### Create surveys
	Survey name
	Survey parameters (loaded, unbiased or somewhere in between)
	Survey cost per user in SWBench
	Total survay cost


##### Survey Tally
	Individual transactions that made up the result maintaining anonymity and integrity.

##### Load rscripts.
	Load a set of scripts that the user would like to add to their portfolio. 
	Passphrase used to encode the script - this could get cumbersome to the user.
	A user will be provided with a passphrase that was used to save the file.
	Cost of the script for sharing.



##### Load rjobs
	Load a set of jobs using the pdbr library. The user can specify the script, the source of the data
	and the number of processors desirable to complete the task.

#### Share workbench
	This is sort of the core functionality, for the system as we would like to be able to provide the 
	ability to a user to share a workbench. Sharing will have a cost associated with it. The sharer 
	gets a sharing initiative that is equally divided by the number of members in the sharing list. If 
	the sharing is individual, then the script has the price set by the creator.	



#### Create Ballot
	Ballot name
	Ballot Start time
	Ballot End time
	Ballot Size 
	Ballot quorum 
	Ballot Bias (number of votes allowed per user, *merican idol : looking at you)

#### Send invite to user (send an invite to a user to participate in the ballot)

#### Confirm invitation into the ballot

#### The voting process is as follows:
	Once an invitation in confirmed, each response is recorded as a vote.
	The vote is confirmed after a successful confirmation.
	During this time, the same user cannot vote more than the number of votes allowed and this number
	can be more than the number of votes totally allowed per voter. Which is ok, because only
	confirmed votes are counted. For example, if a voter is allowed 5 votes, the client is a bot
	sending about 100 votes, before we receive any confirmations, is allowed and we will count it as such.
	Once we have confirmations, only votes that are left will counted ensuring that each user cannot vote 
	more than the number of votes allowed in the ballot per voter.





#### User roles
A guest user will have limited priveleges in posting messages on the chat 
and they can be banned at anytime.
Returning user should be able to reach out to customer support for any issues 
Admin : Will have access to an admin ui.
Support : These are special users who help users out. 

#### Support user story
A user with a support profile needs to see issues that need to be addressed.

##### Create a company record
	. Signup a company with its logo etc.


##### Create a company slideshow.
	
##### Upload company videos.

##### Create a project case study
	. Create project summary
	. Create project financials
	. Create project slideshow

##### Create project slideshow
	. Save images and publish them.
##### Create project reports
	. Save a project report
	. Upload the associated file with the report.
		(the transaction is complete when both the 
			operations are complete)



Questions:
##### What can a user do in guest mode
	Send a few messages.
	Stay online for 5 - 10 mins as a guest user.
	Decrements the number of guest logins allowed.




Ricardos comments:
Get a focused demo so we can get users. According to him the application is in
four parts
. The interface (plugins + web)
. The server to manage the administrivia
. The actual script library
. Some form translation from the UI to an R script
. Close the source - something that Devesh also said.
. Dont waste too much time on the admin stuff.
. Support multiple formats.
. Get data from multiple apis and copy them to a local database.
Sajis comments:
. Get a flexible system supporting multiple formats.
. Get something out of the market soon.


Counter points:
AWS admin is an integral component for the service, so we should 
take some time to get it right.
Supporting multiple formats can be accomplished by providing the user 
with an S3 viewer and data describer.
Enable multiple apis and their data store in either postgres or on an S3 drive.
Released product must support login and security features.
OAUTH support is mandatory.



Economic indicators workbench 
http://www.smart-earn.com/gj/5/{replace with a number from below}

example:

http://www.smart-earn.com/gj/5/16 will list commodity Price index - food time series.

1	Average Hourly Earnings - Private Total
5	National Import Price Index
6	Consumer Price Index for All Urban Consumers
7	Major Sector Productivity Costs
8	PPI Commodities
9	Unemployment Rate
10	Employment Cost Index - All Civilian
11	Commodity Price Index
12	Velocity of M2 Money Stock
13	M2 Money Stock
14	Commodity Price Index - Agriculture Raw Materials
15	Commodity Price Index - Beverages
16	Commodity Price Index - Food
17	Commodity Price Index - Fuel
18	Commodity Price Index - Industrial Inputs
19	Commodity Price Index - Metals
20	Commodity Price Index - Nonfuel
21	Commodity Price Index - Petroleum
22	Real Trade Weighted USD Index - Broad
23	USA Recession Periods
24	Gross National Product (GNP)
25	Gross Domestic Product (GDP)
26	Real Disposable Personal Income - Per capita
27	Federal Funds Interest Rates
28	Bank Prime Loan Rates
29	Treasury Inflation Indexed Securities
30	G.17 Industrial Production and Capacity Utilization
31	Foreign Exchange Rates - H.10
32	G.19 Consumer Credit - Total
33	M1 Money Stock
34	Demand Deposits at Commercial Banks
35	Velocity of M1 Money Stock
36	1-Year Treasury Bill: Secondary Market Rate
37	G.19 Consumer Credit - Revolving
38	G.19 Consumer Credit - Non-revolving
39	Personal Saving Against Disposable Personal Income
40	Real disposable personal income
41	Disposable personal income: Per capita
42	Disposable Personal Income
43	FreddieMac House Price Index-National
45	Total Nonfarm Private Payroll Employment
46	Nonfarm Private Manufacturing Payroll Employment
47	Nonfarm Private Construction Payroll Employment
48	Nonfarm Private Small Payroll Employment(<50)
49	Nonfarm Private Large Payroll Employment(>499)
50	Nonfarm Private Trade/Transportation/Utilities Payroll Employment
51	Nonfarm Private Large Payroll Employment (1000 )
52	Nonfarm Private Large Payroll Employment (500 - 999)
53	Nonfarm Private Goods - Producing Large Payroll Employment (500 - 999)
54	Nonfarm Private Service - Providing Small Payroll Employment (1 - 19)
55	Nonfarm Private Service - Providing Large Payroll Employment (500 - 999)
56	Nonfarm Private Goods - Producing Medium Payroll Employment (50 - 499)
57	Nonfarm Private Goods - Producing Large Payroll Employment (1000 )
58	S&P 500
59	National Composite Home Price Index
60	Dow Jones Composite Average
61	National Weather - Temperature
62	National Weather - Palmer Drough Severity Index
63	National Weather - Precipitation
64	National Producer Price Index - All Commodities
65	Total Nonfarm Payroll - All Employees
66	National Export Price Index
67	Import Price Index - China
68	ISM Manufacturing : PMI Composite Index
69	Consumer Sentiment - University of Michigan
70	Real GDP
71	ISM Manufacturing: New Orders Index
72	Commercial Paper Outstanding
73	Financial Commercial Paper Outstanding
74	Commercial Paper Outstanding, Tier-1
75	Foreign Financial Commercial Paper Outstanding
76	Foreign Nonfinancial Commercial Paper Outstanding
77	Domestic Financial Commercial Paper Outstanding, U.S. Owned
78	Nonfinancial Commercial Paper Outstanding
79	Cass Freight Index - Shipments
80	Cass Freight Index - Expenditures
81	Central Bank Assets for Euro Area (11-17 Countries)
82	Harmonized Index of Consumer Prices: All Items for Euro area (17 countries)
83	Harmonized Index of Consumer Prices: All Items for European Union (28 countries)
84	Gross Domestic Product for Euro Area (17 Countries)
85	Net Loan Losses to Average Total Loans for all U.S. Banks
86	Net Interest Margin for U.S. Banks with average assets between $1B and $15B
87	All-Transactions House Price Index for the United States
88	Purchase Only House Price Index for the United States
89	ISM Manufacturing: Production Index
90	ISM Manufacturing: Employment Index
91	ISM Manufacturing: Inventories Index
92	ISM Manufacturing: Supplier Deliveries Index
93	ISM Manufacturing: Prices Index
94	ISM Manufacturing: New Export Orders Index
95	ISM Manufacturing: Imports Index
96	ISM Manufacturing: Customer Inventories Index
97	ISM Manufacturing: Backlog of Orders Index
98	Economic Policy Uncertainty Index for United States
99	Long-Term Government Bond Yields: 10-year: Main (Including Benchmark) for the United States
100	Total Current Account Balance for the United States
101	Smoothed U.S. Recession Probabilities
102	Household Debt to GDP for United States
103	Total Debt to Equity for United States
104	FOMC Summary of Economic Projections for the Civilian Unemployment Rate, Central Tendency, Midpoint
105	FOMC Summary of Economic Projections for the Growth Rate of Real Gross Domestic Product, Central Ten
106	Longer Run FOMC Summary of Economic Projections for the Personal Consumption Expenditures Inflation
107	St. Louis Fed Financial Stress Index
108	Consumer Price Index for All Urban Consumers: Fuel oil and other fuels
109	Consumer Price Index for All Urban Consumers: All Items Less Food
110	Consumer Price Index for All Urban Consumers: Medical Care
111	Consumer Price Index for All Urban Consumers: Food and Beverages
112	Consumer Price Index for All Urban Consumers: Apparel
113	Consumer Price Index for All Urban Consumers: Rent of primary residence
114	Consumer Price Index for All Urban Consumers: Transportation
115	Consumer Price Index for All Urban Consumers: Housing
116	Consumer Price Index for All Urban Consumers: Purchasing Power of the Consumer Dollar
117	Consumer Price Index for All Urban Consumers: Airline fare
118	Consumer Price Index for All Urban Consumers: Energy
119	Consumer Price Index for All Urban Consumers: Food
120	Chained Consumer Price Index for all Urban Consumers: All items
121	Consumer Price Index for All Urban Consumers: Gasoline (all types)
122	Consumer Price Index for All Urban Consumers: Meats, poultry, fish, and eggs
123	Consumer Price Index for All Urban Consumers: New vehicles
124	Consumer Price Index for Urban Wage Earners and Clerical Workers: All Items
125	Consumer Price Index for All Urban Consumers: Hospital and related services
126	Consumer Price Index for All Urban Consumers: Fuel oil and other fuels
127	Consumer Price Index for All Urban Consumers: Personal computers and peripheral equipment
128	Consumer Price Index for All Urban Consumers: Commodities
129	Consumer Price Index for All Urban Consumers: Education
130	Consumer Price Index for All Urban Consumers: Tuition, other school fees, and childcare
131	Consumer Price Index for All Urban Consumers: Food at home
132	Consumer Price Index for All Urban Consumers: Alcoholic beverages
133	Consumer Price Index for All Urban Consumers: Medical care services
134	Consumer Price Index for All Urban Consumers: Information technology, hardware and services
135	Consumer Price Index for All Urban Consumers: Tobacco and smoking products
136	Consumer Price Index for All Urban Consumers: Used cars and trucks
137	Consumer Price Index for All Urban Consumers: Medical care commodities
138	Consumer Price Index for All Urban Consumers: Shelter
139	5-Year Treasury Constant Maturity Rate
140	10-Year Treasury Constant Maturity Rate
141	30-Year Conventional Mortgage Rate
142	10-Year Treasury Inflation-Indexed Security, Constant Maturity
143	Real Retail and Food Services Sales
144	Retail Sales: Total (Excluding Food Services)
145	Total Business: Inventories to Sales Ratio
146	Total Business Inventories
147	Total Vehicle Sales
148	Retailers Sales
149	Retail Sales and Food Services Excluding Motor Vehicles and Parts Dealers
150	E-Commerce Retail Sales
151	Real Manufacturing and Trade Industries Sales
152	Manufacturers Sales
153	Average Hourly Earnings of All Employees: Retail Trade
154	Motor Vehicle Retail Sales: Domestic Autos
155	Total Business Sales
156	Retail Trade: Furniture and Home Furnishings Stores
157	Motor Vehicle Retail Sales: Heavy Weight Trucks
158	Retail Trade: Clothing and Clothing Accessory Stores
159	All Employees: Retail Trade
160	Retail Trade: Auto and Other Motor Vehicles
161	Retail Trade: Gasoline Stations
162	Dow Jones Industrial Average
163	Dow Jones Utility Average
164	Dow Jones Transportation Average
165	S&P Case-Shiller 20-City Home Price Index
166	Household Debt to GDP for United States
167	Commercial Real Estate Prices for United States
168	US Regular Reformulated Gas Price
169	US Diesel Sales Price
170	US Regular All Formulations Gas Price
171	Trade Weighted U.S. Dollar Index: Broad




Haskell Naming conventions till I get the type system right
==========================================================
Public methods in database operations are query and manage.
insert, update, delete and select are private methods. 

SPA related stuff
==================================================
Incremental save is implemented using a global stream.
What are the conventional key maps for commonly used keyboard actions (CUA?)?



FRP
=================================================
We are currently using promHx from haxe as an frp library. There
is a gap between the theoretical definiton of an frp library and 
what is implemented here. However, the architecture is still better
than the traditional listener approach.

Acronyms (Jargon??)
==============================================
SPA : Single page application
FRP : Functional reactive programming
pbdR : parallel big data for (R)[http://pbd-r.org]



