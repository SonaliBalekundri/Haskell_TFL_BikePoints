# Group Coursework
### ECS713U/P Functional Programming 2024/25
#### Link: https://learnouts.com/student/54/cw/86/

**Groups:** This is a group project. Groups should consist of 3 students. It's recommended that you use git for collaborating with your team members, so you can also do version tracking and more easily resolve conflicts. All students have an account on QM's GitHub (https://github.qmul.ac.uk).

**Task:** During this project your group will implement a stack-based Haskell app for harvesting information from the Web and saving it on a database. Make sure you are using stack to create, build and run your project. Please use "haskell-project" for the Haskell stack project name. The app should also give the users the ability to query the data on the database. The main tasks can be summarised as follows:

1. Your group will be assigned one specific web **API (JSON)** which contains some data of interest.
2. Write one module that defines the Haskell data types you are using (**Types.hs**), another defines a function for downloading JSON documents from the web API (**Fetch.hs**), and another module that parses the downloaded data into the given Haskell datatype (**Parse.hs**).
3. Write a module (**Database.hs**) that creates DB tables, saves/retrieves data from/to a database using again the appropriate Haskell data types.
4. Your parsing module (Parse.hs) should also provide a function that generates a JSON representation of your Haskell data and writes to a file.
5. Your **main** function in **Main.hs** should provide the following commands:
    Command 	Action

    stack run -- create

    	create sqlite database and tables

    stack run -- loaddata

    	download data from API and save to the database

    stack run -- dumpdata

    	generate data.json file with all data on database

    stack run -- <query> <query_arguments>

    	run some queries on the database (your choice)

6. Comment your code using haddock notation so that haddock documentation can be automatically generated for your app.
7. Come up and implement an extra challenging feature (on top of the requirements above) that demonstrates additional technical ability (required for full marks).
8. Write a 1-2 page(s) report explaining what your app does, how to run it, and justify any design choices you have had to make. If you have implemented an extra feature, explain what that feature is, why it was challenging to implement, and what extra technical abilities were required.

> Transport for London | Link: 	https://api.tfl.gov.uk/ 	| group_number `mod` 7 == 6

**Extra modules:** For this project you will be using extra Haskell modules for database access and HTTP requests. You should manage your project’s dependencies with the stack tool, using one of the latest LTS available (lts-22-39). Use sqlite-simple for database access, http-conduit for HTTP requests, and aeson for parsing JSON data. Make sure that you are using an sqlite database (do not use MySQL or Postgres) as this needs to be a self-contained app so we can test.

**The Report:** Write a 1-2 page(s) report explaining how to compile / run / use your application. ***Add report as a pdf document at top level of your stack project***. Give details of what Web source you are using, and how you are extracting the information. Describe any extra features that you have chosen to implement that you think goes beyond what has been asked.
test