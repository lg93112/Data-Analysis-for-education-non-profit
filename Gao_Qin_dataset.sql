-- We want to build a dataset containing attributes as age, gender, married, how many events attended, and 
-- whether a constituent is a volunteer or has any relatives as volunteers in our nonprofit or not as a binary variable. 

-- And we also should include the target attribute "donor" as a binary variable in our dataset. We define a person as donor if he appears in 
-- transactions table. 

-- Find all event attendees and how many events do they attend
SELECT CONSTITUENT_ID, COUNT(EVENT_ID) AS EVENTS
FROM jgough2_FINAL_PROJECT.EVENT_ATTENDEE GROUP BY CONSTITUENT_ID;

-- Find if a person is a volunteer or has any relative as volunteer
-- Find all volunteers
SELECT DISTINCT jgough2_FINAL_PROJECT.VOLUNTEER.CONSTITUENT_ID
FROM jgough2_FINAL_PROJECT.VOLUNTEER;
-- Find persons with relatives as volunteers
SELECT jgough2_FINAL_PROJECT.FAMILY.CONSTITUENT_ID
FROM jgough2_FINAL_PROJECT.FAMILY, jgough2_FINAL_PROJECT.VOLUNTEER
WHERE RELATION_CONSTITUENT_ID IN 
(SELECT jgough2_FINAL_PROJECT.VOLUNTEER.CONSTITUENT_ID FROM jgough2_FINAL_PROJECT.VOLUNTEER);
-- This returns 0 rows, which means that the relatives of constituents in family table are all not volunteers
-- Thus, we can create a VOLUNTEER binary variable whose value is 1 only if the person himself is a volunteer. 

-- Find all donors
SELECT distinct(CONSTITUENT_ID) FROM jgough2_FINAL_PROJECT.TRANSACTIONS;


-- Create donor attribute as target attribute (dependent variable), and select AGE, GENDER, MARRIED attribute
-- GENDER attribute is changed to numeric binary variable where 1 stands for MALE, 0 stands for FEMALE
SELECT CONSTITUENT_ID,
CASE WHEN CONSTITUENT_ID in (SELECT DISTINCT CONSTITUENT_ID FROM jgough2_FINAL_PROJECT.TRANSACTIONS) THEN 1
ELSE 0 END AS donor, 
AGE, CASE GENDER WHEN 'MALE' THEN 1 ELSE 0 END AS GENDER, MARRIED
FROM jgough2_FINAL_PROJECT.CONSTITUENT;

-- Create events attribute whose value will be 0 if the constituent didn't attend any events or equal to the number of events
-- he attended if he is an event attendee
SELECT CONSTITUENT_ID, 0 AS events FROM jgough2_FINAL_PROJECT.CONSTITUENT
WHERE CONSTITUENT_ID NOT IN (SELECT DISTINCT CONSTITUENT_ID FROM jgough2_FINAL_PROJECT.EVENT_ATTENDEE)
UNION
SELECT CONSTITUENT_ID, COUNT(EVENT_ID) AS events FROM jgough2_FINAL_PROJECT.EVENT_ATTENDEE GROUP BY CONSTITUENT_ID;

-- CREATE volunteers binary attribute whose value is 1 if the constituent is a volunteer or 0 if not
SELECT CONSTITUENT_ID,
CASE WHEN CONSTITUENT_ID in (SELECT DISTINCT CONSTITUENT_ID FROM jgough2_FINAL_PROJECT.VOLUNTEER) THEN 1
ELSE 0 END AS volunteer
FROM jgough2_FINAL_PROJECT.CONSTITUENT;

-- Now integrate the results as our final dataset of derived attributes
SELECT t3.donor, t3.AGE as age, t3.GENDER AS gender, t3.MARRIED as married, t3.events, t4.volunteer
FROM 
(SELECT t1.CONSTITUENT_ID, t1.donor, t1.AGE, t1.GENDER, t1.MARRIED, t2.events
FROM
(SELECT CONSTITUENT_ID,
CASE WHEN CONSTITUENT_ID in (SELECT DISTINCT CONSTITUENT_ID FROM jgough2_FINAL_PROJECT.TRANSACTIONS) THEN 1
ELSE 0 END AS donor, 
AGE, CASE GENDER WHEN 'MALE' THEN 1 ELSE 0 END AS GENDER, MARRIED
FROM jgough2_FINAL_PROJECT.CONSTITUENT) AS t1,
(SELECT CONSTITUENT_ID, 0 AS events FROM jgough2_FINAL_PROJECT.CONSTITUENT
WHERE CONSTITUENT_ID NOT IN (SELECT DISTINCT CONSTITUENT_ID FROM jgough2_FINAL_PROJECT.EVENT_ATTENDEE)
UNION
SELECT CONSTITUENT_ID, COUNT(EVENT_ID) AS events FROM jgough2_FINAL_PROJECT.EVENT_ATTENDEE GROUP BY CONSTITUENT_ID) AS t2
WHERE
t1.CONSTITUENT_ID=t2.CONSTITUENT_ID) AS t3,

(SELECT CONSTITUENT_ID,
CASE WHEN CONSTITUENT_ID in (SELECT DISTINCT CONSTITUENT_ID FROM jgough2_FINAL_PROJECT.VOLUNTEER) THEN 1
ELSE 0 END AS volunteer
FROM jgough2_FINAL_PROJECT.CONSTITUENT) AS t4

WHERE t3.CONSTITUENT_ID = t4.CONSTITUENT_ID;

-- And now we materialize this table into a .txt file containing the dataset you used to build your model in R
