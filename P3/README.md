This problem set features data from the United States Department of Transportation on Airline On-Time Statistics and Delay Causes.

Data Download and Description (http://stat-computing.org/dataexpo/2009/the-data.html)

Instructions:
We will only work with the first million rows. Download year 2008.

Problem 1: Find the IATA code that is in the Origin column, but is NOT is the Dest column.

Problem 2:  Which airport, as defined by the IATA code, has at least 10,000 flights and had the lowest probability for a delayed flight in the data?
Define a delay as:
If ArrDelay > 0, then count as a delayed flight to Origin airport.
If DepDelay > 0, then count as a delayed flight to Dest airport.
Total delayed flights equals the sum of the two above.
Probability of delay equals total delayed flights divided by total flights (Origin & Dest) that go through that airport.

Problem 3: Create a spreadsheet of data which calculates the historical probability of flight delay in each of the twenty carriers based off of two characteristics.
DayofWeek - Weekday (1,2,3,4,5) or Weekend (6,7)
DepTime - Day Time (0501 to 1700), Night Time (1701 to 2400), or Red Eye (0000 to 0500)
UniqueCarrier - Carrier (20)
For example, for an AA day time flight on the weekday, the percentage of delayed flights was ~29.3%.

===
The file size of the data is too large, I did not upload it.

My solution: XXX