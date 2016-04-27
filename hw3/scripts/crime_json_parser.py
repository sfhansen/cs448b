# MS&E 231 Assignment 1
# AUTHORS: Samuel Hansen 
# This scripts parses tweets in raw json format and prints out crime's
# data in a tab-separated format. 

import sys
import json
import pytz
from pytz import timezone
import datetime
from datetime import datetime as dt 

for line in sys.stdin:
	#Verifies that input line is complete; otherwise, the loop continues without parsing that line.  
	try:
		whole_file = json.loads(line)
		# print data
	except:
		continue

#Prints out headers
print "\t".join(['category','day_of_week','description','incident_num','latitude','longitude','time','date','resolution']).encode('utf-8').strip()

crimes = whole_file['data']
for crime in crimes:
	category = crime['Category']
	day_of_week = crime['DayOfWeek']
	description = crime["Description"]
	incident_num = crime['IncidentNumber']
	latitude = crime['Location'][1]
	longitude = crime['Location'][0]
	time = crime['Time']
	date = crime['Date']
	resolution = crime['Resolution']

	#Prints out variables in a tab-separated list 
	print "\t".join([category,day_of_week,description,incident_num,latitude,longitude,time,date,resolution]).encode('utf-8').strip()



