connect
#-db r:/asiaddon/development/db/emptrack
-db f:/asiaddon/db/emptrack
-ld EMP_server
-H NTSERVER1
-S EMPTRACK-DEV
-N TCP


#-db r:/asiaddon/development/db/nosweat
-db f:/asiaddon/db/nosweat
-ld NOS_server
-H NTSERVER1
-S ASIADDON-DEV
-N TCP

no-error.
if error-status:error then return error.
