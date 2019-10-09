/* custom/getunc.p YSK   get UNC name for mapped driver */

DEFINE VAR Drive_Name AS CHARACTER NO-UNDO INIT "R:".
DEFINE VAR UNC_Name AS CHARACTER  NO-UNDO.
 
DEF VAR namelen AS INT NO-UNDO INITIAL 100.
DEF VAR retBool AS INT NO-UNDO.
 
UNC_Name = FILL("x",namelen).
RUN WNetGetConnectionA ( Drive_Name,
                   OUTPUT UNC_Name,
                   INPUT-OUTPUT namelen,
                   OUTPUT retBool).
 
IF retBool = 0 THEN
   UNC_Name = SUBSTRING(UNC_Name, 1, namelen).
ELSE
   UNC_Name = "".
 
MESSAGE
    "UNC Name: " UNC_Name " for " Drive_name
    VIEW-AS ALERT-BOX.


 

PROCEDURE WNetGetConnectionA EXTERNAL "mpr.dll" :
  DEFINE INPUT        PARAMETER lpDrive    AS CHAR.
  DEFINE OUTPUT       PARAMETER lpUNCName  AS CHAR.
  DEFINE INPUT-OUTPUT PARAMETER lpnLength  AS LONG.
  DEFINE RETURN       PARAMETER RetBool    AS LONG.
END PROCEDURE.


 
