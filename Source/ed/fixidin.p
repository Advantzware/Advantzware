/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\FIXIDIN.P
**       By: Chris Heins
** Descript: Convert inbound partner location ID's to internal format
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/stringv.i}
DEF INPUT PARAM mast_rec AS RECID NO-UNDO.
DEF INPUT PARAM ws_id AS CHAR NO-UNDO.
DEF OUTPUT PARAM new_id  AS CHAR NO-UNDO.
DEF VAR tempid LIKE ws_id NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR isint AS LOGICAL NO-UNDO.
new_id = ws_id.
FIND edmast WHERE RECID(edmast) = mast_rec NO-LOCK NO-ERROR.
IF NOT AVAIL edmast THEN
DO:
  ws_erc = -1.
  RETURN.
END.
isint = FALSE.
IF id-trim THEN
DO:
  DO i = 1 TO LENGTH(ws_id):
    isint = {rc/isdigit.i SUBSTRING(ws_id,i,1)}.
    IF NOT isint THEN
    LEAVE.
  END.
  IF isint THEN
  tempid = STRING(integer(ws_id)).
  ELSE
  tempid = ws_id.
END. /* id-trim */
ELSE
tempid = ws_id.
IF id-len = 0 THEN
new_id = tempid.
ELSE if tempid > "" then do:
 IF LENGTH(tempid) < id-len THEN
 new_id = fill("0",(id-len - length(tempid))) + tempid.
 ELSE
 new_id = SUBSTRING(tempid,1,id-len).
end.
else new_id = ws_id.
if top-debug then 
run rc/debugmsg.p
 ("partner:" + edmast.partner
+ " id-trim:" + string(id-trim)
+ " id-len:"  + string(id-len)
+ " ws_id in:" + ws_id 
+ " new_id out:" + new_id).
 
