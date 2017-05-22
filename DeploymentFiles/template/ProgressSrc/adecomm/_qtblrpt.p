/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/

/*----------------------------------------------------------------------------

File: qtblrpt.p

Description:
   Quick and dirty table report for both the GUI and character dictionary.
 
Input Parameters:
   p_DbId   - Id of the _Db record corresponding to the current database
   p_PName  - Physical name of the database
   p_DbType - Database type (e.g., PROGRESS)

Author: Tony Lavinio, Laura Stern

Date Created: 10/05/92
    Modified: 07/10/98 D. McMann Added DBVERSION and _Owner check
              03/29/99 by Mario B      BUG# 99-03-26-19 Changed DBNAME to 
                                       "DICTDB" in _Owner check.

----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */

DEFINE INPUT PARAMETER p_DbId 	 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_PName 	 AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_DbType  AS CHAR  NO-UNDO.

DEFINE VAR header_str AS CHAR NO-UNDO.
DEFINE VAR flags      AS CHAR NO-UNDO.

IF INTEGER(DBVERSION("DICTDB")) > 8 THEN DO:
  FIND _File WHERE _File._File-name = "_File"
               AND _File._Owner = "PUB"
               NO-LOCK.
  IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN DO:
    MESSAGE "You do not have permission to use this option."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  FIND LAST _File WHERE (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN")
                    AND NOT _File._Hidden NO-LOCK NO-ERROR.
  IF NOT AVAILABLE _File THEN DO:
    MESSAGE "There are no tables in this database to look at."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.
END.
ELSE DO:
  FIND _File "_File".
  IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN DO:
    MESSAGE "You do not have permission to use this option."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  FIND LAST _File WHERE  NOT _File._Hidden NO-ERROR.
  IF NOT AVAILABLE _File THEN DO:
    MESSAGE "There are no tables in this database to look at."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.
END.  

header_str = "Database: " + p_PName + " (" + p_DbType + ")".
flags = "Flags: 'f' = frozen, 's' = a SQL table".
RUN adecomm/_report.p 
   (INPUT p_DbId, 
    INPUT header_str,
    INPUT "Quick Table Report",
    INPUT flags,
    INPUT "",
    INPUT "adecomm/_qtbldat.p",
    INPUT "",
    INPUT {&Quick_Table_Report}).

/* _qtblrpt.p - end of file */





