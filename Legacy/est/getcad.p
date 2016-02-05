 /* est/getcad.p  A procedure to connect Access Database and retrieve data 
    02/01/05  YSK*/

/*
DEF INPUT PARAM ip-die# AS cha NO-UNDO.
DEF OUTPUT PARAM op-wid AS DEC NO-UNDO.
DEF OUTPUT PARAM op-len AS DEC NO-UNDO.
DEF OUTPUT PARAM op-dep AS DEC NO-UNDO.
*/

DEFINE VARIABLE i AS INTEGER NO-UNDO.

Def var hRecordSet  as com-handle no-undo.
Def var lh-Connection as com-handle no-undo.
Def var lh-Command    as com-handle no-undo.
Def var lv-ODBC-DSN      as character no-undo.
Def var lv-ODBC-SERVER   as character no-undo.
Def var lv-ODBC-USERID   as character no-undo.
Def var lv-ODBC-PASSWD   as character no-undo.
Def var lv-ODBC-QUERY    as character no-undo.
Def var lv-ODBC-STATUS   as character no-undo.
Def var lv-ODBC-RECCOUNT as integer   no-undo.
Def var lv-ODBC-NULL    as character no-undo.
Def var lv-ODBC-CURSOR  as integer   no-undo.
DEF VAR v-leave AS INT NO-UNDO.

DEFINE TEMP-TABLE ttblCADCAM NO-UNDO
  FIELD basisweight AS DECIMAL
  FIELD board LIKE ef.board
  FIELD boardID AS INTEGER
  FIELD cad-no LIKE eb.cad-no
  FIELD cal LIKE ef.cal
  FIELD dep LIKE eb.dep
  FIELD die-no LIKE eb.die-no
  FIELD len LIKE eb.len
  FIELD lin-in LIKE eb.lin-in
  FIELD style LIKE eb.style
  FIELD t-len LIKE eb.t-len
  FIELD t-sqin LIKE eb.t-sqin
  FIELD t-wid LIKE eb.t-wid
  FIELD weight LIKE ef.weight
  FIELD wid LIKE eb.wid.

/* for testing display */
DEFINE QUERY q1 FOR ttblCADCAM SCROLLING.
DEFINE BROWSE b1 QUERY q1 NO-LOCK
             DISPLAY boardid board
             WITH NO-ROW-MARKERS SEPARATORS SIZE 70 BY 12.62 EXPANDABLE.
DEFINE FRAME f1 b1  WITH NO-BOX.

/* Create the connection object for the link to SQL */
Create 'ADODB.Connection' lh-Connection.
/* Create a recordset object ready to return the data */
Create 'ADODB.RecordSet'  hRecordSet.
/* Create a command object for sending the SQL statement */
Create 'ADODB.Command'    lh-Command.

/* Change the below values as necessary */
Assign lv-ODBC-DSN    = 'fibre_mdb'    /* The ODBC DSN */
       lv-ODBC-SERVER = 'localhost'    /* The name of the server hosting the SQL DB and DSN */
       lv-ODBC-USERID = ''             /* The user id for access to the SQL Database */
       lv-ODBC-PASSWD = ''             /* Password required by above user-id */
       lv-ODBC-QUERY  = 'SELECT * from design, board WHERE board.boardid = design.boardid'.

/* Open up the connection to the ODBC Layer */
lh-Connection:Open ( 'data source=' + lv-ODBC-DSN + ';server=' +
                lv-ODBC-SERVER, lv-ODBC-USERID, lv-ODBC-PASSWD, 0 ) no-error.

IF ERROR-STATUS:ERROR THEN
   MESSAGE 'Error:' error-status:NUM-MESSAGES VIEW-AS ALERT-BOX.

/* Check for connection errors */
If ( ERROR-STATUS:NUM-MESSAGES > 0 ) then
DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
  MESSAGE ERROR-STATUS:GET-NUMBER(i)
          ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX.
END.
ELSE DO:
   Assign lh-Command:ActiveConnection  = lh-Connection
          lh-Command:CommandText       = lv-ODBC-QUERY
          lh-Command:CommandType       = 1 /* adCmdText */
          lh-Connection:CursorLocation = 3 /* adUseClient */
          hRecordSet:CursorType      = 3 /* adOpenStatic */
          hRecordSet                 = lh-Command:Execute ( OUTPUT lv-ODBC-NULL, '', 32 )
          lv-ODBC-RECCOUNT             = hRecordSet:RecordCount.

   /* Have we returned any rows ? */
   If ( lv-ODBC-RECCOUNT > 0 ) and not ( lv-ODBC-RECCOUNT = ? ) then
   Do:
      hRecordSet:MoveFirst no-error.

      Do while lv-ODBC-CURSOR < lv-ODBC-RECCOUNT:
         /* Display the data from the query (or create a Progress temp-table for future use) */
         /* Display hRecordSet:Fields ('name'):Value format 'x(20)'. */
         CREATE ttblCADCAM.
         ASSIGN
           ttblCADCAM.board = hRecordSet:Fields ('BOARDCODE'):VALUE
           ttblCADCAM.cal = hRecordSet:Fields ('CALIPER'):VALUE
           ttblCADCAM.dep = hRecordSet:Fields ('DEPTH'):VALUE
           ttblCADCAM.len = hRecordSet:Fields ('LENGTH'):VALUE
           ttblCADCAM.lin-in = hRecordSet:Fields ('RULELENGTH'):VALUE
           ttblCADCAM.t-len = hRecordSet:Fields ('BLANKLENGTH'):VALUE
           ttblCADCAM.t-sqin = hRecordSet:Fields ('AREA'):VALUE
           ttblCADCAM.t-wid = hRecordSet:Fields ('BLANKHEIGHT'):VALUE
           ttblCADCAM.weight = hRecordSet:Fields ('RULELENGTH'):VALUE
           ttblCADCAM.wid = hRecordSet:Fields ('WIDTH'):VALUE
           .

         Assign lv-ODBC-CURSOR = lv-ODBC-CURSOR + 1.
         hRecordSet:MoveNext no-error.
         v-leave = v-leave + 1.
         IF v-leave > 5 THEN LEAVE.  /* only 5 record for test */
      End. /* retrieved a single data row */

   End. /* retrieved all data rows */
   ELSE Assign lv-ODBC-STATUS = 'No records found.'.

       /* Close the ADO connection */
       lh-Connection:Close no-error.

End. /* The connection opened correctly */

/* Release the memory!! */
Release object lh-Connection no-error.
Release object lh-Command    no-error.
Release object hRecordSet  no-error.

Assign lh-Connection = ?
       lh-Command = ? 
       hRecordSet = ?.

/* result testing*/
OPEN QUERY q1 FOR EACH ttblCADCAM.
ENABLE ALL WITH FRAME f1.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

