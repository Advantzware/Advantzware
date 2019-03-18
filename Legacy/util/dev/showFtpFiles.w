&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR li AS INT NO-UNDO.

DEF TEMP-TABLE tt-rec FIELD old-i-no  AS CHAR
                      FIELD new-i-no AS CHAR FORMAT "x(20)"
                      FIELD ftp-site AS CHAR FORMAT "x(30)"
                      FIELD ftp-user AS CHAR 
                      FIELD ftp-passwd AS CHAR FORMAT "x(12)"
                      FIELD ftp-mode AS CHAR 
                      FIELD ftp-software AS CHAR
                      FIELD ftp-dir AS CHAR 
                      FIELD ftp-binary AS CHAR
                      FIELD ftp-script AS CHAR
                      FIELD ftp-cmd AS CHAR
                      INDEX old-i-no old-i-no
                      INDEX new-i-no IS PRIMARY new-i-no.

DEF BUFFER b-tt-rec FOR tt-rec.

DEF BUFFER bf-tt-rec FOR tt-rec.

/*{custom/globdefs.i} */

/* {sys/inc/VAR.i "new shared"} */

/*ASSIGN
 cocode = g_company
 locode = g_loc. */



/* {oe/oe-sysct1.i NEW} */


DEF VAR cExec AS CHAR.
DEF VAR cWinScpIniFile AS CHAR. 
DEF VAR ip-exp-file AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-rec

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-rec.old-i-no tt-rec.new-i-no   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-rec.new-i-no   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-rec BY tt-rec.old-i-no
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-rec BY tt-rec.old-i-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-rec


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_go BROWSE-2 Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWinScpExec D-Dialog 
FUNCTION getWinScpExec RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn_go 
     LABEL "Go" 
     SIZE 11 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-rec SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-rec.old-i-no LABEL "Format" WIDTH 40
     tt-rec.new-i-no LABEL "Description" WIDTH 40
     ENABLE tt-rec.new-i-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 83 BY 22.38
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btn_go AT ROW 1.71 COL 33
     BROWSE-2 AT ROW 2.91 COL 1
     Btn_Cancel AT ROW 25.52 COL 64
     SPACE(0.39) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "View uploaded files at Vendor"
         DEFAULT-BUTTON btn_go CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 btn_go D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-rec BY tt-rec.old-i-no
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* View uploaded files at Vendor */
DO:
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME D-Dialog
DO:
  APPLY 'choose' TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Exit */
DO:
  APPLY 'WINDOW-CLOSE' TO FRAME D-Dialog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go D-Dialog
ON CHOOSE OF btn_go IN FRAME D-Dialog /* Go */
DO:
    IF AVAIL tt-rec THEN DO:
        IF tt-rec.ftp-software EQ "FTP" THEN DO:
            
            
            OUTPUT TO VALUE(".\po\ftpdir.txt").    /* ftp text file */
            
            PUT UNFORMATTED 
                "open " + tt-rec.ftp-site  SKIP   /* ftp server ip address */
                tt-rec.ftp-user            SKIP   /* userid */
                tt-rec.ftp-passwd          SKIP.   /* password */
                
            IF tt-rec.ftp-dir GT "" THEN 
            PUT UNFORMATTED
                "cd " + tt-rec.ftp-dir     SKIP.
                
            PUT UNFORMATTED
                "dir "                     SKIP   /* file to transfer */
                "quit" .
            OUTPUT CLOSE.
            
            DEF VAR cFtpMode AS CHAR NO-UNDO.
            
            OS-COMMAND SILENT VALUE("ftp -v -i -s:.\po\ftpdir.txt 1>po\doftp.log 2>&1").
            
            
        END. /* FTP */
    
        IF tt-rec.ftp-software EQ "winSCP" THEN 
        DO:
            cWinScpIniFile = SEARCH("winScp\winscp.ini").
            
            IF cWinScpIniFile EQ ? THEN 
                cWinScpIniFile = "".
            ELSE 
            DO:
                FILE-INFO:FILE-NAME = cWinScpIniFile.
                cWinScpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
            END.
            
            OUTPUT TO VALUE(".\po\ftpdir.txt"). 
            
            /* New Destination */
            PUT UNFORMATTED 
                "option batch abort"   SKIP.
            PUT UNFORMATTED 
                "option confirm off"   SKIP.

            PUT UNFORMATTED "open " 
             + tt-rec.ftp-mode + ":" + tt-rec.ftp-user + ":" + tt-rec.ftp-passwd + "@" + tt-rec.ftp-site SKIP.
            
            IF tt-rec.ftp-dir GT "" THEN 
                PUT UNFORMATTED
                    "cd " + tt-rec.ftp-dir     SKIP.            

            PUT UNFORMATTED
                "dir "                 SKIP .      /* file to transfer */
            PUT UNFORMATTED    
                "close"                SKIP .     
            PUT UNFORMATTED 
                "Exit"                 SKIP.   
            OUTPUT CLOSE.
            
            
            cExec = getWinScpExec().
                      
            IF cWinScpIniFile GT "" THEN 
                cExec = cExec + " " + "/ini=" + cWinScpIniFile.          
            OS-COMMAND SILENT VALUE(cExec + " /script=.\po\ftpdir.txt > po\doFtp.log").
            
            
        END. /* If WinScp */
        
&IF DEFINED(FWD-VERSION) > 0 &THEN
        open-mime-resource "text/plain" "file:///po/doFtp.log" false.
&ELSE
        
        OS-COMMAND NO-WAIT notepad VALUE("po\doFtp.log").
&ENDIF
        
    END. /* avail tt-rec */
END. /* Choose Go */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
RUN build-table.
  {src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li    AS INT NO-UNDO.
DEF VAR li1   AS INT NO-UNDO.
DEF VAR li2   AS INT NO-UNDO. 
DEF VAR lv-i-no AS CHAR NO-UNDO.

/* gdm - 03300903 */
DEF VAR v-cnt AS INT NO-UNDO.

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-rec.

DO WITH FRAME {&FRAME-NAME}:
    INPUT FROM po/poexport.dat.
    REPEAT:
        
        CREATE tt-rec.
        IMPORT tt-rec.old-i-no tt-rec.new-i-no tt-rec.ftp-site tt-rec.ftp-user tt-rec.ftp-passwd
               tt-rec.ftp-mode tt-rec.ftp-dir tt-rec.ftp-software.
        IF tt-rec.old-i-no BEGINS "#" OR tt-rec.old-i-no EQ "" THEN
          DELETE tt-rec.
    END.
    INPUT CLOSE.
END.



  {&OPEN-QUERY-{&BROWSE-NAME}}

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE btn_go BROWSE-2 Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-rec"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWinScpExec D-Dialog 
FUNCTION getWinScpExec RETURNS CHARACTER
  (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cWinScpExe AS CHAR NO-UNDO.
    DEF VAR cExec      AS CHAR NO-UNDO.
    
    IF SEARCH("C:\Program Files\WinSCP\winscp.com") NE ? 
        OR SEARCH("WinSCP\winscp.com") NE ?
        OR SEARCH("C:\Program Files (x86)\WinSCP\winscp.com") NE ?
        THEN 
    DO:
        
        cExec = SEARCH("WinSCP\winscp.com").
        IF cExec EQ ? THEN 
            cExec = SEARCH("C:\Program Files\WinSCP\winscp.com").
        IF cExec EQ ? THEN 
            cExec = SEARCH("C:\Program Files (x86)\WinSCP\winscp.com").
          
        FILE-INFO:FILE-NAME = cExec.
        cExec = FILE-INFO:FULL-PATHNAME.
        
        cExec = '"' + cExec + '"'.
    END.
    cWinScpExe = cExec.          
    RETURN cWinScpExe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

