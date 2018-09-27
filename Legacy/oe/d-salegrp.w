&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ip-sman-rec-key LIKE sman.rec_key      NO-UNDO.
DEFINE INPUT PARAMETER ip-sman-code    AS CHARACTER     NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE saveNoteCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-note-required AS LOG NO-UNDO.
DEFINE VARIABLE v-rtn-char AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-found AS LOG NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}

DEFINE VARIABLE cocode AS cha NO-UNDO.
DEFINE VARIABLE locode AS cha NO-UNDO.

ASSIGN cocode = gcompany
       locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Standard List Definitions                                            */

&Scoped-Define ENABLED-OBJECTS cbTitle btAddNote btDeleteNote btOk btCancel

&Scoped-Define DISPLAYED-OBJECTS group-desc cbTitle 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAddNote 
     LABEL "+" 
     SIZE 6 BY 1.14
     FONT 6.

DEFINE BUTTON btDeleteNote
    LABEL "X"
    SIZE 6 BY 1.14 
    FONT 6 .

DEFINE BUTTON btCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btOk 
     LABEL "OK" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE cbTitle AS CHARACTER FORMAT "X(256)":U
     LABEL "Group"
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "1","1"
     DROP-DOWN-LIST 
     SIZE 61 BY 10 NO-UNDO.

DEFINE VARIABLE group-desc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sales Rep"
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.



/* ************************  Frame Definitions  *********************** */

DEFINE FRAME dialog-Frame
     
     group-desc AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 12
     cbTitle AT ROW 3.62 COL 16 COLON-ALIGNED WIDGET-ID 26
     btAddNote AT ROW 3.62 COL 80 WIDGET-ID 24
     btDeleteNote AT ROW 3.62 COL 88 
     btOk AT ROW 11.29 COL 25 WIDGET-ID 18
     btCancel AT ROW 11.29 COL 56 WIDGET-ID 20
     SPACE(14.80) SKIP(0.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Sales Group Maintenance".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME dialog-Frame:SCROLLABLE       = FALSE
       FRAME dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dialog-Frame dialog-Frame
ON ESCAPE OF FRAME dialog-Frame /* Add Promise Date Change  Note */
DO:
  APPLY 'window-close' TO FRAME dialog-frame.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dialog-Frame dialog-Frame
ON WINDOW-CLOSE OF FRAME dialog-Frame /* Add Promise Date Change  Note */
DO:
  ASSIGN cbTitle.

  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAddNote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddNote dialog-Frame
ON CHOOSE OF btAddNote IN FRAME dialog-Frame /* + */
DO:

    DEFINE VARIABLE lNewCode     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lNewDesc     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ip-parms     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE op-values    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i            AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iCurRow      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCurCol      AS INTEGER NO-UNDO.
    DEFINE VARIABLE h_d-prompt   AS HANDLE NO-UNDO.
    
    REPEAT:
        ASSIGN lNewCode = ""
               lNewDesc = ""
               op-values = "".
        ip-parms = 
               "type=fill-in,name=fi6,row=4,col=3,enable=false,FORMAT=X(8),scrval=GroupID: " 
            + "|type=fill-in,name=fi2,row=4,col=21,enable=false,FORMAT=X(5),scrval=Name: " 
            + "|type=fill-in,name=fi3,row=4,col=13,enable=true,FORMAT=X(4),width=7"
            + "|type=fill-in,name=fi4,row=4,col=28,enable=true,FORMAT=X(35),width=50"
             + "|type=win,name=win1,row=20,col=33,label=Add New Group Code".
        iCurRow = FRAME {&FRAME-NAME}:ROW.
        iCurCol = FRAME {&FRAME-NAME}:COL.
        RUN custom/d-prompt.w PERSISTENT SET h_d-prompt (INPUT "Normal",
                               INPUT ip-parms,
                               INPUT "" /* validation procedure */,
                               OUTPUT op-values).
        RUN set-position IN h_d-prompt (INPUT 10, INPUT 10).
        DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
            IF ENTRY(i, op-values) EQ "fi3" THEN
              lNewCode = ENTRY(i + 1, op-values).
            IF ENTRY(i, op-values) EQ "fi4" THEN
              lNewDesc = ENTRY(i + 1, op-values).    
        END.
        /* They entered nothing */
        IF lNewCode EQ "" THEN
            RETURN.
        FIND FIRST salesgrpMember NO-LOCK
             WHERE salesgrpMember.company EQ cocode
               AND salesgrpMember.salesmanID EQ lNewCode
             NO-ERROR .
        IF AVAIL(salesgrpMember) THEN DO:
            MESSAGE "This Group Code already exists, please try another..."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.
        ELSE DO:
          CREATE salesgrpMember.
          ASSIGN salesgrpMember.company = cocode
                 salesgrpMember.sman    = IF AVAILABLE sman THEN sman.sman ELSE ""  
                 salesgrpMember.salesmanID = lNewCode
                 salesgrpMember.addrRecKey = ip-sman-rec-key
                 salesgrpMember.salesmanName = lNewDesc.
            cbTitle:ADD-LAST(lNewCode + " - " + lNewDesc, lNewCode).
            cbTitle:SCREEN-VALUE = lNewCode.
        END.
        LEAVE.
    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDeleteNote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeleteNote dialog-Frame
ON CHOOSE OF btDeleteNote IN FRAME dialog-Frame /* + */
DO:
    DEFINE VARIABLE cDeleteCode AS CHARACTER NO-UNDO .
    DEFINE BUFFER bfsalesgrpMember FOR salesgrpMember .
      
      IF cbTitle:SCREEN-VALUE NE "" THEN DO:
        MESSAGE "Delete Currently Selected Record(s)?" VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN RETURN .
      END.


         FIND FIRST bfsalesgrpMember EXCLUSIVE-LOCK
             WHERE bfsalesgrpMember.company EQ cocode
               AND bfsalesgrpMember.salesmanID EQ cbTitle:SCREEN-VALUE
               AND bfsalesgrpMember.sman EQ sman.sman 
             NO-ERROR .   

         IF AVAILABLE bfsalesgrpMember THEN DO:
            cDeleteCode = bfsalesgrpMember.salesmanID .
            DELETE bfsalesgrpMember.
            
            cbTitle:DELETE(cDeleteCode) IN FRAME {&FRAME-NAME} NO-ERROR.
         END.
         FIND FIRST salesgrpMember EXCLUSIVE-LOCK
             WHERE salesgrpMember.company EQ cocode
               AND salesgrpMember.sman EQ sman.sman 
             NO-ERROR . 
         IF AVAILABLE salesgrpMember THEN
                  cbTitle:SCREEN-VALUE = salesgrpMember.salesmanID .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel dialog-Frame
ON CHOOSE OF btCancel IN FRAME dialog-Frame /* Cancel */
DO:
  ASSIGN cbTitle.

  APPLY 'go' TO  FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk dialog-Frame
ON CHOOSE OF btOk IN FRAME dialog-Frame /* OK */
DO:

  ASSIGN cbTitle.
  
 APPLY 'go' TO  FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTitle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTitle dialog-Frame
ON VALUE-CHANGED OF cbTitle IN FRAME dialog-Frame
DO:
  ASSIGN cbTitle.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dialog-Frame 


/* ***************************  Main Block  *************************** */
  
IF ip-sman-rec-key = ? THEN DO:
    RUN DISABLE_ui.
    APPLY 'close' TO CURRENT-WINDOW.
END.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

    RUN promise-date-init.
 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI dialog-Frame  _DEFAULT-ENABLE
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

 /* {&OPEN-QUERY-dialog-Frame}
  GET FIRST dialog-Frame.*/
  DISPLAY group-desc cbTitle 
      WITH FRAME dialog-Frame.
  ENABLE cbTitle btAddNote btDeleteNote btOk btCancel
      WITH FRAME dialog-Frame.
  VIEW FRAME dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promise-date-init dialog-Frame 
PROCEDURE promise-date-init :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND sman WHERE sman.rec_key = ip-sman-rec-key NO-LOCK NO-ERROR.

 
cbTitle:DELETE("1") IN FRAME {&FRAME-NAME} NO-ERROR.
FOR EACH salesgrpMember NO-LOCK
             WHERE salesgrpMember.company EQ cocode
               AND salesgrpMember.sman    = sman.sman  
     WITH FRAME {&FRAME-NAME}.
    cbTitle:ADD-LAST(salesgrpMember.salesmanID + " - " + salesgrpMember.salesmanName, salesgrpMember.salesmanID).        
   
END.
ASSIGN group-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-sman-code + " - " + (IF AVAILABLE sman THEN sman.sNAME ELSE "")  .
       


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
