&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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

/* Local Variable Definitions ---                                       */
def input param ip-cust-no as cha no-undo.
def input-output param op-ship-id as cha no-undo.
def input-output param op-ship-from as cha no-undo.

{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-ship-id cbShipFrom Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS v-ship-id cbShipFrom 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO  NO-CONVERT-3D-COLORS
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cbShipFrom AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ship From" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE v-ship-id AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To Code" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-ship-id AT ROW 2.91 COL 22 COLON-ALIGNED
     cbShipFrom AT ROW 4.33 COL 22 COLON-ALIGNED WIDGET-ID 2
     Btn_OK AT ROW 6.48 COL 24
     SPACE(26.13) SKIP(4.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Ship To Code"
         DEFAULT-BUTTON Btn_OK.


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Ship To Code */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    FIND FIRST shipto WHERE shipto.company = g_company
                        AND shipto.cust-no = ip-cust-no
                        AND shipto.ship-id = v-ship-id:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL shipto THEN DO:
       MESSAGE "Invalid Shipto. Try Help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-ship-id.
       RETURN NO-APPLY.
    END.  

    ASSIGN cbShipFrom.

    IF cbShipFrom EQ ? OR TRIM(cbShipFrom) EQ "" THEN DO:
        MESSAGE "Please select a ship from location from the drop-down." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO cbShipFrom.
        RETURN NO-APPLY.
    END.

    IF cbShipFrom EQ ? THEN
        cbShipFrom = "".
    op-ship-id = v-ship-id:screen-value.
    op-ship-from = cbShipFrom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbShipFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbShipFrom Dialog-Frame
ON LEAVE OF cbShipFrom IN FRAME Dialog-Frame /* Ship From */
DO:
  ASSIGN {&SELF}.
  op-ship-from = cbShipFrom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbShipFrom Dialog-Frame
ON VALUE-CHANGED OF cbShipFrom IN FRAME Dialog-Frame /* Ship From */
DO:
  ASSIGN {&SELF}.
  op-ship-from = cbShipFrom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-id Dialog-Frame
ON HELP OF v-ship-id IN FRAME Dialog-Frame /* Ship To Code */
DO:
    def var char-val as cha no-undo.
    run windows/l-shipto.w (g_company, g_loc, ip-cust-no, focus:screen-value, output char-val).
    if char-val <> "" then self:screen-value = entry(1,char-val).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-id Dialog-Frame
ON LEAVE OF v-ship-id IN FRAME Dialog-Frame /* Ship To Code */
DO:
    FIND FIRST shipto WHERE shipto.company = g_company
                        AND shipto.cust-no = ip-cust-no
                        AND shipto.ship-id = SELF:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL shipto THEN DO:
       MESSAGE "Invalid Shipto. Try Help." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

DEF VAR v-return AS LOG NO-UNDO.
DEF VAR lcLastValue AS CHAR NO-UNDO.
DEF VAR liNumUsrx AS INT NO-UNDO.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  v-ship-id = op-ship-id.
  RUN enable_UI.

  RUN setup-ship-from.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY v-ship-id cbShipFrom 
      WITH FRAME Dialog-Frame.
  ENABLE v-ship-id cbShipFrom Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setup-ship-from Dialog-Frame 
PROCEDURE setup-ship-from :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR liNumUsrx AS INT NO-UNDO.
  DEF VAR v-Ship-Id AS CHAR NO-UNDO.
  DEF VAR cLocList AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    /* Empty the selection-list or combo-box */
      cbShipFrom:LIST-ITEM-PAIRS = ?.
      cbShipFrom:SCREEN-VALUE = "":U.
      cLocList = "".
      liNumUsrx = 0.
      FOR EACH usrx WHERE usrx.company EQ g_company
           AND usrx.uid EQ USERID("NOSWEAT")
          NO-LOCK:
          FIND loc WHERE loc.company EQ usrx.company
                     AND loc.loc     EQ usrx.loc
                   NO-LOCK NO-ERROR.
          IF NOT AVAIL loc THEN
              NEXT.
          liNumUsrx = liNumUsrx + 1.
          cLocList = cLocList + usrx.loc + ",".
          lcLastValue = usrx.loc /* + " " + replace(loc.dscr, ",", " ") */.
          v-return = cbShipFrom:ADD-LAST(caps(usrx.loc) + "                     " + replace(loc.dscr, ",", " "), usrx.loc).            
      END.
      cLocList = TRIM(cLocList, ",").

      /* If the ship from value given in the input parameter is on the users */
      /* list, then default to it */
      IF LOOKUP(op-ship-from, cLocList) GT 0 AND op-ship-from NE "" THEN
        ASSIGN liNumUsrx = 1
               lcLastValue = op-ship-from.
      IF liNumUsrx EQ 1 THEN
          cbShipFrom:SCREEN-VALUE = lcLastValue.
      IF liNumUsrx EQ 0 THEN DO:
          /* Allow all */
          FOR EACH loc WHERE loc.company EQ g_company               
              NO-LOCK:
             v-return = cbShipFrom:ADD-LAST(caps(loc.loc) + "                     " + replace(loc.dscr, ",", " "), loc.loc).            
          END.

          /* take default */
          liNumUsrx = 0.
          FOR EACH loc 
              WHERE loc.company EQ g_company
              NO-LOCK.
              liNumUsrx = liNumUsrx + 1.
          END.
    
          IF liNumUsrx = 1 THEN DO:
              FIND FIRST loc WHERE loc.company EQ g_company NO-LOCK NO-ERROR.
              IF AVAIL loc THEN 
                  cbShipFrom:SCREEN-VALUE = loc.loc.
          END.
          ELSE DO: /* MOre than one location */
              v-ship-id = "".
              FOR EACH shipto
                 WHERE shipto.company EQ g_company
                   AND shipto.cust-no EQ ip-cust-no
                 NO-LOCK
                 BREAK BY shipto.ship-no DESC:
    
                 IF shipto.ship-id EQ ip-cust-no THEN DO:
                   v-ship-id = shipto.ship-id.
                   LEAVE.
                 END.
    
              END. /* each shipto */
    
              IF v-ship-id EQ "" THEN DO:
                 FOR EACH shipto
                   WHERE shipto.company EQ g_company
                     AND shipto.cust-no EQ ip-cust-no
                   NO-LOCK
                   BREAK BY shipto.ship-no DESC:
                     v-ship-id = shipto.ship-id.
                   LEAVE.
                 END. /* each shipto */
              END. /* if blank v-ship-id */
              
              IF v-ship-id GT "" THEN
                  FIND FIRST shipto WHERE shipto.company EQ g_company
                   AND shipto.ship-id EQ v-ship-id  NO-LOCK NO-ERROR.
              IF AVAIL shipto THEN
                 cbShipFrom:SCREEN-VALUE = shipto.loc.
          END. /* ... else (more than one location) */
    
      END. /* if num-usrx = 0 */
  END. /* do */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

