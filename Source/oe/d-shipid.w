&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
DEFINE INPUT PARAMETER ipiOrderQty as INTEGER no-undo.
DEFINE INPUT PARAMETER ipcFGItem as CHARACTER no-undo.
def input-output param op-ship-id as cha no-undo.
def input-output param op-ship-from as cha no-undo.

{custom/globdefs.i}
{methods/template/brwcustomdef.i}

DEFINE TEMP-TABLE tt-oe-shipto 
    FIELD ship-from    AS CHARACTER
    FIELD on-hand-qty  AS INTEGER
    FIELD avail-qty   AS INTEGER
    FIELD q-status    AS CHARACTER 
    FIELD i-count     AS INTEGER
    FIELD IS-SELECTED AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX .

DEFINE VARIABLE cbShipFrom AS CHARACTER  NO-UNDO .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME browse-shipto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-oe-shipto

/* Definitions for BROWSE browse-shipto                                 */
&Scoped-define FIELDS-IN-QUERY-browse-shipto tt-oe-shipto.ship-from tt-oe-shipto.on-hand-qty tt-oe-shipto.avail-qty tt-oe-shipto.q-status   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-shipto   
&Scoped-define SELF-NAME browse-shipto
&Scoped-define QUERY-STRING-browse-shipto FOR EACH tt-oe-shipto      NO-LOCK BY tt-oe-shipto.ship-from
&Scoped-define OPEN-QUERY-browse-shipto OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-shipto      NO-LOCK BY tt-oe-shipto.ship-from.
&Scoped-define TABLES-IN-QUERY-browse-shipto tt-oe-shipto
&Scoped-define FIRST-TABLE-IN-QUERY-browse-shipto tt-oe-shipto


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-browse-shipto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-ship-id browse-shipto Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS v-order-id cShipLoc v-ship-id 

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

DEFINE VARIABLE v-order-id AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Qty Ordered" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cShipLoc AS CHARACTER FORMAT "x(20)":U 
     LABEL "Default Whse"
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-ship-id AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To Code" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-shipto FOR 
      tt-oe-shipto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-shipto Dialog-Frame _FREEFORM
  QUERY browse-shipto NO-LOCK DISPLAY
      tt-oe-shipto.ship-from FORMAT "x(8)":U COLUMN-LABEL "Ship From" WIDTH 16
      tt-oe-shipto.on-hand-qty FORMAT "->,>>>,>>9":U COLUMN-LABEL "On Hand"  WIDTH 18
      tt-oe-shipto.avail-qty FORMAT "->,>>>,>>>,>>9":U COLUMN-LABEL "Available" WIDTH 20 
      tt-oe-shipto.q-status COLUMN-LABEL "Status":U WIDTH 8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 70 BY 10.24
         BGCOLOR 8 FONT 3 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-order-id AT ROW 1.38 COL 31.2 COLON-ALIGNED WIDGET-ID 4
     v-ship-id AT ROW 2.60 COL 31.2 COLON-ALIGNED
     cShipLoc AT ROW 3.82 COL 31.2 COLON-ALIGNED 
     browse-shipto AT ROW 5.05 COL 10.6
     Btn_OK AT ROW 16.29 COL 41.2
     SPACE(36.79) SKIP(1.18)
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
/* BROWSE-TAB browse-shipto v-ship-id Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-order-id IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cShipLoc IN FRAME Dialog-Frame
   NO-ENABLE                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-shipto
/* Query rebuild information for BROWSE browse-shipto
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-shipto
     NO-LOCK BY tt-oe-shipto.ship-from.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-shipto */
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


&Scoped-define BROWSE-NAME browse-shipto
&Scoped-define SELF-NAME browse-shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browse-shipto Dialog-Frame
ON ROW-DISPLAY OF browse-shipto IN FRAME Dialog-Frame
DO:   
    &scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i}    
    IF AVAIL tt-oe-shipto THEN DO:
        IF tt-oe-shipto.q-status = "1" THEN DO:
            ASSIGN tt-oe-shipto.q-status:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = "".
            tt-oe-shipto.q-status:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 2 .
        END.
        ELSE IF tt-oe-shipto.q-status = "2" THEN DO:
            ASSIGN tt-oe-shipto.q-status:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = "".
            tt-oe-shipto.q-status:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 14 .
        END.
        ELSE IF tt-oe-shipto.q-status = "3" THEN DO:
            ASSIGN tt-oe-shipto.q-status:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = "".
            tt-oe-shipto.q-status:BGCOLOR IN BROWSE {&BROWSE-NAME}   =  4.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DEFINE VARIABLE counter AS INTEGER NO-UNDO .
    FIND FIRST shipto WHERE shipto.company = g_company
                        AND shipto.cust-no = ip-cust-no
                        AND shipto.ship-id = v-ship-id:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL shipto THEN DO:
       MESSAGE "Invalid Shipto. Try Help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-ship-id.
       RETURN NO-APPLY.
    END.  
    ASSIGN cbShipFrom = "" .
    do counter = 1 to BROWSE browse-shipto:NUM-SELECTED-ROWS:
      BROWSE browse-shipto:FETCH-SELECTED-ROW(counter).
      ASSIGN cbShipFrom = tt-oe-shipto.ship-from .
   end.

    IF cbShipFrom EQ ? OR TRIM(cbShipFrom) EQ "" THEN DO:
        MESSAGE "Please select a ship from location from the browsers." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    IF cbShipFrom EQ ? THEN
        cbShipFrom = "".
    op-ship-id = v-ship-id:screen-value.
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

{methods/template/brwcustom.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  v-ship-id = op-ship-id.
  v-order-id = ipiOrderQty .
  cShipLoc = op-ship-from .

  RUN enable_UI.

  RUN setup-ship-from.
 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAlloc      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBack       AS INTEGER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ g_company 
               AND itemfg.i-no EQ ipcFGItem  NO-ERROR .
           
        FOR EACH tt-oe-shipto NO-LOCK:
            FOR EACH itemfg-loc NO-LOCK
                WHERE itemfg-loc.company EQ g_company
                AND itemfg-loc.i-no    EQ ipcFGItem
                AND itemfg-loc.loc     EQ tt-oe-shipto.ship-from ,
                FIRST loc NO-LOCK
                WHERE loc.company EQ itemfg-loc.company
                AND loc.loc     EQ itemfg-loc.loc
                :
                RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT iAlloc, OUTPUT iBack).
                ASSIGN
                    tt-oe-shipto.on-hand-qty  = itemfg-loc.q-onh
                    tt-oe-shipto.avail-qty   = itemfg-loc.q-onh + itemfg-loc.q-ono - iAlloc .
                
                IF ipiOrderQty LE itemfg-loc.q-onh THEN
                    tt-oe-shipto.q-status = "1".
                ELSE IF ipiOrderQty GT itemfg-loc.q-onh AND ipiOrderQty LT tt-oe-shipto.avail-qty  THEN
                    tt-oe-shipto.q-status = "2".
                ELSE IF  (ipiOrderQty GT itemfg-loc.q-onh OR ipiOrderQty GT tt-oe-shipto.avail-qty) THEN
                    tt-oe-shipto.q-status = "3".
            END.
            IF tt-oe-shipto.q-status EQ "" THEN
                    ASSIGN tt-oe-shipto.q-status = "3" .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY v-order-id cShipLoc v-ship-id 
      WITH FRAME Dialog-Frame.
  ENABLE v-ship-id browse-shipto Btn_OK 
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
  DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    /* Empty the selection-list or combo-box */
      /*cbShipFrom:LIST-ITEM-PAIRS = ?.
      cbShipFrom:SCREEN-VALUE = "":U.*/
      cLocList = "".
      liNumUsrx = 0.
      EMPTY TEMP-TABLE tt-oe-shipto .  
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
          
          CREATE tt-oe-shipto. 
          ASSIGN tt-oe-shipto.ship-from = caps(usrx.loc) .

      END.
      cLocList = TRIM(cLocList, ",").

      /* If the ship from value given in the input parameter is on the users */
      /* list, then default to it */
      IF LOOKUP(op-ship-from, cLocList) GT 0 AND op-ship-from NE "" THEN
        ASSIGN liNumUsrx = 1
               lcLastValue = op-ship-from.
      IF liNumUsrx EQ 1 THEN
          cbShipFrom = lcLastValue.
      IF liNumUsrx EQ 0 THEN DO:
          /* Allow all */
          FOR EACH loc WHERE loc.company EQ g_company               
              NO-LOCK:
             CREATE tt-oe-shipto. 
             ASSIGN tt-oe-shipto.ship-from = caps(loc.loc) .
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
                  cbShipFrom = loc.loc.
          END.
          ELSE DO: /* MOre than one location */
              v-ship-id = op-ship-id . 
              
              FIND FIRST shipto NO-LOCK
                  WHERE shipto.company EQ g_company
                  AND shipto.ship-id EQ v-ship-id
                  AND shipto.cust-no EQ ip-cust-no  NO-ERROR.
              
              IF AVAIL shipto THEN
                 cbShipFrom = shipto.loc.

            IF NOT AVAIL shipto THEN do:
                v-ship-id = "".
                
                FOR EACH shipto
                    WHERE shipto.company EQ g_company
                    AND shipto.cust-no EQ ip-cust-no
                    NO-LOCK
                    BREAK BY shipto.ship-no DESC:

                    IF shipto.ship-id EQ ip-cust-no THEN DO:
                        v-ship-id = shipto.ship-id.
                        cbShipFrom = shipto.loc.
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
                     cbShipFrom = shipto.loc.
                   LEAVE.
                 END. /* each shipto */
              END. /* if blank v-ship-id */
            END. /* not avail shipto */
          END. /* ... else (more than one location) */
    
      END. /* if num-usrx = 0 */
  END. /* do */

  RUN build-table. 

   CLOSE QUERY browse-shipto.
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY browse-shipto FOR EACH tt-oe-shipto
            NO-LOCK BY tt-oe-shipto.ship-from.

            FIND FIRST tt-oe-shipto NO-LOCK
                WHERE tt-oe-shipto.ship-from EQ cbShipFrom NO-ERROR .
            
            IF AVAIL tt-oe-shipto THEN
               rwRowid = ROWID(tt-oe-shipto) .

            REPOSITION {&browse-name} TO ROWID rwRowid NO-ERROR.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

