&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fg/fglevels.w

  Description: FG Items Levels F/M

  Input Parameters: BUFFER itemfg, BUFFER itemfg

  Output Parameters: Logical Updated

  Author: Ron Stark

  Created: 10.4.2018
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER iprItemFGRowID    AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iprItemFGLocRowID AS ROWID NO-UNDO.

DEFINE OUTPUT PARAMETER oplUpdated AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cUOMList AS CHARACTER NO-UNDO INITIAL "C,CS,EA,L,M".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame itemfg.pur-uom itemfg.beg-date 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame itemfg.pur-uom ~
itemfg.beg-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame itemfg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame itemfg
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH itemfg SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH itemfg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame itemfg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg-loc.ord-level itemfg-loc.ord-max ~
itemfg-loc.ord-min itemfg.pur-uom itemfg-loc.lead-days itemfg.beg-date 
&Scoped-define ENABLED-TABLES itemfg-loc itemfg
&Scoped-define FIRST-ENABLED-TABLE itemfg-loc
&Scoped-define SECOND-ENABLED-TABLE itemfg
&Scoped-Define ENABLED-OBJECTS btnOK btnCancel 
&Scoped-Define DISPLAYED-FIELDS itemfg-loc.ord-level itemfg-loc.ord-max ~
itemfg-loc.ord-min itemfg.pur-uom itemfg-loc.lead-days itemfg.beg-date 
&Scoped-define DISPLAYED-TABLES itemfg-loc itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg-loc
&Scoped-define SECOND-DISPLAYED-TABLE itemfg


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 84 BY 3.81.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 5.52 COL 68 WIDGET-ID 18
     btnCancel AT ROW 5.52 COL 77 WIDGET-ID 16
     itemfg-loc.ord-level AT ROW 1.48 COL 21 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1
     itemfg-loc.ord-max AT ROW 2.67 COL 21 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1
     itemfg-loc.ord-min AT ROW 3.86 COL 21 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1
     itemfg.pur-uom AT ROW 1.48 COL 67 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
          BGCOLOR 15 FONT 1
     itemfg-loc.lead-days AT ROW 2.67 COL 67 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 FONT 1
     itemfg.beg-date AT ROW 3.86 COL 67 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 1
     RECT-28 AT ROW 5.29 COL 67 WIDGET-ID 14
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 20
     SPACE(0.00) SKIP(2.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Levels for Location:" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-28 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.itemfg"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Levels for Location: */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    RUN pUpdate.
    oplUpdated = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.pur-uom Dialog-Frame
ON HELP OF itemfg.pur-uom IN FRAME Dialog-Frame /* Purchased UOM */
DO:
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    
    RUN windows/l-stduom.w (itemfg.company ,cUOMList, SELF:SCREEN-VALUE, OUTPUT cReturnValue).
    IF cReturnValue NE "" THEN
    SELF:SCREEN-VALUE = CAPS(ENTRY(1,cReturnValue)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.pur-uom Dialog-Frame
ON LEAVE OF itemfg.pur-uom IN FRAME Dialog-Frame /* Purchased UOM */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN pValidatePurUOM NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END. /* if lastkey */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FIND itemfg     NO-LOCK WHERE ROWID(itemfg)     EQ iprItemFGRowID.
FIND itemfg-loc NO-LOCK WHERE ROWID(itemfg-loc) EQ iprItemFGLocRowID.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + itemfg-loc.loc.
  RUN sys/ref/uom-fg.p (NO, OUTPUT cUOMList).
  RUN enable_UI.
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE itemfg THEN 
    DISPLAY itemfg.pur-uom itemfg.beg-date 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE itemfg-loc THEN 
    DISPLAY itemfg-loc.ord-level itemfg-loc.ord-max itemfg-loc.ord-min 
          itemfg-loc.lead-days 
      WITH FRAME Dialog-Frame.
  ENABLE btnOK btnCancel itemfg-loc.ord-level itemfg-loc.ord-max 
         itemfg-loc.ord-min itemfg.pur-uom itemfg-loc.lead-days itemfg.beg-date 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdate Dialog-Frame 
PROCEDURE pUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bEB FOR eb.
    
    DO WITH FRAME {&FRAME-NAME}:
        FIND CURRENT itemfg     EXCLUSIVE-LOCK.
        FIND CURRENT itemfg-loc EXCLUSIVE-LOCK.
        ASSIGN {&ENABLED-FIELDS}.
        FIND CURRENT itemfg-loc NO-LOCK.
        FIND CURRENT itemfg     NO-LOCK.
        FIND FIRST fg-set NO-LOCK
             WHERE fg-set.company EQ itemfg.company 
               AND fg-set.set-no  EQ itemfg.i-no
             NO-ERROR.
        IF AVAILABLE itemfg AND AVAILABLE fg-set THEN
        FOR EACH eb NO-LOCK
            WHERE eb.company  EQ itemfg.company
              AND eb.cust-no  EQ itemfg.cust-no
              AND eb.stock-no EQ itemfg.i-no
            :
            FIND FIRST bEB EXCLUSIVE-LOCK
                 WHERE ROWID(bEB) EQ ROWID(eb)
                 NO-WAIT NO-ERROR.
            IF AVAILABLE bEB THEN
            bEB.pur-man = itemfg.pur-man.
        END. /* each eb */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidatePurUOM Dialog-Frame 
PROCEDURE pValidatePurUOM :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        itemfg.pur-uom:SCREEN-VALUE = CAPS(itemfg.pur-uom:SCREEN-VALUE).
        /* take out per Joe - task 10021210 */ /* ticket 24648 */
        IF NOT CAN-FIND(FIRST uom
                        WHERE uom.uom EQ itemfg.pur-uom:SCREEN-VALUE  
                          AND CAN-DO(cUOMList, uom.uom)) THEN DO:
            MESSAGE
                TRIM(itemfg.pur-uom:LABEL) + " is invalid, try help..."       
            VIEW-AS ALERT-BOX ERROR.                                          
            RETURN ERROR.                                                         
        END. /* if not can-find */                                                                    
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPurMan Dialog-Frame
PROCEDURE SetPurMan:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplSetHeader AS LOGICAL NO-UNDO.
    
/*    RUN SetPurMan(itemfg.isaset).*/
    
    
/*    DO WITH FRAME {&FRAME-NAME}:                                               */
/*        IF iplSetHeader THEN DO:                                               */
/*            itemfg.pur-man:REPLACE("Unitized",YES,"Purchased") NO-ERROR.       */
/*            itemfg.pur-man:REPLACE("Not Unitized",NO,"Manufactured") NO-ERROR. */
/*            itemfg.pur-man:HELP = "Is the Set Unitized?".                      */
/*        END.                                                                   */
/*        ELSE DO:                                                               */
/*            itemfg.pur-man:REPLACE("Purchased",YES,"Unitized") NO-ERROR.       */
/*            itemfg.pur-man:REPLACE("Manufactured",NO,"Not Unitized") NO-ERROR. */
/*            itemfg.pur-man:HELP = "Is the Item (P)urchased or (M)anufactured?".*/
/*        END. /* else */                                                        */
/*    END. /* with frame */                                                      */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


