&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-company   AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-cust-no   AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-ship-id   AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-shipnotes AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-shipnotesreckey AS CHAR NO-UNDO.


/* Local Variable Definitions ---                                       */

DEF VAR v-ord    AS INT  FORMAT ">>>>>9"     EXTENT 2 NO-UNDO.
DEF VAR v-date   AS DATE FORMAT "99/99/9999" EXTENT 2 NO-UNDO.
DEF VAR v-ship-i AS CHAR FORMAT "x(60)"      EXTENT 4 NO-UNDO.
Define Variable hNotesProc as Handle NO-UNDO. 

DEF BUFFER bf-shipto FOR shipto.

RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 fl_ordfr fl_ordto fl_datefr ~
fl_dateto tgl_BOL tgl_release tgl_order Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fl_ordfr fl_ordto fl_datefr fl_dateto ~
tgl_BOL tgl_release tgl_order 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fl_datefr AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "    From Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE fl_dateto AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE fl_ordfr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "From Order #" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE fl_ordto AS INTEGER FORMAT ">>>>>9":U INITIAL 999999 
     LABEL "To Order #" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 8.81.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 2.14.

DEFINE VARIABLE tgl_BOL AS LOGICAL INITIAL no 
     LABEL "Update Ship Notes of open BOL" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1.19 NO-UNDO.

DEFINE VARIABLE tgl_order AS LOGICAL INITIAL no 
     LABEL "Update Ship Notes of open Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1.19 NO-UNDO.

DEFINE VARIABLE tgl_release AS LOGICAL INITIAL no 
     LABEL "Update Ship Notes of open Release Tickets" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fl_ordfr AT ROW 1.95 COL 15.4 COLON-ALIGNED
     fl_ordto AT ROW 1.95 COL 44.6 COLON-ALIGNED
     fl_datefr AT ROW 3.38 COL 15.4 COLON-ALIGNED
     fl_dateto AT ROW 3.38 COL 44.6 COLON-ALIGNED
     tgl_BOL AT ROW 5.52 COL 8.6
     tgl_release AT ROW 6.86 COL 8.6
     tgl_order AT ROW 8.24 COL 8.6
     Btn_OK AT ROW 10.48 COL 13.2
     Btn_Cancel AT ROW 10.48 COL 37.2
     RECT-3 AT ROW 1.24 COL 2
     RECT-4 AT ROW 10.05 COL 2
     SPACE(1.19) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update Ship Notes"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Ship Notes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DEF VAR v-flg AS LOG.
    DEF VAR v-cnt AS INT.

    DEF VAR v-shipnotes LIKE ip-shipnotes NO-UNDO.

    ASSIGN Btn_OK:SENSITIVE = FALSE.

    ASSIGN
        fl_ordfr
        fl_ordto
        fl_datefr
        fl_dateto
        tgl_BOL
        tgl_release
        tgl_order
        
        v-ord[1] = fl_ordfr
        v-ord[2] = fl_ordto
        v-date[1] =  fl_datefr
        v-date[2] =  fl_dateto

        v-shipnotes =  ip-shipnotes

        v-ship-i[1] = SUBSTR(v-shipnotes,1,INDEX(v-shipnotes,"|") - 1)
        v-shipnotes = TRIM(SUBSTR(v-shipnotes,INDEX(v-shipnotes,"|") + 1))
        v-ship-i[2] = SUBSTR(v-shipnotes,1,INDEX(v-shipnotes,"|") - 1)
        v-shipnotes = TRIM(SUBSTR(v-shipnotes,INDEX(v-shipnotes,"|") + 1))
        v-ship-i[3] = SUBSTR(v-shipnotes,1,INDEX(v-shipnotes,"|") - 1)
        v-shipnotes = TRIM(SUBSTR(v-shipnotes,INDEX(v-shipnotes,"|") + 1))
        v-ship-i[4] = TRIM(v-shipnotes).

    RUN toggle-values(NO).

    IF tgl_BOL THEN RUN ip-upd-BOL.

    IF tgl_release THEN RUN ip-upd-REL.

    IF tgl_order THEN RUN ip-upd-ORDRel.

    IF NOT tgl_BOL     AND 
       NOT tgl_release AND
       NOT tgl_order 
      THEN DO:

        MESSAGE "Ship Notes of open BOL, Release Tickets and Orders for " SKIP
            "   Ship Id " bf-shipto.ship-id " are NOT updated..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "CHOOSE" TO Btn_Cancel IN FRAME {&FRAME-NAME}.

    END.
    ELSE DO:

        MESSAGE "Update Complete..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.


        APPLY "CHOOSE" TO Btn_Cancel IN FRAME {&FRAME-NAME}.

    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgl_BOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl_BOL Dialog-Frame
ON VALUE-CHANGED OF tgl_BOL IN FRAME Dialog-Frame /* Update Ship Notes of open BOL */
DO:
  ASSIGN tgl_BOL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgl_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl_order Dialog-Frame
ON VALUE-CHANGED OF tgl_order IN FRAME Dialog-Frame /* Update Ship Notes of open Orders */
DO:
  ASSIGN tgl_order.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgl_release
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl_release Dialog-Frame
ON VALUE-CHANGED OF tgl_release IN FRAME Dialog-Frame /* Update Ship Notes of open Release Tickets */
DO:
  ASSIGN tgl_release.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN toggle-values(YES).

  FIND FIRST bf-shipto NO-LOCK
      WHERE bf-shipto.company EQ ip-company 
        AND bf-shipto.cust-no EQ ip-cust-no 
        AND bf-shipto.ship-id EQ ip-ship-id NO-ERROR.
  IF NOT AVAIL bf-shipto THEN DO:
      MESSAGE 
          "Internal error... Ship ID not found"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.

      APPLY "CHOOSE" TO Btn_Cancel IN FRAME {&FRAME-NAME}.
  END.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

END.
IF VALID-HANDLE(hNotesProc) THEN  
    DELETE OBJECT hNotesProc.
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
  DISPLAY fl_ordfr fl_ordto fl_datefr fl_dateto tgl_BOL tgl_release tgl_order 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 RECT-4 fl_ordfr fl_ordto fl_datefr fl_dateto tgl_BOL 
         tgl_release tgl_order Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-upd-BOL Dialog-Frame 
PROCEDURE ip-upd-BOL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH oe-bolh EXCLUSIVE-LOCK
        WHERE oe-bolh.company  EQ bf-shipto.company
          AND oe-bolh.posted   EQ NO
          AND oe-bolh.ord-no   GE v-ord[1]
          AND oe-bolh.ord-no   LE v-ord[2]
          AND oe-bolh.cust-no  EQ bf-shipto.cust-no
          AND oe-bolh.bol-date GE v-date[1]
          AND oe-bolh.bol-date LE v-date[2]          
          AND oe-bolh.ship-id  EQ bf-shipto.ship-id
          AND oe-bolh.deleted  EQ NO
        USE-INDEX post-ord:

        ASSIGN
            oe-bolh.ship-i[1] = v-ship-i[1]
            oe-bolh.ship-i[2] = v-ship-i[2]
            oe-bolh.ship-i[3] = v-ship-i[3]
            oe-bolh.ship-i[4] = v-ship-i[4].
            
        RUN CopyShipNote IN hNotesProc (INPUT ip-shipnotesreckey, oe-bolh.rec_key).
        
                             
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-upd-ORDrel Dialog-Frame 
PROCEDURE ip-upd-ORDrel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company  EQ bf-shipto.company
          AND oe-ord.ord-no   GE v-ord[1]
          AND oe-ord.ord-no   LE v-ord[2]
          AND oe-ord.cust-no  EQ bf-shipto.cust-no
          AND oe-ord.ord-date GE v-date[1]
          AND oe-ord.ord-date LE v-date[2],
          
      EACH oe-rel EXCLUSIVE-LOCK
          WHERE oe-rel.company EQ oe-ord.company
            AND oe-rel.ord-no  EQ oe-ord.ord-no
            AND oe-rel.ship-id EQ bf-shipto.ship-id
            AND oe-rel.stat    NE "C" 
            AND oe-rel.stat    NE "Z" :

        ASSIGN
            oe-rel.ship-i[1] = v-ship-i[1] 
            oe-rel.ship-i[2] = v-ship-i[2] 
            oe-rel.ship-i[3] = v-ship-i[3] 
            oe-rel.ship-i[4] = v-ship-i[4].
            
         RUN CopyShipNote IN hNotesProc (INPUT ip-shipnotesreckey, oe-rel.rec_key).   
            
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-upd-REL Dialog-Frame 
PROCEDURE ip-upd-REL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

     FOR EACH oe-relh EXCLUSIVE-LOCK
        WHERE oe-relh.company  EQ bf-shipto.company
          AND oe-relh.cust-no  EQ bf-shipto.cust-no
          AND oe-relh.ship-id  EQ bf-shipto.ship-id
          AND oe-relh.rel-date GE v-date[1]
          AND oe-relh.rel-date LE v-date[2]
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.ord-no   GE v-ord[1]
          AND oe-relh.ord-no   LE v-ord[2]
        USE-INDEX cust:
        
        ASSIGN
            oe-relh.ship-i[1] = v-ship-i[1]
            oe-relh.ship-i[2] = v-ship-i[2]
            oe-relh.ship-i[3] = v-ship-i[3]
            oe-relh.ship-i[4] = v-ship-i[4].

         RUN CopyShipNote IN hNotesProc (INPUT ip-shipnotesreckey, oe-relh.rec_key).

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toggle-values Dialog-Frame 
PROCEDURE toggle-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip_value AS LOG NO-UNDO.

FIND FIRST reftable EXCLUSIVE-LOCK
    WHERE reftable.reftable EQ "d-shp2nt" 
      AND reftable.company  EQ ip-company 
      AND reftable.loc      EQ "BOL" NO-ERROR.
IF NOT AVAIL reftable THEN DO:

    CREATE reftable.
    ASSIGN
        reftable.reftable = "d-shp2nt"
        reftable.company  = ip-company
        reftable.loc      = "BOL".

END.
ELSE DO:

    IF NOT ip_value 
      THEN ASSIGN reftable.code = tgl_BOL:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                  reftable.code = IF reftable.code EQ ""
                                    THEN "NO" ELSE reftable.code.

    ASSIGN 
     tgl_BOL:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF reftable.code EQ ""
                                                     THEN "NO" 
                                                     ELSE reftable.code.

END.
    

RELEASE reftable.


FIND FIRST reftable EXCLUSIVE-LOCK
    WHERE reftable.reftable EQ "d-shp2nt" 
      AND reftable.company  EQ ip-company 
      AND reftable.loc      EQ "Releases" NO-ERROR.
IF NOT AVAIL reftable THEN DO:

    CREATE reftable.
    ASSIGN
        reftable.reftable = "d-shp2nt"
        reftable.company  = ip-company
        reftable.loc      = "Releases".

END.
ELSE DO:

    IF NOT ip_value 
      THEN ASSIGN reftable.code = tgl_release:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                  reftable.code = IF reftable.code EQ ""
                                    THEN "NO" ELSE reftable.code.

    ASSIGN 
     tgl_release:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF reftable.code EQ ""
                                                        THEN "NO" 
                                                        ELSE reftable.code.

END.

RELEASE reftable.

FIND FIRST reftable EXCLUSIVE-LOCK
    WHERE reftable.reftable EQ "d-shp2nt" 
      AND reftable.company  EQ ip-company 
      AND reftable.loc      EQ "Orders" NO-ERROR.
IF NOT AVAIL reftable THEN DO:

    CREATE reftable.
    ASSIGN
        reftable.reftable = "d-shp2nt"
        reftable.company  = ip-company
        reftable.loc      = "Orders".

END.
ELSE DO:

    IF NOT ip_value 
      THEN ASSIGN reftable.code = tgl_order:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                  reftable.code = IF reftable.code EQ ""
                                    THEN "NO" ELSE reftable.code.

    ASSIGN 
     tgl_order:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF reftable.code EQ ""
                                                        THEN "NO" 
                                                        ELSE reftable.code.

END.
   
RELEASE reftable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

