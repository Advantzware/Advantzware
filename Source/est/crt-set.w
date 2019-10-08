&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\crt-set.w
  
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
def input param ip-rowid as rowid no-undo.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

def new shared buffer xest for est.
def new shared buffer xeb for eb.
def new shared buffer xef for ef.

DEF SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb.

DEF VAR ll-crt-itemfg AS LOG INIT NO NO-UNDO.
DEF VAR k_frac AS DEC INIT "6.25" NO-UNDO. 
DEF VAR lv-rowid AS ROWID NO-UNDO.

{cec/msfcalc.i}

{sys/inc/setprint.i}
{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH eb SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH eb SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog eb
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog eb


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_stock-no fi_part-no fi_part-dscr1 ~
fi_part-dscr2 fi_procat fi_len fi_wid fi_dep rd_alloc tb_unitize Btn_SAVE ~
RECT-5 
&Scoped-Define DISPLAYED-OBJECTS fi_stock-no fi_part-no fi_part-dscr1 ~
fi_part-dscr2 fi_procat fi_len fi_wid fi_dep rd_alloc tb_unitize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 rd_alloc 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_SAVE AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_dep AS DECIMAL FORMAT ">9.9999" INITIAL 0 
     LABEL "Depth" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_len AS DECIMAL FORMAT ">9.9999" INITIAL 0 
     LABEL "FG Length" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_part-dscr1 AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_part-dscr2 AS CHARACTER FORMAT "x(30)" 
     LABEL "Part Description" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "x(15)" 
     LABEL "Set Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_procat AS CHARACTER FORMAT "x(5)" 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_stock-no AS CHARACTER FORMAT "x(15)" 
     LABEL "Set FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_wid AS DECIMAL FORMAT ">9.9999" INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1 NO-UNDO.

DEFINE VARIABLE rd_alloc AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Assembled", no,
"Unassembled", yes,
"Assembled w/Part Receipts", ?
     SIZE 41 BY 2.62 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 12.62.

DEFINE VARIABLE tb_unitize AS LOGICAL INITIAL no 
     LABEL "Unitize?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi_stock-no AT ROW 1.48 COL 24 COLON-ALIGNED
     fi_part-no AT ROW 2.48 COL 24 COLON-ALIGNED
     fi_part-dscr1 AT ROW 3.48 COL 24 COLON-ALIGNED
     fi_part-dscr2 AT ROW 4.48 COL 24 COLON-ALIGNED
     fi_procat AT ROW 5.52 COL 24 COLON-ALIGNED
     fi_len AT ROW 6.71 COL 24 COLON-ALIGNED
     fi_wid AT ROW 7.71 COL 24 COLON-ALIGNED
     fi_dep AT ROW 8.71 COL 24 COLON-ALIGNED
     rd_alloc AT ROW 9.81 COL 26 NO-LABEL
     tb_unitize AT ROW 12.43 COL 26
     Btn_SAVE AT ROW 14.57 COL 30
     RECT-5 AT ROW 1 COL 1
     "Set Allocation :" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 10.52 COL 7
          FGCOLOR 9 
     SPACE(52.00) SKIP(5.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "SET Information"
         DEFAULT-BUTTON Btn_SAVE.


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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET rd_alloc IN FRAME D-Dialog
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "ASI.eb"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* SET Information */
DO:
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_SAVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SAVE D-Dialog
ON CHOOSE OF Btn_SAVE IN FRAME D-Dialog /* Save */
DO:
    RUN valid-stock-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-part-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    do with frame {&frame-name}:
       ASSIGN fi_stock-no
              fi_part-no
              fi_part-dscr1
              fi_part-dscr2
              fi_procat
              fi_len
              fi_wid
              fi_dep
              rd_alloc
              tb_unitize.

       create tt-eb-set.
       assign tt-eb-set.est-type = IF est.est-type LE 4 THEN 2 ELSE 6
              tt-eb-set.company = est.company
              tt-eb-set.loc = est.loc
              tt-eb-set.est-no = est.est-no
              tt-eb-set.form-no = 0
              tt-eb-set.blank-no = 0
              tt-eb-set.est-int = int(est.est-no)
              tt-eb-set.stock-no = fi_stock-no
              tt-eb-set.part-no = fi_part-no
              tt-eb-set.part-dscr1 = fi_part-dscr1
              tt-eb-set.part-dscr2 = fi_part-dscr2
              tt-eb-set.len = fi_len
              tt-eb-set.wid = fi_wid
              tt-eb-set.dep = fi_dep
              tt-eb-set.procat = fi_procat
              tt-eb-set.set-is-assembled = rd_alloc
              tt-eb-set.pur-man = tb_unitize.

         IF tt-eb-set.set-is-assembled NE ? THEN
           tt-eb-set.set-is-assembled = NOT tt-eb-set.set-is-assembled.    

         IF est.est-type >= 5 THEN DO:
            {sys/inc/k16bb.i tt-eb-set.len  } 
            {sys/inc/k16bb.i tt-eb-set.wid  } 
            {sys/inc/k16bb.i tt-eb-set.dep  } 
         END.
    end.
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-dscr1 D-Dialog
ON HELP OF fi_part-dscr1 IN FRAME D-Dialog /* Item Name */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 4, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-dscr1 D-Dialog
ON LEAVE OF fi_part-dscr1 IN FRAME D-Dialog /* Item Name */
DO:
  /*IF CAN-FIND(FIRST eb
              WHERE eb.company    EQ cocode
                AND eb.loc        EQ locode
                AND eb.part-dscr1 BEGINS fi_part-dscr1:SCREEN-VALUE
                AND ((eb.est-type GE 1 AND eb.est-type LE 4 AND
                      est.est-type GE 1 AND est.est-type LE 4) OR
                     (eb.est-type GT 4 AND est.est-type GT 4))) THEN DO:
    RUN est/l-eb.w (cocode, locode, est.est-type, ?, 4, fi_part-dscr1:SCREEN-VALUE, OUTPUT lv-rowid).
    RUN new-eb (lv-rowid).
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no D-Dialog
ON HELP OF fi_part-no IN FRAME D-Dialog /* Set Cust Part# */
DO:
  RUN est/l-eb.w (cocode, locode, est.est-type, ?, 3, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no D-Dialog
ON LEAVE OF fi_part-no IN FRAME D-Dialog /* Set Cust Part# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-part-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF length(fi_part-no:SCREEN-VALUE in frame {&frame-name}) GT 12 THEN
        MESSAGE "Set Cust Part# should be Limited To:" SKIP
        "12 Characters if there are more than 9 components." SKIP
        "13 Characters if there are less than 9 components." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_procat D-Dialog
ON HELP OF fi_procat IN FRAME D-Dialog /* Category */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   run windows/l-fgcat.w (est.company,focus:screen-value, output char-val).
   if char-val <> "" then 
      assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
               /*        itemfg.procat-desc:screen-value = entry(2,char-val) */
                       .
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_procat D-Dialog
ON LEAVE OF fi_procat IN FRAME D-Dialog /* Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_stock-no D-Dialog
ON HELP OF fi_stock-no IN FRAME D-Dialog /* Set FG Item# */
DO:
  RUN est/l-ebstk.w (cocode, locode, est.est-type, ?, 5, FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
  RUN new-eb (lv-rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_stock-no D-Dialog
ON LEAVE OF fi_stock-no IN FRAME D-Dialog /* Set FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-stock-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rd_alloc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_alloc D-Dialog
ON VALUE-CHANGED OF rd_alloc IN FRAME D-Dialog                      /*Task# 01291403*/
DO:

    ASSIGN rd_alloc.
    IF rd_alloc THEN
      ASSIGN
        tb_unitize:SCREEN-VALUE = "NO"
        tb_unitize = NO.
    ELSE
       ASSIGN
        tb_unitize:SCREEN-VALUE = "YES"
        tb_unitize = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.
find est where rowid(est) = ip-rowid no-lock NO-ERROR.

IF AVAIL est THEN DO:
  {src/adm/template/dialogmn.i}
END.

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
  DISPLAY fi_stock-no fi_part-no fi_part-dscr1 fi_part-dscr2 fi_procat fi_len 
          fi_wid fi_dep rd_alloc tb_unitize 
      WITH FRAME D-Dialog.
  ENABLE fi_stock-no fi_part-no fi_part-dscr1 fi_part-dscr2 fi_procat fi_len 
         fi_wid fi_dep rd_alloc tb_unitize Btn_SAVE RECT-5 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF est.ord-no EQ 0 AND est.ord-date EQ ? THEN.
    ELSE DISABLE fi_stock-no.
    IF est.est-type LE 4 THEN DISABLE tb_unitize.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH tt-eb-set:
    DELETE tt-eb-set.
  END.

  ASSIGN
   rd_alloc   = IF est.est-type GE 5 THEN v-alloc ELSE v-allocf
   tb_unitize = rd_alloc NE YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF est.est-type GE 5 THEN 
  DO:
     IF v-cecscrn-char NE "Decimal" THEN
        ASSIGN
           fi_len:FORMAT IN FRAME {&FRAME-NAME} = ">>9.99" 
           fi_wid:FORMAT IN FRAME {&FRAME-NAME} = ">>9.99"
           fi_dep:FORMAT IN FRAME {&FRAME-NAME} = ">>9.99".
     ELSE
        ASSIGN
           fi_len:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999" 
           fi_wid:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999"
           fi_dep:FORMAT IN FRAME {&FRAME-NAME} = ">>9.999999".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-eb D-Dialog 
PROCEDURE new-eb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR ll-alloc LIKE eb.set-is-assembled NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAIL eb                                         AND
       (eb.stock-no   NE fi_stock-no:SCREEN-VALUE   OR
        eb.part-no    NE fi_part-no:SCREEN-VALUE    OR
        eb.part-dscr1 NE fi_part-dscr1:SCREEN-VALUE)    THEN DO:

      ASSIGN
       fi_stock-no:SCREEN-VALUE   = eb.stock-no
       fi_part-no:SCREEN-VALUE    = eb.part-no
       fi_part-dscr1:SCREEN-VALUE = eb.part-dscr1
       fi_part-dscr2:SCREEN-VALUE = eb.part-dscr2
       fi_procat:SCREEN-VALUE     = eb.procat
       fi_len:SCREEN-VALUE        = STRING(eb.len)
       fi_wid:SCREEN-VALUE        = STRING(eb.wid)
       fi_dep:SCREEN-VALUE        = STRING(eb.dep)
       rd_alloc                   = eb.set-is-assembled
       tb_unitize                 = eb.pur-man.

      IF rd_alloc NE ? THEN rd_alloc = NOT rd_alloc.

      DISPLAY rd_alloc.
    END.
  END.

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
  {src/adm/template/snd-list.i "eb"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no D-Dialog 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF fi_part-no:SCREEN-VALUE EQ "" OR
       CAN-FIND(FIRST eb WHERE eb.company   EQ est.company
                           AND eb.est-no    EQ est.est-no
                           AND (eb.part-no  EQ fi_part-no:SCREEN-VALUE OR
                                eb.stock-no EQ fi_part-no:SCREEN-VALUE)
                           AND eb.form-no   NE 0) THEN DO:
      IF fi_part-no:SCREEN-VALUE EQ "" THEN
        MESSAGE TRIM(fi_part-no:LABEL) + " must be entered..." VIEW-AS ALERT-BOX.
      ELSE
        MESSAGE TRIM(fi_part-no:LABEL) + " already exists on estimate..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fi_part-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat D-Dialog 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    IF fi_procat:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Category must be entered. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fi_procat.
       RETURN ERROR.
    END.
    IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = est.company and
                                      fgcat.procat = fi_procat:screen-value)
    THEN DO:
        MESSAGE "Invalid Category. Try help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fi_procat.
        RETURN ERROR.
    END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-stock-no D-Dialog 
PROCEDURE valid-stock-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-ans AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF fi_stock-no:SCREEN-VALUE NE "" THEN DO:
      IF CAN-FIND(FIRST eb WHERE eb.company  EQ est.company
                             AND eb.est-no   EQ est.est-no
                             AND eb.stock-no EQ fi_stock-no:SCREEN-VALUE 
                             AND eb.form-no  NE 0) THEN DO:
        MESSAGE TRIM(fi_stock-no:LABEL) + " already exists on estimate..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO fi_stock-no.
        RETURN ERROR.
      END.

      ELSE
      IF NOT ll-crt-itemfg THEN DO:
        FIND FIRST itemfg
            WHERE itemfg.company EQ est.company
              AND itemfg.i-no    EQ fi_stock-no:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF NOT AVAIL itemfg THEN DO:
          MESSAGE "This item does not exist, would you like to add it?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
          ll-crt-itemfg = ll-ans.
          IF NOT ll-crt-itemfg THEN RETURN ERROR.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

