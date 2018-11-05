&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: addon/rfq/d-rfqset.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-est-type AS INT NO-UNDO.

DEF BUFFER bf-item FOR rfqitem.
DEF VAR lv-set-recid AS RECID NO-UNDO.
DEF VAR lv-new-set AS LOG NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR ld-sqin AS DEC NO-UNDO.
DEF VAR ld-msf AS DEC NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

{cec/msfcalc.i}

{sys/inc/setprint.i}

def var k_frac as dec init "6.25" no-undo.

{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rfqitem

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame rfqitem.stock-no ~
rfqitem.part-no rfqitem.part-dscr1 rfqitem.part-dscr2 rfqitem.procat ~
rfqitem.len rfqitem.wid rfqitem.dep 
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH rfqitem SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH rfqitem SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame rfqitem
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame rfqitem


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 btn_qty-msf btn_update Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS rfqitem.stock-no rfqitem.part-no ~
rfqitem.part-dscr1 rfqitem.part-dscr2 rfqitem.procat rfqitem.len ~
rfqitem.wid rfqitem.dep 
&Scoped-define DISPLAYED-TABLES rfqitem
&Scoped-define FIRST-DISPLAYED-TABLE rfqitem
&Scoped-Define DISPLAYED-OBJECTS rd_alloc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 rfqitem.stock-no rfqitem.part-no rfqitem.part-dscr1 ~
rfqitem.part-dscr2 rfqitem.procat rfqitem.len rfqitem.wid rfqitem.dep ~
rd_alloc 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Close" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn_qty-msf 
     LABEL "" 
     SIZE 58 BY 1.

DEFINE BUTTON btn_update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_msf AS DECIMAL FORMAT "->>,>>9.999":U INITIAL 0 
     LABEL "MSF" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE rd_alloc AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Assembled", "A",
"Unassembled", "U"
     SIZE 41 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74.6 BY 12.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      rfqitem SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rfqitem.stock-no AT ROW 1.71 COL 20 COLON-ALIGNED
          LABEL "Set FG Item#"
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
     rfqitem.part-no AT ROW 2.67 COL 20 COLON-ALIGNED
          LABEL "Set Cust Part#"
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
     rfqitem.part-dscr1 AT ROW 3.62 COL 20 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     rfqitem.part-dscr2 AT ROW 4.57 COL 20 COLON-ALIGNED
          LABEL "Part Description"
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     rfqitem.procat AT ROW 5.76 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.len AT ROW 6.76 COL 20 COLON-ALIGNED
          LABEL "F.G. Length"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.wid AT ROW 7.76 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.dep AT ROW 8.81 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     btn_qty-msf AT ROW 10.29 COL 10
     rfqitem.qty[99] AT ROW 10.29 COL 20 COLON-ALIGNED
          LABEL "Qty"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     fi_msf AT ROW 10.29 COL 46 COLON-ALIGNED
     rd_alloc AT ROW 11.71 COL 27 NO-LABEL
     btn_update AT ROW 14.33 COL 15
     Btn_Cancel AT ROW 14.33 COL 51
     "Set Allocation :" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 11.71 COL 8
          FGCOLOR 9 
     RECT-5 AT ROW 1 COL 1
     SPACE(0.59) SKIP(2.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "SET Information"
         CANCEL-BUTTON Btn_Cancel.


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN rfqitem.dep IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi_msf IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_msf:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN rfqitem.len IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN rfqitem.part-dscr1 IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN rfqitem.part-dscr2 IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN rfqitem.part-no IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN rfqitem.procat IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfqitem.qty[99] IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
ASSIGN 
       rfqitem.qty[99]:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR RADIO-SET rd_alloc IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfqitem.stock-no IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN rfqitem.wid IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "rfq.rfqitem"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* SET Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Close */
DO:
  IF SELF:LABEL EQ "&Cancel" THEN RUN validate-set.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_qty-msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_qty-msf Dialog-Frame
ON CHOOSE OF btn_qty-msf IN FRAME Dialog-Frame
DO:
  IF AVAIL eb THEN RUN est/d-estmsf.w (ROWID(eb)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_update Dialog-Frame
ON CHOOSE OF btn_update IN FRAME Dialog-Frame /* Update */
DO:
    IF SELF:LABEL = "&Update" THEN DO:
  /*     RUN check-use NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN.
    */
      SELF:LABEL = "&Save".
      btn_cancel:LABEL = "&Cancel".

      DO WITH FRAME {&FRAME-NAME}:
         ENABLE {&list-1}.
      /*   IF bf-est.ord-no eq 0 AND bf-est.ord-date EQ ? THEN
           APPLY "entry" TO eb.stock-no.
         ELSE DO:
           DISABLE eb.stock-no.
           APPLY "entry" TO eb.part-no.
         END.       */
       END.
    END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
         RUN valid-part-no NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
         RUN valid-procat NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
         FIND CURRENT rfqitem EXCLUSIVE-LOCK.
         ASSIGN {&DISPLAYED-FIELDS}.

         IF ip-est-type >= 5 THEN DO:
            {sys/inc/k16bb.i rfqitem.len  } 
            {sys/inc/k16bb.i rfqitem.wid  } 
            {sys/inc/k16bb.i rfqitem.dep  } 
         END.

         RUN validate-set.
         ASSIGN rd_alloc.
         FIND FIRST itemfg WHERE itemfg.company = rfqitem.company
                      AND itemfg.i-no = rfqitem.stock-no EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL itemfg THEN
         DO:
            itemfg.alloc = rd_alloc EQ "U".
            FIND CURRENT itemfg NO-LOCK.
         END.

         APPLY "go" TO FRAME {&frame-name}.
    END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.procat Dialog-Frame
ON HELP OF rfqitem.procat IN FRAME Dialog-Frame /* Category */
DO:
  DEF VAR char-val AS cha NO-UNDO.
   run windows/l-fgcat.w (rfqitem.company,focus:screen-value, output char-val).
   if char-val <> "" then 
      assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
               /*        itemfg.procat-desc:screen-value = entry(2,char-val) */.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEF VAR ll AS LOG NO-UNDO.

SESSION:DATA-ENTRY-RETURN = YES.
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   FIND FIRST rfqitem WHERE RECID(rfqitem) = ip-recid NO-LOCK NO-ERROR.
   btn_qty-msf:LABEL = TRIM(rfqitem.qty[99]:LABEL) + ": " +
                       TRIM(STRING(rfqitem.qty[1],rfqitem.qty[99]:FORMAT)) +
                        FILL(" ",10) +
                        TRIM(fi_msf:LABEL) + ": " +
                        TRIM(STRING(fi_msf,fi_msf:FORMAT)).

   FIND FIRST itemfg WHERE itemfg.company = rfqitem.company
                      AND itemfg.i-no = rfqitem.stock-no NO-LOCK NO-ERROR.
   IF AVAIL itemfg THEN rd_alloc = IF itemfg.alloc THEN "U" ELSE "A".

   RUN enable_UI.
   IF lv-new-set THEN RUN enable-all.

   RUN enable_UI.

   IF ip-est-type >= 5 THEN
   DO:
      IF v-cecscrn-char NE "Decimal" THEN
         ASSIGN rfqitem.len:FORMAT = ">>>9.99"
                rfqitem.wid:FORMAT = ">>>9.99"
                rfqitem.dep:FORMAT = ">>>9.99".
      ELSE
         ASSIGN rfqitem.len:FORMAT = ">>>9.999999"
                rfqitem.wid:FORMAT = ">>>9.999999"
                rfqitem.dep:FORMAT = ">>>9.999999".

      ASSIGN
         rfqitem.len:SCREEN-VALUE = STRING({sys/inc/k16.i rfqitem.len})
         rfqitem.wid:SCREEN-VALUE = STRING({sys/inc/k16.i rfqitem.wid})
         rfqitem.dep:SCREEN-VALUE = STRING({sys/inc/k16.i rfqitem.dep}).
   END.

   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-use Dialog-Frame 
PROCEDURE check-use :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {est/checkuse.i "no-cancel"}
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-set Dialog-Frame 
PROCEDURE create-set :
/*------------------------------------------------------------------------------
  Purpose:     /* ce/set-info.a */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* FIND FIRST bf-est
      WHERE bf-est.company EQ bf-eb.company
        AND bf-est.est-no  EQ bf-eb.est-no
      NO-LOCK NO-ERROR.

  {ce/set-info.a 6 "bf-" "bf-"}

  lv-new-set = yes.
  lv-set-recid = RECID(bf-eb).
 */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-all Dialog-Frame 
PROCEDURE enable-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    btn_update:LABEL = "&Save".
    ENABLE {&DISPLAYED-FIELDS}.
    DISABLE fi_msf.
    APPLY "entry" TO rfqitem.stock-no.
  END.

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
  DISPLAY rd_alloc 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE rfqitem THEN 
    DISPLAY rfqitem.stock-no rfqitem.part-no rfqitem.part-dscr1 rfqitem.part-dscr2 
          rfqitem.procat rfqitem.len rfqitem.wid rfqitem.dep 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-5 btn_qty-msf btn_update Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-2box Dialog-Frame 
PROCEDURE upd-2box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF VAR ll AS LOG NO-UNDO.


  MESSAGE "Is this a 2 Piece Box?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.

  IF ll THEN DO:
    FIND eb WHERE RECID(eb) = lv-set-recid .
    FIND bf-eb WHERE RECID(bf-eb) EQ ip-recid NO-LOCK NO-ERROR.
    ASSIGN eb.stock-no = bf-eb.stock-no
           eb.part-no = bf-eb.part-no
           eb.part-dscr1 = bf-eb.part-dscr1
           eb.part-dscr2 = bf-eb.part-dscr2
           eb.procat = bf-eb.procat
           eb.len = bf-eb.len
           eb.wid = bf-eb.wid
           eb.dep = bf-eb.dep
           .
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no Dialog-Frame 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-eb FOR eb.

      
  li = 0.
  FOR EACH b-eb
      WHERE b-eb.company EQ bf-eb.company
        AND b-eb.est-no  EQ bf-eb.est-no
        AND b-eb.form-no NE 0
      NO-LOCK:
    li = li + 1.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    IF li GT 1 THEN
      IF eb.part-no:SCREEN-VALUE EQ "" OR
         CAN-FIND(FIRST b-eb WHERE b-eb.company   EQ est.company
                               AND b-eb.est-no    EQ est.est-no
                               AND (b-eb.part-no  EQ eb.part-no:SCREEN-VALUE OR
                                    b-eb.stock-no EQ eb.part-no:SCREEN-VALUE)
                               AND b-eb.form-no   NE 0) THEN DO:
        IF eb.part-no:SCREEN-VALUE EQ "" THEN
          MESSAGE TRIM(eb.part-no:LABEL) + " must be entered..." VIEW-AS ALERT-BOX.
        ELSE
          MESSAGE TRIM(eb.part-no:LABEL) + " already exists on estimate..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO eb.part-no.
        RETURN ERROR.
      END.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat Dialog-Frame 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    IF rfqitem.procat:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Category must be entered. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO rfqitem.procat.
       RETURN ERROR.
    END.
    IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = rfqitem.company and
                                      fgcat.procat = rfqitem.procat:screen-value)
    THEN DO:
        MESSAGE "Invalid Category. Try help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rfqitem.procat.
        RETURN ERROR.
    END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-set Dialog-Frame 
PROCEDURE validate-set :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-item FOR rfqitem.

  FIND FIRST bf-item OF rfq WHERE bf-item.seq = 999 NO-ERROR.
  IF AVAIL bf-item THEN DO:
     IF bf-item.part-no = "" THEN DELETE bf-item.
  END.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

