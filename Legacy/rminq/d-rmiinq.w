&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: po\d-poordl.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid2 AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER ip-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN cocode = g_company.
ASSIGN locode = g_loc.

{sa/sa-sls01.i}

DEFINE VARIABLE lv-do-all-items    AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-item-imported   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-import-rejected AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-ship-from       AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE ll-ask-import      AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-rowid           AS ROWID NO-UNDO.
DEFINE VARIABLE cv-s-codes         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv-s-dscrs         AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cOrd-ok AS CHARACTER INITIAL ["R,I,S,P,A,N,U"].

DO TRANSACTION:
  {sys/inc/relmerge.i}
END.

RUN sys/ref/s-codes.p (OUTPUT cv-s-codes, OUTPUT cv-s-dscrs).

DEFINE VARIABLE lv-item-recid   AS RECID         NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL       NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL       NO-UNDO.
DEFINE BUFFER bf-rell FOR oe-rell.
DEFINE BUFFER bf-ordl FOR oe-ordl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-rcpth rm-rdtlh

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame rm-rcpth.i-no rm-rcpth.po-no ~
rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num rm-rcpth.trans-date ~
rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty ~
rm-rcpth.pur-uom rm-rdtlh.cost  ~
rm-rdtlh.tag2 rm-rdtlh.user-id rm-rdtlh.receiver-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame rm-rcpth.i-no ~
rm-rcpth.po-no rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num ~
rm-rcpth.trans-date rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin ~
rm-rdtlh.tag rm-rdtlh.qty rm-rcpth.pur-uom rm-rdtlh.cost rm-rdtlh.tag2 ~
rm-rdtlh.user-id rm-rdtlh.receiver-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame rm-rcpth rm-rdtlh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame rm-rcpth
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Dialog-Frame rm-rdtlh
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH rm-rcpth WHERE rm-rcpth.company EQ cocode NO-LOCK, ~
      EACH rm-rdtlh OF rm-rcpth SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH rm-rcpth ~
      WHERE rm-rcpth.company EQ cocode NO-LOCK, EACH rm-rdtlh OF rm-rcpth  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame rm-rcpth rm-rdtlh
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame rm-rcpth
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame rm-rdtlh


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rm-rcpth.i-no rm-rcpth.po-no ~
rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num rm-rcpth.trans-date ~
rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty ~
rm-rcpth.pur-uom rm-rdtlh.cost  ~
rm-rdtlh.tag2 rm-rdtlh.user-id rm-rdtlh.receiver-no 
&Scoped-define ENABLED-TABLES rm-rcpth rm-rdtlh
&Scoped-define FIRST-ENABLED-TABLE rm-rcpth
&Scoped-define second-ENABLED-TABLE rm-rcpth
&Scoped-Define ENABLED-OBJECTS fi_ext-cost fi_cost-uom Btn_OK Btn_Done Btn_Cancel ~
RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS rm-rcpth.i-no rm-rcpth.po-no ~
rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num rm-rcpth.trans-date ~
rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty ~
rm-rcpth.pur-uom rm-rdtlh.cost  ~
rm-rdtlh.tag2 rm-rdtlh.user-id rm-rdtlh.receiver-no 
&Scoped-define DISPLAYED-TABLES rm-rcpth rm-rdtlh
&Scoped-define FIRST-DISPLAYED-TABLE rm-rcpth
&Scoped-define SECOND-DISPLAYED-TABLE rm-rdtlh
&Scoped-Define DISPLAYED-OBJECTS fi_ext-cost fi_cost-uom

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_ext-cost AS DECIMAL FORMAT "->>,>>>,>>9.99":U 
     LABEL "Ext Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cost-uom AS CHARACTER FORMAT "x(10)":U 
     LABEL "Cost/UOM" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.8 BY 3.57.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.8 BY 10.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      rm-rcpth, 
      rm-rdtlh SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rm-rcpth.i-no AT ROW 2.43 COL 20 COLON-ALIGNED 
          LABEL "Item#" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     rm-rcpth.po-no AT ROW 2.43 COL 63 COLON-ALIGNED HELP
          ""
          LABEL "Vendor PO#" FORMAT "x(9)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     rm-rcpth.job-no AT ROW 2.43 COL 105 COLON-ALIGNED 
          LABEL "Job#" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
    rm-rcpth.job-no2 AT ROW 2.43 COL 120 COLON-ALIGNED NO-LABEL FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     rm-rdtlh.s-num AT ROW 3.62 COL 20 COLON-ALIGNED
          LABEL "S" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     rm-rcpth.trans-date AT ROW 3.62 COL 63 COLON-ALIGNED
          LABEL "TR Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     rm-rcpth.rita-code AT ROW 3.62 COL 105 COLON-ALIGNED
          LABEL "C" FORMAT "x(1)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     rm-rdtlh.loc AT ROW 4.81 COL 20 COLON-ALIGNED
          LABEL "Whs" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     rm-rdtlh.loc-bin AT ROW 4.81 COL 63 COLON-ALIGNED 
          LABEL "Bin" FORMAT "x(8)"      
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     rm-rdtlh.tag AT ROW 4.81 COL 105 COLON-ALIGNED
          LABEL "Tag" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     rm-rdtlh.qty AT ROW 6 COL 20 COLON-ALIGNED
          LABEL "Qty" FORMAT "->>>>,>>9.9<<<<<"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     rm-rcpth.pur-uom AT ROW 6 COL 63 COLON-ALIGNED
          LABEL "Qty/UOM" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     rm-rdtlh.cost AT ROW 6 COL 105 COLON-ALIGNED
          LABEL "Cost" FORMAT "->>>,>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     fi_cost-uom AT ROW 7.19 COL 20 COLON-ALIGNED
          
     fi_ext-cost AT ROW 7.19 COL 63 COLON-ALIGNED
     rm-rdtlh.tag2 AT ROW 7.19 COL 105 COLON-ALIGNED 
          LABEL "Cert/Lot/Mill#" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     rm-rdtlh.user-id AT ROW 8.38 COL 20 COLON-ALIGNED
          LABEL "UserID" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     rm-rdtlh.receiver-no AT ROW 8.38 COL 63 COLON-ALIGNED
          LABEL "Invoice Link" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Btn_OK AT ROW 13.29 COL 37
     Btn_Done AT ROW 13.24 COL 57
     Btn_Cancel AT ROW 13.24 COL 77.2
     RECT-21 AT ROW 11.71 COL 1
     RECT-38 AT ROW 1 COL 1
     SPACE(0.99) SKIP(4.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Raw Materials Inventory".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN rm-rcpth.i-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rcpth.po-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rcpth.job-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rcpth.job-no2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN rm-rdtlh.s-num IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.job-no2 IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rcpth.trans-date IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rcpth.rita-code IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rdtlh.loc IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rdtlh.loc-bin IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rm-rdtlh.tag IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rdtlh.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rcpth.pur-uom IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rdtlh.cost IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_cost-uom IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_ext-cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rdtlh.tag2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rdtlh.user-id IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rdtlh.receiver-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "rm-rcpth,rm-rdtlh "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "rm-rcpth.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Order Release Item Update */
DO:
    DEFINE VARIABLE char-val  AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE hlp-recid AS RECID         NO-UNDO.
    DEFINE VARIABLE lw-focus  AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE ll AS LOGICAL INITIAL YES  NO-UNDO.
    DEFINE VARIABLE op-rowid AS ROWID          NO-UNDO.
    
    lw-focus = FOCUS.

    CASE lw-focus:NAME :
        
         WHEN "loc" THEN DO:
            RUN windows/l-loc.w (g_company,rm-rdtlh.loc:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN DO :
               ASSIGN rm-rdtlh.loc:SCREEN-VALUE  = ENTRY(1,char-val).
            END.  
         END.
         WHEN "loc-bin" THEN DO:
             RUN rm/l-locbin.w (g_company,rm-rdtlh.loc:screen-value, OUTPUT char-val).
             IF char-val <> "" THEN
                 ASSIGN rm-rdtlh.loc-bin:SCREEN-VALUE  = ENTRY(1,char-val).
         END.
         WHEN "job-no" OR 
         WHEN "job-no2" THEN DO:
             RUN windows/l-jobno.w (g_company,rm-rcpth.job-no:screen-value, OUTPUT char-val, OUTPUT hlp-recid).
             IF char-val <> "" THEN
                 ASSIGN rm-rcpth.job-no:screen-value  = ENTRY(1,char-val)
                        rm-rcpth.job-no2:screen-value = ENTRY(2,char-val).
         END.
         WHEN "po-no" THEN DO:
            RUN windows/l-ponopo.w (g_company,yes,rm-rcpth.po-no:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN DO :
               ASSIGN rm-rcpth.po-no:SCREEN-VALUE  = ENTRY(1,char-val).
            END.  
         END.

    END CASE.

    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Order Release Item Update */
ANYWHERE
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order Release Item Update */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    DISABLE TRIGGERS FOR LOAD OF oe-rell .

    IF lv-item-recid NE ? THEN DO:
       
    END.
    APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
  &IF DEFINED (adm-panel) NE 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
  DEFINE VARIABLE v-qty    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ll        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE op-error  AS LOGICAL     NO-UNDO.

  IF ip-type EQ "view" THEN DO: 
     APPLY "go" TO FRAME {&FRAME-NAME}.
     RETURN.
  END.
 /* Code placed here will execute PRIOR to standard behavior. */

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-tag-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-po-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-user NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cost NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.



  RUN update-record.

APPLY "go" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME rm-rcpth.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.i-no Dialog-Frame
ON LEAVE OF rm-rcpth.i-no IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rdtlh.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.tag Dialog-Frame
ON LEAVE OF rm-rdtlh.tag IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-tag-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rdtlh.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.cost Dialog-Frame
ON LEAVE OF rm-rdtlh.cost IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      RUN valid-cost NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.po-no Dialog-Frame
ON LEAVE OF rm-rcpth.po-no IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rcpth.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.job-no Dialog-Frame
ON LEAVE OF rm-rcpth.job-no IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rcpth.rita-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.rita-code Dialog-Frame
ON LEAVE OF rm-rcpth.rita-code IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.USER-ID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.USER-ID Dialog-Frame
ON LEAVE OF rm-rdtlh.USER-ID IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-user NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rdtlh.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.loc Dialog-Frame
ON LEAVE OF rm-rdtlh.loc IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rdtlh.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.loc-bin Dialog-Frame
ON LEAVE OF rm-rdtlh.loc-bin IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rcpth.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.job-no2 Dialog-Frame
ON LEAVE OF rm-rcpth.job-no2 IN FRAME Dialog-Frame /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.pur-uom Dialog-Frame
ON LEAVE OF rm-rcpth.pur-uom IN FRAME Dialog-Frame /* Qty/UOM */
DO:
  IF LASTKEY NE -1 THEN RUN convert-uoms.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cost-uom Dialog-Frame
ON LEAVE OF fi_cost-uom IN FRAME Dialog-Frame /* Cost/UOM */
DO:
  IF LASTKEY NE -1 THEN RUN convert-uoms.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   
    FIND FIRST rm-rcpth NO-LOCK 
        WHERE rm-rcpth.company EQ cocode 
          AND ROWID(rm-rcpth) EQ ip-recid NO-ERROR.

    FIND FIRST  rm-rdtlh NO-LOCK 
         WHERE rm-rdtlh.company EQ cocode
           AND ROWID(rm-rdtlh)  EQ ip-recid2  NO-ERROR .

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN ll-order-warned                     = NO.
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
            btn_done:SENSITIVE                        = YES.
            btn_ok:HIDDEN                             = YES.
            btn_cancel:HIDDEN                         = YES.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        IF ip-type EQ "update" THEN DISABLE fi_ext-cost fi_cost-uom.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF AVAILABLE rm-rcpth  THEN 
    DO:
        
        fi_ext-cost = DEC(rm-rdtlh.qty ) * DEC(rm-rdtlh.cost ) .
        fi_cost-uom = rm-rcpth.pur-uom .
        DISPLAY  rm-rcpth.i-no rm-rcpth.po-no 
            rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num rm-rcpth.trans-date 
            rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty 
            rm-rcpth.pur-uom rm-rdtlh.cost  fi_cost-uom 
            rm-rdtlh.tag2 rm-rdtlh.user-id rm-rdtlh.receiver-no fi_ext-cost 
            WITH FRAME Dialog-Frame.
    END.

    IF ip-type NE "view" THEN DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.


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
  DISPLAY fi_ext-cost fi_cost-uom
      WITH FRAME Dialog-Frame.
  IF AVAILABLE rm-rcpth THEN 
    DISPLAY rm-rcpth.i-no rm-rcpth.po-no 
      rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num rm-rcpth.trans-date 
      rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty 
      rm-rcpth.pur-uom rm-rdtlh.cost  
      rm-rdtlh.tag2 rm-rdtlh.user-id rm-rdtlh.receiver-no
      WITH FRAME Dialog-Frame.
  ENABLE rm-rcpth.i-no rm-rcpth.po-no 
      rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num rm-rcpth.trans-date 
      rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty 
      rm-rcpth.pur-uom rm-rdtlh.cost  
      rm-rdtlh.tag2 rm-rdtlh.user-id rm-rdtlh.receiver-no 
      fi_cost-uom fi_ext-cost Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag-no Dialog-Frame 
PROCEDURE valid-tag-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-tag LIKE rm-rdtlh.tag NO-UNDO.
  DEF BUFFER b-item FOR item.
  DEF BUFFER b-rm-rdtlh FOR rm-rdtlh.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
  lv-tag = rm-rdtlh.tag:SCREEN-VALUE .
      
   IF rm-rdtlh.tag:SCREEN-VALUE  NE "" AND
       rm-rcpth.rita-code:SCREEN-VALUE  EQ "R" AND  
       int(rm-rdtlh.qty:SCREEN-VALUE ) GT 0 THEN do:

       FIND FIRST  b-rm-rdtlh NO-LOCK
           WHERE b-rm-rdtlh.company EQ cocode
             AND b-rm-rdtlh.tag     EQ lv-tag 
             AND ROWID(b-rm-rdtlh)  NE ROWID(rm-rdtlh) NO-ERROR .

       IF AVAIL b-rm-rdtlh THEN DO:
           MESSAGE "This Tag Number has already been used..." VIEW-AS ALERT-BOX INFO.
           rm-rdtlh.tag:SCREEN-VALUE  = rm-rdtlh.tag.
           APPLY "entry" TO rm-rdtlh.tag .
           RETURN ERROR.
       END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no Dialog-Frame 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST po-ord
                    WHERE po-ord.company EQ cocode
                       AND string(po-ord.po-no)   EQ rm-rcpth.po-no:SCREEN-VALUE )
    THEN DO:
      MESSAGE "invalid Purchase Order..." VIEW-AS ALERT-BOX ERROR.
      rm-rcpth.po-no:SCREEN-VALUE  = string(rm-rcpth.po-no).
      APPLY "entry" TO rm-rcpth.po-no .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no Dialog-Frame 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-hdr
                    WHERE job-hdr.company EQ cocode
                       AND job-hdr.job-no EQ rm-rcpth.job-no:SCREEN-VALUE )
    THEN DO:
      MESSAGE "Invalid Job#..." VIEW-AS ALERT-BOX ERROR.
      rm-rcpth.job-no:SCREEN-VALUE  = string(rm-rcpth.job-no).
      APPLY "entry" TO rm-rcpth.job-no .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cost Dialog-Frame 
PROCEDURE valid-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF int(rm-rdtlh.cost:SCREEN-VALUE) LT 0 
    THEN DO:
      MESSAGE "Cost can not be nagetive." VIEW-AS ALERT-BOX ERROR.
      rm-rdtlh.cost:SCREEN-VALUE  = string(rm-rdtlh.cost).
      APPLY "entry" TO rm-rdtlh.cost .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-user Dialog-Frame 
PROCEDURE valid-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST users
                    WHERE users.USER_id EQ rm-rdtlh.USER-ID:SCREEN-VALUE )
    THEN DO:
      MESSAGE "Invalid User..." VIEW-AS ALERT-BOX ERROR.
      rm-rdtlh.USER-ID:SCREEN-VALUE  = string(rm-rdtlh.USER-ID).
      APPLY "entry" TO rm-rdtlh.USER-ID .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-code Dialog-Frame 
PROCEDURE valid-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
      
    IF LOOKUP(rm-rcpth.rita-code:SCREEN-VALUE,"R,I,T,A,C") EQ 0
    THEN DO:
      MESSAGE "Invalid Code..." VIEW-AS ALERT-BOX ERROR.
      rm-rcpth.rita-code:SCREEN-VALUE  = string(rm-rcpth.rita-code).
      APPLY "entry" TO rm-rcpth.rita-code .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc Dialog-Frame 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ cocode
                       AND loc.loc  EQ rm-rdtlh.loc:SCREEN-VALUE )
    THEN DO:
      MESSAGE "Invalid Warehouse..." VIEW-AS ALERT-BOX ERROR.
      rm-rdtlh.loc:SCREEN-VALUE  = rm-rdtlh.loc.
      APPLY "entry" TO rm-rdtlh.loc.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin Dialog-Frame 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST rm-bin
                    WHERE rm-bin.company EQ cocode
                       AND rm-bin.loc  EQ rm-rdtlh.loc:SCREEN-VALUE
                       AND rm-bin.loc-bin EQ rm-rdtlh.loc-bin:SCREEN-VALUE )
    THEN DO:
      MESSAGE "Invalid Bin..." VIEW-AS ALERT-BOX ERROR.
      rm-rdtlh.loc-bin:SCREEN-VALUE  = rm-rdtlh.loc-bin.
      APPLY "entry" TO rm-rdtlh.loc-bin.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 Dialog-Frame 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-hdr
                    WHERE job-hdr.company EQ cocode
                       AND job-hdr.job-no EQ rm-rcpth.job-no:SCREEN-VALUE 
                       AND job-hdr.job-no2 EQ int(rm-rcpth.job-no2:SCREEN-VALUE) )
    THEN DO:
      MESSAGE "Invalid Job#..." VIEW-AS ALERT-BOX ERROR.
      rm-rcpth.job-no2:SCREEN-VALUE  = string(rm-rcpth.job-no2).
      APPLY "entry" TO rm-rcpth.job-no2 .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUM

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no Dialog-Frame 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-item FOR item.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    rm-rcpth.i-no:SCREEN-VALUE  = CAPS(rm-rcpth.i-no:SCREEN-VALUE ).

    IF NOT CAN-FIND(FIRST b-item
                    WHERE b-item.company EQ cocode
                      AND b-item.i-no    EQ rm-rcpth.i-no:SCREEN-VALUE )
    THEN DO:
      MESSAGE "Invalid RM Item#..." VIEW-AS ALERT-BOX ERROR.
      rm-rcpth.i-no:SCREEN-VALUE  = rm-rcpth.i-no.
      APPLY "entry" TO rm-rcpth.i-no .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record Dialog-Frame 
PROCEDURE update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-rcpth FOR rm-rcpth.
  DEF BUFFER b-rdtlh FOR rm-rdtlh.

  DISABLE TRIGGERS FOR LOAD OF rm-rcpth.
  DISABLE TRIGGERS FOR LOAD OF rm-rdtlh.

  DO WITH FRAME {&FRAME-NAME}:
    FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(rm-rcpth).
    FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(rm-rdtlh).

    ASSIGN
     b-rcpth.i-no       = rm-rcpth.i-no:SCREEN-VALUE 
     b-rcpth.po-no      = rm-rcpth.po-no:SCREEN-VALUE 
     b-rcpth.job-no     = rm-rcpth.job-no:SCREEN-VALUE 
     b-rcpth.job-no2    = INT(rm-rcpth.job-no2:SCREEN-VALUE )
     b-rcpth.rita-code  = rm-rcpth.rita-code:SCREEN-VALUE 
     b-rdtlh.s-num      = INT(rm-rdtlh.s-num:SCREEN-VALUE )
     b-rcpth.trans-date = DATE(rm-rcpth.trans-date:SCREEN-VALUE )
     b-rdtlh.loc        = rm-rdtlh.loc:SCREEN-VALUE
     b-rdtlh.loc-bin    = rm-rdtlh.loc-bin:SCREEN-VALUE 
     b-rdtlh.tag        = rm-rdtlh.tag:SCREEN-VALUE 
     b-rdtlh.qty        = DEC(rm-rdtlh.qty:SCREEN-VALUE )
     b-rcpth.pur-uom    = rm-rcpth.pur-uom:SCREEN-VALUE 
     b-rdtlh.cost       = DEC(rm-rdtlh.cost:SCREEN-VALUE )
     b-rdtlh.tag2       = rm-rdtlh.tag2:SCREEN-VALUE 
     b-rdtlh.user-id    = rm-rdtlh.user-id:SCREEN-VALUE 
     b-rdtlh.receiver-no = rm-rdtlh.receiver-no:SCREEN-VALUE 
     b-rdtlh.job-no     = b-rcpth.job-no
     b-rdtlh.job-no2    = b-rcpth.job-no2
     b-rdtlh.rita-code  = b-rcpth.rita-code
     b-rcpth.user-id    = b-rdtlh.user-id.

    IF b-rcpth.rita-code EQ "I" THEN
       RUN update-mat-act-cost(BUFFER b-rcpth,
                               BUFFER b-rdtlh).

    FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(rm-rcpth) NO-LOCK NO-ERROR.
    FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(rm-rdtlh) NO-LOCK NO-ERROR.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-uoms Dialog-Frame 
PROCEDURE convert-uoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-len         LIKE po-ordl.s-len      NO-UNDO.
  DEF VAR v-wid         LIKE po-ordl.s-len      NO-UNDO.
  DEF VAR v-dep         LIKE po-ordl.s-len      NO-UNDO. 
  DEF VAR v-bwt         LIKE po-ordl.s-len      NO-UNDO.
  DEF VAR lv-out-qty    LIKE rm-rdtlh.qty       NO-UNDO.
  DEF VAR lv-out-cost   LIKE rm-rdtlh.cost      NO-UNDO.
  DEF VAR lv-qty-uom    LIKE rm-rcpth.pur-uom   NO-UNDO.
  DEF VAR lv-cost-uom   LIKE rm-rcpth.pur-uom   NO-UNDO.
  DEF VAR lv-po-no      LIKE po-ordl.po-no      NO-UNDO.

  DEF BUFFER b-rcpth FOR rm-rcpth.
  DEF BUFFER b-rdtlh FOR rm-rdtlh.

  DO WITH FRAME {&FRAME-NAME}:
    FIND b-rcpth NO-LOCK WHERE ROWID(b-rcpth) EQ ROWID(rm-rcpth)  NO-ERROR.
    FIND b-rdtlh NO-LOCK WHERE ROWID(b-rdtlh) EQ ROWID(rm-rdtlh)  NO-ERROR.

    FIND FIRST ITEM NO-LOCK
        WHERE item.company EQ rm-rcpth.company
          AND item.i-no    EQ rm-rcpth.i-no NO-ERROR.
        
    IF AVAIL item THEN DO:
      IF item.cons-uom EQ "" THEN item.cons-uom = b-rcpth.pur-uom.

      ASSIGN
       lv-qty-uom  = item.cons-uom
       lv-cost-uom = item.cons-uom
       v-dep       = item.s-dep
       lv-po-no    = INT(b-rcpth.po-no) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN lv-po-no = 0.

      RELEASE po-ordl.
      IF lv-po-no NE 0 THEN
      FIND FIRST po-ordl NO-LOCK
          WHERE po-ordl.company   EQ b-rcpth.company
            AND po-ordl.po-no     EQ lv-po-no
            AND po-ordl.i-no      EQ b-rcpth.i-no
            AND po-ordl.job-no    EQ b-rcpth.job-no
            AND po-ordl.job-no2   EQ b-rcpth.job-no2
            AND po-ordl.item-type EQ YES 
            AND po-ordl.s-num     EQ b-rdtlh.s-num
            AND po-ordl.b-num     EQ b-rdtlh.b-num NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        ASSIGN
         v-len = po-ordl.s-len
         v-wid = po-ordl.s-wid
         v-bwt = 0.
        {rm/pol-dims.i}
      END.

      FIND FIRST job NO-LOCK
          WHERE job.company EQ b-rcpth.company
            AND job.job-no  EQ b-rcpth.job-no
            AND job.job-no2 EQ b-rcpth.job-no2 NO-ERROR.
      IF AVAIL job THEN DO:
        FIND FIRST job-mat NO-LOCK
            WHERE job-mat.company  EQ b-rcpth.company
              AND job-mat.job      EQ job.job
              AND job-mat.i-no     EQ b-rcpth.i-no
              AND job-mat.frm      EQ b-rdtlh.s-num
              AND job-mat.blank-no EQ b-rdtlh.b-num NO-ERROR.
        IF AVAIL job-mat THEN
          ASSIGN
           v-len         = job-mat.len
           v-wid         = job-mat.wid
           v-bwt         = job-mat.basis-w.
      END.
      IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
      IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid
                                   ELSE
                                   IF AVAIL item THEN item.s-wid ELSE 0.
      IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
    END.
    FIND CURRENT item NO-LOCK NO-ERROR.
    /* convert qty */
    IF rm-rcpth.pur-uom:SCREEN-VALUE  EQ lv-qty-uom THEN
      lv-out-qty = DEC(rm-rdtlh.qty:SCREEN-VALUE ).
    ELSE
      RUN custom/convquom.p(b-rcpth.company,
                            rm-rcpth.pur-uom:SCREEN-VALUE  ,
                            lv-qty-uom,
                            v-bwt, v-len, v-wid, v-dep,
                            DEC(rm-rdtlh.qty:SCREEN-VALUE ),
                            OUTPUT lv-out-qty).

    /* convert cost */
    IF fi_cost-uom:SCREEN-VALUE  EQ "L" THEN
      lv-out-cost = DEC(rm-rdtlh.cost:SCREEN-VALUE ) / lv-out-qty.
    ELSE
      RUN custom/convcuom.p(b-rcpth.company,
                            fi_cost-uom:SCREEN-VALUE ,
                            lv-cost-uom,
                            v-bwt, v-len, v-wid, v-dep,
                            DEC(rm-rdtlh.cost:SCREEN-VALUE ),
                            OUTPUT lv-out-cost).
.
    ASSIGN
     rm-rdtlh.qty:SCREEN-VALUE      = STRING(lv-out-qty)
     rm-rcpth.pur-uom:SCREEN-VALUE  = lv-qty-uom
     rm-rdtlh.cost:SCREEN-VALUE     = STRING(lv-out-cost)
     fi_cost-uom:SCREEN-VALUE      = lv-cost-uom.
     fi_ext-cost:SCREEN-VALUE       = STRING(DEC(rm-rdtlh.qty:SCREEN-VALUE ) *
                                             DEC(rm-rdtlh.cost:SCREEN-VALUE ) ) .

    DISPLAY fi_ext-cost WITH FRAME Dialog-Frame.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-mat-act-cost Dialog-Frame 
PROCEDURE update-mat-act-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE PARAMETER BUFFER b-rm-rcpth FOR rm-rcpth.
   DEFINE PARAMETER BUFFER b-rm-rdtlh FOR rm-rdtlh.

   DEF VAR v-bwt       LIKE item.basis-w  NO-UNDO.
   DEF VAR v-len       LIKE item.s-len    NO-UNDO.
   DEF VAR v-wid       LIKE item.s-wid    NO-UNDO.
   DEF VAR v-dep       LIKE item.s-dep    NO-UNDO.
   DEF VAR v-cost      LIKE rm-rdtlh.cost NO-UNDO.

   FIND FIRST job NO-LOCK
       WHERE job.company EQ b-rm-rcpth.company 
         AND job.job-no  EQ b-rm-rcpth.job-no 
         AND job.job-no2 EQ b-rm-rcpth.job-no2 NO-ERROR.

   FIND FIRST ITEM NO-LOCK
       WHERE item.company  EQ b-rm-rcpth.company 
        AND item.i-no     EQ b-rm-rcpth.i-no NO-ERROR.

   IF AVAIL job AND AVAIL ITEM THEN
   DO:
      FOR EACH job-mat WHERE
          job-mat.company  EQ job.company AND
          job-mat.job      EQ job.job AND
          job-mat.job-no   EQ job.job-no AND
          job-mat.job-no2  EQ job.job-no2 AND
          job-mat.frm      EQ b-rm-rdtlh.s-num AND
          job-mat.i-no     EQ b-rm-rcpth.i-no
          NO-LOCK
          BREAK BY job-mat.blank-no DESC:

         IF LAST(job-mat.blank-no) OR
            job-mat.blank-no EQ b-rm-rdtlh.b-num THEN
            LEAVE.
      END.

      IF AVAIL job-mat THEN DO:
         ASSIGN
           v-bwt = job-mat.basis-w
           v-len = job-mat.len
           v-wid = job-mat.wid
           v-dep = item.s-dep.

         IF v-len EQ 0 THEN v-len = item.s-len.

         IF v-wid EQ 0 THEN
           v-wid = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.

         IF v-bwt EQ 0 THEN v-bwt = item.basis-w.

         IF item.cons-uom EQ b-rm-rcpth.pur-uom THEN
           v-cost = b-rm-rdtlh.cost.
         ELSE
           RUN sys/ref/convcuom.p(item.cons-uom, b-rm-rcpth.pur-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  b-rm-rdtlh.cost, OUTPUT v-cost).    

         FIND FIRST mat-act 
             WHERE mat-act.company   EQ job-mat.company
               AND mat-act.mat-date  EQ b-rm-rcpth.post-date
               AND mat-act.job       EQ job.job
               AND mat-act.job-no    EQ job-mat.job-no
               AND mat-act.job-no2   EQ job-mat.job-no2
               AND mat-act.s-num     EQ job-mat.frm
               AND mat-act.b-num     EQ job-mat.blank-no
               AND mat-act.i-no      EQ job-mat.i-no
               AND mat-act.rm-i-no   EQ job-mat.i-no
               AND mat-act.tag       EQ b-rm-rdtlh.tag
               AND mat-act.loc       EQ b-rm-rdtlh.loc
               AND mat-act.loc-bin   EQ b-rm-rdtlh.loc-bin
             NO-ERROR.

         IF AVAIL mat-act THEN DO:
            IF b-rm-rcpth.pur-uom NE job-mat.sc-uom THEN
               RUN sys/ref/convcuom.p(b-rm-rcpth.pur-uom, job-mat.sc-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      v-cost, OUTPUT v-cost).

            ASSIGN
               mat-act.cost = v-cost
               mat-act.ext-cost = v-cost * mat-act.qty.

            RELEASE mat-act.
         END.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
