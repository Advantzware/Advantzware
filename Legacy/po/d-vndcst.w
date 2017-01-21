&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&SCOPED-DEFINE WINDOW-NAME CURRENT-WINDOW
&SCOPED-DEFINE FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: po\d-vndcst.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}


{custom/gcompany.i}
{sys/inc/var.i SHARED}
{sys/inc/varasgn.i}
{sys/inc\msfcalc.i}

DEFINE INPUT PARAMETER v-term LIKE report.term-id NO-UNDO.
DEFINE INPUT PARAMETER v-recid AS RECID NO-UNDO.
DEFINE INPUT PARAMETER ip-rm AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ip-i-no AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.  /* Qty to use for cost determination */
DEFINE INPUT PARAMETER ipcUom AS CHARACTER NO-UNDO.  /* UOm to use for cost determination */
DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE fil_id AS RECID NO-UNDO.
DEFINE VARIABLE v-vend-no AS CHARACTER NO-UNDO .

DEFINE TEMP-TABLE tt-report FIELD key-03 LIKE report.key-03
                         FIELD key-04 LIKE report.key-04
                         FIELD key-02 LIKE report.key-02 LABEL "MSF"
                         FIELD vend-name AS CHARACTER 
                         FIELD report-cost AS DECIMAL
                         FIELD cost-uom AS CHARACTER
                         FIELD disc-days LIKE vend.disc-days
                         FIELD ext-price AS DECIMAL
                         FIELD rec-id AS RECID
                         FIELD vend-item AS CHAR
                         FIELD wid-min AS DECIMAL 
                         FIELD wid-max AS DECIMAL
                         FIELD len-min AS DECIMAL
                         FIELD len-max AS DECIMAL 
                         FIELD po-no AS CHARACTER  .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE DIALOG-BOX
&SCOPED-DEFINE DB-AWARE NO 

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME Dialog-Frame
&SCOPED-DEFINE BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&SCOPED-DEFINE INTERNAL-TABLES tt-report ITEM job-mat

/* Definitions for BROWSE BROWSE-2                                      */
&SCOPED-DEFINE FIELDS-IN-QUERY-BROWSE-2 tt-report.key-03 tt-report.po-no tt-report.vend-name tt-report.key-04 tt-report.key-02 tt-report.report-cost tt-report.cost-uom tt-report.disc-days tt-report.ext-price tt-report.vend-item tt-report.wid-min tt-report.wid-max tt-report.len-min tt-report.len-max   
&SCOPED-DEFINE ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&SCOPED-DEFINE SELF-NAME BROWSE-2
&SCOPED-DEFINE QUERY-STRING-BROWSE-2 FOR EACH tt-report NO-LOCK
&SCOPED-DEFINE OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK.
&SCOPED-DEFINE TABLES-IN-QUERY-BROWSE-2 tt-report
&SCOPED-DEFINE FIRST-TABLE-IN-QUERY-BROWSE-2 tt-report


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&SCOPED-DEFINE FIELDS-IN-QUERY-Dialog-Frame job-mat.frm job-mat.blank-no ~
job-mat.i-no ITEM.i-name job-mat.len job-mat.wid job-mat.n-up 
&SCOPED-DEFINE OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}
&SCOPED-DEFINE QUERY-STRING-Dialog-Frame FOR EACH ITEM SHARE-LOCK, ~
      EACH job-mat OF ITEM SHARE-LOCK
&SCOPED-DEFINE OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ITEM SHARE-LOCK, ~
      EACH job-mat OF ITEM SHARE-LOCK.
&SCOPED-DEFINE TABLES-IN-QUERY-Dialog-Frame ITEM job-mat
&SCOPED-DEFINE FIRST-TABLE-IN-QUERY-Dialog-Frame ITEM
&SCOPED-DEFINE SECOND-TABLE-IN-QUERY-Dialog-Frame job-mat


/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS BROWSE-2 btn_vencst Btn_OK Btn_Cancel 
&SCOPED-DEFINE DISPLAYED-FIELDS job-mat.frm job-mat.blank-no job-mat.i-no ~
item.i-name job-mat.len job-mat.wid job-mat.n-up 
&SCOPED-DEFINE DISPLAYED-TABLES job-mat ITEM
&SCOPED-DEFINE FIRST-DISPLAYED-TABLE job-mat
&SCOPED-DEFINE SECOND-DISPLAYED-TABLE ITEM
&SCOPED-DEFINE DISPLAYED-OBJECTS fi_dep 

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

DEFINE BUTTON btn_vencst 
     LABEL "Add Vendor Cost" 
     SIZE 20.2 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_dep AS DECIMAL FORMAT ">,>>9.99<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-report SCROLLING.

DEFINE QUERY Dialog-Frame FOR 
      item, 
      job-mat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-report.key-03 LABEL "Vendor#" WIDTH 13
      tt-report.po-no LABEL "Last Po#" WIDTH 20
      tt-report.vend-name LABEL "Name" WIDTH 30
      tt-report.key-04 LABEL "Tons"    WIDTH 10
      tt-report.key-02                 WIDTH 10
      tt-report.report-cost LABEL "Cost"
      tt-report.cost-uom LABEL "UOM" WIDTH 6
      tt-report.disc-days LABEL "Lead"
      tt-report.ext-price LABEL "Ext Cost" FORMAT "->>,>>>,>>9.99"
      tt-report.vend-item LABEL "Vendor Item" WIDTH 28
      tt-report.wid-min LABEL "Wid Min"
      tt-report.wid-max LABEL "Wid Max"
      tt-report.len-min LABEL "Len Min"
      tt-report.len-max LABEL "Len Max"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 190 BY 10.48
         BGCOLOR 8 FONT 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     job-mat.frm AT ROW 2.67 COLUMN 3.4 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     job-mat.blank-no AT ROW 2.67 COLUMN 6.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     job-mat.i-no AT ROW 2.67 COLUMN 12.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     item.i-name AT ROW 2.67 COLUMN 34.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     job-mat.len AT ROW 2.67 COLUMN 78.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     job-mat.wid AT ROW 2.67 COLUMN 89.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi_dep AT ROW 2.67 COLUMN 100.4 COLON-ALIGNED NO-LABEL
     job-mat.n-up AT ROW 2.67 COLUMN 112.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     BROWSE-2 AT ROW 4.33 COLUMN 3
     btn_vencst AT ROW 16.24 COLUMN 3.2
     Btn_OK AT ROW 16.24 COLUMN 38
     Btn_Cancel AT ROW 16.24 COLUMN 70
     "Name" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COLUMN 42.4
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COLUMN 81.4
     "S / B" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.95 COLUMN 5.4
     "Depth" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COLUMN 103.4
     "RM Item#" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.95 COLUMN 17.4
     "#Up" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.95 COLUMN 114.4
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COLUMN 93.4
     SPACE(92.39) SKIP(15.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Vendor Cost"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
/* BROWSE-TAB BROWSE-2 n-up Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN job-mat.blank-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_dep IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.frm IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN item.i-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.i-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.len IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.n-up IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.wid IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.item,ASI.job-mat OF ASI.item"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&SCOPED-DEFINE SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vendor Cost */
DO:
 /* APPLY "END-ERROR":U TO SELF.*/
  /* This was done so that adding a new vendor is not undone on cancel */
    fil_id = ?.
    APPLY 'GO' TO FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&SCOPED-DEFINE SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    /* This was done so that adding a new vendor is not undone on cancel */
    fil_id = ?.
    APPLY 'GO' TO FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    IF AVAIL tt-report THEN
        fil_id = tt-report.rec-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn_vencst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_vencst Dialog-Frame
ON CHOOSE OF btn_vencst IN FRAME Dialog-Frame /* Add Vendor Cost */
DO:
  
  IF ip-rm THEN
    RUN windows\d-vndcst.w(INPUT ip-i-no, v-term).
  ELSE
    RUN windows\d-vndcfg.w(INPUT ip-i-no, v-term).

  RUN build-table.
  {&open-query-{&browse-name}}

  IF AVAILABLE tt-report THEN fil_id = tt-report.rec-id.
  APPLY 'go' TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE BROWSE-NAME BROWSE-2


&SCOPED-DEFINE SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-2 IN FRAME Dialog-Frame
DO:
 
IF AVAILABLE tt-report AND tt-report.key-03 EQ v-vend-no THEN DO:
    ASSIGN 
        tt-report.key-03:BGCOLOR IN BROWSE {&BROWSE-NAME}      = 14
        .
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND job-mat NO-LOCK WHERE RECID(job-mat) EQ v-recid NO-ERROR.
  IF AVAILABLE job-mat THEN DO WITH FRAME {&FRAME-NAME}:
     FIND FIRST ITEM NO-LOCK WHERE ITEM.company EQ job-mat.company
                       AND ITEM.i-no EQ job-mat.i-no NO-ERROR.
     fi_dep = IF job-mat.dep GT 0 THEN job-mat.dep ELSE ITEM.s-dep.

     IF INDEX("1234",ITEM.mat-type) GT 0 THEN
       tt-report.key-02:LABEL IN BROWSE {&browse-name} = "BF".
  END.
  ELSE
  DO:
     FIND job-prep NO-LOCK WHERE RECID(job-prep) EQ v-recid NO-ERROR.
     IF AVAILABLE job-prep THEN
     DO:

        FIND FIRST prep NO-LOCK WHERE
             prep.company EQ job-prep.company AND
             prep.CODE EQ job-prep.CODE
              NO-ERROR.

        IF AVAILABLE prep THEN
           FIND FIRST ITEM NO-LOCK WHERE
                ITEM.company EQ prep.company AND
                ITEM.i-no EQ prep.i-no
                NO-ERROR.

        IF AVAILABLE ITEM THEN
           fi_dep = ITEM.s-dep.
     END.
  END.
  
  RUN build-table.
  IF NOT ip-rm THEN DO:
      tt-report.key-04:VISIBLE IN BROWSE browse-2 = FALSE.
      tt-report.key-02:LABEL IN BROWSE browse-2 = "Qty".
  END.

  RUN enable_UI.  
 /* FIND FIRST tt-report WHERE tt-report.vend-no = v-vend-no
                                                 NO-LOCK NO-ERROR.
  IF AVAIL tt-report THEN 
      ip-recid-line = RECID(tt-report) .
  
   REPOSITION {&browse-name} TO RECID (ip-recid-line) NO-ERROR.*/
  

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
  DEFINE VARIABLE cUom AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dQty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCalcCost AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dSetup AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lIsABoard AS LOGICAL NO-UNDO.
  DEFINE VARIABLE v-fgitem AS CHARACTER NO-UNDO.
  EMPTY TEMP-TABLE tt-report.
  DEFINE BUFFER bf-report FOR report.
  FOR EACH bf-report NO-LOCK WHERE bf-report.term-id EQ v-term :
      FIND report WHERE ROWID(report) EQ ROWID(bf-report) EXCLUSIVE-LOCK.

      RELEASE e-item-vend.
      RELEASE e-itemfg-vend.

      FIND FIRST e-item-vend WHERE RECID(e-item-vend) EQ report.rec-id
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE e-item-vend THEN
      FIND FIRST e-itemfg-vend NO-LOCK WHERE RECID(e-itemfg-vend) EQ report.rec-id
          NO-ERROR.
      cUom = "".
      IF AVAILABLE e-item-vend THEN
          cUom = e-item-vend.std-uom.
      ELSE IF AVAILABLE e-itemfg-vend THEN
          cUom = e-itemfg-vend.std-uom.
      dCalcCost = DECIMAL(report.key-01).
      IF cUom EQ "" AND AVAILABLE e-item-vend THEN DO:
          
          FIND FIRST e-itemfg NO-LOCK WHERE e-itemfg.company EQ e-itemfg-vend.company
              AND e-itemfg.i-no EQ e-itemfg-vend.vend-no
               NO-ERROR.
          IF AVAILABLE e-itemfg THEN
              cUom = e-itemfg.std-uom.

      END.
               
      IF report.key-08 EQ "RECALC" AND AVAILABLE(e-itemfg-vend) AND ipdQty GT 0 THEN DO:
      
          RUN getVendCost (INPUT ipdQty, INPUT ipcUom, 
                           INPUT ROWID(e-itemfg-vend), 
                           OUTPUT dCalcCost, OUTPUT dSetup).
          ASSIGN
            report.key-01 = STRING(dCalcCost)
            report.key-02 = STRING(ipdQty)
            report.key-05 = STRING((dSetup / ipdQty),"9999999999.9999")
            report.key-06 = STRING(dSetup,"9999999999.9999").
       
      END.
      
      FIND FIRST vend  NO-LOCK
          WHERE vend.company EQ g_company
            AND vend.vend-no EQ IF AVAILABLE e-item-vend THEN e-item-vend.vend-no
                                                    ELSE e-itemfg-vend.vend-no
          NO-ERROR.
       
      CREATE tt-report.
      ASSIGN tt-report.key-02 = IF report.key-08 EQ "RECALC" THEN STRING(ipdQty) ELSE report.key-02
             tt-report.key-03 = report.key-03
             tt-report.key-04 = report.key-04
             tt-report.vend-name = IF AVAILABLE vend THEN vend.NAME ELSE ""
             tt-report.report-cost = dCalcCost
             tt-report.disc-days  = IF AVAILABLE vend THEN vend.disc-days ELSE 0 
             tt-report.ext-price  = DECIMAL(report.key-02) * tt-report.report-cost
             tt-report.rec-id = RECID(report)
             tt-report.cost-uom = cUom.

        IF AVAILABLE e-itemfg-vend THEN
            ASSIGN
            tt-report.vend-item = e-itemfg-vend.vend-item
            tt-report.wid-min   = e-itemfg-vend.roll-w[27]
            tt-report.wid-max   = e-itemfg-vend.roll-w[28]
            tt-report.len-min   = e-itemfg-vend.roll-w[29]
            tt-report.len-max   = e-itemfg-vend.roll-w[30] .

       
          IF AVAILABLE job-mat THEN
          FIND FIRST job-hdr NO-LOCK WHERE job-hdr.company EQ job-mat.company
                                       AND job-hdr.job-no  EQ job-mat.job-no
                                       AND job-hdr.job-no2 EQ job-mat.job-no2
                                       NO-ERROR.
          
        IF AVAILABLE job-hdr THEN
            v-fgitem  = job-hdr.i-no .
        ELSE
            v-fgitem  = ip-i-no .

            FOR EACH fg-rcpth FIELDS(r-no rita-code po-no) NO-LOCK WHERE
            fg-rcpth.company EQ g_company AND
            fg-rcpth.i-no EQ v-fgitem AND
            /*fg-rcpth.job-no EQ job-hdr.job-no AND
            fg-rcpth.job-no2 EQ job-hdr.job-no2 AND*/
            fg-rcpth.po-no NE "" AND
            fg-rcpth.rita-code EQ "R",
              FIRST fg-rdtlh FIELDS() NO-LOCK WHERE
              fg-rdtlh.r-no EQ fg-rcpth.r-no AND
              fg-rdtlh.rita-code EQ fg-rcpth.rita-code
             
                   BY fg-rcpth.trans-date DESCENDING
                   BY fg-rdtlh.trans-time DESCENDING:
            
            FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ g_company 
                AND po-ord.po-no EQ INTEGER(fg-rcpth.po-no) NO-ERROR.
            IF AVAILABLE po-ord AND tt-report.key-03 EQ po-ord.vend-no THEN
                ASSIGN v-vend-no = po-ord.vend-no .
            IF AVAILABLE po-ord AND tt-report.key-03 EQ po-ord.vend-no AND NOT ip-rm THEN
                    tt-report.po-no = fg-rcpth.po-no .
           LEAVE.
           END.

  END.
  lIsABoard = TRUE.
  IF AVAILABLE job-mat THEN DO:
  
      FIND FIRST job-hdr NO-LOCK WHERE job-hdr.company EQ job-mat.company
                                 AND job-hdr.job-no  EQ job-mat.job-no
                                 AND job-hdr.job-no2 EQ job-mat.job-no2
                                 NO-ERROR.
      
      FIND ITEM NO-LOCK WHERE ITEM.company EQ job-mat.company
                          AND ITEM.i-no    EQ job-mat.i-no
                          NO-ERROR.
      IF AVAILABLE ITEM AND ITEM.mat-type NE "B" THEN
        lIsABoard = FALSE.
  END.
  IF AVAILABLE job-hdr AND job-hdr.est-no GT "" THEN
      FIND FIRST est NO-LOCK WHERE est.company EQ job-hdr.company
                               AND est.est-no  EQ job-hdr.est-no
                               NO-ERROR.

  IF AVAILABLE est AND lIsABoard THEN DO:
      adder-blok:
      FOR EACH tt-report.
      
    
           FOR EACH ef NO-LOCK
              WHERE ef.company EQ est.company
                AND ef.est-no  EQ est.est-no
                AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man) :

            DO li = 1 TO 6:
              IF ef.adder[li] NE ""                                          AND
                 NOT CAN-FIND(FIRST e-item-vend
                              WHERE e-item-vend.company EQ g_company
                                AND e-item-vend.i-no    EQ ef.adder[li]
                                AND e-item-vend.vend-no EQ tt-report.key-03) THEN DO:

                DELETE tt-report.
                NEXT adder-blok.
              END.
            END.
          END.
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
  DISPLAY fi_dep 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE ITEM THEN 
    DISPLAY ITEM.i-name 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE job-mat THEN 
    DISPLAY job-mat.frm job-mat.blank-no job-mat.i-no job-mat.len job-mat.wid 
          job-mat.n-up 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-2 btn_vencst Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVendCost Dialog-Frame 
PROCEDURE getVendCost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipdQtyInp AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipcUomInp AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprVendRow AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdSetup AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-wid AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-len AS DECIMAL NO-UNDO.
DEFINE VARIABLE ld-dim-charge AS DECIMAL NO-UNDO.
DEFINE VARIABLE i AS  INTEGER NO-UNDO.
DEFINE VARIABLE v-qty-comp AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-setup AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-basis-w AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-dep AS DECIMAL NO-UNDO.

DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
DEFINE BUFFER bf-e-itemfg FOR e-itemfg.

FIND bf-e-itemfg-vend NO-LOCK WHERE ROWID(bf-e-itemfg-vend) EQ iprVendRow NO-ERROR.
/* Internal error - should be logged if we had a system log */
IF NOT AVAIL bf-e-itemfg-vend THEN
    RETURN.

FIND FIRST bf-e-itemfg NO-LOCK
     WHERE bf-e-itemfg.company EQ bf-e-itemfg-vend.company
     AND bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
     NO-ERROR.
FIND bf-itemfg NO-LOCK WHERE bf-itemfg.company EQ cocode
    AND bf-itemfg.i-no EQ bf-e-itemfg-vend.i-no 
    NO-ERROR.

  ASSIGN
     v-wid          = bf-itemfg.t-wid
     v-len          = bf-itemfg.t-len
     v-dep          = 0
     v-basis-w      = bf-itemfg.t-wid * bf-itemfg.t-len * 100
     v-basis-w      = bf-itemfg.weight-100 /
                              (IF v-corr THEN (v-basis-w * .007)
                                         ELSE (v-basis-w / 144) /
                               1000).
  v-qty-comp = ipdQtyInp.
  
      /* quantity was already converted */
/*   IF ipcUomInp NE bf-e-itemfg-vend.std-uom THEN            */
/*     RUN sys/ref/convquom.p(ipcUomInp, bf-e-itemfg.std-uom, */
/*                            v-basis-w, v-len, v-wid, v-dep, */
/*                            ipdQtyInp, OUTPUT v-qty-comp).  */

 /* quantity may be below 1 */
 /* {sys/inc/roundup.i v-qty-comp} */
   
  DO i = 1 TO EXTENT(bf-e-itemfg-vend.run-qty):      
      
    IF v-qty-comp LE bf-e-itemfg-vend.run-qty[i] THEN LEAVE.
  END.
  IF i GT 20 THEN
      i = 20.

  
  v-setup = bf-e-itemfg-vend.setups[i].
      
  ld-dim-charge = 0.

  RUN est/dim-charge.p (bf-e-itemfg-vend.rec_key,
                        v-wid,
                        v-len,
                        INPUT-OUTPUT ld-dim-charge).     
  
  IF AVAILABLE bf-e-itemfg-vend THEN
    ASSIGN opdCost = bf-e-itemfg-vend.run-cost[i] + ld-dim-charge.
           opdSetup = v-setup.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

