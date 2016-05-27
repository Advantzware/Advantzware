&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
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

def input param v-term like report.term-id no-undo.
def input param v-recid as recid no-undo.
DEF INPUT PARAM ip-rm AS LOG NO-UNDO.
DEF INPUT PARAM ip-i-no AS CHAR NO-UNDO.
DEF INPUT PARAM ipdQty AS DEC NO-UNDO.  /* Qty to use for cost determination */
DEF INPUT PARAM ipcUom AS CHAR NO-UNDO.  /* UOm to use for cost determination */
DEF VAR li AS INT NO-UNDO.
def shared var fil_id as recid no-undo.
DEF VAR v-vend-no AS CHAR NO-UNDO .

def temp-table tt-report field key-03 like report.key-03
                         field key-04 like report.key-04
                         field key-02 like report.key-02 LABEL "MSF"
                         field vend-name as cha 
                         field report-cost as dec
                         FIELD cost-uom AS CHAR
                         field disc-days like vend.disc-days
                         field ext-price as dec
                         field rec-id as recid
                         FIELD vend-item AS CHAR
                         FIELD wid-min AS DECIMAL 
                         FIELD wid-max AS DECIMAL
                         FIELD len-min AS DECIMAL
                         FIELD len-max AS DECIMAL 
                         FIELD po-no   AS CHAR 
                         .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-report item job-mat

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-report.key-03 tt-report.po-no tt-report.vend-name tt-report.key-04 tt-report.key-02 tt-report.report-cost tt-report.cost-uom tt-report.disc-days tt-report.ext-price tt-report.vend-item tt-report.wid-min tt-report.wid-max tt-report.len-min tt-report.len-max   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-report NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-report


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame job-mat.frm job-mat.blank-no ~
job-mat.i-no item.i-name job-mat.len job-mat.wid job-mat.n-up 
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH item SHARE-LOCK, ~
      EACH job-mat OF item SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH item SHARE-LOCK, ~
      EACH job-mat OF item SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame item job-mat
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame item
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame job-mat


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 btn_vencst Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS job-mat.frm job-mat.blank-no job-mat.i-no ~
item.i-name job-mat.len job-mat.wid job-mat.n-up 
&Scoped-define DISPLAYED-TABLES job-mat item
&Scoped-define FIRST-DISPLAYED-TABLE job-mat
&Scoped-define SECOND-DISPLAYED-TABLE item
&Scoped-Define DISPLAYED-OBJECTS fi_dep 

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
      tt-report.key-03 label "Vendor#" WIDTH 13
      tt-report.po-no label "Last Po#" WIDTH 20
      tt-report.vend-name label "Name" WIDTH 30
      tt-report.key-04 label "Tons"    WIDTH 10
      tt-report.key-02                 WIDTH 10
      tt-report.report-cost label "Cost"
      tt-report.cost-uom    LABEL "UOM" WIDTH 6
      tt-report.disc-days   label "Lead"
      tt-report.ext-price   label "Ext Cost" FORM "->>,>>>,>>9.99"
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
     job-mat.frm AT ROW 2.67 COL 3.4 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     job-mat.blank-no AT ROW 2.67 COL 6.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     job-mat.i-no AT ROW 2.67 COL 12.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     item.i-name AT ROW 2.67 COL 34.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     job-mat.len AT ROW 2.67 COL 78.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     job-mat.wid AT ROW 2.67 COL 89.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi_dep AT ROW 2.67 COL 100.4 COLON-ALIGNED NO-LABEL
     job-mat.n-up AT ROW 2.67 COL 112.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     BROWSE-2 AT ROW 4.33 COL 3
     btn_vencst AT ROW 16.24 COL 3.2
     Btn_OK AT ROW 16.24 COL 38
     Btn_Cancel AT ROW 16.24 COL 70
     "Name" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 42.4
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 81.4
     "S / B" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.95 COL 5.4
     "Depth" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 103.4
     "RM Item#" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.95 COL 17.4
     "#Up" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.95 COL 114.4
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 93.4
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

&Scoped-define SELF-NAME Dialog-Frame
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

&Scoped-define SELF-NAME Btn_Cancel
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   fil_id = tt-report.rec-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_vencst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_vencst Dialog-Frame
ON CHOOSE OF btn_vencst IN FRAME Dialog-Frame /* Add Vendor Cost */
DO:
  
  IF ip-rm THEN
    RUN windows\d-vndcst.w(INPUT ip-i-no, v-term).
  ELSE
    RUN windows\d-vndcfg.w(INPUT ip-i-no, v-term).

  RUN build-table.
  {&open-query-{&browse-name}}

  IF AVAIL tt-report THEN fil_id = tt-report.rec-id.
  APPLY 'go' TO this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2


&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-2 IN FRAME Dialog-Frame
DO:
 
IF AVAIL tt-report AND tt-report.key-03 = v-vend-no THEN DO:
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

  find job-mat where recid(job-mat) = v-recid no-lock no-error.
  if avail job-mat then DO WITH FRAME {&FRAME-NAME}:
     find first item where item.company = job-mat.company
                       and item.i-no = job-mat.i-no no-lock no-error.
     fi_dep = IF job-mat.dep GT 0 THEN job-mat.dep ELSE item.s-dep.

     IF INDEX("1234",item.mat-type) GT 0 THEN
       tt-report.key-02:LABEL IN BROWSE {&browse-name} = "BF".
  end.
  ELSE
  DO:
     find job-prep where recid(job-prep) = v-recid no-lock no-error.
     if avail job-prep then
     DO:

        FIND FIRST prep WHERE
             prep.company EQ job-prep.company AND
             prep.CODE EQ job-prep.CODE
             NO-LOCK NO-ERROR.

        IF AVAIL prep THEN
           find first item where
                item.company = prep.company AND
                item.i-no = prep.i-no
                no-lock no-error.

        IF AVAIL ITEM THEN
           fi_dep = item.s-dep.
     END.
  end.
  
  run build-table.
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
  DEF VAR cUom AS CHAR NO-UNDO.
  DEF VAR dQty AS DEC NO-UNDO.
  DEF VAR dCalcCost AS DEC NO-UNDO.
  DEF VAR dSetup AS DEC NO-UNDO.
  DEF VAR lIsABoard AS LOG NO-UNDO.
  DEF VAR v-fgitem AS CHAR NO-UNDO.
  EMPTY TEMP-TABLE tt-report.
  DEF BUFFER bf-report FOR report.
  for each bf-report where bf-report.term-id eq v-term NO-LOCK:
      FIND report WHERE ROWID(report) EQ ROWID(bf-report) EXCLUSIVE-LOCK.

      RELEASE e-item-vend.
      RELEASE e-itemfg-vend.

      FIND FIRST e-item-vend WHERE RECID(e-item-vend) EQ report.rec-id
          NO-LOCK NO-ERROR.
      IF NOT AVAIL e-item-vend THEN
      FIND FIRST e-itemfg-vend where recid(e-itemfg-vend) EQ report.rec-id
          NO-LOCK NO-ERROR.
      cUom = "".
      IF AVAIL e-item-vend THEN
          cUom = e-item-vend.std-uom.
      ELSE IF AVAIL e-itemfg-vend THEN
          cUom = e-itemfg-vend.std-uom.
      dCalcCost = DECIMAL(report.key-01).
      IF cUom EQ "" AND AVAIL e-item-vend THEN DO:
          
          FIND FIRST e-itemfg WHERE e-itemfg.company EQ e-itemfg-vend.company
              AND e-itemfg.i-no EQ e-itemfg-vend.vend-no
              NO-LOCK NO-ERROR.
          IF AVAIL e-itemfg THEN
              cUom = e-itemfg.std-uom.

      END.
               
      IF report.key-08 EQ "RECALC" AND AVAIL(e-itemfg-vend) AND ipdQty GT 0 THEN DO:
      
          RUN getVendCost (INPUT ipdQty, INPUT ipcUom, 
                           INPUT ROWID(e-itemfg-vend), 
                           OUTPUT dCalcCost, OUTPUT dSetup).
          ASSIGN
            report.key-01 = STRING(dCalcCost)
            report.key-02 = STRING(ipdQty)
            report.key-05  = STRING((dSetup / ipdQty),"9999999999.9999")
            report.key-06  = STRING(dSetup,"9999999999.9999").
       
      END.
      
      find first vend
          where vend.company = g_company
            and vend.vend-no = IF AVAIL e-item-vend THEN e-item-vend.vend-no
                                                    ELSE e-itemfg-vend.vend-no
          no-lock no-error.
       
      create tt-report.
      assign tt-report.key-02 = IF report.key-08 EQ "RECALC" THEN string(ipdQty) ELSE report.key-02
             tt-report.key-03 = report.key-03
             tt-report.key-04 = report.key-04
             tt-report.vend-name = if avail vend then vend.name else ""
             tt-report.report-cost = dCalcCost
             tt-report.disc-days  = if avail vend then vend.disc-days else 0 
             tt-report.ext-price  = dec(report.key-02) * tt-report.report-cost
             tt-report.rec-id = recid(report)
             tt-report.cost-uom = cUom.

        IF AVAIL e-itemfg-vend THEN
            ASSIGN
            tt-report.vend-item = e-itemfg-vend.vend-item
            tt-report.wid-min   = e-itemfg-vend.roll-w[27]
            tt-report.wid-max   = e-itemfg-vend.roll-w[28]
            tt-report.len-min   = e-itemfg-vend.roll-w[29]
            tt-report.len-max   = e-itemfg-vend.roll-w[30] .

       
          IF AVAIL job-mat THEN
          FIND FIRST job-hdr WHERE job-hdr.company = job-mat.company
                           AND job-hdr.job-no  = job-mat.job-no
                           AND job-hdr.job-no2 = job-mat.job-no2
                         NO-LOCK NO-ERROR.
          
        IF AVAIL job-hdr THEN
            v-fgitem  = job-hdr.i-no .
        ELSE
            v-fgitem  = ip-i-no .

            FOR EACH fg-rcpth FIELDS(r-no rita-code po-no) WHERE
            fg-rcpth.company EQ g_company AND
            fg-rcpth.i-no EQ v-fgitem AND
            /*fg-rcpth.job-no EQ job-hdr.job-no AND
            fg-rcpth.job-no2 EQ job-hdr.job-no2 AND*/
            fg-rcpth.po-no NE "" AND
            fg-rcpth.rita-code EQ "R"
            NO-LOCK,
              FIRST fg-rdtlh fields() WHERE
              fg-rdtlh.r-no EQ fg-rcpth.r-no AND
              fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
              NO-LOCK
                   BY fg-rcpth.trans-date DESC
                   BY fg-rdtlh.trans-time DESC:
            
            FIND FIRST po-ord WHERE po-ord.company = g_company 
                AND po-ord.po-no = int(fg-rcpth.po-no) NO-LOCK NO-ERROR.
            IF AVAIL po-ord AND tt-report.key-03 = po-ord.vend-no THEN
                ASSIGN v-vend-no = po-ord.vend-no .
            IF AVAIL po-ord AND tt-report.key-03 = po-ord.vend-no AND NOT ip-rm THEN
                    tt-report.po-no = fg-rcpth.po-no .
           LEAVE.
           END.

  end.
  lIsABoard = TRUE.
  IF AVAIL job-mat THEN DO:
  
      FIND FIRST job-hdr WHERE job-hdr.company = job-mat.company
                           AND job-hdr.job-no  = job-mat.job-no
                           AND job-hdr.job-no2 = job-mat.job-no2
                         NO-LOCK NO-ERROR.
      
      FIND ITEM WHERE ITEM.company EQ job-mat.company
                  AND ITEM.i-no    EQ job-mat.i-no
                NO-LOCK NO-ERROR.
      IF AVAIL ITEM AND ITEM.mat-type NE "B" THEN
        lIsABoard = FALSE.
  END.
  IF AVAIL job-hdr AND job-hdr.est-no GT "" THEN
      FIND FIRST est WHERE est.company = job-hdr.company
                       AND est.est-no  = job-hdr.est-no
                     NO-LOCK NO-ERROR.

  IF AVAIL est AND lIsABoard THEN DO:
      adder-blok:
      FOR EACH tt-report.
      
    
           FOR EACH ef
              WHERE ef.company EQ est.company
                AND ef.est-no  EQ est.est-no
                AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
              NO-LOCK:

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
  IF AVAILABLE item THEN 
    DISPLAY item.i-name 
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
DEF INPUT PARAMETER ipdQtyInp      AS DEC NO-UNDO.
DEF INPUT PARAMETER ipcUomInp      AS CHAR NO-UNDO.
DEF INPUT PARAMETER iprVendRow  AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opdCost    AS DEC NO-UNDO.
DEF OUTPUT PARAMETER opdSetup   AS DEC NO-UNDO.
DEF VAR v-wid                   AS DEC NO-UNDO.
DEF VAR v-len                   AS DEC NO-UNDO.
DEF VAR ld-dim-charge           AS DEC NO-UNDO.
DEF VAR i                       AS  INT NO-UNDO.
DEF VAR v-qty-comp              AS DEC NO-UNDO.
DEF VAR v-setup                 AS DEC NO-UNDO.
DEF VAR v-basis-w               AS DEC NO-UNDO.
DEF VAR v-dep                   AS DEC NO-UNDO.

DEF BUFFER bf-itemfg FOR itemfg.
DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
DEF BUFFER bf-e-itemfg FOR e-itemfg.

FIND bf-e-itemfg-vend WHERE ROWID(bf-e-itemfg-vend) EQ iprVendRow NO-LOCK NO-ERROR.
/* Internal error - should be logged if we had a system log */
IF NOT AVAIL bf-e-itemfg-vend THEN
    RETURN.

FIND FIRST bf-e-itemfg WHERE bf-e-itemfg.company EQ bf-e-itemfg-vend.company
      AND bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
    NO-LOCK NO-ERROR.
FIND bf-itemfg WHERE bf-itemfg.company EQ cocode
    AND bf-itemfg.i-no = bf-e-itemfg-vend.i-no 
    NO-LOCK NO-ERROR.

  ASSIGN
     v-wid          = bf-itemfg.t-wid
     v-len          = bf-itemfg.t-len
     v-dep          = 0
     v-basis-w      = bf-itemfg.t-wid * bf-itemfg.t-len * 100
     v-basis-w      = bf-itemfg.weight-100 /
                              (if v-corr then (v-basis-w * .007)
                                         else (v-basis-w / 144) /
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
  IF i > 20 THEN
      i = 20.

  
  v-setup = bf-e-itemfg-vend.setups[i].
      
  ld-dim-charge = 0.

  RUN est/dim-charge.p (bf-e-itemfg-vend.rec_key,
                        v-wid,
                        v-len,
                        INPUT-OUTPUT ld-dim-charge).     
  
  IF AVAIL bf-e-itemfg-vend THEN
    ASSIGN opdCost = bf-e-itemfg-vend.run-cost[i] + ld-dim-charge.
           opdSetup = v-setup.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

