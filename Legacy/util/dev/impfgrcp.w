&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}


DEF TEMP-TABLE FGReceiptRow LIKE fg-rctd
             field TableRowid as rowid.

DEF TEMP-TABLE tt-rcpth LIKE fg-rcpth.
DEF TEMP-TABLE tt-rdtlh LIKE fg-rdtlh.

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO.


DEF TEMP-TABLE tt-email NO-UNDO FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .
{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW}

DEF VAR v-fgpostgl AS CHAR NO-UNDO.                        
def var v-post-date as date init today no-undo.

{custom/globdefs.i}
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.
/*                    */
/*assign              */
/* cocode = gcompany. */
/*IF cocode EQ '' THEN*/
/*    cocode = '001'. */

DEF STREAM logFile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Definitions for FRAME fMain                                          */
&Scoped-define QUERY-STRING-fMain FOR EACH itemfg SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH itemfg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-fMain itemfg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS xmlFile btOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS xmlFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Close" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btOk 
     LABEL "Import" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE xmlFile AS CHARACTER FORMAT "x(60)" 
     LABEL "XML File" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     xmlFile AT ROW 3.62 COL 12 COLON-ALIGNED WIDGET-ID 2
     btOk AT ROW 6.48 COL 19 WIDGET-ID 6
     BtnCancel AT ROW 6.48 COL 48 WIDGET-ID 8
     "Enter an FG Receipts XML file and press OK" VIEW-AS TEXT
          SIZE 55 BY 1.19 AT ROW 1.48 COL 17 WIDGET-ID 4
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.2 BY 10.48
         CANCEL-BUTTON BtnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Import FG Receipts"
         HEIGHT             = 10.48
         WIDTH              = 79.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "asi.itemfg"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Import FG Receipts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Import FG Receipts */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel wWin
ON CHOOSE OF BtnCancel IN FRAME fMain /* Close */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk wWin
ON CHOOSE OF btOk IN FRAME fMain /* Import */
DO:
  DEF VAR v-i-no LIKE itemfg.i-no NO-UNDO.
  
  run PreFGImport.  /*validate records from XML file */
  RUN ImportFGReceipts.
  
  MESSAGE "FG Receipts import is completed!  Posting now......"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
      
  /*run PostFGImport.*/   
  run fg/fgpost.p (input table FGReceiptRow). 
  
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME xmlFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL xmlFile wWin
ON HELP OF xmlFile IN FRAME fMain /* XML File */
DO:
   DEF VAR chFile AS CHAR FORMAT "X(80)" NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

   DEF VAR v-path AS CHAR NO-UNDO.
   /*
   ASSIGN v-path = TRIM(scr-label-file:SCREEN-VALUE).

    IF TRIM(v-path) EQ "" THEN DO:
        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ cocode
              AND sys-ctrl.name EQ "CASLABEL" NO-ERROR.
        IF AVAIL sys-ctrl THEN
            ASSIGN v-path = TRIM(sys-ctrl.char-fld).

    END.
    
    RUN sys\ref\char-fld-help.w(INPUT cocode,
                                INPUT v-path,
                                OUTPUT chFile).

  
   */

      system-dialog get-file chFile 
                    title "Select FG Receipts XML File"
                    filters "XML (*.xml) " "*.xml"
                    initial-dir v-path
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE ll-ok.

      IF ll-ok THEN
         ASSIGN xmlFile:SCREEN-VALUE = chFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY xmlFile 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE xmlFile btOk BtnCancel 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work wWin 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportFGReceipts wWin 
PROCEDURE ImportFGReceipts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 

  /*  write xml files ===
  FOR EACH fg-rctd WHERE rita-code = "R":
      CREATE tt-rctd.
      BUFFER-COPY fg-rctd TO tt-rctd.
  END.

  TEMP-TABLE tt-rctd:WRITE-XML("FILE","c:\temp\fg-rctd.xml", TRUE).
  ==========*/
  
  def var lv-rno as int no-undo.
  
  FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL fg-rctd AND fg-rctd.r-no GT lv-rno THEN lv-rno = fg-rctd.r-no.
  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
 
  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL fg-rctd THEN NEXT.
    LEAVE.
  END.

  empty temp-table FGReceiptRow.  
  TEMP-TABLE FGReceiptRow:READ-XML ("File", xmlfile:screen-value in frame {&frame-name}, "Empty",?,NO).
  
  FOR EACH FGReceiptRow:
   
    create fg-rctd.
    buffer-copy FGReceiptRow to fg-rctd.
    assign fg-rctd.r-no = lv-rno
           fg-rctd.rita-code = "R"
           fg-rctd.trans-time   = TIME
           /*fg-rctd.rct-date    = TODAY
             fg-rctd.units-pallet = 1
             fg-rctd.cases-unit   = 1
             fg-rctd.ext-cost     = 0
           */
           
           FGReceiptRow.TableRowid = rowid(fg-rctd)
           lv-rno = lv-rno + 1
           .     
  def var lv-error as log no-undo.
  def var lv-error-msg as cha no-undo.          
  run ValidateFGImport (output lv-error, output lv-error-msg).

     RUN fg/invrecpt.p (ROWID(fg-rctd), 1).   
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostFGImport wWin 
PROCEDURE PostFGImport :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF BUFFER b-fg-rcpts FOR fg-rcpts.
  DEF BUFFER b-fg-rdtl FOR fg-rdtl.
  DEF BUFFER b-fg-bin FOR fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  DEF BUFFER b-itemfg1 FOR itemfg.
  DEF BUFFER ps-rctd FOR fg-rctd .
  DEF BUFFER b-po-ordl FOR po-ordl.
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR v-one-item AS LOG.
  DEF VAR v-dec AS DEC DECIMALS 10.
  DEF VAR v-po-no LIKE rm-rcpt.po-no NO-UNDO.
  DEF VAR x AS INT NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR v-r-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-i-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-t-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-overrun-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-underrun-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  DEF VAR v-recid AS RECID NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  DEF VAR ld-cvt-qty AS DEC NO-UNDO.
  DEF VAR ld-cvt-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR v-autobin  AS cha NO-UNDO.
  DEF VAR v-newhdr AS LOG NO-UNDO. 
  DEF VAR v-fin-qty AS DEC NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR v-trnum LIKE gl-ctrl.trnum NO-UNDO.
  DEF VAR uperiod AS INT NO-UNDO.
  DEF VAR sysdate AS DATE INIT TODAY NO-UNDO.    
  DEF VAR v-date LIKE sysdate NO-UNDO.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.

  define var cocode as cha no-undo.
  define var g_company as cha no-undo.
  DEF VAR fg-uom-list  AS CHAR NO-UNDO.
  
  do transaction:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}
    {sys/inc/adjustgl.i}
    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
    {sys/inc/fgpost.i}
  END.

  v-fgpostgl = fgpostgl.
  
  RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
  
for each FGReceiptRow no-lock /* where FGReceiptRow.TableRowid <> ? */ : 
  assign cocode = FGReceiptRow.company
         g_company = FGReceiptRow.company
         .
  
  FIND FIRST period NO-LOCK
      WHERE period.company EQ  cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

  FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
                         AND sys-ctrl.name    EQ "AUTOPOST"
       NO-LOCK NO-ERROR.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  /* Create a single workfile record for the finished good being posted */
  CREATE w-fg-rctd.
  BUFFER-COPY FGReceiptRow TO w-fg-rctd
  ASSIGN w-fg-rctd.row-id  = FGReceiptRow.TableRowid
         w-fg-rctd.has-rec = YES.

  FOR EACH w-fg-rctd,

        FIRST itemfg
        WHERE itemfg.company eq cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no

        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.r-no:            

      {fg/fg-post.i w-fg-rctd w-fg-rctd}

      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.

      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

      IF AVAIL fg-rctd THEN DO:
        ASSIGN
         fg-rctd.rita-code = "P"  /* posted */
         fg-rctd.post-date = v-post-date
         fg-rctd.tag2      = w-fg-rctd.tag2.

        FOR EACH fg-rcpts
            WHERE fg-rcpts.company EQ fg-rctd.company
              AND fg-rcpts.r-no    EQ fg-rctd.r-no:
          fg-rcpts.rita-code = fg-rctd.rita-code.
        END.
      END.

    END.  /* for each fg-rctd */

    FIND CURRENT itemfg NO-LOCK NO-ERROR.

    FOR EACH w-fg-rctd
        BREAK BY w-fg-rctd.i-no
              BY w-fg-rctd.job-no
              BY w-fg-rctd.job-no2
              BY w-fg-rctd.loc
              BY w-fg-rctd.loc-bin
              BY w-fg-rctd.tag:

      IF LAST-OF(w-fg-rctd.tag) THEN DO:
        IF TRIM(w-fg-rctd.tag) NE "" THEN 
        /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
        
        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company EQ g_company
              AND fg-bin.i-no    EQ loadtag.i-no
              AND fg-bin.tag     EQ loadtag.tag-no
            USE-INDEX tag:
          RUN fg/calcbinq.p (ROWID(fg-bin)).
        END.

        /* IF w-fg-rctd.tag <> "" then*/
        FIND FIRST loadtag
            WHERE loadtag.company   EQ g_company
              AND loadtag.item-type EQ NO
              AND loadtag.tag-no    EQ w-fg-rctd.tag
              AND loadtag.i-no      EQ w-fg-rctd.i-no
              AND loadtag.job-no    EQ w-fg-rctd.job-no
            USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL loadtag THEN DO:
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ g_company
                AND fg-bin.i-no    EQ loadtag.i-no
                AND fg-bin.tag     EQ loadtag.tag-no
              /*AND fg-bin.job-no = loadtag.job-no
                AND fg-bin.job-no2 = loadtag.job-no2*/
                AND fg-bin.qty     GT 0
              USE-INDEX tag NO-LOCK NO-ERROR.
          IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/
             TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/ 
            ASSIGN
             loadtag.loc          = w-fg-rctd.loc2   
             loadtag.loc-bin      = w-fg-rctd.loc-bin2
             loadtag.qty          = fg-bin.qty
             loadtag.pallet-count = fg-bin.qty
             loadtag.partial      = fg-bin.partial-count
             loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.
          ELSE /*partial transfer */
            ASSIGN
             loadtag.loc     = w-fg-rctd.loc
             loadtag.loc-bin = w-fg-rctd.loc-bin.

          FIND CURRENT loadtag NO-LOCK NO-ERROR.
        END.
      END.
    END.

    FOR EACH w-inv:
      DELETE w-inv.
    END.

    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK:

      CREATE w-inv.
      w-inv.row-id = w-fg-rctd.row-id.
    END.

    RUN fg/invrecpt.p (?, 2).

    FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK
        BREAK BY w-fg-rctd.i-no:

      IF LAST-OF(w-fg-rctd.i-no) THEN DO:
        RUN fg/updfgcs1.p (RECID(itemfg), NO).

        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ cocode
              AND oe-ordl.opened  EQ YES
              AND oe-ordl.i-no    EQ w-fg-rctd.i-no
              AND oe-ordl.job-no  EQ ""
              AND oe-ordl.cost    EQ 0
            USE-INDEX opened NO-LOCK
            BREAK BY oe-ordl.ord-no
            TRANSACTION :

          DO i = 1 TO 1000:
            FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
            IF AVAIL b-oe-ordl THEN DO:
              IF itemfg.prod-uom EQ "M" THEN
                b-oe-ordl.cost = itemfg.total-std-cost.
              ELSE
                RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                        THEN "EA" ELSE itemfg.prod-uom),
                                       "M", 0, 0, 0, 0,
                                       itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
              LEAVE.
            END.
          END.
        END.
      END.
    END.

    IF v-fgpostgl NE "None" THEN DO TRANSACTION:
      /* gdm - 11050906 */
      REPEAT:
       FIND FIRST gl-ctrl EXCLUSIVE-LOCK
         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
       IF AVAIL gl-ctrl THEN DO:
         ASSIGN v-trnum       = gl-ctrl.trnum + 1
                gl-ctrl.trnum = v-trnum.

         FIND CURRENT gl-ctrl NO-LOCK.
         
         RUN gl-from-work (1, v-trnum).
         RUN gl-from-work (2, v-trnum).
         
         LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */
    END.
    FIND FIRST w-job NO-ERROR.
    IF AVAIL w-job THEN DO:
      RUN jc/d-jclose.w.
    END.

    IF v-adjustgl THEN DO TRANSACTION:
      /** GET next G/L TRANS. POSTING # **/
      FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK.
      ASSIGN
       v-trnum       = gl-ctrl.trnum + 1
       gl-ctrl.trnum = v-trnum.
      FIND CURRENT gl-ctrl NO-LOCK.
      FOR EACH work-job BREAK BY work-job.actnum:
         CREATE gltrans.
        ASSIGN
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "ADJUST"
         gltrans.tr-date = v-post-date
         gltrans.period  = period.pnum
         gltrans.trnum   = v-trnum.

        IF work-job.fg THEN
          ASSIGN
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT FG".
        ELSE
          ASSIGN
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT COGS".

        RELEASE gltrans.
      END. /* each work-job */
    END.
    IF v-got-fgemail THEN DO:
      RUN send-fgemail (v-fgemail-file).
    END.
  end. /* for each FGReceiptRow */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreFGImport wWin 
PROCEDURE PreFGImport :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  def var lv-error as log no-undo.
  def var lv-error-msg as cha no-undo.
  
  lv-error = no. 
  TEMP-TABLE FGReceiptRow:READ-XML ("File", xmlfile:screen-value in frame {&frame-name}, "Empty",?,NO).
  
  /* validate main key columns - company,rita-code,tag,po-no,job-no,i-no,loc,loc-bin,tag2,loc2,loc-bin2,cost-uom */
  for each FGReceiptRow no-lock: 
/*      run validate-company (FGReceiptRow.Company, output lv-error, output lv-error-msg).                   */
/*      run validate-ritaCode(FGReceiptRow.rita-code,output lv-error, output lv-error-msg).                  */
/*      run validate-tag (FGReceiptRow.tag, output lv-error, output lv-error-msg).                           */
/*      run validate-po-no (FGReceiptRow.po-no, output lv-error, output lv-error-msg).                       */
/*      run validate-job-no(FGReceiptRow.job-no, FGReceiptRow.job-no2, output lv-error, output lv-error-msg).*/
/*      run validate-i-no (FGReceiptRow.i-no, output lv-error, output lv-error-msg).                         */
/*      run validate-loc (FGReceiptRow.loc, FGReceiptRow.loc-bin, output lv-error, output lv-error-msg).     */
/*      run validate-tag(FGReceiptRow.tag2, output lv-error, output lv-error-msg).                           */
/*      run validate-loc (FGReceiptRow.loc2, FGReceiptRow.loc-bin2, output lv-error, output lv-error-msg).   */
/*      run validate-uom (FGReceiptRow.cost-uom, output lv-error, output lv-error-msg).                      */
    /*    run ValidateFGImport (output lv-error, output lv-error-msg). */
           
  end.
      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateFGImport wWin 
PROCEDURE ValidateFGImport :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    def output param op-error as log no-undo.
    def output param op-error-msg as cha no-undo.
    
    def var lv-uom-list as cha no-undo.
    
    /* validate rita-code */
    if not can-do(FGReceiptRow.rita-code,"R,T") 
    then assign op-error = yes
                op-error-msg = "Invalid RITA code!"
                .
                
    /* validate tag */
    
    /* validate po-no */
                
    /* validate cost-uom */
    RUN sys/ref/uom-fg.p (NO, OUTPUT lv-uom-list).
    IF INDEX(lv-uom-list,FGReceiptRow.cost-uom) LE 0 
    THEN assign op-error = yes
                op-error-msg = "Invalid Cost-UOM!"     
                .
    /* cost calc if no cost imported */
    if FGReceiptRow.std-cost = 0 then do:
       if FGReceiptRow.job-no <> "" then do:
          find job-hdr where job-hdr.company = cocode
                         and job-hdr.job-no = FGReceiptRow.job-no                
                         and job-hdr.job-no2 = FGReceiptRow.job-no2 no-lock no-error. 
          if avail job-hdr then FGReceiptRow.std-cost = (job-hdr.std-mat-cost +
                                   job-hdr.std-lab-cost +
                                   job-hdr.std-fix-cost +
                                   job-hdr.std-var-cost) . 
       end.
       else if FGReceiptRow.po-no <> "" then do:
           def var lv-cost as dec no-undo.
           find first po-ordl where po-ordl.company = cocode and
                              po-ordl.po-no = integer(FGReceiptRow.po-no) and
                              po-ordl.i-no = FGReceiptRow.i-no
                                  no-lock no-error.
           if avail po-ordl then do:
                  assign FGReceiptRow.cost-uom = po-ordl.pr-uom
                         lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1).
                  find fg-rctd where rowid(fg-rctd) = FGReceiptRow.TableRowid no-lock no-error.
                  DEF VAR lv-use-full-qty AS LOG.
                  DEF VAR lv-full-qty AS DEC NO-UNDO.      
                  DEF VAR lvCalcCostUom LIKE fg-rctd.cost-uom NO-UNDO.
                  DEF VAR lvCalcStdCost LIKE fg-rctd.std-cost NO-UNDO.
                  DEF VAR lvCalcExtCost LIKE fg-rctd.ext-cost NO-UNDO.
                  DEF VAR lvCalcFrtCost LIKE fg-rctd.frt-cost NO-UNDO.
                  DEF VAR lvSetupPerCostUom AS DEC NO-UNDO.  
                  RUN fg/calcRcptCostFromPO.p 
                    (INPUT cocode ,
                     INPUT ROWID(po-ordl),
                     INPUT ROWID(fg-rctd),
                     INPUT FGReceiptRow.qty-case,
                     INPUT string(FGReceiptRow.cases),
                     INPUT fg-rctd.partial,
                     INPUT fg-rctd.job-no,
                     INPUT fg-rctd.job-no2,
                     INPUT fg-rctd.cost-uom,
                     INPUT fg-rctd.t-qty,
                     OUTPUT lv-use-full-qty,
                     OUTPUT lv-full-qty,
                     OUTPUT lvCalcCostUom,
                     OUTPUT lvCalcStdCost,
                     OUTPUT lvCalcExtCost,
                     OUTPUT lvCalcFrtCost,
                     OUTPUT lvSetupPerCostUom).
      
                  ASSIGN FGReceiptRow.cost-uom = lvCalcCostUom
                         FGReceiptRow.std-cost = (lvCalcStdCost)
                         FGReceiptRow.ext-cost = (lvCalcExtCost)
                         lv-cost = FGReceiptRow.std-cost.    
                            
                  /*RUN convert-vend-comp-curr(INPUT-OUTPUT lv-cost).*/
                  FIND FIRST po-ord WHERE po-ord.company EQ po-ordl.company AND
                                          po-ord.po-no   EQ po-ordl.po-no    NO-LOCK NO-ERROR.
                  IF AVAIL po-ord THEN DO:
                     FIND FIRST vend WHERE vend.company EQ po-ord.company AND
                                           vend.vend-no EQ po-ord.vend-no NO-LOCK NO-ERROR.
                     IF AVAIL vend THEN  DO:
                        FIND FIRST company WHERE company.company EQ cocode NO-LOCK.
                        IF vend.curr-code NE company.curr-code THEN DO:
                           FIND FIRST currency WHERE currency.company EQ po-ord.company AND
                                                     currency.c-code EQ vend.curr-code  
                                                     NO-LOCK NO-ERROR.
                           IF AVAIL currency THEN lv-cost = lv-cost * currency.ex-rate.               
                        END.
                     END.
                  END.      
           END.
           FGReceiptRow.std-cost = lv-cost.                                          
       end.
       else if FGReceiptRow.i-no <> "" then do:
            find itemfg where itemfg.company = cocode
                          and itemfg.i-no = FGReceiptRow.i-no no-lock no-error.
            if avail itemfg then 
               assign FGReceiptRow.std-cost = itemfg.avg-cost
                      FGReceiptRow.cost-uom = itemfg.prod-uom
                      .
       end. 
    end.
                       
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

