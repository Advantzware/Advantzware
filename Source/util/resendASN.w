&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\resendASN.w

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

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

{oe/rep/oe-lad.i NEW}

  DEFINE  NEW SHARED VARIABLE ccXMLOutput AS CHARACTER NO-UNDO.
  DEFINE  NEW SHARED VARIABLE clXMLOutput AS LOGICAL NO-UNDO.
  DEFINE  NEW SHARED VARIABLE ciXMLOutput AS INTEGER NO-UNDO.
  
  DEFINE  NEW SHARED STREAM cXMLOutput.
assign
 clXMLOutput = true
 .
cocode = "001".
DEFINE VARIABLE v-process AS LOG NO-UNDO.
def stream s1.
DEF STREAM s2.
DEF STREAM s3.
def temp-table ttasn
    field shipid as int
    field cFileName as char
    index i1 shipid.
DEF TEMP-TABLE ttMissingASN
    FIELD iMissOrder AS int
    FIELD iMissBol AS int
    .
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fiCustPO fiBOL# fiOrder btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiCustPO fiBOL# fiOrder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Send ASN" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fiBOL# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "BOL#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Enter BOL # to unpost" NO-UNDO.

DEFINE VARIABLE fiCustPO AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Order#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiCustPO AT ROW 2.67 COL 28 COLON-ALIGNED WIDGET-ID 2
     fiBOL# AT ROW 3.71 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Estimate Number"
     fiOrder AT ROW 4.76 COL 28 COLON-ALIGNED WIDGET-ID 4
     btn-process AT ROW 7.19 COL 18
     btn-cancel AT ROW 7.19 COL 50
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1 COL 2
     RECT-17 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.4 BY 9.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Send Missing ASNs"
         HEIGHT             = 9.05
         WIDTH              = 90.6
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mark BOL as unposted */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mark BOL as unposted */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Send ASN */
DO:
   RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiCustPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustPo C-Win
ON LEAVE OF fiCustPO IN FRAME FRAME-A /* Send ASN */
DO:
DEFINE VARIABLE iBolNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
DEFINE VARIABLE cPONum AS CHARACTER NO-UNDO.
DEFINE VARIABLE lInvoicefound AS LOGICAL NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN fiBOL# fiCustPO fiOrder.
  END.
  iBolNo = INTEGER(fiBOL#).
  cPoNum = fiCustPo.
  iOrder = INTEGER(fiOrder).
  IF fiCustPO NE "" THEN DO:
      FIND FIRST oe-boll NO-LOCK 
        WHERE oe-boll.company EQ cocode
          AND oe-boll.po-no EQ cPoNum
          NO-ERROR.
      //              MESSAGE "avail" avail(oe-boll) SKIP cocode SKIP "cpo" cPoNum SKIP fiCustPO
      //    VIEW-AS ALERT-BOX.
      IF NOT AVAILABLE oe-boll THEN DO:
          MESSAGE "BOL does not exist for PO# entered"
          VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
      END.     
      ELSE
        ASSIGN fiBOL#:SCREEN-VALUE = STRING(oe-boll.bol-no)
               fiOrder:SCREEN-VALUE = STRING(oe-boll.ord-no)
               iBOLNo = oe-boll.bol-no
               iOrder = oe-boll.ord-no
               .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiBOL#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBOL# C-Win
ON HELP OF fiBOL# IN FRAME FRAME-A /* BOL# */
DO:
     DEFINE VARIABLE char-val AS cha NO-UNDO.
     DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

     RUN windows/l-bolh.w (g_company,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lv-eb-tmpid).
     FIND FIRST oe-bolh WHERE RECID(oe-bolh) EQ lv-eb-tmpid NO-LOCK NO-ERROR.
     IF AVAILABLE(oe-bolh) THEN DO:                            
            FOCUS:SCREEN-VALUE = STRING(oe-bolh.bol-no).
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  RUN enable_UI.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createBolList C-Win
PROCEDURE createBolList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
def var a as char.

def var b as char.
def var i as int.
def var isFound as log.

def var c as int.
def var j as int.
def var k as int.

 os-command silent value("dir /b /o-d n:\cxml\asn\processed > c:\tmp\invprocessed.txt").

input STREAM s2 from c:\tmp\invprocessed.txt.
repeat i = 1 to 300:
  import STREAM s2 unformatted a.

  // disp a format "x(60)".
  input stream s1 from value("n:\cxml\asn\processed\" + a).
    repeat:
       import stream s1 unformatted b.
       j =  index(b, "shipmentid").
       
       create ttasn.
       assign ttasn.shipid    = integer(substring(b, j + 12 , 7))
              ttasn.cFileName = a
              .
       c = c + 1.
       leave.
    end.
 end.
INPUT STREAM s2 CLOSE.
INPUT STREAM s1 CLOSE.

OUTPUT STREAM s3 TO c:\tmp\missingASNReport.txt.
j = 0.
 for each oe-bolh no-lock
    where oe-bolh.company eq '001' 
      and oe-bolh.bol-date ge today - 20
      and oe-bolh.prt-date ge today - 1
      and oe-bolh.printed = yes
      // and oe-bolh.cust-no eq "DSG"
      and can-find(first sys-ctrl-shipto no-lock
                      where sys-ctrl-shipto.name = "cxmlasn"
                        and sys-ctrl-shipto.company eq '001'
                        and sys-ctrl-shipto.cust-vend-no eq oe-bolh.cust-no
                   )
      use-index bol-date
      :
    find first ttasn no-lock
        where ttasn.shipid = oe-bolh.bol-no
        no-error.
    if not avail ttasn then do:
        FIND FIRST oe-boll no-lock
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.bol-no EQ oe-bolh.bol-no
            NO-ERROR.
        IF AVAIL oe-boll THEN DO:
            CREATE ttMissingASN.
            ASSIGN 
               iMissOrder = oe-boll.ord-no
               iMissBol   =  oe-boll.bol-no
               .
         END.
          disp STREAM s3 oe-bolh.bol-no 
                avail(ttasn) oe-bolh.cust-no oe-bolh.bol-date
                oe-bolh.prt-date.
             j = j + 1.    
/*             isFound = no.                                                      */
/*             for each AuditHdr no-lock                                          */
/*                 where AuditHdr.AuditDateTime ge datetime(today - 5, 0)         */
/*                 and auditHdr.auditTable begins "cxmlbol"                       */
/*                 ,                                                              */
/*                 each auditDtl no-lock                                          */
/*                     where auditDtl.auditID eq AuditHdr.AuditID                 */
/*                 :                                                              */
/*                 if index(AuditDtl.AuditBeforeValue, string(oe-bolh.bol-no)) > 0*/
/*                    then isFound = yes.                                         */
/*              end.                                                              */
/*              disp STREAM s3 isFound.                                           */
           
    end.
    else 
      k = k + 1.
end.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiCustPO fiBOL# fiOrder 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fiCustPO fiBOL# fiOrder btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEFINE VARIABLE iBolNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
DEFINE VARIABLE cPONum AS CHARACTER NO-UNDO.
DEFINE VARIABLE lInvoicefound AS LOGICAL NO-UNDO.
DEFINE VARIABLE doSearch AS LOGICAL NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN fiBOL# fiCustPO fiOrder.
  END.
  
  iBolNo = INTEGER(fiBOL#).
  cPoNum = fiCustPo.
  iOrder = INTEGER(fiOrder).
  
  IF iBolNo EQ 0 AND cPoNum = "" AND iOrder = 0 THEN DO:
      MESSAGE 'Search for missing ASNs' VIEW-AS ALERT-BOX
          QUESTION BUTTONS YES-NO UPDATE doSearch.
      IF doSearch THEN DO:
          RUN createBolList.
          FOR EACH ttMissingASN:     
             output stream cXMLOutput to value("c:\temp\testasn" + string(iMissBol) + ".xml").
             RUN cxml/cxmlbol.p (INPUT cocode, INPUT iMissBol, INPUT iMissOrder).         
          END.
      END.
  END.
  ELSE DO:
      IF fiCustPO NE "" THEN DO:
          FIND FIRST oe-boll NO-LOCK 
            WHERE oe-boll.company EQ cocode
              AND oe-boll.po-no EQ cPoNum
              NO-ERROR.
    
          IF NOT AVAILABLE oe-boll THEN DO:
              MESSAGE "BOL does not exist for PO# entered"
              VIEW-AS ALERT-BOX.
              RETURN NO-APPLY.
          END.     
          ELSE
            ASSIGN fiBOL#:SCREEN-VALUE = STRING(oe-boll.bol-no)
                   fiOrder:SCREEN-VALUE = STRING(oe-boll.ord-no)
                   iBOLNo = oe-boll.bol-no
                   iOrder = oe-boll.ord-no
                   .
      END.
      
      FIND FIRST oe-bolh NO-LOCK 
        WHERE oe-bolh.company EQ cocode
          AND oe-bolh.bol-no EQ iBolNo
        NO-ERROR.
      IF NOT AVAILABLE oe-bolh THEN DO:
          MESSAGE "BOL does not exist!"
          VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
      END.
      
      FIND FIRST oe-ord NO-LOCK 
        WHERE  oe-ord.ord-no EQ iOrder 
          AND  oe-ord.company EQ cocode
        NO-ERROR.
      IF NOT AVAIL oe-ord THEN DO:
          MESSAGE "Order does not exist"
              VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
    
      END. 
    
     output stream cXMLOutput to value("c:\temp\testasn" + string(ibolNo) + ".xml").
     RUN cxml/cxmlbol.p (INPUT cocode, INPUT iBolNo, INPUT iOrder).
      SESSION:SET-WAIT-STATE("").
            ASSIGN fiBOL#:SCREEN-VALUE = ""
                   fiOrder:SCREEN-VALUE = ""
                   .
  END.
  MESSAGE "Process Is Completed." VIEW-AS ALERT-BOX.
  APPLY "close" TO THIS-PROCEDURE.


RETURN NO-APPLY.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

