&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgexp 
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
DEFINE INPUT PARAMETER lcSearch   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER lcsearchby AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}

{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEFINE STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS CHARACTER NO-UNDO.
DEF VAR cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN cTextListToSelect = "Vendor,Name,Status,Address1,Address2,City,State,Zip,Country,Postal Code,Tax ID#,Type,Type Name,Contact,Buyer,Buyer Name,Area code,Phone," +
                           "Fax Area Code,Fax,Fax prefix,Fax Country,Overrun Percentage,Underrun Percentage,Default G/L#,G/L Dscr,Remit to,Remit Address1,Remit Address2,Remit City," +
                           "Remit State,Remit Zip,Remit Country,Remit Postal Code,Check Memo,Currency Code,Currency Dscr,Tax,1099 Code,EDI,Credit Card/ACH,Terms,Terms Dscr,Discount%,POEXPORT," +
                           "Whs,Max PO Cost,Freight Pay,Lead Time Days,Carrier,Carrier Dscr,FOB,Pay Type,Account#,Swift Code,Routing"

      cFieldListToSelect = "vend-no,name,active,add1,add2,city,state,zip,country,postal,tax-id,type,type-dscr,contact,buyer,buyer-dscr,area-code,phone," +
                           "fax-area,fax,fax-prefix,fax-country,over-pct,under-pct,actnum,actdscr,remit,r-add1,r-add2,r-city," +
                           "r-state,r-zip,r-country,r-postal,check-memo,curr-code,curr-dscr,tax-gr,code-1099,an-edi-vend,tb-cc,terms,terms-dscr,disc-%,po-export," +
                           "loc,rebate-%,frt-pay,disc-days,carrier,carrier-dscr,fob-code,pay-type,Bank-Acct,SwiftBIC,Bank-RTN" .

        ASSIGN cTextListToDefault  = "Vendor,Name,Address1,Address2,City,State,Country,Postal Code,Zip,Remit to,Remit Address1,Remit Address2,Remit City,Remit State,Remit Country,Remit Postal Code,Remit Zip," +
                                 "Terms,Default G/L#,Area code,Phone,Fax Area Code,Fax,1099 Code,Tax ID#,Type,Tax,Carrier,Account#,Swift Code,Routing"  .
{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_vend end_vend ~
sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend sl_avail sl_selected ~
fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR vend, ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
     LABEL "&Default" 
     SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.1.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "x(10)" 
     LABEL "From Vendor" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzzz" 
     LABEL "To Vendor" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\VendorExport.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 52 BY 1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.38.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 2.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL yes 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
     begin_vend AT ROW 3.95 COL 25.2 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number" WIDGET-ID 142
     end_vend AT ROW 3.95 COL 65 COLON-ALIGNED HELP
          "Enter Ending Vendor" WIDGET-ID 144
     sl_avail AT ROW 9.43 COL 7 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 9.43 COL 42 WIDGET-ID 56
     sl_selected AT ROW 9.43 COL 62.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.62 COL 42 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 11.81 COL 42 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 13 COL 42 WIDGET-ID 136
     btn_down AT ROW 14.1 COL 42 WIDGET-ID 132
     fi_file AT ROW 17.1 COL 18.8 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     tb_OpenCSV AT ROW 17.19 COL 87.8 RIGHT-ALIGNED WIDGET-ID 34
     tb_excel AT ROW 19.05 COL 4 WIDGET-ID 32
     tbAutoClose AT ROW 19.1 COL 43 WIDGET-ID 58
     btn-ok AT ROW 20.05 COL 32.6 WIDGET-ID 14
     btn-cancel AT ROW 20.05 COL 52.6 WIDGET-ID 12
     "Selected Columns" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 8.71 COL 69.4 WIDGET-ID 138
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
     " Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 8.05 COL 5 WIDGET-ID 86
     "Available Columns" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 8.71 COL 13.4 WIDGET-ID 140
     RECT-6 AT ROW 8.38 COL 4 WIDGET-ID 30
     RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
     RECT-8 AT ROW 16.48 COL 4 WIDGET-ID 84
     SPACE(2.99) SKIP(2.98)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Export Vendor to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgexp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-fgexp:SCROLLABLE       = FALSE
       FRAME rd-fgexp:HIDDEN           = TRUE.

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_excel:HIDDEN IN FRAME rd-fgexp           = TRUE
       tb_excel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export Vendor to Excel */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_vend" then do:
           ls-cur-val = lw-focus:screen-value.
           RUN windows/l-vendno.w (cocode, locode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_vend" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-vendno.w (cocode, locode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg*/

END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export Vendor to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend rd-fgexp
ON LEAVE OF begin_vend IN FRAME rd-fgexp /* From Vendor */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgexp
ON CHOOSE OF btn-cancel IN FRAME rd-fgexp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgexp
ON CHOOSE OF btn-ok IN FRAME rd-fgexp /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  
  ASSIGN 
    fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    fi_file:SCREEN-VALUE =  cFileName.
  
  RUN GetSelectionList.  
  run run-report.
  
  IF NOT tb_OpenCSV THEN 
    DO:        
        MESSAGE "CSV file have been created." SKIP(1)
            "~"OK"~" to open CSV file?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
            TITLE "" UPDATE lChoice AS LOGICAL.
 
        IF lChoice THEN
        DO:
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
        END.
    END. /* IF NOT tb_OpenCSV THEN  */
    
    IF tbAutoClose:CHECKED THEN 
    APPLY "END-ERROR":U TO SELF.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
DO:
  DEF VAR cSelectedList AS CHARACTER NO-UNDO.

  APPLY "DEFAULT-ACTION" TO sl_avail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
DO:
  DEF VAR cSelectedList AS CHARACTER NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgexp
ON CHOOSE OF btn_down IN FRAME rd-fgexp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgexp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgexp /* << Remove */
DO:
  APPLY "DEFAULT-ACTION" TO sl_selected  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgexp
ON CHOOSE OF btn_Up IN FRAME rd-fgexp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend rd-fgexp
ON LEAVE OF end_vend IN FRAME rd-fgexp /* To Vendor */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON HELP OF fi_file IN FRAME rd-fgexp /* Name */
DO:
   DEF VAR ls-filename AS CHARACTER NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

   SYSTEM-DIALOG GET-FILE ls-filename 
                 TITLE "Select File to Save "
                 FILTERS "Excel Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 INITIAL-DIR "c:\tmp"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgexp
DO:
  
   IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
       sl_selected:NUM-ITEMS = 0)
   THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
               ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
               .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgexp
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
              ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              .
    END.           
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
    .
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgexp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-fgexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-fgexp /* Open CSV? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
  RUN DisplaySelectionList.
  btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    RUN Set-Sort-Data.

    APPLY "entry" TO begin_vend.
    fi_file:SCREEN-VALUE = "c:\tmp\VendorExport.csv".
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgexp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-fgexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgexp 
PROCEDURE DisplaySelectionDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS CHARACTER NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToDefault)   .
  END.            
  sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS CHARACTER NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */
                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.
  
 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgexp 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS CHARACTER NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */
                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.
  
 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  {sys/ref/SelColCorrect.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgexp  _DEFAULT-ENABLE
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
  DISPLAY begin_vend end_vend sl_avail sl_selected fi_file tb_OpenCSV 
          tbAutoClose 
      WITH FRAME rd-fgexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_vend end_vend sl_avail Btn_Def sl_selected 
         Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV tbAutoClose 
         btn-ok btn-cancel 
      WITH FRAME rd-fgexp.
  VIEW FRAME rd-fgexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-fgexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgexp 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS CHARACTER NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

 DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           /* ttRptSelected.FieldLength */
        .   
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgexp 
PROCEDURE Move-Field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

  DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
      WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    DO:
      IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
        ldummy = sl_selected:DELETE(i + 1)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
        .
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF BUFFER b-vend FOR vend.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

FOR EACH b-vend WHERE b-vend.company = cocode
        AND b-vend.vend-no GE begin_vend
        AND b-vend.vend-no LE end_vend
        NO-LOCK:

    v-excel-detail-lines = "".

    FOR EACH ttRptSelected:
        v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue-itemfg(BUFFER b-vend,ttRptSelected.FieldList)).
/*         CASE ttRptSelected.FieldList:                                                               */
/*             WHEN "itemfg.i-no" THEN                                                                 */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-no).            */
/*             WHEN "itemfg.procat" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.procat).          */
/*             WHEN "itemfg.i-name" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-name).          */
/*             WHEN "itemfg.part-no" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.part-no).         */
/*             WHEN "itemfg.est-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.est-no).          */
/*             WHEN "itemfg.item" THEN                                                                */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.item).           */
/*             WHEN "itemfg.vend-no" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.vend-no).         */
/*             WHEN "itemfg.part-dscr1" THEN                                                           */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.part-dscr1).      */
/*             WHEN "itemfg.i-code" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-code).          */
/*             WHEN "itemfg.cad-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.cad-no).          */
/*             WHEN "itemfg.spc-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.spc-no).          */
/*             WHEN "itemfg.stocked" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.stocked)). */
/*             WHEN "itemfg.q-onh" THEN                                                                */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.q-onh)).   */
/*         END CASE.                                                                                   */
    END.

    PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_OpenCSV THEN
      OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgexp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
    IF begin_vend:SCREEN-VALUE EQ "" THEN do:
        FIND FIRST vend WHERE vend.company EQ cocode NO-LOCK NO-ERROR.
        begin_vend:SCREEN-VALUE = vend.vend-no.
        FIND LAST vend WHERE vend.company EQ cocode NO-LOCK NO-ERROR.
        end_vend:SCREEN-VALUE   = vend.vend-no.
    END.

    IF lcSearch NE "" THEN
        begin_vend:SCREEN-VALUE = lcSearch.
    IF lcsearchby NE "" THEN
        end_vend:SCREEN-VALUE = lcsearchby.
     
END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEF VAR lc-line AS CHAR NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR.

    IF ipl-end THEN
        lc-return = ipc-param + "ZZZZZZZZZZZZZZZ".
    ELSE
        lc-return = ipc-param.

  RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lc-header AS CHAR NO-UNDO.

FOR EACH ttRptSelected:
    lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
END.
/*     lc-header = lc-header + appendXLLine ("PO #").      */
/*     lc-header = lc-header + appendXLLine ("Vendor #").  */
/*     lc-header = lc-header + appendXLLine ("Due Date").  */
/*     lc-header = lc-header + appendXLLine ("Ship ID").   */
/*     lc-header = lc-header + appendXLLine ("Ship Name"). */
/*     lc-header = lc-header + appendXLLine ("Job #").     */
/*     lc-header = lc-header + appendXLLine ("Item #").    */
/*     lc-header = lc-header + appendXLLine ("Item Name"). */

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR vend, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "tb-cc"  THEN DO:
            FIND FIRST vend WHERE vend.company EQ ipb-itemfg.company
                AND vend.vend-no EQ ipb-itemfg.vend-no NO-LOCK NO-ERROR.
            IF AVAIL vend THEN
            lc-return = IF vend.payment-type EQ "ACH" THEN "Yes" ELSE "No".
        END.
        WHEN "terms-dscr"  THEN DO:
            FIND terms WHERE terms.company EQ ipb-itemfg.company
                AND terms.t-code EQ  ipb-itemfg.terms NO-LOCK NO-ERROR.
            IF AVAIL terms THEN
                lc-return = terms.dscr.
            ELSE
                lc-return = "".
        END.
         WHEN "buyer-dscr"  THEN DO:
            FIND buyer WHERE buyer.company EQ ipb-itemfg.company
                AND buyer.buyer EQ  ipb-itemfg.buyer NO-LOCK NO-ERROR.
            IF AVAIL buyer THEN
                lc-return = buyer.buyer-n.
            ELSE
                lc-return = "".
        END.
        WHEN "type-dscr" THEN DO:
            FIND ventype WHERE ventype.company EQ ipb-itemfg.company
                AND ventype.TYPE EQ ipb-itemfg.TYPE NO-LOCK NO-ERROR.
            IF AVAIL ventype THEN
                 lc-return = ventype.Dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "carrier-dscr"  THEN DO:
            FIND FIRST carrier WHERE carrier.company EQ ipb-itemfg.company
                AND carrier.carrier EQ  ipb-itemfg.carrier  NO-LOCK NO-ERROR.
 
            IF AVAIL carrier THEN
                lc-return = carrier.dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "curr-dscr"  THEN DO:
            FIND FIRST currency WHERE currency.company EQ ipb-itemfg.company
                AND currency.c-code EQ  ipb-itemfg.curr-code NO-LOCK NO-ERROR.
            IF AVAIL currency THEN
                lc-return = currency.c-desc.
            ELSE
                lc-return = "".
        END.
        WHEN "fob-code"  THEN DO:
            CASE ipb-itemfg.fob-code :
                WHEN "DEST" THEN
                    lc-return = "Destination".
                WHEN "ORIG" THEN
                    lc-return = "Origin".
                OTHERWISE
                    lc-return = "None".
            END CASE.
        END.
        WHEN "active" THEN DO:
            CASE ipb-itemfg.active :
                WHEN "A" THEN
                    lc-return = "Active".
                WHEN "I" THEN
                    lc-return = "Inactive".
            END CASE.
        END.
        
                
        WHEN "pay-type"  THEN DO:
            FIND FIRST vend WHERE vend.company EQ ipb-itemfg.company
                AND vend.vend-no EQ ipb-itemfg.vend-no NO-LOCK NO-ERROR.
            IF AVAIL vend THEN 
                lc-return = vend.payment-type.
            ELSE
                lc-return = "".
                      
        END.
        WHEN "Bank-RTN"  THEN DO:
            FIND FIRST vend WHERE vend.company EQ ipb-itemfg.company
                AND vend.vend-no EQ ipb-itemfg.vend-no NO-LOCK NO-ERROR.
            IF AVAIL vend THEN 
                lc-return = string(vend.Bank-RTN,"999999999").
            ELSE
                lc-return = "".
        END.
        
        WHEN "dfuncTotMSFPTD"  THEN DO:
            /*IF g_period NE 0 THEN lc-return = STRING(ipb-itemfg.ptd-msf[g_period]).*/
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-itemfg:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

