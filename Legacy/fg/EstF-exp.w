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
/*DEFINE INPUT PARAMETER Autofind AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER Broword   AS CHAR NO-UNDO.*/
DEFINE INPUT PARAMETER ipcEstFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcEstTo   AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}

{custom/gloc.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc
 gcompany = g_company
 gloc = g_loc.

DEFINE STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
ASSIGN cTextListToSelect = "Estimate#,Est Date,Cust #,Ship To,Cust Part#,Item Description,FG Item#,Qty,Style,Board," +
                           "Caliper,Category,Box L,Box W,Box D,Qty/Set,Colors,Coating,Form#,Blank#,W#Up," +
                           "L#Up,# Up,Die Inches,Inks/Form,Passes/Form,Coatings/Form,Coat Passes/Form,Purch/Manuf," +
                           "Case Code,Case Width,Case Length,Case Depth,Blank W,Blank L,Grs Sht W,Grs Sht L,GS W Out,GS L  Out," +
                           "Ink1,Ink2,Ink3,Ink4,Ink5,Ink6,Ink7,Ink8,Ink9,Ink10,Mach1,Mach2,Mach3,Mach4,Mach5,Mach6,March7,Mach8,Mach9,Mach10," +
                           "1st ID,Last ID,Created Date,Last Updated Date,Unit Count,Units/Pallet,Pallet Count,Sales Rep,Sales Rep Name"
      cFieldListToSelect = "est-no,est-date,cust-no,ship-id,part-no,part-dscr1,stock-no,bl-qty,style,board," +
                           "cal,procat,len,wid,dep,cust-%,i-col,i-coat,form-no,blank-no,num-wid," +
                           "num-len,num-up,die-in,f-col,f-pass,f-coat,f-coat-p,pur-man," +
                           "cas-no,cas-wid,cas-len,cas-dep,t-wid,t-len,gsh-wid,gsh-len,n-out,n-out-l," +
                           "i-code2[1],i-code2[2],i-code2[3],i-code2[4],i-code2[5],i-code2[6],i-code2[7],i-code2[8],i-code2[9],i-code2[10]," +
                           "m-code2[1],m-code2[2],m-code2[3],m-code2[4],m-code2[5],m-code2[6],m-code2[7],m-code2[8],m-code2[9],m-code2[10]," +
                           "entered-id,updated-id,create-date,update-date,unt-cnt,unt-plt,plt-cnt,sales-rep,sales-rep-name".
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_est end_est ~
sl_avail Btn_Add sl_selected Btn_Remove btn_Up btn_down tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est end_est  ~
 sl_avail ~
sl_selected tb_excel tb_runExcel fi_file 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-estf rd-fgexp 
FUNCTION getValue-estf RETURNS CHARACTER
  ( BUFFER ipb-estf FOR est, ipc-field AS CHAR, ipc-form AS INT,ipc-blank AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_est AS CHARACTER FORMAT "x(15)" 
     LABEL "From Estimate" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
     LABEL "To Estimate" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-mach.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 9.29.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
     begin_est AT ROW 3.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Estimate Number" WIDGET-ID 142
     end_est AT ROW 3.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending Estimate #" WIDGET-ID 144
     sl_avail AT ROW 12.24 COL 9 NO-LABEL WIDGET-ID 26
     Btn_Add AT ROW 12.24 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     sl_selected AT ROW 12.24 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 13.43 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 14.62 COL 44 WIDGET-ID 136
     btn_down AT ROW 15.81 COL 44 WIDGET-ID 132
     tb_excel AT ROW 18.91 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 18.91 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 19.86 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 21.71 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 21.71 COL 60.2 WIDGET-ID 12
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.52 COL 64.4 WIDGET-ID 138
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.52 COL 9.4 WIDGET-ID 140
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.52 COL 3 WIDGET-ID 86
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-6 AT ROW 10.76 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 18.62 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Folding Estimate To Excel" WIDGET-ID 100.


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
       begin_est:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".
  
ASSIGN 
       end_est:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export FG Items to Excel */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_est" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-est.w (cocode, locode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_est" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-est.w (cocode, locode, ls-cur-val, output char-val).
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
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export FG Items to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est rd-fgexp
ON LEAVE OF begin_est IN FRAME rd-fgexp /* From Item # */
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
  RUN GetSelectionList.  
  run run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  APPLY "DEFAULT-ACTION" TO sl_avail.

  /*
  DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */
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
 /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    ldummy = sl_selected:DELETE(i).
  END
  */
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


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est rd-fgexp
ON LEAVE OF end_est IN FRAME rd-fgexp /* To Item # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* If Yes, File Name */
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

  
/* for pairs
    DEF VAR cSelectedList AS cha NO-UNDO.
    cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
         sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
        sl_selected:NUM-ITEMS
        SKIP cSelectedList
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */

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


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-fgexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-fgexp /* Auto Run Excel? */
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
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    RUN Set-Sort-Data.

    APPLY "entry" TO begin_est.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR cTmpList AS cha NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
    RETURN.
  END.
        
  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
  END.
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS: /* task 08191414 */
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
        ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
  END.

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
  DISPLAY begin_est end_est sl_avail sl_selected tb_excel 
          tb_runExcel fi_file 
      WITH FRAME rd-fgexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_est end_est  sl_avail Btn_Add 
         sl_selected Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok 
         btn-cancel 
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
 DEF VAR cTmpList AS cha NO-UNDO.

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
DEF BUFFER b-est FOR est.
DEF BUFFER bf-eb FOR eb.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

FOR EACH b-est WHERE b-est.company = gcompany
    AND trim(b-est.est-no) >= trim(begin_est)
    AND trim(b-est.est-no) <= trim(end_est)
    AND b-est.est-type >= 1 and b-est.est-type <= 4 NO-LOCK,
    EACH bf-eb WHERE bf-eb.company = gcompany
    AND bf-eb.est-no = b-est.est-no NO-LOCK BY bf-eb.form-no :
    
    IF INT(b-est.est-no) GT INT(end_est)
    OR INT(b-est.est-no) LT INT(begin_est) THEN 
        NEXT.
  
    v-excel-detail-lines = "".

    FOR EACH ttRptSelected:
        v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue-estf(BUFFER b-est, ttRptSelected.FieldList, bf-eb.form-no, bf-eb.blank-no)).
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
/*             WHEN "itemfg.style" THEN                                                                */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.style).           */
/*             WHEN "itemfg.cust-no" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.cust-no).         */
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
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
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
    IF ipcEstFrom <> "" THEN
    ASSIGN 
        begin_est:SCREEN-VALUE = assignParam(ipcEstFrom,NO)
        end_est:SCREEN-VALUE   = assignParam(ipcEstTo,NO)
        .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-estf rd-fgexp 
FUNCTION getValue-estf RETURNS CHARACTER
  ( BUFFER ipb-estf FOR est, ipc-field AS CHAR ,ipc-form AS INT,ipc-blank AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR machine AS CHAR EXTENT 15 NO-UNDO.
    DEFINE VAR j AS INT INIT 1 NO-UNDO.

    FIND FIRST eb WHERE eb.company = ipb-estf.company
            AND eb.est-no EQ ipb-estf.est-no
            AND eb.form-no = ipc-form AND eb.blank-no = ipc-blank  NO-LOCK NO-ERROR.
        FIND FIRST est-qty WHERE est-qty.company = ipb-estf.company
            AND est-qty.est-no = ipb-estf.est-no
            AND est-qty.eqty = eb.eqty NO-LOCK NO-ERROR.
        FIND FIRST ef WHERE ef.company = ipb-estf.company
            AND ef.est-no = ipb-estf.est-no
            AND ef.form-no = eb.form-no NO-LOCK NO-ERROR.
        FOR EACH est-op WHERE est-op.company = est-qty.company
            AND est-op.est-no = est-qty.est-no
            AND est-op.line < 500
            AND est-op.qty eq est-qty.eqty  NO-LOCK:
            machine[j] = est-op.m-code .
            j = j + 1 .
            IF j > 10 THEN LEAVE .
        END.
        FIND FIRST sman WHERE sman.company EQ eb.company
            AND sman.sman    EQ eb.sman NO-LOCK NO-ERROR.

    CASE ipc-field :

        WHEN "cust-no"  THEN DO:
            IF AVAIL eb THEN
                lc-return = eb.cust-no.
        END.
        WHEN "part-no"  THEN DO:
            IF AVAIL eb THEN
            lc-return = eb.part-no.
        END.
        WHEN "ship-id"  THEN DO:
            IF AVAIL eb THEN
            lc-return = eb.ship-id.
        END.
        WHEN "part-dscr1"  THEN DO:
            IF AVAIL eb THEN
            lc-return = eb.part-dscr1.
        END.
        WHEN "stock-no"  THEN DO:
            IF AVAIL eb THEN
            lc-return = eb.stock-no.
        END.
        WHEN "bl-qty"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.bl-qty).
        END.
        WHEN "style"  THEN DO:
            IF AVAIL eb THEN
            lc-return = eb.style.
        END.
        WHEN "procat"  THEN DO:
            IF AVAIL eb THEN
            lc-return = eb.procat.
        END.
        WHEN "len"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.len).
        END.
        WHEN "wid"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.wid).
        END.
        WHEN "dep"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.dep).
        END.
        WHEN "cust-%"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.cust-%).
        END.
        WHEN "i-col"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-col).
        END.
        WHEN "i-coat"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-coat).
        END.
        WHEN "form-no"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.form-no).
        END.
        WHEN "blank-no"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.blank-no).
        END.
        WHEN "num-wid"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.num-wid).
        END.
        WHEN "num-len"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.num-len).
        END.
        WHEN "num-up"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.num-up).
        END.
        WHEN "die-in"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.die-in).
        END.
        WHEN "pur-man"  THEN DO:
            IF AVAIL eb THEN do:
                IF eb.pur-man EQ NO THEN
                    lc-return = "Manuf.".
                ELSE 
                    lc-return = "Purch.".
            END.
        END.
         WHEN "unt-cnt"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.cas-cnt).
        END.
         WHEN "unt-plt"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.cas-pal).
        END.
         WHEN "plt-cnt"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.tr-cnt).
        END.
        WHEN "cas-no"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.cas-no).
        END.
        WHEN "cas-len"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.cas-len).
        END.
        WHEN "cas-wid"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.cas-wid).
        END.
        WHEN "cas-dep"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.cas-dep).
        END.
        WHEN "t-wid"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.t-wid).
        END.
        WHEN "t-len"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.t-len).
        END.
        WHEN "i-code2[1]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[1]).
        END.
         WHEN "i-code2[2]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[2]).
        END.
         WHEN "i-code2[3]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[3]).
        END.
         WHEN "i-code2[4]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[4]).
        END.
         WHEN "i-code2[5]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[5]).
        END.
         WHEN "i-code2[6]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[6]).
        END.
         WHEN "i-code2[7]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[7]).
        END.
         WHEN "i-code2[8]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[8]).
        END.
         WHEN "i-code2[9]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[9]).
        END.
         WHEN "i-code2[10]"  THEN DO:
            IF AVAIL eb THEN
            lc-return = string(eb.i-code2[10]).
        END.
        WHEN "board"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.board).
        END.
        WHEN "cal"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.cal).
        END.
        WHEN "f-col"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.f-col).
        END.
        WHEN "f-pass"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.f-pass).
        END.
        WHEN "f-coat"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.f-coat).
        END.
        WHEN "f-coat-p"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.f-coat-p).
        END.
        WHEN "gsh-wid"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.gsh-wid).
        END.
        WHEN "gsh-len"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.gsh-len).
        END.
        WHEN "n-out"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.n-out).
        END.
        WHEN "n-out-l"  THEN DO:
            IF AVAIL ef THEN
            lc-return = string(ef.n-out-l).
        END.
        WHEN "m-code2[1]" THEN do:
            lc-return =  machine[1] .
        END.
        WHEN "m-code2[2]" THEN do:
            lc-return =  machine[2] .
        END.
        WHEN "m-code2[3]" THEN do:
            lc-return =  machine[3] .
        END.
        WHEN "m-code2[4]" THEN do:
            lc-return =  machine[4] .
        END.
        WHEN "m-code2[5]" THEN do:
            lc-return =  machine[5] .
        END.
        WHEN "m-code2[6]" THEN do:
            lc-return =  machine[6] .
        END.
        WHEN "m-code2[7]" THEN do:
            lc-return =  machine[7] .
        END.
        WHEN "m-code2[8]" THEN do:
            lc-return =  machine[8] .
        END.
        WHEN "m-code2[9]" THEN do:
            lc-return =  machine[9] .
        END.
        WHEN "m-code2[10]" THEN do:
            lc-return =  machine[10] .
        END.
        WHEN "create-date"  THEN DO:
          lc-return = STRING(ipb-estf.est-date) .
        END.
        WHEN "update-date"  THEN DO:
            lc-return = STRING(ipb-estf.mod-date) .
        END.
        WHEN "dfuncAlloc"  THEN DO:
           /* CASE ipb-estf.alloc :
                WHEN YES THEN
                    lc-return = "Assembled".
                WHEN NO THEN
                    lc-return = "Unassembled".
                OTHERWISE
                    lc-return = "Assembled w/Part Receipts".
            END CASE.*/
        END.
        WHEN "dfuncTotMSFPTD"  THEN DO:
            /*IF g_period NE 0 THEN lc-return = STRING(ipb-estf.ptd-msf[g_period]).*/
        END.
        WHEN "sales-rep"  THEN DO:
            IF AVAIL eb THEN
            lc-return = eb.sman.
        END.
        WHEN "sales-rep-name"  THEN DO:
            IF AVAIL sman THEN
            lc-return = sman.sname.
        END.

        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-estf:BUFFER-FIELD(ipc-field).
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

