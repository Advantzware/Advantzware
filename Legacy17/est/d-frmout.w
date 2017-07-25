&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est/d-frmout.w
  
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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE var opCADCAM AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE opCADCAM AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR k_frac as dec init 6.25 no-undo.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).


{est/frmotvar.i "shared"}
{sys/inc/var.i shared}
{custom/gcompany.i}  

gcompany = cocode.


{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
   DEF TEMP-TABLE tt-64-dec NO-UNDO
       FIELD DEC AS DEC DECIMALS 6.

   DO v-count = 0 TO 63:
       CREATE tt-64-dec.
       tt-64-dec.DEC = v-count / 64.0.
       RELEASE tt-64-dec.
   END.
END.

/*do transaction:
  {sys/inc/artioscad.i}
end. */

def var iFormNumber as int no-undo.
def var iBlankNumber as int no-undo.
def var iNumofCADForm as int no-undo.
def var iProjectCount as int init 50 no-undo.
def var lv-copy-qty as int extent 20 no-undo.
def var lv-copy-rel as int extent 20 no-undo.
def var lv-estqty-recid as recid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cust-no ship-to style-cod flue test f-tab ~
len wid dep cst-part colr-dscr fg-no board glu-cod bndl-cod plat-cod ~
bndng-cod Btn_OK Btn_Cancel RECT-1 fg-cat quantity 
&Scoped-Define DISPLAYED-OBJECTS cust-no ship-to style-cod style-dscr flue ~
test f-tab len wid dep cst-part colr-dscr fg-no board glu-cod bndl-cod ~
plat-cod bndng-cod fg-cat quantity 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE bndl-cod AS CHARACTER FORMAT "X(15)":U 
     LABEL "Bundle Code" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE bndng-cod AS CHARACTER FORMAT "X(15)":U 
     LABEL "Banding Code" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE board AS CHARACTER FORMAT "X(12)":U 
     LABEL "Board" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE colr-dscr AS CHARACTER FORMAT "X(15)":U 
     LABEL "Color Description" 
     VIEW-AS FILL-IN 
     SIZE 39.4 BY 1 NO-UNDO.

DEFINE VARIABLE cst-part AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 39.4 BY 1 NO-UNDO.

DEFINE VARIABLE cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE dep AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Depth" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE f-tab AS LOGICAL FORMAT "In/Out" INITIAL NO 
     LABEL "Tab" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE fg-cat AS CHARACTER FORMAT "X(5)":U INITIAL "0" 
     LABEL "FG Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fg-no AS CHARACTER FORMAT "X(15)":U INITIAL "0" 
     LABEL "FG Item Code" 
     VIEW-AS FILL-IN 
     SIZE 39.4 BY 1 NO-UNDO.

DEFINE VARIABLE flue AS CHARACTER FORMAT "X(5)":U 
     LABEL "Flute" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE glu-cod AS CHARACTER FORMAT "X(15)":U 
     LABEL "Glue Code" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE len AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE plat-cod AS CHARACTER FORMAT "X(15)":U 
     LABEL "Pallet Code" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE quantity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE style-cod AS CHARACTER FORMAT "X(8)":U 
     LABEL "Style Code" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE style-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE test AS CHARACTER FORMAT "X(6)":U 
     LABEL "Test" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE wid AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 20.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cust-no AT ROW 1.81 COL 19.2 COLON-ALIGNED WIDGET-ID 176
     ship-to AT ROW 1.81 COL 50.4 COLON-ALIGNED WIDGET-ID 178
     style-cod AT ROW 3.14 COL 19.2 COLON-ALIGNED WIDGET-ID 180
     style-dscr AT ROW 3.14 COL 37 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     flue AT ROW 4.67 COL 19.2 COLON-ALIGNED WIDGET-ID 184
     test AT ROW 4.67 COL 38.4 COLON-ALIGNED WIDGET-ID 186
     f-tab AT ROW 4.67 COL 58.2 COLON-ALIGNED WIDGET-ID 188
     len AT ROW 6.05 COL 19.2 COLON-ALIGNED WIDGET-ID 190
     wid AT ROW 6.05 COL 38.4 COLON-ALIGNED WIDGET-ID 194
     dep AT ROW 6.05 COL 58.2 COLON-ALIGNED WIDGET-ID 192
     cst-part AT ROW 8.24 COL 19 COLON-ALIGNED WIDGET-ID 88
     colr-dscr AT ROW 9.57 COL 19 COLON-ALIGNED WIDGET-ID 162
     fg-no AT ROW 10.95 COL 19 COLON-ALIGNED WIDGET-ID 42
     fg-cat AT ROW 12.29 COL 19 COLON-ALIGNED WIDGET-ID 196
     quantity AT ROW 12.29 COL 44.4 COLON-ALIGNED WIDGET-ID 198
     board AT ROW 15.57 COL 18 COLON-ALIGNED WIDGET-ID 174
     glu-cod AT ROW 16.67 COL 18 COLON-ALIGNED WIDGET-ID 164
     bndl-cod AT ROW 17.76 COL 18 COLON-ALIGNED WIDGET-ID 168
     plat-cod AT ROW 18.86 COL 18 COLON-ALIGNED WIDGET-ID 170
     bndng-cod AT ROW 19.95 COL 18 COLON-ALIGNED WIDGET-ID 172
     Btn_OK AT ROW 22.19 COL 16
     Btn_Cancel AT ROW 22.19 COL 48.2
     "DEFAULT Materials" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 13.95 COL 8 WIDGET-ID 166
     RECT-1 AT ROW 1.1 COL 2 WIDGET-ID 82
     SPACE(0.59) SKIP(3.17)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Create Estimate from FARM OUT"
         CANCEL-BUTTON Btn_Cancel.


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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN style-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Create Estimate from FARM OUT */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bndl-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bndl-cod D-Dialog
ON HELP OF bndl-cod IN FRAME D-Dialog /* Bundle Code */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   RUN windows/l-item.w (gcompany,"","C",focus:SCREEN-VALUE,OUTPUT char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bndl-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bndl-cod D-Dialog
ON LEAVE OF bndl-cod IN FRAME D-Dialog /* Bundle Code */
DO:
     if bndl-cod:screen-value <> "" and
         not can-find(item where item.company = gcompany
                      and item.i-no = bndl-cod:SCREEN-VALUE
                      AND LOOKUP(ITEM.mat-type,"C" ) GT 0)
      then do:
         message "Invalid Entry. Try Help. " view-as alert-box error.
         apply "entry" to bndl-cod.
         return no-apply.

    END.                                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL board D-Dialog
ON HELP OF board IN FRAME D-Dialog /* Board */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   run windows/l-board.w (gcompany,"",focus:SCREEN-VALUE,output char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL board D-Dialog
ON LEAVE OF board IN FRAME D-Dialog /* Board */
DO:
    if board:screen-value <> "" and
         not can-find(item where item.company = gcompany
                      and item.i-no = board:screen-value
                      AND LOOKUP(ITEM.mat-type,"P,R,B,F" ) GT 0)
      then do:
         message "Invalid Board. Try Help. " view-as alert-box error.
         apply "entry" to board.
         return no-apply.

    END.
                            .                                    
 END.

 /* _UIB-CODE-BLOCK-END */
 &ANALYZE-RESUME


 &Scoped-define SELF-NAME Btn_Cancel
 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
   ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  opCADCAM = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:

     DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN valid-fgitem NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-part-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    assign iFormNumber = 0
         iBlankNumber = 0 .
 
    session:set-wait-state("general").
  
     run create-ttfrmout.

     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE). 
  
 SESSION:set-wait-state("").
  
 APPLY "close" TO THIS-PROCEDURE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cst-part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cst-part D-Dialog
ON HELP OF cst-part IN FRAME D-Dialog /* Cust Part# */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.

   run windows/l-cstprt.w (gcompany, "", focus:screen-value, "", output char-val, output look-recid).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                          fg-no:screen-value  = entry(4,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cst-part D-Dialog
ON LEAVE OF cst-part IN FRAME D-Dialog /* Cust Part# */
DO:
    IF LASTKEY NE -1 THEN DO:
   assign {&self-name}.
   RUN valid-part-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-no D-Dialog
ON HELP OF cust-no IN FRAME D-Dialog /* Cust# */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   RUN windows/l-custact.w (gcompany,"", OUTPUT char-val, OUTPUT look-recid).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dep D-Dialog
ON HELP OF dep IN FRAME D-Dialog /* Depth */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
 /*  RUN windows/l-item.w (gcompany,"","G,S,T",focus:SCREEN-VALUE,OUTPUT char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val) 
                           .                                                        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-tab D-Dialog
ON HELP OF f-tab IN FRAME D-Dialog /* Tab */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   /*RUN windows/l-item.w (gcompany,"","G,S,T",focus:SCREEN-VALUE,OUTPUT char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .         */                           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-cat D-Dialog
ON HELP OF fg-cat IN FRAME D-Dialog /* FG Category */
DO:
     def var char-val as cha no-undo.
     def var look-recid as recid no-undo.

           run windows/l-fgcat.w (gcompany,fg-cat:SCREEN-VALUE,output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
               ASSIGN
               self:screen-value  = entry(1,char-val) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-cat D-Dialog
ON LEAVE OF fg-cat IN FRAME D-Dialog /* FG Category */
DO:
     IF LASTKEY NE -1 THEN DO:
         IF fg-cat:SCREEN-VALUE NE "" THEN do:
             RUN valid-procat NO-ERROR.
             IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
         END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fg-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-no D-Dialog
ON HELP OF fg-no IN FRAME D-Dialog /* FG Item Code */
DO:
     def var char-val as cha no-undo.
     def var look-recid as recid no-undo.

           run windows/l-itemfa.w (gcompany, "", focus:screen-value, output char-val, output look-recid).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
               ASSIGN
               self:screen-value  = entry(1,char-val) .
              FIND FIRST itemfg WHERE RECID(itemfg) = look-recid NO-LOCK NO-ERROR.
             IF AVAIL itemfg THEN
             cst-part:SCREEN-VALUE = itemfg.part-no .
               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-no D-Dialog
ON LEAVE OF fg-no IN FRAME D-Dialog /* FG Item Code */
DO:
   IF LASTKEY NE -1 THEN DO:
   assign {&self-name}.
   RUN valid-fgitem NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME flue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL flue D-Dialog
ON HELP OF flue IN FRAME D-Dialog /* Flute */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   run windows/l-flute.w (gcompany,output char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL flue D-Dialog
ON LEAVE OF flue IN FRAME D-Dialog /* Flute */
DO:
  def var ls-board as cha no-undo.
  DEF VAR lv-mat-types AS CHAR INIT "B" NO-UNDO.

  flue:SCREEN-VALUE =  CAPS(flue:SCREEN-VALUE).


  IF LASTKEY NE -1 THEN DO: 
      IF flue:SCREEN-VALUE NE "" THEN do:
          RUN valid-flute NO-ERROR. 
          IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      END.
  END.

    FOR EACH item
        WHERE item.company   EQ gcompany
       /*   AND CAN-DO(lv-mat-types,item.mat-type)*/
          AND item.industry  EQ "2"
          AND item.i-code    EQ "E"
          AND item.flute     EQ flue:SCREEN-VALUE
          AND item.reg-no    EQ test:SCREEN-VALUE
        USE-INDEX mat-type NO-LOCK
        BY item.i-no:
       
      board:SCREEN-VALUE = item.i-no.
    /*  RUN new-board.*/
      LEAVE.
    END.


     /*if not avail item or (avail item and item.i-no = "" ) then   */
   /*  if ls-board = "" THEN RUN new-flute-test.*/
   /*end.   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glu-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glu-cod D-Dialog
ON HELP OF glu-cod IN FRAME D-Dialog /* Glue Code */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   RUN windows/l-item.w (gcompany,"","G,S,T",focus:SCREEN-VALUE,OUTPUT char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glu-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glu-cod D-Dialog
ON LEAVE OF glu-cod IN FRAME D-Dialog /* Glue Code */
DO:
   if glu-cod:screen-value <> "" and
         not can-find(item where item.company = gcompany
                      and item.i-no = glu-cod:SCREEN-VALUE
                      AND LOOKUP(ITEM.mat-type,"G,T" ) GT 0)
      then do:
         message "Invalid Entry. Try Help. " view-as alert-box error.
         apply "entry" to glu-cod.
         return no-apply.

    END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL len D-Dialog
ON LEAVE OF len IN FRAME D-Dialog /* Length */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR len-num AS INT NO-UNDO.
   
    if lastkey = -1 then return.
    v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.

   IF v-cecscrn-dec THEN
   DO:
      len-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
          /* eb.len:screen-value = string( len-num +  op-dec) . */
      END.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wid D-Dialog
ON LEAVE OF wid IN FRAME D-Dialog /* Length */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR len-num AS INT NO-UNDO.
   
    if lastkey = -1 then return.
    v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.

   IF v-cecscrn-dec THEN
   DO:
      len-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
          /* eb.len:screen-value = string( len-num +  op-dec) . */
      END.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dep D-Dialog
ON LEAVE OF dep IN FRAME D-Dialog /* Length */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR len-num AS INT NO-UNDO.
   
    if lastkey = -1 then return.
    v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.

   IF v-cecscrn-dec THEN
   DO:
      len-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
          /* eb.len:screen-value = string( len-num +  op-dec) . */
      END.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME plat-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL plat-cod D-Dialog
ON HELP OF plat-cod IN FRAME D-Dialog /* Pallet Code */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   RUN windows/l-item.w (gcompany,"","D",focus:SCREEN-VALUE,OUTPUT char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME plat-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL plat-cod D-Dialog
ON LEAVE OF plat-cod IN FRAME D-Dialog /* Pallet Code */
DO:
   if plat-cod:screen-value <> "" and
         not can-find(item where item.company = gcompany
                      and item.i-no = plat-cod:SCREEN-VALUE
                      AND LOOKUP(ITEM.mat-type,"D" ) GT 0)
      then do:
         message "Invalid Entry. Try Help. " view-as alert-box error.
         apply "entry" to plat-cod.
         return no-apply.

    END.                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME quantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quantity D-Dialog
ON HELP OF quantity IN FRAME D-Dialog /* Quantity */
DO:
     def var char-val as cha no-undo.
     def var look-recid as recid no-undo.
     def var char-val2 as cha no-undo.        
     def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.

         /*  IF est.est-type LE 6 THEN DO: */
           /*  lv-estqty-recid = if avail est-qty then recid(est-qty) else ?.*/
             run est/estqtyfr.w (len:screen-value,wid:SCREEN-VALUE, quantity:SCREEN-VALUE, output char-val, output char-val2, output date-val, output date-val2) .
             if char-val <> "?" 
                then assign quantity:screen-value = entry(1,char-val)
                            lv-copy-qty[1] = integer(entry(1,char-val))
                            lv-copy-qty[2] = integer(entry(2,char-val))
                            lv-copy-qty[3] = integer(entry(3,char-val))
                            lv-copy-qty[4] = integer(entry(4,char-val))
                            lv-copy-qty[5] = integer(entry(5,char-val))
                            lv-copy-qty[6] = integer(entry(6,char-val))
                            lv-copy-qty[7] = integer(entry(7,char-val))
                            lv-copy-qty[8] = integer(entry(8,char-val))
                            lv-copy-qty[9] = integer(entry(9,char-val))
                            lv-copy-qty[10] = integer(entry(10,char-val))
                            lv-copy-rel[1] = INTEGER(entry(11,char-val))
                            lv-copy-rel[2] = integer(entry(12,char-val))
                            lv-copy-rel[3] = integer(entry(13,char-val))
                            lv-copy-rel[4] = integer(entry(14,char-val))
                            lv-copy-rel[5] = integer(entry(15,char-val))
                            lv-copy-rel[6] = integer(entry(16,char-val))
                            lv-copy-rel[7] = integer(entry(17,char-val))
                            lv-copy-rel[8] = integer(entry(18,char-val))
                            lv-copy-rel[9] = integer(entry(19,char-val))
                            lv-copy-rel[10] = integer(entry(20,char-val)).
             if char-val2 <> "?" 
                then assign lv-copy-qty[11] = integer(entry(1,char-val2))
                            lv-copy-qty[12] = integer(entry(2,char-val2))
                            lv-copy-qty[13] = integer(entry(3,char-val2))
                            lv-copy-qty[14] = integer(entry(4,char-val2))
                            lv-copy-qty[15] = integer(entry(5,char-val2))
                            lv-copy-qty[16] = integer(entry(6,char-val2))
                            lv-copy-qty[17] = integer(entry(7,char-val2))
                            lv-copy-qty[18] = integer(entry(8,char-val2))
                            lv-copy-qty[19] = integer(entry(9,char-val2))
                            lv-copy-qty[20] = integer(entry(10,char-val2))
                            lv-copy-rel[11] =  integer(entry(11,char-val2))
                            lv-copy-rel[12] = integer(entry(12,char-val2))  
                            lv-copy-rel[13] = integer(entry(13,char-val2))  
                            lv-copy-rel[14] = integer(entry(14,char-val2))  
                            lv-copy-rel[15] = integer(entry(15,char-val2))  
                            lv-copy-rel[16] = integer(entry(16,char-val2))  
                            lv-copy-rel[17] = integer(entry(17,char-val2))  
                            lv-copy-rel[18] = integer(entry(18,char-val2))  
                            lv-copy-rel[19] = integer(entry(19,char-val2))  
                            lv-copy-rel[20] = integer(entry(20,char-val2)) 
                 .
        /*   END.*/
               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-to D-Dialog
ON HELP OF ship-to IN FRAME D-Dialog /* Ship To */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   RUN windows/l-shipto.w (gcompany,"",cust-no:SCREEN-VALUE,"", OUTPUT char-val).
   
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-to D-Dialog
ON LEAVE OF ship-to IN FRAME D-Dialog /* Ship To */
DO:
   IF LASTKEY NE -1 THEN DO:
       IF ship-to:SCREEN-VALUE NE "" THEN do:
           RUN valid-ship-id NO-ERROR.
           IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
       END.
  END.                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod D-Dialog
ON HELP OF style-cod IN FRAME D-Dialog /* Style Code */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   run windows/l-stylec.w (gcompany,focus:SCREEN-VALUE, output char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .    
   IF self:SCREEN-VALUE NE "" THEN DO:
       FIND FIRST style WHERE style.company = cocode
           AND style.style EQ self:SCREEN-VALUE NO-LOCK NO-ERROR .

       IF AVAIL style THEN
           ASSIGN style-dscr:SCREEN-VALUE = style.dscr .
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod D-Dialog
ON LEAVE OF style-cod IN FRAME D-Dialog /* Style Code */
DO:
       
    IF self:SCREEN-VALUE NE "" THEN DO:
        RUN valid-style NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
    END.

   IF self:SCREEN-VALUE NE "" THEN DO:
       FIND FIRST style WHERE style.company = cocode
           AND style.style EQ self:SCREEN-VALUE NO-LOCK NO-ERROR .

       IF AVAIL style THEN
           ASSIGN style-dscr:SCREEN-VALUE = style.dscr .
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL test D-Dialog
ON HELP OF test IN FRAME D-Dialog /* Test */
DO:
   def var char-val as cha no-undo.
   def var look-recid as recid no-undo.
   
   run windows/l-test.w (gcompany,locode,flue:SCREEN-VALUE,output char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                        ASSIGN
                          self:screen-value  = entry(1,char-val)
                           .                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL test D-Dialog
ON LEAVE OF test IN FRAME D-Dialog /* Test */
DO:
  def var ls-board as cha no-undo.
  DEF VAR lv-mat-types AS CHAR INIT "B" NO-UNDO.

  IF LASTKEY NE -1 THEN DO: 
      IF test:SCREEN-VALUE NE "" THEN do:
        /*  RUN valid-test NO-ERROR. */
          IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      END.
  END.

    FOR EACH item
        WHERE item.company   EQ gcompany
        /*  AND CAN-DO(lv-mat-types,item.mat-type)*/
          AND item.industry  EQ "2"
          AND item.i-code    EQ "E"
          AND item.flute     EQ flue:SCREEN-VALUE
          AND item.reg-no    EQ test:SCREEN-VALUE
        USE-INDEX mat-type NO-LOCK
        BY item.i-no:
        
      board:SCREEN-VALUE = item.i-no.
    /*  RUN new-board.*/
      LEAVE.
    END.

     /*if not avail item or (avail item and item.i-no = "" ) then   */
   /*  if ls-board = "" THEN RUN new-flute-test.*/
   /*end.   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

/*{src/adm/template/dialogmn.i}*/

   RUN enable_UI.

  {methods/nowait.i}
     
  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}

     ASSIGN 
        board:SCREEN-VALUE = ""
          fg-no:SCREEN-VALUE = ""
          cst-part:SCREEN-VALUE = "" 
          colr-dscr:SCREEN-VALUE = ""
          cust-no:SCREEN-VALUE = ""
          ship-to:SCREEN-VALUE = "" 
          style-cod:SCREEN-VALUE = ""
          style-dscr:SCREEN-VALUE = ""
          flue:SCREEN-VALUE = "" 
          test:SCREEN-VALUE = ""
          len:SCREEN-VALUE = "0" 
          wid:SCREEN-VALUE = "0"
          dep:SCREEN-VALUE = "0"
          quantity:SCREEN-VALUE = "0" .

    APPLY "entry" TO cust-no IN FRAME {&FRAME-NAME}.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ttfrmout D-Dialog 
PROCEDURE create-ttfrmout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    

                            

  ASSIGN
   iFormNumber = iFormNumber + 1
   iBlankNumber = 1             .
  
  CREATE tt-frmout.
  ASSIGN 
      
         tt-frmout.part-no       = cst-part
         tt-frmout.stack-no  = fg-no
         tt-frmout.colr-dscr     = colr-dscr
         tt-frmout.glu-cod     = glu-cod
         tt-frmout.bndl-cod     = bndl-cod
         tt-frmout.plat-cod     = plat-cod
         tt-frmout.bndng-cod     = bndng-cod
         tt-frmout.form-no   = iFormNumber
         tt-frmout.blank-no   = iBlankNumber 

         tt-frmout.cust-no   = cust-no 
         tt-frmout.ship-id   = ship-to 
         tt-frmout.style   = style-cod 
         tt-frmout.flute   = flue 
         tt-frmout.test   = test 
         tt-frmout.TAB   = f-tab 
         tt-frmout.len   = len 
         tt-frmout.wid   = wid 
         tt-frmout.dep   = dep 
         tt-frmout.bord  = board  
         tt-frmout.quantity = quantity 
         tt-frmout.cat = fg-cat 
         tt-frmout.copy-qty[2] = lv-copy-qty[2] 
         tt-frmout.copy-qty[3] = lv-copy-qty[3] 
         tt-frmout.copy-qty[4] = lv-copy-qty[4] 
         tt-frmout.copy-qty[5] = lv-copy-qty[5] 
         tt-frmout.copy-qty[6] = lv-copy-qty[6] 
         tt-frmout.copy-qty[7] = lv-copy-qty[7] 
         tt-frmout.copy-qty[8] = lv-copy-qty[8] 
         tt-frmout.copy-qty[9] = lv-copy-qty[9] 
         tt-frmout.copy-qty[10] = lv-copy-qty[10]
         
         tt-frmout.copy-qty[11] = lv-copy-qty[11] 
         tt-frmout.copy-qty[12] = lv-copy-qty[12] 
         tt-frmout.copy-qty[13] = lv-copy-qty[13] 
         tt-frmout.copy-qty[14] = lv-copy-qty[14] 
         tt-frmout.copy-qty[15] = lv-copy-qty[15] 
         tt-frmout.copy-qty[16] = lv-copy-qty[16] 
         tt-frmout.copy-qty[17] = lv-copy-qty[17] 
         tt-frmout.copy-qty[18] = lv-copy-qty[18] 
         tt-frmout.copy-qty[19] = lv-copy-qty[19] 
         tt-frmout.copy-qty[20] = lv-copy-qty[20] 

         tt-frmout.copy-rel[1] = lv-copy-rel[1]
         tt-frmout.copy-rel[2] = lv-copy-rel[2] 
         tt-frmout.copy-rel[3] = lv-copy-rel[3] 
         tt-frmout.copy-rel[4] = lv-copy-rel[4] 
         tt-frmout.copy-rel[5] = lv-copy-rel[5] 
         tt-frmout.copy-rel[6] = lv-copy-rel[6] 
         tt-frmout.copy-rel[7] = lv-copy-rel[7] 
         tt-frmout.copy-rel[8] = lv-copy-rel[8] 
         tt-frmout.copy-rel[9] = lv-copy-rel[9] 
         tt-frmout.copy-rel[10] = lv-copy-rel[10]
         
         tt-frmout.copy-rel[11] = lv-copy-rel[11] 
         tt-frmout.copy-rel[12] = lv-copy-rel[12] 
         tt-frmout.copy-rel[13] = lv-copy-rel[13] 
         tt-frmout.copy-rel[14] = lv-copy-rel[14] 
         tt-frmout.copy-rel[15] = lv-copy-rel[15] 
         tt-frmout.copy-rel[16] = lv-copy-rel[16] 
         tt-frmout.copy-rel[17] = lv-copy-rel[17] 
         tt-frmout.copy-rel[18] = lv-copy-rel[18] 
         tt-frmout.copy-rel[19] = lv-copy-rel[19] 
         tt-frmout.copy-rel[20] = lv-copy-rel[20]  .
   
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
  DISPLAY cust-no ship-to style-cod style-dscr flue test f-tab len wid dep 
          cst-part colr-dscr fg-no board glu-cod bndl-cod plat-cod bndng-cod 
          fg-cat quantity 
      WITH FRAME D-Dialog.
  ENABLE cust-no ship-to style-cod flue test f-tab len wid dep cst-part 
         colr-dscr fg-no board glu-cod bndl-cod plat-cod bndng-cod Btn_OK 
         Btn_Cancel RECT-1 fg-cat quantity 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fgitem D-Dialog 
PROCEDURE valid-fgitem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
     
         IF fg-no:SCREEN-VALUE  NE "" THEN DO:
           FIND FIRST itemfg
               WHERE itemfg.company  EQ gcompany
               AND itemfg.i-no    EQ fg-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
           IF NOT AVAIL itemfg  THEN DO:
               MESSAGE "Invalid Fg Item, try help..." VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO fg-no .
               RETURN ERROR.
           END.
       END.
  END.


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
     IF cst-part:SCREEN-VALUE  NE "" THEN DO:
        /* FIND FIRST itemfg
                    WHERE itemfg.company  EQ gcompany
                      AND itemfg.part-no    EQ cst-part:SCREEN-VALUE  NO-LOCK NO-ERROR.
       IF NOT AVAIL itemfg  THEN DO:
      MESSAGE "Invalid Cust Part, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cst-part .
      RETURN ERROR.
       END.*/
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat B-table-Win 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fg-cat:SCREEN-VALUE  = CAPS(fg-cat:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST fgcat
                    WHERE fgcat.company EQ cocode
                      AND fgcat.procat  EQ fg-cat:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-cat .
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id B-table-Win 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST shipto
                    WHERE shipto.company EQ gcompany
                      AND shipto.cust-no EQ cust-no:SCREEN-VALUE
                      AND shipto.ship-id EQ ship-to:SCREEN-VALUE)  THEN DO:
      MESSAGE "            Invalid entry, try help...             "  VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ship-to .
        RETURN ERROR.
      
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style B-table-Win 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST style
                    WHERE style.company  EQ gcompany
                      AND style.style    EQ style-cod:SCREEN-VALUE
                      AND style.industry EQ "2")  THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO style-cod .
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-flute B-table-Win 
PROCEDURE valid-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST stack-flute
                    WHERE stack-flute.company  EQ gcompany
                     AND stack-flute.loc     EQ locode
                     AND stack-flute.code    EQ flue:SCREEN-VALUE)  THEN DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO flue .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-64-dec V-table-Win 
PROCEDURE valid-64-dec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-dec AS DEC DECIMALS 6 NO-UNDO.
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   DEFINE OUTPUT PARAMETER op-dec AS DEC DECIMALS 6 NO-UNDO.
    
    FIND FIRST tt-64-dec WHERE
      substring(string(tt-64-dec.DEC),1,3) EQ substring(string(ip-dec),1,3) NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-64-dec  THEN
      op-error = YES.
    ELSE  op-dec = tt-64-dec.DEC .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
