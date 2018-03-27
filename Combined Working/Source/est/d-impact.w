&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est/d-artioscad.w
  
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

DEFINE TEMP-TABLE ttblCADCAM NO-UNDO
  FIELD board LIKE ef.board
  FIELD boardID AS INTEGER
  FIELD cad-no LIKE eb.cad-no
  FIELD cal LIKE ef.cal
  FIELD dep LIKE eb.dep
  FIELD die-no LIKE eb.die-no
  FIELD len LIKE eb.len
  FIELD lin-in LIKE eb.lin-in
  FIELD style LIKE eb.style
  FIELD t-len LIKE eb.t-len
  FIELD t-sqin LIKE eb.t-sqin
  FIELD t-wid LIKE eb.t-wid
  FIELD weight LIKE ef.weight
  FIELD wid LIKE eb.wid.

{est/artiosvar.i "shared"}


def temp-table tt-CompStyle NO-UNDO
    field form-num as int
    field style as cha
    FIELD pur-man AS LOG
    field blank-num as int
    field NumOfComponents as int
    field compRatio as dec extent 30
    field compStyle as cha extent 30
    field compQty as int extent 30
    field compNumUp as int extent 30
    FIELD compPurMan AS LOG EXTENT 10.
                                 
{sys/inc/var.i shared}
{custom/gcompany.i}  
gcompany = cocode.

do transaction:
  {sys/inc/artioscad.i}
end.

def var iFormNumber as int no-undo.
def var iBlankNumber as int no-undo.
def var iNumofCADForm as int no-undo.
def var iProjectCount as int init 50 no-undo.

def temp-table tt-SubDir NO-UNDO field DirName as cha.

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
&Scoped-Define ENABLED-OBJECTS cadNumber btnCAD#Lookup iSetQty cCategory ~
rs-man-pur Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cadNumber iSetQty cCategory rs-man-pur 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCAD#Lookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.38.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.71
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.71
     BGCOLOR 8 .

DEFINE VARIABLE cadNumber AS CHARACTER FORMAT "x(200)" 
     LABEL "CAD File/Project #" 
     VIEW-AS FILL-IN 
     SIZE 77.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE cCategory AS CHARACTER FORMAT "X(5)":U 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.38 NO-UNDO.

DEFINE VARIABLE iSetQty AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Estimate Qty" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.38 NO-UNDO.

DEFINE VARIABLE rs-man-pur AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Manufactured", "M",
"Purchased", "P"
     SIZE 33 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cadNumber AT ROW 2.19 COL 20 COLON-ALIGNED
     btnCAD#Lookup AT ROW 2.19 COL 99.6 WIDGET-ID 4
     iSetQty AT ROW 4.1 COL 20 COLON-ALIGNED WIDGET-ID 42
     cCategory AT ROW 5.57 COL 20 COLON-ALIGNED WIDGET-ID 88
     rs-man-pur AT ROW 7.67 COL 21 NO-LABEL WIDGET-ID 118
     Btn_OK AT ROW 10.05 COL 28
     Btn_Cancel AT ROW 10.05 COL 61
     "================================================================================" VIEW-AS TEXT
          SIZE 81.6 BY .62 AT ROW 8.86 COL 19 WIDGET-ID 40
     SPACE(8.39) SKIP(2.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Create Estimate from CADCAM Software"
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Create Estimate from CADCAM Software */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCAD#Lookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCAD#Lookup D-Dialog
ON CHOOSE OF btnCAD#Lookup IN FRAME D-Dialog
DO:
    def var cCadFile as cha no-undo.
    
    def var okClicked as log no-undo.
    
    SYSTEM-DIALOG GET-FILE cCadFile 
                TITLE 'Select Artios CAD File to insert'
                FILTERS 'XML Files    (*.xml)' '*.xml'
                INITIAL-DIR artioscad-chr
                MUST-EXIST USE-FILENAME UPDATE okClicked.
  

  IF okClicked THEN
     ASSIGN cadNumber:screen-value = cCadFile.
     
     
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
  def var iExt as int no-undo. 
  def var cArtiosCadFile as cha no-undo.
  def var iRevision as int init 65 no-undo.
  def var iRevExt as int no-undo.
  
  assign iFormNumber = 0
         iBlankNumber = 0
         cadNumber /*cStyle */
         cCategory rs-man-pur iSetQty
             /* dRatio-1 dRatio-2
              dRatio-3 dRatio-4 dRatio-5 dRatio-6 dRatio-7 dRatio-8 dRatio-9*/
              .

  if artioscad-chr = "" then artioscad-chr = "c:\artios\asi\".
  if substring(artioscad-chr,length(artioscad-chr),1) <> "/" and
     substring(artioscad-chr,length(artioscad-chr),1) <> "\" then
     artioscad-chr = artioscad-chr + "\".
  
  if index(cadNumber,"\") > 0 or index(cadNumber,"/") > 0 or
     index(cadNumber,"ARD") > 0 then cArtiosCadFile = cadNumber.
  else    cArtiosCadFile = artioscad-chr + cadnumber + ".xml".
 
  session:set-wait-state("general").
  
  /*run AssignCADFormInfo. not for impact cad */
  
  /* run getSubDirList.   run from leave of cadnumber */
  
  if search(cArtiosCadFile ) <> ? then do:  /* import single CAD file */
     run create-ttCad (cArtiosCadFile).
  end.
  else do iExt = 1 to iProjectCount: 
    /* import Project CAD file ###### + %%(2 digit extension) +  @ (1 character revision) */
    
     for each tt-SubDir :    
       if search(tt-SubDir.DirName + cadNumber + string(iExt,"99") + ".ARD" ) <> ? then do:       
          
          run create-ttCad (tt-SubDir.DirName + cadNumber + string(iExt,"99") + ".ARD" ).               
       end.
        /* check revision file */
       do iRevExt = 65 to 90:
     
          if search(tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt) + ".ARD" ) <> ? 
             then  run create-ttCad (tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt) + ".ARD" ).               
          else if search(tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ) <> ? 
             then  run create-ttCad (tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ).  
       end.
     end.  /* each tt-SubDir */
                 
  end. 
  session:set-wait-state("").
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cadNumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cadNumber D-Dialog
ON LEAVE OF cadNumber IN FRAME D-Dialog /* CAD File/Project # */
DO:
    def var icnt as int no-undo.
    
    ASSIGN cadNumber .
    /*run getNumofCADForm (output iNumofCADForm).*/

    /* if iNumofCADForm > 1 then do icnt = 2 to iNumofCadForm: */
/*        cb-CADSeq:add-last(string(icnt)). */
/*     end. */
    
    /*vNumOfCADForm:screen-value  = "Total Number of CAD File: " + string(iNumofCadForm).  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategory D-Dialog
ON HELP OF cCategory IN FRAME D-Dialog /* Category */
DO:
   def var char-val as cha no-undo. 

   run windows/l-fgcat.w (gcompany,self:screen-value,output char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         self:screen-value  = entry(1,char-val).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategory D-Dialog
ON LEAVE OF cCategory IN FRAME D-Dialog /* Category */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-category(OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iSetQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iSetQty D-Dialog
ON LEAVE OF iSetQty IN FRAME D-Dialog /* Estimate Qty */
DO:
      /* assign iCompQty-1:screen-value in frame {&frame-name} = iSetQty:screen-value */
/*                  . */
                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-man-pur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-man-pur D-Dialog
ON VALUE-CHANGED OF rs-man-pur IN FRAME D-Dialog
DO:
   assign /*rs-man-pur-1:screen-value in frame {&frame-name} = rs-man-pur:screen-value*/
          rs-man-pur .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignCADFormInfo D-Dialog 
PROCEDURE assignCADFormInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-CompStyle for tt-CompStyle.
/*                                                                                                                */
/*   do with frame {&frame-name}:                                                                                 */
/*     find first bf-CompStyle where bf-CompStyle.form-num = int(substring(cb-CADSeq:screen-value,1,2)) no-error. */
/*     if not avail bf-CompStyle then do:                                                                         */
/*        create bf-CompStyle.                                                                                    */
/*        assign bf-CompStyle.form-num = int(substring(cb-CADSeq,1,2)).                                           */
/*     end.                                                                                                       */
/*                                                                                                                */
/*     assign bf-CompStyle.Style = cStyle:screen-value in frame {&frame-name}                                     */
/*            bf-CompStyle.pur-man = IF rs-man-pur:SCREEN-VALUE EQ "M" THEN NO ELSE YES                           */
/*            bf-CompStyle.NumOfComponents = int(iNumofParts:screen-value)                                        */
/*            bf-CompStyle.compRatio[1] = dec(dRatio-1:screen-value)                                              */
/*            bf-CompStyle.compRatio[2] = dec(dRatio-2:screen-value)                                              */
/*            bf-CompStyle.compRatio[3] = dec(dRatio-3:screen-value)                                              */
/*            bf-CompStyle.compRatio[4] = dec(dRatio-4:screen-value)                                              */
/*            bf-CompStyle.compRatio[5] = dec(dRatio-5:screen-value)                                              */
/*            bf-CompStyle.compRatio[6] = dec(dRatio-6:screen-value)                                              */
/*            bf-CompStyle.compRatio[7] = dec(dRatio-7:screen-value)                                              */
/*            bf-CompStyle.compRatio[8] = dec(dRatio-8:screen-value)                                              */
/*            bf-CompStyle.compRatio[9] = dec(dRatio-9:screen-value)                                              */
/*            bf-CompStyle.compRatio[10] = dec(dRatio-10:screen-value)                                            */
/*                                                                                                                */
/*            bf-CompStyle.compStyle[1] = cStyleComp-1:screen-value                                               */
/*            bf-CompStyle.compStyle[2] = cStyleComp-2:screen-value                                               */
/*            bf-CompStyle.compStyle[3] = cStyleComp-3:screen-value                                               */
/*            bf-CompStyle.compStyle[4] = cStyleComp-4:screen-value                                               */
/*            bf-CompStyle.compStyle[5] = cStyleComp-5:screen-value                                               */
/*            bf-CompStyle.compStyle[6] = cStyleComp-6:screen-value                                               */
/*            bf-CompStyle.compStyle[7] = cStyleComp-7:screen-value                                               */
/*            bf-CompStyle.compStyle[8] = cStyleComp-8:screen-value                                               */
/*            bf-CompStyle.compStyle[9] = cStyleComp-9:screen-value                                               */
/*            bf-CompStyle.compStyle[10] = cStyleComp-10:screen-value                                             */
/*                                                                                                                */
/*            bf-CompStyle.compQty[1] = int(iCompQty-1:screen-value)                                              */
/*            bf-CompStyle.compQty[2] = int(iCompQty-2:screen-value)                                              */
/*            bf-CompStyle.compQty[3] = int(iCompQty-3:screen-value)                                              */
/*            bf-CompStyle.compQty[4] = int(iCompQty-4:screen-value)                                              */
/*            bf-CompStyle.compQty[5] = int(iCompQty-5:screen-value)                                              */
/*            bf-CompStyle.compQty[6] = int(iCompQty-6:screen-value)                                              */
/*            bf-CompStyle.compQty[7] = int(iCompQty-7:screen-value)                                              */
/*            bf-CompStyle.compQty[8] = int(iCompQty-8:screen-value)                                              */
/*            bf-CompStyle.compQty[9] = int(iCompQty-9:screen-value)                                              */
/*            bf-CompStyle.compQty[10] = int(iCompQty-10:screen-value)                                            */
/*                                                                                                                */
/*            bf-CompStyle.compNumUp[1] = int(iNumUp-1:screen-value)                                              */
/*            bf-CompStyle.compNumUp[2] = int(iNumUp-2:screen-value)                                              */
/*            bf-CompStyle.compNumUp[3] = int(iNumUp-3:screen-value)                                              */
/*            bf-CompStyle.compNumUp[4] = int(iNumUp-4:screen-value)                                              */
/*            bf-CompStyle.compNumUp[5] = int(iNumUp-5:screen-value)                                              */
/*            bf-CompStyle.compNumUp[6] = int(iNumUp-6:screen-value)                                              */
/*            bf-CompStyle.compNumUp[7] = int(iNumUp-7:screen-value)                                              */
/*            bf-CompStyle.compNumUp[8] = int(iNumUp-8:screen-value)                                              */
/*            bf-CompStyle.compNumUp[9] = int(iNumUp-9:screen-value)                                              */
/*            bf-CompStyle.compNumUp[10] = int(iNumUp-10:screen-value)                                            */
/*            bf-CompStyle.compPurMan[1] = IF rs-man-pur-1:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[2] = IF rs-man-pur-2:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[3] = IF rs-man-pur-3:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[4] = IF rs-man-pur-4:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[5] = IF rs-man-pur-5:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[6] = IF rs-man-pur-6:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[7] = IF rs-man-pur-7:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[8] = IF rs-man-pur-8:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[9] = IF rs-man-pur-9:SCREEN-VALUE = "M" THEN NO ELSE YES                    */
/*            bf-CompStyle.compPurMan[10] = IF rs-man-pur-10:SCREEN-VALUE = "M" THEN NO ELSE YES.                 */
/*   end.                                                                                                         */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ttCad D-Dialog 
PROCEDURE create-ttCad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ipFilename as cha no-undo.
  DEF VAR ldProjectId AS DEC NO-UNDO.

def var cFilenameJPG as cha no-undo.
/*def var resultx as int no-undo.*/

def var iSeq as int no-undo.
DEF VAR cDieInch AS cha NO-UNDO.
def var cCadPath as cha no-undo.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "CADFILE"  NO-LOCK NO-ERROR.

ASSIGN
   iSeq = 1
   iFormNumber = /*iFormNumber + 1*/ 1
   iBlankNumber = 1
   cCadPath = if avail sys-ctrl and sys-ctrl.char-fld <> "" then sys-ctrl.char-fld else "c:\tmp\"
   .
/*   cFileNameJPG = cCadPath + /*chCtrlFrame:CadX:ReturnTextCode4Param("#W$")*/
                  SUBSTRING(ipFilename, R-INDEX(ipFilename, "\") + 1) + ".jpg"
   
   resultx = chCtrlFrame:CadX:OpenDesign (ipFilename,0)
   resultx = chCtrlFrame:CadX:SetOverlayClass(3,1) /*print dimensions*/
   resultx = chCtrlFrame:CadX:SaveAsBitmap(1,cFilenameJPG, 600, 600, 20, , , 1, 100).

find first tt-CompStyle where tt-CompStyle.form-num = iFormNumber no-error.
 
  cDieInch = chCtrlFrame:CadX:ReturnTextCode4Param("#LENRULE").
  IF INDEX(cDieInch,"+") > 0 THEN cDieInch = SUBSTRING(cDieInch,1,INDEX(cDieInch,"+") - 1).
  
  CREATE tt-artios.
  ASSIGN tt-artios.cadnum = /*cadNumber*/ chCtrlFrame:CadX:ReturnTextCode4Param("#ITEM$")
         tt-artios.cadfile = ipFileName
         tt-artios.custname = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(CUST,NAME$)")
         tt-artios.partname = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC1$)") 
         tt-artios.partnum = chCtrlFrame:CadX:ReturnTextCode4Param("#ITEM$")
         tt-artios.style = /*chCtrlFrame:CadX:ReturnTextCode4Param("#CFN$")*/ 
                               if avail tt-CompStyle then tt-CompStyle.CompStyle[1] else cStyle
         tt-artios.flute = ""
         tt-artios.test = "" 
         tt-artios.board = substring(chCtrlFrame:CadX:ReturnTextCode4Param("BRD$"),1,9)
         tt-artios.len = chCtrlFrame:CadX:ReturnNumericCode4Param("L")
         tt-artios.wid = chCtrlFrame:CadX:ReturnNumericCode4Param("W")
         tt-artios.dep = chCtrlFrame:CadX:ReturnNumericCode4Param("D")
         tt-artios.t-len = chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEX")
         tt-artios.t-wid = chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEY")
         tt-artios.seq = iSeq
         tt-artios.ratio = if avail tt-CompStyle then tt-CompStyle.CompRatio[1] else dRatio-1
         tt-artios.form-num = iFormNumber
         tt-artios.blank-num = iBlankNumber
         tt-artios.die-in = DEC(cDieInch) 
         tt-artios.setQty = iSetQty
         tt-artios.CompQty = if avail tt-CompStyle then tt-CompStyle.CompQty[1] else iCompQty-1
         tt-artios.procat = cCategory
         tt-artios.cust-no = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(CUST,NUMBR$)")
         tt-artios.sman    = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(SLSPN,SNAME$)")
         tt-artios.dienum =  chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC3$)")
         tt-artios.NumOfComponents = tt-CompStyle.NumOfComponents
         tt-artios.CompNumUp = if avail tt-CompStyle then tt-CompStyle.CompNumUp[1] else 1
         tt-artios.DesignImg = cFileNameJPG
         tt-artios.pur-man = IF AVAIL tt-CompStyle THEN tt-CompStyle.CompPurMan[1] ELSE
                             IF rs-man-pur = "M" THEN NO
                             ELSE YES
         tt-artios.grain   = chCtrlFrame:CadX:ReturnTextCode4Param("#GRAIN$")
         tt-artios.DesignerName = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DSGNR,FNAME$)")
                                 + " " +
                                 chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DSGNR,LNAME$)")
                          .
           
       if avail tt-compstyle and tt-CompStyle.NumOfComponents > 1 then do while iSeq < tt-CompStyle.NumOfComponents:
  
          create tt-artios.
          assign iSeq = iSeq + 1
                 iBlankNumber = iBlankNumber + 1
                 tt-artios.cadnum = /*cadNumber*/ chCtrlFrame:CadX:ReturnTextCode4Param("#ITEM$")
                 tt-artios.cadfile = ipFileName
                 tt-artios.custname = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(CUST,NAME$)")
                 tt-artios.partname = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC1$)") 
                 tt-artios.partnum = chCtrlFrame:CadX:ReturnTextCode4Param("#ITEM$")
                 tt-artios.style = /*chCtrlFrame:CadX:ReturnTextCode4Param("#CFN$")*/ 
                                     if avail tt-CompStyle then tt-CompStyle.CompStyle[iSeq] else cStyle
                 tt-artios.flute = ""
                 tt-artios.test = "" 
                 tt-artios.board = substring(chCtrlFrame:CadX:ReturnTextCode4Param("BRD$"),1,9)
                 tt-artios.len = chCtrlFrame:CadX:ReturnNumericCode4Param("L")
                 tt-artios.wid = chCtrlFrame:CadX:ReturnNumericCode4Param("W")
                 tt-artios.dep = chCtrlFrame:CadX:ReturnNumericCode4Param("D")
                 tt-artios.t-len = chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEX")
                 tt-artios.t-wid = chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEY")
                 tt-artios.seq = iSeq
                 tt-artios.ratio = if avail tt-CompStyle then tt-CompStyle.CompRatio[iSeq] else dRatio-2
                 tt-artios.form-num = iFormNumber
                 tt-artios.blank-num = iBlankNumber
                 tt-artios.die-in =  DEC(cDieInch)
                 tt-artios.setQty = iSetQty
                 tt-artios.CompQty =  if avail tt-CompStyle then tt-CompStyle.CompQty[iSeq] else 0
                 tt-artios.procat = cCategory                      
                 tt-artios.cust-no = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(CUST,NUMBR$)")
                 tt-artios.sman    = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(SLSPN,SNAME$)")
                 tt-artios.dienum =  chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC3$)")
                 tt-artios.NumOfComponents = tt-CompStyle.NumOfComponents
                 tt-artios.CompNumUp = if avail tt-CompStyle then tt-CompStyle.CompNumUp[iSeq] else 1
                 tt-artios.DesignImg = cFileNameJPG
                 tt-artios.pur-man = if avail tt-CompStyle then tt-CompStyle.CompPurMan[iSeq] else
                                     IF rs-man-pur EQ "M" THEN NO ELSE YES
                 tt-artios.grain   = chCtrlFrame:CadX:ReturnTextCode4Param("#GRAIN$")
                 tt-artios.DesignerName = chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DSGNR,FNAME$)")
                                 + " " + 
                                 chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(DSGNR,LNAME$)")
                  .
       end.

       resultx = chCtrlFrame:CadX:CloseDesign().
*/           

        /*find first tt-CompStyle where tt-CompStyle.form-num = iFormNumber no-error.*/

        RUN est/loadImpactXml.p (ipFilename, OUTPUT ldProjectId).
        
        FIND xmlProject WHERE xmlProject.projectId = ldProjectid NO-LOCK.
        FIND xmlProjectInfo WHERE xmlProjectInfo.projectId = xmlProject.projectId NO-LOCK.

        iFormNumber = 0.
        iBlankNumber = 1.
        FOR EACH xmlLayouts NO-LOCK WHERE xmlLayouts.projectId = xmlProjectInfo.projectId:

           FOR EACH xmllayoutsForm NO-LOCK WHERE xmlLayoutsForm.projectId = xmlLayouts.projectId,
               EACH xmllayoutsFormDesign NO-LOCK WHERE xmllayoutsformdesign.projectid = xmllayouts.projectid
               BREAK BY xmlLayoutsFormDesign.layoutName /*BY xmllayoutFormDesign.*/                                         
               :
        
               IF FIRST-OF(xmlLayoutsFormDesign.LayoutName) THEN iFormNumber = iFormNumber + 1.

        CREATE tt-artios.
  ASSIGN tt-artios.cadnum = "zzz" 
         tt-artios.cadfile = ipFileName
         tt-artios.custname = xmlProjectInfo.ProjectCustomer
         tt-artios.cust-no = xmlProjectInfo.ProjectCustomer
         tt-artios.sman    = "zzz"
         tt-artios.partname = "" 
         tt-artios.partnum = xmlLayoutsFormDesign.OneUpCustomerPartNumber /* set xmlProjectInfo.ProjectSetPartNumber*/ 
         tt-artios.style = xmlLayoutsFormDesign.OneUpBoxStyle /* cStyle*/
         tt-artios.flute = ""
         tt-artios.test = "" 
         tt-artios.board = xmlLayoutsFormDesign.OneUpBoardGrade /*LayerMaterialName*/
         tt-artios.len = dec(xmlLayoutsFormDesign.OneUpBoxLength)
         tt-artios.wid = dec(xmlLayoutsFormDesign.OneUpBoxWidth)
         tt-artios.dep = dec(xmlLayoutsFormDesign.OneUpBoxDepth)
         tt-artios.t-len = dec(xmlLayoutsFormDesign.OneUpBlankX)
         tt-artios.t-wid = dec(xmlLayoutsFormDesign.OneUpBlankY)
         tt-artios.seq = iSeq
         /*tt-artios.ratio = if avail tt-CompStyle then tt-CompStyle.CompRatio[1] else dRatio-1*/
         tt-artios.form-num = iFormNumber
         tt-artios.blank-num = iBlankNumber
         tt-artios.die-in = DEC(cDieInch) 
         tt-artios.setQty = 1 /*iSetQty*/
         tt-artios.CompQty = iSetQty
         /*tt-artios.CompQty = if avail tt-CompStyle then tt-CompStyle.CompQty[1] else iCompQty-1*/
         tt-artios.procat = cCategory
         tt-artios.dienum =  ""
/*          tt-artios.NumOfComponents = tt-CompStyle.NumOfComponents                          */
/*          tt-artios.CompNumUp = if avail tt-CompStyle then tt-CompStyle.CompNumUp[1] else 1 */
         tt-artios.DesignImg = cFileNameJPG
         tt-artios.pur-man = IF AVAIL tt-CompStyle THEN tt-CompStyle.CompPurMan[1] ELSE
                             IF rs-man-pur = "M" THEN NO
                             ELSE YES
         tt-artios.grain   =   xmlLayoutsFormDesign.OneUpFluteGrainDirection
         tt-artios.DesignerName = xmlLayoutsFormDesign.OneUpDesignNumber
        .

                                 .
           END. /*xmllayoutsform */

        END.  /*xmlLayouts */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects D-Dialog 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* def var tmpCount as int no-undo.                                   */
/* def var hCompRatio as handle no-undo.                              */
/* def var hStyle as handle no-undo.                                  */
/* def var hQty as handle no-undo.                                    */
/* def var iRow as int no-undo.                                       */
/*                                                                    */
/* iRow = 14.                                                         */
/* /* create widget-pool "wComponents". */                            */
/* do tmpCount = 1 to iNumOfParts:                                    */
/*      create fill-in hCompRatio  /* in widget-pool "wComponents" */ */
/*                ASSIGN DATA-TYPE = "Decimal"                        */
/*                  FORMAT = ">>9.99%"                                */
/*                  COLUMN = 18.5                                     */
/*                  ROW = iRow                                        */
/*                  /* SCREEN-VALUE = hBufField:BUFFER-VALUE */       */
/*                  frame = frame {&frame-name}:handle                */
/*                  HIDDEN = NO                                       */
/*                  sensitive = yes.                                  */
/*      iRow = iRow + 2.                                              */
/*                                                                    */
/*      .                                                             */
/* end.                                                               */
/*                                                                    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayCADFormInfo D-Dialog 
PROCEDURE displayCADFormInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-compStyle for tt-CompStyle.
  
 
/*   find first bf-CompStyle where bf-CompStyle.form-num = int(substring(cb-CADSeq,1,2)) no-error. */
/*   if not avail bf-CompStyle then do:                                                            */
/*      create bf-CompStyle.                                                                       */
/*      assign bf-CompStyle.form-num = int(substring(cb-CADSeq,1,2)).                              */
/*   end.                                                                                          */
/*                                                                                                          */
/*  assign bf-CompStyle.Style = cStyle:screen-value in frame {&frame-name}                                  */
/*         bf-CompStyle.pur-man = IF rs-man-pur:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "M" THEN NO ELSE YES */
/*         bf-CompStyle.NumOfComponents = int(iNumofParts:screen-value)                                     */
/*         bf-CompStyle.compRatio[1] = dec(dRatio-1:screen-value)                                           */
/*         bf-CompStyle.compRatio[2] = dec(dRatio-2:screen-value)                                           */
/*         bf-CompStyle.compRatio[3] = dec(dRatio-3:screen-value)                                           */
/*         bf-CompStyle.compRatio[4] = dec(dRatio-4:screen-value)                                           */
/*         bf-CompStyle.compRatio[5] = dec(dRatio-5:screen-value)                                           */
/*         bf-CompStyle.compRatio[6] = dec(dRatio-6:screen-value)                                           */
/*         bf-CompStyle.compRatio[7] = dec(dRatio-7:screen-value)                                           */
/*         bf-CompStyle.compRatio[8] = dec(dRatio-8:screen-value)                                           */
/*         bf-CompStyle.compRatio[9] = dec(dRatio-9:screen-value)                                           */
/*         bf-CompStyle.compRatio[10] = dec(dRatio-10:screen-value)                                         */
/*                                                                                                          */
/*         bf-CompStyle.compStyle[1] = cStyleComp-1:screen-value                                            */
/*         bf-CompStyle.compStyle[2] = cStyleComp-2:screen-value                                            */
/*         bf-CompStyle.compStyle[3] = cStyleComp-3:screen-value                                            */
/*         bf-CompStyle.compStyle[4] = cStyleComp-4:screen-value                                            */
/*         bf-CompStyle.compStyle[5] = cStyleComp-5:screen-value                                            */
/*         bf-CompStyle.compStyle[6] = cStyleComp-6:screen-value                                            */
/*         bf-CompStyle.compStyle[7] = cStyleComp-7:screen-value                                            */
/*         bf-CompStyle.compStyle[8] = cStyleComp-8:screen-value                                            */
/*         bf-CompStyle.compStyle[9] = cStyleComp-9:screen-value                                            */
/*         bf-CompStyle.compStyle[10] = cStyleComp-10:screen-value                                          */
/*                                                                                              */
/*         bf-CompStyle.compQty[1] = int(iCompQty-1:screen-value)                               */
/*         bf-CompStyle.compQty[2] = int(iCompQty-2:screen-value)                               */
/*         bf-CompStyle.compQty[3] = int(iCompQty-3:screen-value)                               */
/*         bf-CompStyle.compQty[4] = int(iCompQty-4:screen-value)                               */
/*         bf-CompStyle.compQty[5] = int(iCompQty-5:screen-value)                               */
/*         bf-CompStyle.compQty[6] = int(iCompQty-6:screen-value)                               */
/*         bf-CompStyle.compQty[7] = int(iCompQty-7:screen-value)                               */
/*         bf-CompStyle.compQty[8] = int(iCompQty-8:screen-value)                               */
/*         bf-CompStyle.compQty[9] = int(iCompQty-9:screen-value)                               */
/*         bf-CompStyle.compQty[10] = int(iCompQty-10:screen-value)                             */
/*                                                                                              */
/*         bf-CompStyle.compNumUp[1] = int(iNumUp-1:screen-value)                               */
/*         bf-CompStyle.compNumUp[2] = int(iNumUp-2:screen-value)                               */
/*         bf-CompStyle.compNumUp[3] = int(iNumUp-3:screen-value)                               */
/*         bf-CompStyle.compNumUp[4] = int(iNumUp-4:screen-value)                               */
/*         bf-CompStyle.compNumUp[5] = int(iNumUp-5:screen-value)                               */
/*         bf-CompStyle.compNumUp[6] = int(iNumUp-6:screen-value)                               */
/*         bf-CompStyle.compNumUp[7] = int(iNumUp-7:screen-value)                               */
/*         bf-CompStyle.compNumUp[8] = int(iNumUp-8:screen-value)                               */
/*         bf-CompStyle.compNumUp[9] = int(iNumUp-9:screen-value)                               */
/*         bf-CompStyle.compNumUp[10] = int(iNumUp-10:screen-value)                             */
/*                                                                                              */
/*         bf-CompStyle.compPurMan[1] = IF rs-man-pur-1:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[2] = IF rs-man-pur-2:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[3] = IF rs-man-pur-3:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[4] = IF rs-man-pur-4:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[5] = IF rs-man-pur-5:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[6] = IF rs-man-pur-6:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[7] = IF rs-man-pur-7:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[8] = IF rs-man-pur-8:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[9] = IF rs-man-pur-9:SCREEN-VALUE EQ "M" THEN NO ELSE YES.   */
/*         bf-CompStyle.compPurMan[10] = IF rs-man-pur-10:SCREEN-VALUE EQ "M" THEN NO ELSE YES. */
/*                                                                                              */
/*                                                                                                                  */
/*       find first tt-CompStyle where tt-CompStyle.form-num = int(substring(cb-CADSeq:screen-value,1,2)) no-error. */
/*       if avail tt-CompStyle then                                                                                 */
/*       assign cStyle:screen-value = tt-CompStyle.Style                                                            */
/*                iNumofParts:screen-value = string(tt-CompStyle.NumOfComponents)                                   */
/*                dRatio-1:screen-value = string(tt-CompStyle.compRatio[1])                                         */
/*                dRatio-2:screen-value = string(tt-CompStyle.compRatio[2])                                         */
/*                dRatio-3:screen-value = string(tt-CompStyle.compRatio[3])                                         */
/*                dRatio-4:screen-value = string(tt-CompStyle.compRatio[4])                                         */
/*                dRatio-5:screen-value = string(tt-CompStyle.compRatio[5])                                         */
/*                dRatio-6:screen-value = string(tt-CompStyle.compRatio[6])                                         */
/*                dRatio-7:screen-value = string(tt-CompStyle.compRatio[7])                                         */
/*                dRatio-8:screen-value = string(tt-CompStyle.compRatio[8])                                         */
/*                dRatio-9:screen-value = string(tt-CompStyle.compRatio[9])                                         */
/*                dRatio-10:screen-value = string(tt-CompStyle.compRatio[10])                                       */
/*                cStyleComp-1:screen-value = tt-CompStyle.compStyle[1]                                             */
/*                cStyleComp-2:screen-value = tt-CompStyle.compStyle[2]                                             */
/*                cStyleComp-3:screen-value = tt-CompStyle.compStyle[3]                                             */
/*                cStyleComp-4:screen-value = tt-CompStyle.compStyle[4]                                             */
/*                cStyleComp-5:screen-value = tt-CompStyle.compStyle[5]                                             */
/*                cStyleComp-6:screen-value = tt-CompStyle.compStyle[6]                                             */
/*                cStyleComp-7:screen-value = tt-CompStyle.compStyle[7]                                             */
/*                cStyleComp-8:screen-value = tt-CompStyle.compStyle[8]                                             */
/*                cStyleComp-9:screen-value = tt-CompStyle.compStyle[9]                                             */
/*                cStyleComp-10:screen-value = tt-CompStyle.compStyle[10]                                           */
/*                iCompQty-1:screen-value = string(tt-CompStyle.compQty[1])                                         */
/*                iCompQty-2:screen-value = string(tt-CompStyle.compQty[2])                                         */
/*                iCompQty-3:screen-value = string(tt-CompStyle.compQty[3])                                         */
/*                iCompQty-4:screen-value = string(tt-CompStyle.compQty[4])                                         */
/*                iCompQty-5:screen-value = string(tt-CompStyle.compQty[5])                                         */
/*                iCompQty-6:screen-value = string(tt-CompStyle.compQty[6])                                         */
/*                iCompQty-7:screen-value = string(tt-CompStyle.compQty[7])                                         */
/*                iCompQty-8:screen-value = string(tt-CompStyle.compQty[8])                                         */
/*                iCompQty-9:screen-value = string(tt-CompStyle.compQty[9])                                         */
/*                iCompQty-10:screen-value = string(tt-CompStyle.compQty[10])                                       */
/*                iNumUp-1:screen-value = string(bf-CompStyle.compNumUp[1])                                         */
/*                iNumUp-2:screen-value = string(bf-CompStyle.compNumUp[2])                                         */
/*                iNumUp-3:screen-value = string(bf-CompStyle.compNumUp[3])                                         */
/*                iNumUp-4:screen-value = string(bf-CompStyle.compNumUp[4])                                         */
/*                iNumUp-5:screen-value = string(bf-CompStyle.compNumUp[5])                                         */
/*                iNumUp-6:screen-value = string(bf-CompStyle.compNumUp[6])                                         */
/*                iNumUp-7:screen-value = string(bf-CompStyle.compNumUp[7])                                         */
/*                iNumUp-8:screen-value = string(bf-CompStyle.compNumUp[8])                                         */
/*                iNumUp-9:screen-value = string(bf-CompStyle.compNumUp[9])                                         */
/*                iNumUp-10:screen-value = string(bf-CompStyle.compNumUp[10])                                       */
/*                                                                                                                  */
/*                rs-man-pur-1:screen-value = IF bf-CompStyle.compPurMan[1] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-2:screen-value = IF bf-CompStyle.compPurMan[2] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-3:screen-value = IF bf-CompStyle.compPurMan[3] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-4:screen-value = IF bf-CompStyle.compPurMan[4] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-5:screen-value = IF bf-CompStyle.compPurMan[5] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-6:screen-value = IF bf-CompStyle.compPurMan[6] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-7:screen-value = IF bf-CompStyle.compPurMan[7] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-8:screen-value = IF bf-CompStyle.compPurMan[8] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-9:screen-value = IF bf-CompStyle.compPurMan[9] EQ NO THEN "M" ELSE "P"    */
/*                rs-man-pur-10:screen-value = IF bf-CompStyle.compPurMan[10] EQ NO THEN "M" ELSE "P". */
/*                                                                                                     */
/*         else assign                                                                                 */
/*                cStyle:screen-value = ""                                                             */
/*                rs-man-pur:SCREEN-VALUE = "M"                                                        */
/*                iNumofParts:screen-value = "1"                                                       */
/*                dRatio-1:screen-value = "100"                                                        */
/*                dRatio-2:screen-value = ""                                                           */
/*                dRatio-3:screen-value = ""                                                           */
/*                dRatio-4:screen-value = ""                                                           */
/*                dRatio-5:screen-value = ""                                                           */
/*                dRatio-6:screen-value = ""                                                           */
/*                dRatio-7:screen-value = ""                                                           */
/*                dRatio-8:screen-value = ""                                                           */
/*                dRatio-9:screen-value = ""                                                           */
/*                dRatio-10:screen-value = ""                                                          */
/*                cStyleComp-1:screen-value = ""                                                       */
/*                cStyleComp-2:screen-value = ""                                                       */
/*                cStyleComp-3:screen-value = ""                                                       */
/*                cStyleComp-4:screen-value = ""                                                       */
/*                cStyleComp-5:screen-value = ""                                                       */
/*                cStyleComp-6:screen-value = ""                                                       */
/*                cStyleComp-7:screen-value = ""                                                       */
/*                cStyleComp-8:screen-value = ""                                                       */
/*                cStyleComp-9:screen-value = ""                                                       */
/*                cStyleComp-10:screen-value = ""                                                      */
/*                iCompQty-1:screen-value = ""                                                         */
/*                iCompQty-2:screen-value = ""                                                         */
/*                iCompQty-3:screen-value = ""                                                         */
/*                iCompQty-4:screen-value = ""                                                         */
/*                iCompQty-5:screen-value = ""                                                         */
/*                iCompQty-6:screen-value = ""                                                         */
/*                iCompQty-7:screen-value = ""                                                         */
/*                iCompQty-8:screen-value = ""                                                         */
/*                iCompQty-9:screen-value = ""                                                         */
/*                iCompQty-10:screen-value = ""                                                        */
/*                iNumUp-1:screen-value = ""                                                           */
/*                iNumUp-2:screen-value = ""                                                           */
/*                iNumUp-3:screen-value = ""                                                           */
/*                iNumUp-4:screen-value = ""                                                           */
/*                iNumUp-5:screen-value = ""                                                           */
/*                iNumUp-6:screen-value = ""                                                           */
/*                iNumUp-7:screen-value = ""                                                           */
/*                iNumUp-8:screen-value = ""                                                           */
/*                iNumUp-9:screen-value = ""                                                           */
/*                iNumUp-10:screen-value = ""                                                          */
/*                rs-man-pur-1:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-2:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-3:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-4:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-5:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-6:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-7:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-8:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-9:SCREEN-VALUE = "M"                                                      */
/*                rs-man-pur-10:SCREEN-VALUE = "M".                                                    */
/*                                                                                                     */
/*       assign cb-CADSeq.                                                                             */

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
  DISPLAY cadNumber iSetQty cCategory rs-man-pur 
      WITH FRAME D-Dialog.
  ENABLE cadNumber btnCAD#Lookup iSetQty cCategory rs-man-pur Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCADCAM D-Dialog 
PROCEDURE getCADCAM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  input cad# and die#
  Notes:       ttblCADCAM values extracted and used in ce/b-estitem.w
------------------------------------------------------------------------------*/
  /* DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipCADNumber AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipDieNumber AS CHARACTER NO-UNDO. */
/*    */
/*   DEFINE VARIABLE lvBoardID AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE hRecordSet AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE hConnection AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE hCommand AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE odbcDSN AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcServer AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcUserID AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcPassword AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcQuery AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcStatus AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcRecCount AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE odbcNull AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcCursor AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE i AS INTEGER NO-UNDO. */
/*    */
/*   FIND sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ipCompany */
/*                           AND sys-ctrl.name EQ 'Artios' NO-ERROR. */
/*   IF NOT AVAILABLE sys-ctrl THEN */
/*   DO: */
/*     MESSAGE 'No System Control Record for Artios Exists!' SKIP */
/*       'Unable to connect to CADCAM Database.' VIEW-AS ALERT-BOX. */
/*     RETURN. */
/*   END. */
/*    */
/*   /* Create the connection object for the link to SQL */ */
/*   CREATE 'ADODB.Connection' hConnection. */
/*   /* Create a recordset object ready to return the data */ */
/*   CREATE 'ADODB.RecordSet' hRecordSet. */
/*   /* Create a command object for sending the SQL statement */ */
/*   CREATE 'ADODB.Command' hCommand. */
/*   /* Change the below values as necessary */ */
/*   ASSIGN */
/*     odbcDSN = ENTRY(1,sys-ctrl.char-fld) /* The ODBC DSN */ */
/*     odbcServer = ENTRY(2,sys-ctrl.char-fld) /* The name of the server hosting the SQL DB and DSN */ */
/*     odbcUserID = '' /* The user id for access to the SQL Database */ */
/*     odbcPassword = ''. /* Password required by above user-id */ */
/*   /* Open up the connection to the ODBC Layer */ */
/*   hConnection:Open ('data source=' + odbcDSN + ';server=' + */
/*                      odbcServer,odbcUserID,odbcPassword,0) NO-ERROR. */
/*    */
/*   IF ERROR-STATUS:ERROR THEN */
/*   MESSAGE 'Error:' ERROR-STATUS:NUM-MESSAGES VIEW-AS ALERT-BOX. */
/*    */
/*   /* Check for connection errors */ */
/*   IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN */
/*   DO i = 1 TO ERROR-STATUS:NUM-MESSAGES: */
/*     MESSAGE ERROR-STATUS:GET-NUMBER(i) */
/*             ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX. */
/*   END. */
/*   ELSE */
/*   DO: */
/*     CREATE ttblCADCAM. */
/*     ttblCADCAM.die-no = ipDieNumber. */
/*     ASSIGN */
/*       odbcQuery = 'SELECT * FROM design, board WHERE board.boardid = design.boardid' */
/*       hCommand:ActiveConnection  = hConnection */
/*       hCommand:CommandText = odbcQuery */
/*       hCommand:CommandType = 1 /* adCmdText */ */
/*       hConnection:CursorLocation = 3 /* adUseClient */ */
/*       hRecordSet:CursorType = 3 /* adOpenStatic */ */
/*       hRecordSet = hCommand:Execute (OUTPUT odbcNull,'',32) */
/*       odbcRecCount = hRecordSet:RecordCount. */
/*     /* Have we returned any rows ? */ */
/*     If odbcRecCount GT 0 AND NOT odbcRecCount EQ ? THEN */
/*     DO: */
/*       hRecordSet:MoveFirst no-error. */
/*       DO WHILE odbcCursor LT odbcRecCount: */
/*         ttblCADCAM.cad-no = hRecordSet:Fields ('DESIGNNAME'):VALUE. */
/*         IF ttblCADCAM.cad-no EQ ipCadNumber THEN */
/*         DO: */
/*           ASSIGN */
/*             ttblCADCAM.board = hRecordSet:Fields ('BOARDCODE'):VALUE */
/*             ttblCADCAM.cal = hRecordSet:Fields ('CALIPER'):VALUE */
/*             ttblCADCAM.dep = hRecordSet:Fields ('DEPTH'):VALUE */
/*             ttblCADCAM.len = hRecordSet:Fields ('LENGTH'):VALUE */
/*             ttblCADCAM.lin-in = hRecordSet:Fields ('RULELENGTH'):VALUE */
/*             ttblCADCAM.style = '' /* hRecordSet:Fields ('STYLE'):VALUE */ */
/*             ttblCADCAM.t-len = hRecordSet:Fields ('BLANKLENGTH'):VALUE */
/*             ttblCADCAM.t-sqin = hRecordSet:Fields ('AREA'):VALUE */
/*             ttblCADCAM.t-wid = hRecordSet:Fields ('BLANKHEIGHT'):VALUE */
/*             ttblCADCAM.weight = hRecordSet:Fields ('BASISWEIGHT'):VALUE */
/*             ttblCADCAM.wid = hRecordSet:Fields ('WIDTH'):VALUE. */
/*           LEAVE. */
/*         END. */
/*         odbcCursor = odbcCursor + 1. */
/*         hRecordSet:MoveNext NO-ERROR. */
/*       END. /* retrieved a single data row */ */
/*     END. /* retrieved all data rows */ */
/*     ELSE odbcStatus = 'No records found.'. */
/*    */
/*     /* Close the ADO connection */ */
/*     hConnection:Close no-error. */
/*   END. /* The connection opened correctly */ */
/*    */
/*   /* Release the memory!! */ */
/*   RELEASE OBJECT hConnection NO-ERROR. */
/*   RELEASE OBJECT hCommand NO-ERROR. */
/*   RELEASE OBJECT hRecordSet NO-ERROR. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCADCAM2 D-Dialog 
PROCEDURE getCADCAM2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipCADNumber AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipDieNumber AS CHARACTER NO-UNDO. */
/*    */
/*   DEFINE VARIABLE i AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE hAccess AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE tables AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE hFile AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE hTable AS CHARACTER NO-UNDO. */
/*    */
/*   DEFINE VARIABLE lvBoardID AS INTEGER NO-UNDO. */
/*    */
/*   CREATE ttblCADCAM. */
/*   ASSIGN */
/*     ttblCADCAM.die-no = ipDieNumber */
/*     tables = 'Design,Board'. */
/*   CREATE 'Access.Application' hAccess CONNECT TO 'c:\fibre\dcenter.mdb'. */
/*   DO i = 1 TO NUM-ENTRIES(tables): */
/*     ASSIGN */
/*       hTable = CAPS(ENTRY(i,tables)) */
/*       hFile = 'c:\fibre\' + ENTRY(i,tables) + '.txt'. */
/*     hAccess:application:docmd:TransferText (2,,hTable,hFile,TRUE,). */
/*   END. */
/*   RELEASE OBJECT hAccess. */
/*   DO i = 1 TO NUM-ENTRIES(tables): */
/*     INPUT FROM VALUE('c:\fibre\' + ENTRY(i,tables) + '.txt') NO-ECHO. */
/*     IMPORT DELIMITER ',' ^. */
/*     REPEAT: */
/*       CASE ENTRY(i,tables): */
/*         WHEN 'Board' THEN */
/*         DO: */
/*           IMPORT DELIMITER ',' lvBoardID */
/*             ttblCADCAM.board */
/*             ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ */
/*             ttblCADCAM.weight. */
/*           IF ttblCADCAM.boardID EQ boardID THEN LEAVE. */
/*         END. */
/*         WHEN 'Design' THEN */
/*         DO: */
/*           IMPORT DELIMITER ',' */
/*             ttblCADCAM.cad-no */
/*             ^ ^ ^ ^ ^ */
/*             ttblCADCAM.boardID */
/*             ^ ^ ^ ^ ^ ^ */
/*             ttblCADCAM.len */
/*             ttblCADCAM.wid */
/*             ttblCADCAM.dep */
/*             ttblCADCAM.t-len */
/*             ttblCADCAM.t-wid */
/*             ttblCADCAM.t-sqin */
/*             ttblCADCAM.cal */
/*             ttblCADCAM.lin-in. */
/*           IF ttblCADCAM.cad-no EQ ipCadNumber THEN LEAVE. */
/*         END. */
/*       END CASE. */
/*     END. */
/*     INPUT CLOSE. */
/*   END. */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNumofCADForm D-Dialog 
PROCEDURE getNumofCADForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output param opNumofCADForm as int no-undo.
    
  def var cTMPCadFIle as cha no-undo.
  def var iExt as int no-undo.
  def var iRevExt as int no-undo.
  def var cCAD# as cha no-undo.
  
  empty temp-table tt-subdir.

  /* if artioscad-chr = "" then artioscad-chr = "c:\artios\asi\". */
/*   if substring(artioscad-chr,length(artioscad-chr),1) <> "/" and */
/*      substring(artioscad-chr,length(artioscad-chr),1) <> "\" then */
/*      artioscad-chr = artioscad-chr + "\". */

  
  
  
/*   if substring(cadPath,length(cadPath),1) <> "/" and                                                                  */
/*      substring(cadPath,length(cadPath),1) <> "\" then                                                                 */
/*      cadPath = cadPath + "\".                                                                                         */
/*                                                                                                                       */
/*   create tt-subDir.                                                                                                   */
/*   assign tt-SubDir.DirName = cadPath.                                                                                 */
/*                                                                                                                       */
/*   run getSubDirList (cadPAth).                                                                                        */
/*                                                                                                                       */
/*   if index(CadNumber,".ard") = 0 and                                                                                  */
/*      search(cadNumber + ".ard") = ? then do:                                                                          */
/*      /*  if artioscad-chr = "" then artioscad-chr = "c:\artios\asi\". */                                              */
/*      /*     if substring(artioscad-chr,length(artioscad-chr),1) <> "/" and */                                         */
/*      /*        substring(artioscad-chr,length(artioscad-chr),1) <> "\" then  artioscad-chr = artiosc ad-chr + "\". */ */
/*                                                                                                                       */
/*      for each tt-SubDir:                                                                                              */
/*        cTMPCadFile = tt-SubDir.DirName + CadNumber.                                                                   */
/*                                                                                                                       */
/*        do iExt = 1 to iProjectCount:                                                                                  */
/*           if search(cTmpCadFile + string(iExt,"99") + ".ARD" ) <> ? then do:                                          */
/*              assign opNumofCADForm = opNumofCADForm + 1                                                               */
/*                        cCAD# = CadNumber + string(iExt,"99").                                                         */
/*              /*cb-CADSeq:add-last(string(opNumOfCadForm,"99") + " " + cCAD#) in frame {&frame-name}.*/                */
/*           end.                                                                                                        */
/*           /* check revision file existing */                                                                          */
/*          do iRevExt = 65 to 90:  /* A - Z */                                                                          */
/*                                                                                                                       */
/*            if search(cTMPCadFile + string(iExt,"99") + chr(iRevExt) + ".ARD" ) <> ? or                                */
/*               search(cTMPCadFile + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ) <> ?                              */
/*             then do:                                                                                                  */
/*                assign opNumofCADForm = opNumofCADForm + 1                                                             */
/*                           cCAD# = CadNumber + string(iExt,"99") + chr(iRevExt).                                       */
/*                /*cb-CADSeq:add-last(string(opNumOfCadForm,"99") + " " + cCAD#).                 */                    */
/*             end.                                                                                                      */
/*          end.                                                                                                         */
/*        end.                                                                                                           */
/*      end.  /*for each */                                                                                              */
/*   end.  /* if search = ? */                                                                                           */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSubDirList D-Dialog 
PROCEDURE getSubDirList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcCurrentDirectory AS CHARACTER.

  def var cFileName as cha no-undo.
  def var cFilePath as cha no-undo.
  def var cAttrib as cha no-undo.

/* INPUT FROM OS-DIR(artioscad-chr).                     */
/* REPEAT:                                               */
/*      IMPORT cFileName cFilePath cAttrib.              */
/*      IF INDEX(cAttrib,"D") <> 0                       */
/*              AND NOT cFileName BEGINS "." THEN        */
/*      DO:                                              */
/*          CREATE tt-SubDir.                            */
/*          ASSIGN tt-SubDir.DirName = cFilePath + "\".  */
/*      END.                                             */
/* END.                                                  */

   INPUT FROM OS-DIR (ipcCurrentDirectory).
    
   REPEAT:
        IMPORT cFileName.
        IF cFileName = '.' OR cFileName = '..' OR cFileName = ? THEN NEXT.
        FILE-INFO:FILE-NAME = ipcCurrentDirectory + cFileName.
        IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN NEXT.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
            /*PUT UNFORMATTED FILE-INFO:FULL-PATHNAME SKIP.*/
            CREATE tt-subdir.
            ASSIGN tt-subdir.dirname = FILE-INFO:FULL-PATHNAME + "\" 
                   .
            RUN getSubDirList(INPUT FILE-INFO:FULL-PATHNAME + "\").
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-category D-Dialog 
PROCEDURE valid-category :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
   
      IF NOT CAN-FIND(FIRST fgcat WHERE
         fgcat.company EQ cocode AND
         fgcat.procat  EQ cCategory:SCREEN-VALUE) OR
         cCategory:SCREEN-VALUE EQ "" THEN DO:
         MESSAGE "Invalid Category." VIEW-AS ALERT-BOX ERROR.
         op-error = YES.
         APPLY "entry" TO cCategory.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style D-Dialog 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-handle AS WIDGET-HANDLE.
   DEFINE INPUT PARAMETER ip-check-blank AS LOG NO-UNDO.
   DEFINE OUTPUT parameter op-error AS LOG NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
      IF (ip-handle:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST style
                      WHERE style.company  EQ gcompany
                        AND style.style    EQ ip-handle:SCREEN-VALUE
                        AND style.industry EQ "2")) OR
          (ip-check-blank AND ip-handle:SCREEN-VALUE EQ "") THEN DO:
        MESSAGE "Invalid Style." VIEW-AS ALERT-BOX ERROR.
        op-error = YES.
        APPLY "entry" TO ip-handle.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

