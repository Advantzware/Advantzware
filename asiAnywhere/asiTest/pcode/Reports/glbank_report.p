

/*------------------------------------------------------------------------
    File        : glbank_report.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttGLBankAccountReportUp NO-UNDO
        FIELD bnkrpt AS CHAR
        FIELD bnkext AS CHAR.

DEFINE DATASET dsGLBankAccountReportUp FOR ttGLBankAccountReportUp.
    DEFINE INPUT PARAMETER  prmUser         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbebnk        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegbnkdr     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendbnk       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendbnkdr     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmshwnote      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmshwselc      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmshwmisc      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmshwadd       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmshwphn       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmalnote       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmListOrder    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmReckey       AS CHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER cError          AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLBankAccountReportUp.

     IF prmUser       = ? THEN ASSIGN     prmUser        = "".   
     IF prmAction     = ? THEN ASSIGN     prmAction      = "". 
     IF prmbebnk      = ? THEN ASSIGN     prmbebnk       = "". 
     IF prmbegbnkdr   = ? THEN ASSIGN     prmbegbnkdr    = "". 
     IF prmendbnk     = ? THEN ASSIGN     prmendbnk      = "".  
     IF prmendbnkdr   = ? THEN ASSIGN     prmendbnkdr    = "".  
     IF prmshwnote    = ? THEN ASSIGN     prmshwnote     = "". 
     IF prmshwselc    = ? THEN ASSIGN     prmshwselc     = "". 
     IF prmshwmisc    = ? THEN ASSIGN     prmshwmisc     = "". 
     IF prmshwadd     = ? THEN ASSIGN     prmshwadd      = "". 
     IF prmshwphn     = ? THEN ASSIGN     prmshwphn      = "". 
     IF prmalnote     = ? THEN ASSIGN     prmalnote      = "". 
     IF prmOut        = ? THEN ASSIGN     prmOut         = "". 
     IF prmListOrder  = ? THEN ASSIGN     prmListOrder   = "".
     IF prmReckey     = ? THEN ASSIGN     prmReckey      = "".



DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE NEW SHARED VAR gcompany AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


assign
 cocode = prmComp 
 v-today = TODAY 
 g_company = cocode
 g_user    = prmUser 
 gcompany  = cocode.



FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .




&Scoped-define PROGNAME bank_.
&Scoped-define LISTORDER Bank,Bank Name
&Scoped-define WHERE-STATEMENT bank.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY bank-query FOR bank.
    

DEFINE VARIABLE begin_bank-code       AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_bank_bank-name  AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE end_bank-code         AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_bank_bank-name    AS CHARACTER FORMAT "X(30)" NO-UNDO.


DEFINE BUFFER bprgrms FOR prgrms.

DEFINE VARIABLE output-option AS INTEGER NO-UNDO.
DEFINE VARIABLE spooled AS LOGICAL NO-UNDO.
DEFINE VARIABLE spool-date AS DATE NO-UNDO.
DEFINE VARIABLE spool-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE spool-ampm AS CHARACTER NO-UNDO.

DEFINE VARIABLE orientation AS CHARACTER NO-UNDO.
DEFINE VARIABLE show-notes AS LOGICAL LABEL "Show Notes" NO-UNDO.
DEFINE VARIABLE notes-type AS INTEGER NO-UNDO.
DEFINE VARIABLE show-parameters AS LOGICAL NO-UNDO.
DEFINE VARIABLE show-misc-fields AS LOGICAL LABEL "Show Misc Fields" NO-UNDO.
DEFINE VARIABLE show-addresses AS LOGICAL LABEL "Show Addresses" NO-UNDO.
DEFINE VARIABLE show-phones AS LOGICAL LABEL "Show Phones" NO-UNDO.
DEFINE VARIABLE list-order AS INTEGER NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE misc-label AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE VARIABLE x-pos AS INTEGER NO-UNDO.
DEFINE VARIABLE y-pos AS INTEGER NO-UNDO.

DEFINE VARIABLE h_Viper AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_VReport AS WIDGET-HANDLE NO-UNDO.
   
   




  IF prmAction = "bnkrpt" THEN DO:

   ASSIGN
        begin_bank-code      = prmbebnk    
        begin_bank_bank-name = prmbegbnkdr 
        end_bank-code        = prmendbnk    
        end_bank_bank-name   = prmendbnkdr 
        list-order           = IF prmListOrder = "1" THEN 1 ELSE 2
        show-notes           = IF prmshwnote = "True" THEN TRUE ELSE FALSE
        show-parameters      = IF prmshwselc = "True" THEN TRUE ELSE FALSE
        show-misc-fields     = IF prmshwmisc = "True" THEN TRUE ELSE FALSE     
        show-addresses       = IF prmshwadd  = "True" THEN TRUE ELSE FALSE       
        show-phones          = IF prmshwphn = "True" THEN TRUE ELSE FALSE 
        notes-type           =  int(prmReckey) .
                    

        lines-per-page = 55 .


        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "BankList" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "BankList" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        run run-report. 
        

   
  CREATE ttGLBankAccountReportUp.
  
    ASSIGN ttGLBankAccountReportUp.bnkrpt = vTextFile .

  END.
/*****************************************************************************************/
  
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND bprgrms WHERE bprgrms.prgmname = "{&PROGNAME}" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bprgrms THEN
    RETURN. 

{methods/lstlogic/header.i}
{methods/lstlogic/footer.i}

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

    OUTPUT TO VALUE(list-name) PAGE-SIZE VALUE(lines-per-page) PAGED.

    DO:
        VIEW FRAME f-header.
        VIEW FRAME f-continued.
    END.

    IF show-parameters THEN
        DO:
        RUN show-parameters.
        END.
    
    RUN List-Logic.


        DO:
            HIDE FRAME f-continued NO-PAUSE.
            VIEW FRAME f-end-footer.
            OUTPUT CLOSE.
       END.

   
    
 END PROCEDURE.   /*end run report*/

PROCEDURE List-Logic:
    
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY bank-query FOR EACH bank NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        bank.bank-code GE begin_bank-code AND
        bank.bank-code LE end_bank-code.
    WHEN 2 THEN
    OPEN QUERY bank-query FOR EACH bank NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        bank.bank-name GE begin_bank_bank-name AND
        bank.bank-name LE end_bank_bank-name.
         
  END CASE.
  GET FIRST bank-query.
  DO WHILE AVAILABLE(bank)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
      
    {methods/lstlogic/custom/bank_.i}
    DOWN.
    GET NEXT bank-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_bank-code.
  IMPORT begin_bank_bank-name.
  IMPORT end_bank-code.
  IMPORT end_bank_bank-name.
END PROCEDURE.

PROCEDURE Show-Parameters:
  DEFINE VARIABLE notetype-descrip AS CHARACTER FORMAT "X(20)" NO-UNDO.

  ASSIGN
    notetype-descrip = IF notes-type = 1 THEN "(All)"
                  ELSE IF notes-type = 2 THEN "(Viewed Only)"
                  ELSE "(Not Viewed Only)"
    y-pos = 500.

  DO:
    DISPLAY SKIP(3)
      "SELECTED PARAMETERS:" TO 40 SKIP(1)
      ENTRY(list-order,"{&LISTORDER}") FORMAT "X(40)"
          LABEL "List Order" COLON 40 SKIP(1)
        WITH FRAME f1-parameters SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
    RUN Show-Selections.
    DISPLAY SKIP(1)
&IF "{&SHOWNOTES}" = "yes" &THEN
      show-notes COLON 40
      notetype-descrip WHEN show-notes NO-LABEL SKIP
&ENDIF
&IF "{&SHOWMISCFLDS}" = "yes" &THEN
      show-misc-fields COLON 40 SKIP
&ENDIF
&IF "{&SHOWADDRESSES}" = "yes" &THEN
      show-addresses COLON 40 SKIP
&ENDIF
&IF "{&SHOWPHONES}" = "yes" &THEN
      show-phones COLON 40 SKIP
&ENDIF
        WITH FRAME f2-parameters SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
    PAGE.
  END.
END PROCEDURE.



PROCEDURE Show-Selections:
     
  DISPLAY
    begin_bank-code COLON 40
    begin_bank_bank-name COLON 40
    end_bank-code COLON 40
    end_bank_bank-name COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.

