
/*------------------------------------------------------------------------
    File        : ControlRep.p
    Purpose     : Style File
    Main File   : cerep\r-ctrl.w
    Syntax      :

    Description : Return a Dataset of Request For Control File

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttControlFileRep NO-UNDO
        FIELD control-file-rep AS CHAR.

    DEFINE DATASET dsControlFileRep FOR ttControlFileRep .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsControlFileRep.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


IF  prmUser      = ?        THEN ASSIGN     prmUser        = "".

    
    def var list-name as cha no-undo.
    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHAR NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.


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
 v-today = TODAY . 

    
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

  
    ASSIGN
        init-dir    = v-webrootpath
        vtextfile = "Control" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  


  IF prmAction = "Control" THEN DO:
    
      run run-report.

    
        CREATE ttControlFileRep.
        ASSIGN ttControlFileRep.control-file-rep = vtextfile.
   

  END.

  
    /*****************************************PROCEDURE run-report :*****************************************************/
 PROCEDURE run-report :

{sys/form/r-top.f}

def var choice as log  no-undo.
def var i      as int  no-undo.

def var head as ch format "x(78)" extent 5.

assign
 str-tit2 = "Cost Estimating - Control File"
 {sys/inc/ctrtext.i str-tit2 56}
 
 head[1]  = " ======  Estimating  Defaults  ====== "
 head[4]  = " =====  Mark Up Options  ====== "
 head[2]  = " ===== GS&A MARK UP PERCENTAGES ====="
 head[3]  = " ======= What If/Print Options ======= ".

{sys/ref/ce-ctrl.f}


/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .

assign list-name = tmp-dir + vtextfile . 

{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

display str-tit with frame r-top.
 
/*display skip(1).*/

for each ce-ctrl where (ce-ctrl.company = cocode and
       ce-ctrl.loc     = locode)  NO-LOCK with frame ce-ctrl:
  display skip(2).
  {sys/ref/ce-ctrl.v &format=56}
  down.
  if line-counter gt 35 then page.
end.
   
/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

end procedure.
