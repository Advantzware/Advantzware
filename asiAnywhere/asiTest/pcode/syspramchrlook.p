            /*------------------------------------------------------------------------
    File        : syspramchrlook.p
    Purpose     : 

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttSysParamCharValLook NO-UNDO 
    FIELD nameval  AS CHAR.

DEFINE DATASET dsSysParamCharValLook FOR ttSysParamCharValLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSysParamCharValLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF VAR g_company AS CHAR NO-UNDO.

ASSIGN g_company = prmComp.

{sys/ref/sys-ctrl.i}

     

def temp-table tt-sys-val field name like sys-ctrl.name
                          field name-val as cha.

def var li-cnt as int no-undo.
 
  if can-do(name-fld-list,"POEXPORT") then do:
     do li-cnt = 1 to num-entries(str-init[lookup("POEXPORT",name-fld-list)]):
        create tt-sys-val.
        assign tt-sys-val.name = "POEXPORT"
               tt-sys-val.name-val = entry(li-cnt, str-init[lookup("POEXPORT",name-fld-list)]).
     end.          
  end.


if prmAction <> "search" then do:
    FOR EACH tt-sys-val       WHERE true  NO-LOCK by tt-sys-val.name-val:
        create ttSysParamCharValLook.
            assign                                         
                
               ttSysParamCharValLook.nameval    =  tt-sys-val.name-val                         
               
                 .
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 






