

/*------------------------------------------------------------------------
    File         : DeptLook.p
    Purpose     :  Departments Lookup

    Syntax      :

    Description : Return a Dataset of all departments

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttDepartmentLook NO-UNDO 
    FIELD vDeptCode      AS CHARACTER
    FIELD vDeptDscr      AS CHARACTER
    FIELD vDeptFc        AS INTEGER
    .
                                           
    
DEFINE DATASET dsDepartmentLook FOR ttDepartmentLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDepartmentLook.


IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField      = ? THEN ASSIGN prmField      = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH dept NO-LOCK:
       create ttDepartmentLook.
            assign                                         
                ttDepartmentLook.vDeptCode = dept.code
                ttDepartmentLook.vDeptDscr = dept.dscr 
                ttDepartmentLook.vDeptFc   = dept.fc .                
    END.	 /* FOR EACH dept */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "code" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH dept WHERE dept.code = prmText no-lock:
                create ttDepartmentLook.
                assign                                         
                    ttDepartmentLook.vDeptCode = dept.code
                    ttDepartmentLook.vDeptDscr = dept.dscr 
                    ttDepartmentLook.vDeptFc   = dept.fc .               
            END.	 /* FOR EACH dept */                      
        END.  /*IF prmCondition = EQUAL*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH dept where dept.code BEGINS prmText no-lock:
                create ttDepartmentLook.
                assign                 
                    ttDepartmentLook.vDeptCode = dept.code
                    ttDepartmentLook.vDeptDscr = dept.dscr 
                    ttDepartmentLook.vDeptFc   = dept.fc . 
            END.  /*FOR EACH dept where */         
        END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = code*/     
   

    IF prmField = "description" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH dept WHERE dept.dscr = prmText no-lock:
                create ttDepartmentLook.
                assign                                         
                    ttDepartmentLook.vDeptCode = dept.code
                    ttDepartmentLook.vDeptDscr = dept.dscr 
                    ttDepartmentLook.vDeptFc   = dept.fc .               
            END. /*FOR EACH dept where */
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH dept where dept.dscr BEGINS prmText no-lock:
                create ttDepartmentLook.
                assign                 
                    ttDepartmentLook.vDeptCode = dept.code
                    ttDepartmentLook.vDeptDscr = dept.dscr 
                    ttDepartmentLook.vDeptFc   = dept.fc . 
            END.  /*FOR EACH dept where */ 
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = description */

    if prmField = "seq"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH dept WHERE dept.fc = integer(prmText) no-lock:
                create ttDepartmentLook.
                assign                                         
                    ttDepartmentLook.vDeptCode = dept.code
                    ttDepartmentLook.vDeptDscr = dept.dscr 
                    ttDepartmentLook.vDeptFc   = dept.fc .               
            END. /*FOR EACH dept where */
        END. /*if prmCondition = EQUAL */    
    end.  /* if prmField = seq  */

END.  /* IF prmAction = search then do: */

