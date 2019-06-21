/*------------------------------------------------------------------------
    File        : AccountLook.p
    Purpose     : Account No

    Syntax      :

    Description : Return a Dataset of all customer a/c no

    Author(s)   : 
    Created     : oct 14 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttAccountNoLook NO-UNDO 
    FIELD actno       AS CHAR 
    FIELD dscr        AS CHAR
    FIELD acc-type    AS CHAR     .

DEFINE DATASET dsAccountNoLook FOR ttAccountNoLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmjournal   AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAccountNoLook.
       
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

IF prmjournal = "journal" THEN DO:

    if prmAction <> "search" then do:
        FOR EACH account WHERE  account.company = prmComp
            AND account.TYPE NE "T"  NO-LOCK :
            create ttAccountNoLook.
                assign                                        
                   ttAccountNoLook.actno     =  account.actnum
                   ttAccountNoLook.dscr      =  account.dscr
                   ttAccountNoLook.acc-type  =  account.TYPE   .
        END.	 /* FOR EACH item */     

    END.  /*if prmAction <> "search" then do*/ 


    IF prmAction = "search" then do:

        if prmField = "Account"  then do:
            if prmCondition = "EQUAL" then do:
                  FOR EACH account WHERE  account.company = prmComp AND account.TYPE NE "T" 
                       AND account.actnum = prmText  NO-LOCK :
                      create ttAccountNoLook.
                      assign                                        
                          ttAccountNoLook.actno     =  account.actnum
                          ttAccountNoLook.dscr      =  account.dscr
                          ttAccountNoLook.acc-type  =  account.TYPE   .
                      END.	 /* FOR EACH account */   
              END. /*if prmCondition = EQUAL */
            IF prmCondition = "BEGIN" then do:
                 FOR EACH account WHERE  account.company = prmComp AND account.TYPE NE "T" 
                      AND account.actnum BEGINS prmText  NO-LOCK :
                      create ttAccountNoLook.
                      assign                                        
                          ttAccountNoLook.actno     =  account.actnum
                          ttAccountNoLook.dscr      =  account.dscr
                          ttAccountNoLook.acc-type  =  account.TYPE   .
                      END.	 /* FOR EACH account */ 
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = est  */
           IF prmField = "Dscr" then do:
             if prmCondition = "EQUAL" then do:
                 FOR EACH account WHERE  account.company = prmComp AND account.TYPE NE "T" 
                      AND account.dscr = prmText  NO-LOCK :
                      create ttAccountNoLook.
                      assign                                        
                          ttAccountNoLook.actno     =  account.actnum
                          ttAccountNoLook.dscr      =  account.dscr
                          ttAccountNoLook.acc-type  =  account.TYPE   .
                      END.	 /* FOR EACH account */ 
             END. /*if prmCondition = EQUAL*/
             IF prmCondition = "BEGIN" then do:
                  FOR EACH account WHERE  account.company = prmComp AND account.TYPE NE "T" 
                       AND account.dscr BEGINS prmText  NO-LOCK :
                      create ttAccountNoLook.
                      assign                                        
                          ttAccountNoLook.actno     =  account.actnum
                          ttAccountNoLook.dscr      =  account.dscr
                          ttAccountNoLook.acc-type  =  account.TYPE   .
                      END.	 /* FOR EACH account */ 
             END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = i-no */

    END.  /* IF prmAction = search then do: */

END.

ELSE do:
if prmAction <> "search" then do:
    FOR EACH account WHERE  account.company = prmComp  NO-LOCK :
        create ttAccountNoLook.
            assign                                        
               ttAccountNoLook.actno     =  account.actnum
               ttAccountNoLook.dscr      =  account.dscr
               ttAccountNoLook.acc-type  =  account.TYPE   .
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    
    if prmField = "Account"  then do:
        if prmCondition = "EQUAL" then do:
              FOR EACH account WHERE  account.company = prmComp AND account.actnum = prmText  NO-LOCK :
                  create ttAccountNoLook.
                  assign                                        
                      ttAccountNoLook.actno     =  account.actnum
                      ttAccountNoLook.dscr      =  account.dscr
                      ttAccountNoLook.acc-type  =  account.TYPE   .
                  END.	 /* FOR EACH account */   
          END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
             FOR EACH account WHERE  account.company = prmComp AND account.actnum BEGINS prmText  NO-LOCK :
                  create ttAccountNoLook.
                  assign                                        
                      ttAccountNoLook.actno     =  account.actnum
                      ttAccountNoLook.dscr      =  account.dscr
                      ttAccountNoLook.acc-type  =  account.TYPE   .
                  END.	 /* FOR EACH account */ 
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "Dscr" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH account WHERE  account.company = prmComp AND account.dscr = prmText  NO-LOCK :
                  create ttAccountNoLook.
                  assign                                        
                      ttAccountNoLook.actno     =  account.actnum
                      ttAccountNoLook.dscr      =  account.dscr
                      ttAccountNoLook.acc-type  =  account.TYPE   .
                  END.	 /* FOR EACH account */ 
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
              FOR EACH account WHERE  account.company = prmComp AND account.dscr BEGINS prmText  NO-LOCK :
                  create ttAccountNoLook.
                  assign                                        
                      ttAccountNoLook.actno     =  account.actnum
                      ttAccountNoLook.dscr      =  account.dscr
                      ttAccountNoLook.acc-type  =  account.TYPE   .
                  END.	 /* FOR EACH account */ 
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = i-no */
     
END.  /* IF prmAction = search then do: */



END.
