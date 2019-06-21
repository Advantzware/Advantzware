




/*------------------------------------------------------------------------
    File        : CustPalletLook.p
    Purpose     : Pallet

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustPalletLook NO-UNDO 
    FIELD vmat AS CHARACTER
    FIELD vdscr AS CHARACTER
    FIELD vhjjkkkk AS CHAR
   .
                                           
    
DEFINE DATASET dsCustPalletLook FOR ttCustPalletLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmIndustry  AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustPalletLook.
       
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

if prmAction <> "search" then do:
       FOR EACH item WHERE  item.company = prmComp /*and  (item.industry = prmIndustry or prmIndustry = "")*/
           and item.mat-type = "D" NO-LOCK :
                 create ttCustPalletLook.
                 assign                                     
                    ttCustPalletLook.vmat = item.i-no
                    ttCustPalletLook.vdscr= item.i-name
                    

                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
     if prmField = "matcode"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH item WHERE  item.company = prmComp /* and  (item.industry = prmIndustry or prmIndustry = "")*/
                and item.mat-type = "D"  AND item.i-no = prmText NO-LOCK :
                 create ttCustPalletLook.
                 assign                                     
                     ttCustPalletLook.vmat  = item.i-no
                     ttCustPalletLook.vdscr = item.i-name  .
                 END.

              END. /*FOR EACH state*/
          
         IF prmCondition = "BEGIN" then do:
                  FOR EACH item WHERE  item.company = prmComp /*and  (item.industry = prmIndustry or prmIndustry = "")*/
                    and item.mat-type = "D"  AND item.i-no BEGINS prmText NO-LOCK :
                 assign                                     
                    ttCustPalletLook.vmat  = item.i-no
                    ttCustPalletLook.vdscr = item.i-name  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */




