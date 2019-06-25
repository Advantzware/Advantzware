



/*------------------------------------------------------------------------
    File        : CustBinLook.p
    Purpose     : ZipCode

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustBinLook NO-UNDO 
    FIELD vjob         AS CHARACTER
    FIELD v#           AS CHARACTER
    FIELD vloc         AS CHARACTER
    FIELD vbinloc      AS CHARACTER
    FIELD vtag         AS CHARACTER
    FIELD vCustomer    AS INTEGER
    FIELD vqty         AS DECIMAL
    FIELD hdhjdjd      AS CHAR
    .
                                           
    
DEFINE DATASET dsCustBinLook FOR ttCustBinLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLoc       AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustBinLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField      = "".
IF prmLoc       = ? THEN ASSIGN prmLoc        = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
        
    FOR EACH fg-bin WHERE  fg-bin.company = prmComp AND fg-bin.i-no = "" 
        AND (fg-bin.loc = prmLoc or prmLoc = "") NO-LOCK :
        create ttCustBinLook.
        assign                                     
            ttCustBinLook.vjob        = fg-bin.job-no
            ttCustBinLook.v#          = fg-bin.i-no
            ttCustBinLook.vloc        = fg-bin.loc 
            ttCustBinLook.vbinloc     = fg-bin.loc-bin
            ttCustBinLook.vtag        = fg-bin.tag 
            ttCustBinLook.vCustomer   = fg-bin.job-no2
            ttCustBinLook.vqty        = fg-bin.qty 
            .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
     if prmField = "Binloc"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH fg-bin WHERE  fg-bin.company = prmComp AND fg-bin.i-no = "" 
                 AND (fg-bin.loc = prmLoc or prmLoc = "") AND fg-bin.loc-bin = prmText NO-LOCK :
                 create ttCustBinLook.
                 assign                                     
                    ttCustBinLook.vjob        = fg-bin.job-no
                    ttCustBinLook.v#          = fg-bin.i-no
                    ttCustBinLook.vloc        = fg-bin.loc
                    ttCustBinLook.vbinloc     = fg-bin.loc-bin
                    ttCustBinLook.vtag        = fg-bin.tag 
                    ttCustBinLook.vCustomer   = fg-bin.job-no2
                    ttCustBinLook.vqty        = fg-bin.qty
 .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH fg-bin WHERE  fg-bin.company = prmComp AND fg-bin.i-no = "" 
                 AND (fg-bin.loc = prmLoc or prmLoc = "") AND fg-bin.loc-bin BEGINS prmText NO-LOCK :
                      create ttCustBinLook.
                 assign                                     
                    ttCustBinLook.vjob        = fg-bin.job-no
                    ttCustBinLook.v#          = fg-bin.i-no
                    ttCustBinLook.vloc        = fg-bin.loc
                    ttCustBinLook.vbinloc     = fg-bin.loc-bin
                    ttCustBinLook.vtag        = fg-bin.tag 
                    ttCustBinLook.vCustomer   = fg-bin.job-no2
                    ttCustBinLook.vqty        = fg-bin.qty
  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */


