



/*------------------------------------------------------------------------
    File        : BinLook.p
    Purpose     : ZipCode

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBinLook NO-UNDO 
    FIELD vjob         AS CHARACTER
    FIELD v#           AS CHARACTER
    FIELD vloc         AS CHARACTER
    FIELD vbinloc      AS CHARACTER
    FIELD vtag         AS CHARACTER
    FIELD vCustomer    AS INTEGER
    FIELD vqty         AS DECIMAL
    .
                                           
    
DEFINE DATASET dsBinLook FOR ttBinLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBinLook.
       
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
        
    FOR EACH fg-bin NO-LOCK BY fg-bin.loc-bin:
        FIND FIRST ttBinLook WHERE ttBinLook.vbinloc = fg-bin.loc-bin NO-LOCK NO-ERROR.
        IF AVAIL ttBinLook THEN  NEXT.
        IF fg-bin.job-no <> "" THEN
                 create ttBinLook.
                 assign                                     
                    ttBinLook.vjob        = fg-bin.job-no
                    ttBinLook.v#          = fg-bin.i-no
                    ttBinLook.vloc        = fg-bin.loc 
                    ttBinLook.vbinloc     = fg-bin.loc-bin
                    ttBinLook.vtag        = fg-bin.tag 
                    ttBinLook.vCustomer   = fg-bin.job-no2
                    ttBinLook.vqty        = fg-bin.qty 

                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
     if prmField = "Binloc"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH fg-bin WHERE  fg-bin.loc-bin = prmText  NO-LOCK :
                 FIND FIRST ttBinLook WHERE ttBinLook.vbinloc = fg-bin.loc-bin NO-LOCK NO-ERROR.
                 IF AVAIL ttBinLook THEN  NEXT.
                 create ttBinLook.
                 assign                                     
                    ttBinLook.vjob        = fg-bin.job-no
                    ttBinLook.v#          = fg-bin.i-no
                    ttBinLook.vloc        = fg-bin.loc
                    ttBinLook.vbinloc     = fg-bin.loc-bin
                    ttBinLook.vtag        = fg-bin.tag 
                    ttBinLook.vCustomer   = fg-bin.job-no2
                    ttBinLook.vqty        = fg-bin.qty
 .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH fg-bin WHERE fg-bin.loc-bin BEGINS prmText NO-LOCK :
                      FIND FIRST ttBinLook WHERE ttBinLook.vbinloc = fg-bin.loc-bin NO-LOCK NO-ERROR.
                      IF AVAIL ttBinLook THEN  NEXT.
                      create ttBinLook.
                 assign                                     
                    ttBinLook.vjob        = fg-bin.job-no
                    ttBinLook.v#          = fg-bin.i-no
                    ttBinLook.vloc        = fg-bin.loc
                    ttBinLook.vbinloc     = fg-bin.loc-bin
                    ttBinLook.vtag        = fg-bin.tag 
                    ttBinLook.vCustomer   = fg-bin.job-no2
                    ttBinLook.vqty        = fg-bin.qty
  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */


