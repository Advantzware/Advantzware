/*------------------------------------------------------------------------
    File        : trns_fgbin_look.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTrnsFgBintgLook2 NO-UNDO 
    FIELD vtag             AS CHAR 
    FIELD vJobNo           AS CHARACTER
    FIELD vJob2            AS INT
    FIELD vLoc             AS CHARACTER
    FIELD vLocBin          AS CHAR
    FIELD vqty             AS INT 
    FIELD vcust            AS CHAR
    FIELD vLoc2            AS CHARACTER
    FIELD vLocBin2         AS CHAR
    FIELD vtag2            AS CHAR
    FIELD abc              AS CHAR.

DEFINE DATASET dsTrnsFgBintgLook FOR ttTrnsFgBintgLook2 .

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmitem          AS CHAR  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTrnsFgBintgLook.
   
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmitem      = ? THEN ASSIGN prmitem      = "".

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN cocode = prmComp.




if prmAction = "Select" then do:
    MESSAGE "tsting ".

  FOR EACH fg-bin WHERE fg-bin.company = prmComp 
                  AND ASI.fg-bin.i-no = prmitem
                  AND fg-bin.qty <> 0
      NO-LOCK: 
        
   
      CREATE ttTrnsFgBintgLook2.

        ASSIGN
         ttTrnsFgBintgLook2.vLoc2    = fg-bin.loc
         ttTrnsFgBintgLook2.vLocBin2 = fg-bin.loc-bin
         ttTrnsFgBintgLook2.vtag2    = fg-bin.tag
         ttTrnsFgBintgLook2.vtag     = fg-bin.tag
         ttTrnsFgBintgLook2.vLoc     = fg-bin.loc           
         ttTrnsFgBintgLook2.vLocBin  = fg-bin.loc-bin 
         ttTrnsFgBintgLook2.vJobNo   = fg-bin.job-no
         ttTrnsFgBintgLook2.vJob2    = fg-bin.job-no2
         ttTrnsFgBintgLook2.vcust    = fg-bin.cust-no.
 
 MESSAGE "tsting2  " ttTrnsFgBintgLook2.vLoc2.

  END. /*FOR EACH fg-bin*/

END.  /*ifif prmAction = "select"*/



    /******************Search***********************************/

 IF prmAction = "PoSearch" then do:
      if prmField = "Tag#"  then do:
          if prmCondition = "EQUAL" then do:
             FOR EACH fg-bin WHERE fg-bin.company = prmComp 
                  AND ASI.fg-bin.i-no = prmitem
                  AND fg-bin.qty <> 0
                  AND fg-bin.tag = prmText
                  NO-LOCK: 
        

                  CREATE ttTrnsFgBintgLook2.
                 
                  ASSIGN
                   ttTrnsFgBintgLook2.vLoc2    = fg-bin.loc
                   ttTrnsFgBintgLook2.vLocBin2 = fg-bin.loc-bin
                   ttTrnsFgBintgLook2.vtag2    = fg-bin.tag
                   ttTrnsFgBintgLook2.vtag     = fg-bin.tag
                   ttTrnsFgBintgLook2.vLoc     = fg-bin.loc           
                   ttTrnsFgBintgLook2.vLocBin  = fg-bin.loc-bin 
                   ttTrnsFgBintgLook2.vJobNo   = fg-bin.job-no
                   ttTrnsFgBintgLook2.vJob2    = fg-bin.job-no2
                   ttTrnsFgBintgLook2.vcust    = fg-bin.cust-no.

              END. /*FOR EACH fg-bin*/
          END.
     
 
 
 
         IF prmCondition = "BEGIN" then do:
             FOR EACH fg-bin WHERE fg-bin.company = prmComp 
                  AND ASI.fg-bin.i-no = prmitem
                  AND fg-bin.qty <> 0
                  AND fg-bin.tag BEGINS prmText
                  NO-LOCK: 
        

                  CREATE ttTrnsFgBintgLook2.
                 
                  ASSIGN
                   ttTrnsFgBintgLook2.vLoc2    = fg-bin.loc
                   ttTrnsFgBintgLook2.vLocBin2 = fg-bin.loc-bin
                   ttTrnsFgBintgLook2.vtag2    = fg-bin.tag
                   ttTrnsFgBintgLook2.vtag     = fg-bin.tag
                   ttTrnsFgBintgLook2.vLoc     = fg-bin.loc           
                   ttTrnsFgBintgLook2.vLocBin  = fg-bin.loc-bin 
                   ttTrnsFgBintgLook2.vJobNo   = fg-bin.job-no
                   ttTrnsFgBintgLook2.vJob2    = fg-bin.job-no2
                   ttTrnsFgBintgLook2.vcust    = fg-bin.cust-no.


              END. /*FOR EACH fg-bin*/
         END.                        
    END.  /* if prmField = state  */


 if prmField = "Item"  then do:
     if prmCondition = "EQUAL" then do:
         FOR EACH fg-bin WHERE fg-bin.company = prmComp 
                  AND ASI.fg-bin.i-no = prmitem
                  AND fg-bin.qty <> 0
                  AND fg-bin.i-no = prmText
                  NO-LOCK: 
        

                  CREATE ttTrnsFgBintgLook2.
                 
                  ASSIGN
                   ttTrnsFgBintgLook2.vLoc2    = fg-bin.loc
                   ttTrnsFgBintgLook2.vLocBin2 = fg-bin.loc-bin
                   ttTrnsFgBintgLook2.vtag2    = fg-bin.tag
                   ttTrnsFgBintgLook2.vtag     = fg-bin.tag
                   ttTrnsFgBintgLook2.vLoc     = fg-bin.loc           
                   ttTrnsFgBintgLook2.vLocBin  = fg-bin.loc-bin 
                   ttTrnsFgBintgLook2.vJobNo   = fg-bin.job-no
                   ttTrnsFgBintgLook2.vJob2    = fg-bin.job-no2
                   ttTrnsFgBintgLook2.vcust    = fg-bin.cust-no.


          END. /*FOR EACH fg-bin*/
     END.

     IF prmCondition = "BEGIN" then do:
         FOR EACH fg-bin WHERE fg-bin.company = prmComp 
                  AND ASI.fg-bin.i-no = prmitem
                  AND fg-bin.qty <> 0
                  AND fg-bin.i-no BEGINS prmText
                  NO-LOCK: 
        

                  CREATE ttTrnsFgBintgLook2.
                 
                  ASSIGN
                   ttTrnsFgBintgLook2.vLoc2    = fg-bin.loc
                   ttTrnsFgBintgLook2.vLocBin2 = fg-bin.loc-bin
                   ttTrnsFgBintgLook2.vtag2    = fg-bin.tag
                   ttTrnsFgBintgLook2.vtag     = fg-bin.tag
                   ttTrnsFgBintgLook2.vLoc     = fg-bin.loc           
                   ttTrnsFgBintgLook2.vLocBin  = fg-bin.loc-bin 
                   ttTrnsFgBintgLook2.vJobNo   = fg-bin.job-no
                   ttTrnsFgBintgLook2.vJob2    = fg-bin.job-no2
                   ttTrnsFgBintgLook2.vcust    = fg-bin.cust-no.

          END. /*FOR EACH fg-bin*/
     END.
 END.

END.  /* IF prmAction = search then do: */











