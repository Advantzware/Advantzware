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
DEFINE TEMP-TABLE ttTrnsFgBinLook NO-UNDO 
    FIELD vtag             AS CHAR 
    FIELD vINo             AS CHARACTER
    FIELD vIName           AS CHAR
    FIELD vJobNo           AS CHARACTER
    FIELD vJob2            AS INT
    FIELD vLoc             AS CHARACTER
    FIELD vLocBin          AS CHAR
    FIELD vqty             AS INT 
    FIELD vcust            AS CHAR
    FIELD vQtyCas          AS INT 
    FIELD extra            AS CHAR.

DEFINE DATASET dsTrnsFgBinLook FOR ttTrnsFgBinLook .

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTag          AS CHAR  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTrnsFgBinLook.
   
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmTag       = ? THEN ASSIGN prmTag       = "".

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

  FOR EACH fg-bin WHERE fg-bin.company = prmComp 
      NO-LOCK: 

   

     FIND FIRST itemfg WHERE itemfg.company = cocode AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        

      CREATE ttTrnsFgBinLook.

        ASSIGN
         ttTrnsFgBinLook.vINo    = fg-bin.i-no
         ttTrnsFgBinLook.vIName  = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
         ttTrnsFgBinLook.vJobNo  = fg-bin.job-no
         ttTrnsFgBinLook.vJob2   = fg-bin.job-no2
         ttTrnsFgBinLook.vLoc    = fg-bin.loc
         ttTrnsFgBinLook.vLocBin = fg-bin.loc-bin
         ttTrnsFgBinLook.vtag    = fg-bin.tag
         ttTrnsFgBinLook.vcust   = fg-bin.cust-no
         ttTrnsFgBinLook.vQtyCas = fg-bin.case-count   .


  END. /*FOR EACH fg-bin*/

END.  /*ifif prmAction = "select"*/



    /******************Search***********************************/

 IF prmAction = "PoSearch" then do:
      if prmField = "Tag#"  then do:
          if prmCondition = "EQUAL" then do:
              FOR EACH fg-bin WHERE fg-bin.company = prmComp
                              AND   fg-bin.tag = prmText
                  NO-LOCK:     
                  
                  FIND FIRST itemfg WHERE itemfg.company = cocode 
                                    AND itemfg.i-no = fg-bin.i-no 
                                    NO-LOCK NO-ERROR.


                  CREATE ttTrnsFgBinLook.
                  
                  ASSIGN
                      ttTrnsFgBinLook.vINo    = fg-bin.i-no
                      ttTrnsFgBinLook.vIName  = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
                      ttTrnsFgBinLook.vJobNo  = fg-bin.job-no
                      ttTrnsFgBinLook.vJob2   = fg-bin.job-no2
                      ttTrnsFgBinLook.vLoc    = fg-bin.loc
                      ttTrnsFgBinLook.vLocBin = fg-bin.loc-bin
                      ttTrnsFgBinLook.vtag    = fg-bin.tag
                      ttTrnsFgBinLook.vcust   = fg-bin.cust-no
                      ttTrnsFgBinLook.vQtyCas = fg-bin.case-count   .


             END. /*FOR EACH fg-bin*/

              

          END.
      END.
 
 
 
         IF prmCondition = "BEGIN" then do:
             FOR EACH fg-bin WHERE fg-bin.company = prmComp
                              AND   fg-bin.tag BEGINS prmText
                  NO-LOCK:     
                  
                  FIND FIRST itemfg WHERE itemfg.company = cocode 
                                    AND itemfg.i-no = fg-bin.i-no 
                                    NO-LOCK NO-ERROR.


                  CREATE ttTrnsFgBinLook.
                  
                  ASSIGN
                      ttTrnsFgBinLook.vINo    = fg-bin.i-no
                      ttTrnsFgBinLook.vIName  = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
                      ttTrnsFgBinLook.vJobNo  = fg-bin.job-no
                      ttTrnsFgBinLook.vJob2   = fg-bin.job-no2
                      ttTrnsFgBinLook.vLoc    = fg-bin.loc
                      ttTrnsFgBinLook.vLocBin = fg-bin.loc-bin
                      ttTrnsFgBinLook.vtag    = fg-bin.tag
                      ttTrnsFgBinLook.vcust   = fg-bin.cust-no
                      ttTrnsFgBinLook.vQtyCas = fg-bin.case-count   .


             END. /*FOR EACH fg-bin*/
         END.                        
END.  /* if prmField = state  */


 if prmField = "Item"  then do:
     if prmCondition = "EQUAL" then do:
         FOR EACH fg-bin WHERE fg-bin.company = prmComp
                         AND   fg-bin.i-no = prmText
                  NO-LOCK:     
                  
                  FIND FIRST itemfg WHERE itemfg.company = cocode 
                                    AND itemfg.i-no = fg-bin.i-no 
                                    NO-LOCK NO-ERROR.


                  CREATE ttTrnsFgBinLook.
                  
                  ASSIGN
                      ttTrnsFgBinLook.vINo    = fg-bin.i-no
                      ttTrnsFgBinLook.vIName  = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
                      ttTrnsFgBinLook.vJobNo  = fg-bin.job-no
                      ttTrnsFgBinLook.vJob2   = fg-bin.job-no2
                      ttTrnsFgBinLook.vLoc    = fg-bin.loc
                      ttTrnsFgBinLook.vLocBin = fg-bin.loc-bin
                      ttTrnsFgBinLook.vtag    = fg-bin.tag
                      ttTrnsFgBinLook.vcust   = fg-bin.cust-no
                      ttTrnsFgBinLook.vQtyCas = fg-bin.case-count   .


             END. /*FOR EACH fg-bin*/
     END.

     IF prmCondition = "BEGIN" then do:
         FOR EACH fg-bin WHERE fg-bin.company = prmComp
                         AND   fg-bin.i-no BEGINS prmText
                  NO-LOCK:     
                  
                  FIND FIRST itemfg WHERE itemfg.company = cocode 
                                    AND itemfg.i-no = fg-bin.i-no 
                                    NO-LOCK NO-ERROR.


                  CREATE ttTrnsFgBinLook.
                  
                  ASSIGN
                      ttTrnsFgBinLook.vINo    = fg-bin.i-no
                      ttTrnsFgBinLook.vIName  = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
                      ttTrnsFgBinLook.vJobNo  = fg-bin.job-no
                      ttTrnsFgBinLook.vJob2   = fg-bin.job-no2
                      ttTrnsFgBinLook.vLoc    = fg-bin.loc
                      ttTrnsFgBinLook.vLocBin = fg-bin.loc-bin
                      ttTrnsFgBinLook.vtag    = fg-bin.tag
                      ttTrnsFgBinLook.vcust   = fg-bin.cust-no
                      ttTrnsFgBinLook.vQtyCas = fg-bin.case-count   .


          END. /*FOR EACH fg-bin*/
     END.

END.  /* IF prmAction = search then do: */











