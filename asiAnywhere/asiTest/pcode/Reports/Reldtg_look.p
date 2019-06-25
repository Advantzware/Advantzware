/*------------------------------------------------------------------------
    File        : Reldtg_look.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLabelMenuReturnTagLook NO-UNDO 
    FIELD vtag             AS CHAR  
    FIELD vPoNo           AS INT
    FIELD vQty            AS DEC
    FIELD vINo            AS CHARACTER
    FIELD vIName          AS CHARACTER
    FIELD vJobNo          AS CHARACTER
    FIELD vJob2           AS INT
    FIELD vLoc            AS CHARACTER
    FIELD vLocBin         AS CHAR
    FIELD ordno           AS INT
    FIELD vabc             AS CHAR
     .

DEFINE DATASET dsLabelMenuReturnTagLook FOR ttLabelMenuReturnTagLook .

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReTag       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsLabelMenuReturnTagLook.
   
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR lv-cost AS DEC DECIMALS 4 NO-UNDO.

DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR lv-loc LIKE fg-rctd.loc NO-UNDO.
DEF VAR lv-loc-bin LIKE fg-rctd.loc-bin NO-UNDO.
DEF VAR lv-qty-case LIKE fg-rctd.qty-case NO-UNDO.
DEF VAR lv-cases LIKE fg-rctd.cases NO-UNDO.
DEF VAR lv-cases-unit LIKE fg-rctd.cases-unit NO-UNDO.
DEF VAR lv-partial LIKE fg-rctd.partial NO-UNDO.
DEF VAR ll-casetag AS LOG NO-UNDO.
DEF VAR lv-do-what AS cha NO-UNDO.
DEF VAR lv-first-time AS LOG INIT YES NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.
DEF VAR v-bardir-chr AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmReTag     = ? THEN ASSIGN prmReTag     = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
/*{sys/inc/varasgn.i}*/
ASSIGN cocode = prmComp 
    /*locode = g_loc*/ .



DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
    DEF VAR period_pos AS INTEGER NO-UNDO.

    IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
       INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
       INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
    ELSE
    ASSIGN
      period_pos = INDEX(PROGRAM-NAME(1),".")
      v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
      v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

  


  if prmAction = "loadtg" then do:

    FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO 
                     and loadtag.is-case-tag = NO
                     AND loadtag.qty > 0  NO-LOCK BY loadtag.tag-no:

       

        
   CREATE ttLabelMenuReturnTagLook.

   ASSIGN
    ttLabelMenuReturnTagLook.vtag       = loadtag.tag-no
    ttLabelMenuReturnTagLook.vPoNo      = loadtag.po-no
    ttLabelMenuReturnTagLook.vJobNo     = loadtag.job-no 
    ttLabelMenuReturnTagLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuReturnTagLook.vINo       = loadtag.i-no 
    ttLabelMenuReturnTagLook.vIName     = loadtag.i-name
    ttLabelMenuReturnTagLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuReturnTagLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuReturnTagLook.ordno      = loadtag.ord-no
    ttLabelMenuReturnTagLook.vQty       = loadtag.qty
    .
    
    
   
   

  END. /* for each loadtag */



END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "tagSearch" then do:
    if prmField = "Tag#"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO
                and loadtag.is-case-tag = NO
                AND loadtag.qty > 0
                AND loadtag.tag-no = prmText NO-LOCK BY loadtag.tag-no:
   
   CREATE ttLabelMenuReturnTagLook.

  ASSIGN
    ttLabelMenuReturnTagLook.vtag        = loadtag.tag-no
    ttLabelMenuReturnTagLook.vPoNo      = loadtag.po-no
    ttLabelMenuReturnTagLook.vJobNo     = loadtag.job-no 
    ttLabelMenuReturnTagLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuReturnTagLook.vINo       = loadtag.i-no 
    ttLabelMenuReturnTagLook.vIName     = loadtag.i-name
    ttLabelMenuReturnTagLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuReturnTagLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuReturnTagLook.ordno      = loadtag.ord-no
    ttLabelMenuReturnTagLook.vQty    = loadtag.qty
    .

   
            END. /* for each loadtag */
        END.

        
        IF prmCondition = "BEGIN" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO 
                and loadtag.is-case-tag = NO
                AND loadtag.qty > 0
                AND loadtag.tag-no BEGINS prmText NO-LOCK BY loadtag.tag-no:

               ASSIGN
    ttLabelMenuReturnTagLook.vtag        = loadtag.tag-no
    ttLabelMenuReturnTagLook.vPoNo      = loadtag.po-no
    ttLabelMenuReturnTagLook.vJobNo     = loadtag.job-no 
    ttLabelMenuReturnTagLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuReturnTagLook.vINo       = loadtag.i-no 
    ttLabelMenuReturnTagLook.vIName     = loadtag.i-name
    ttLabelMenuReturnTagLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuReturnTagLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuReturnTagLook.ordno      = loadtag.ord-no
    ttLabelMenuReturnTagLook.vQty    = loadtag.qty
    .   
            END. /* for each loadtag */
        END.
     END.  /* if prmField = state  */


     if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH loadtag WHERE loadtag.company = prmComp 
                 and loadtag.item-type = NO  
                 and loadtag.is-case-tag = NO
                 AND loadtag.qty > 0 
                 AND loadtag.i-no EQ prmText NO-LOCK BY loadtag.tag-no:

                ASSIGN
    ttLabelMenuReturnTagLook.vtag        = loadtag.tag-no
    ttLabelMenuReturnTagLook.vPoNo      = loadtag.po-no
    ttLabelMenuReturnTagLook.vJobNo     = loadtag.job-no 
    ttLabelMenuReturnTagLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuReturnTagLook.vINo       = loadtag.i-no 
    ttLabelMenuReturnTagLook.vIName     = loadtag.i-name
    ttLabelMenuReturnTagLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuReturnTagLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuReturnTagLook.ordno      = loadtag.ord-no
    ttLabelMenuReturnTagLook.vQty    = loadtag.qty
    .   

             END. /* for each loadtag */              
         END. /*FOR EACH prmcondition*/

        IF prmCondition = "BEGIN" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO 
                and loadtag.is-case-tag = NO
                AND loadtag.qty > 0
                AND loadtag.i-no BEGINS prmText NO-LOCK:

                ASSIGN
    ttLabelMenuReturnTagLook.vtag        = loadtag.tag-no
    ttLabelMenuReturnTagLook.vPoNo      = loadtag.po-no
    ttLabelMenuReturnTagLook.vJobNo     = loadtag.job-no 
    ttLabelMenuReturnTagLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuReturnTagLook.vINo       = loadtag.i-no 
    ttLabelMenuReturnTagLook.vIName     = loadtag.i-name
    ttLabelMenuReturnTagLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuReturnTagLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuReturnTagLook.ordno      = loadtag.ord-no
    ttLabelMenuReturnTagLook.vQty    = loadtag.qty
    .

   

            END. /* for each loadtag */
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */


FOR EACH ttLabelMenuReturnTagLook  NO-LOCK:

    FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-bardir-chr = sys-ctrl.char-fld.


    FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ cocode
        AND job-hdr.job-no  GE ttLabelMenuReturnTagLook.vJobNo
        AND job-hdr.job-no  LE ttLabelMenuReturnTagLook.vJobNo
         NO-ERROR.

    IF AVAIL job-hdr THEN
       v-cust-no = job-hdr.cust-no.
    ELSE DO:
       FIND FIRST oe-ord WHERE 
            oe-ord.company EQ cocode AND
            oe-ord.ord-no  EQ INT(ttLabelMenuReturnTagLook.ordno)
            NO-LOCK NO-ERROR.

       IF AVAIL oe-ord THEN
          v-cust-no = oe-ord.cust-no.
     END.     

     IF v-cust-no NE "" THEN
        FIND FIRST reftable WHERE
             reftable.reftable EQ "cp-lab-p" AND
             reftable.company  EQ cocode AND
             reftable.loc      GE ttLabelMenuReturnTagLook.vINo AND
             reftable.loc      LE ttLabelMenuReturnTagLook.vINo AND
             reftable.CODE     EQ v-cust-no
             NO-LOCK NO-ERROR.

     IF AVAIL reftable AND reftable.dscr NE "" THEN
        ttLabelMenuReturnTagLook.vabc = (IF reftable.dscr <> "" THEN reftable.dscr ELSE v-bardir-chr).
     ELSE
        IF INT(ttLabelMenuReturnTagLook.ordno) NE 0 AND
           INT(ttLabelMenuReturnTagLook.ordno) NE 0 THEN
        DO:
           FIND FIRST oe-rel WHERE
                oe-rel.company EQ cocode AND
                oe-rel.i-no    GE ttLabelMenuReturnTagLook.vINo AND
                oe-rel.i-no    LE ttLabelMenuReturnTagLook.vINo AND
                oe-rel.ord-no  GE INT(ttLabelMenuReturnTagLook.ordno) AND
                oe-rel.ord-no  LE INT(ttLabelMenuReturnTagLook.ordno)
                NO-LOCK NO-ERROR.
           
           IF AVAIL oe-rel THEN 
              FIND FIRST shipto NO-LOCK 
               WHERE shipto.company EQ cocode 
                 AND shipto.cust-no EQ oe-rel.cust-no 
                 AND shipto.ship-id EQ oe-rel.ship-id 
               USE-INDEX ship-id NO-ERROR.
           ELSE
              FIND FIRST shipto NO-LOCK
               WHERE shipto.company EQ cocode 
                 AND shipto.cust-no EQ v-cust-no 
                 AND shipto.ship-id EQ v-cust-no
                  USE-INDEX ship-id NO-ERROR.
           
              IF AVAIL shipto THEN DO:
                 IF AVAIL oe-rel THEN
                    v-cust-no = oe-rel.cust-no.
           
                 FIND FIRST sys-ctrl-shipto NO-LOCK
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                            ttLabelMenuReturnTagLook.vabc = sys-ctrl.char-fld.
                          ELSE
                             ttLabelMenuReturnTagLook.vabc = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          ttLabelMenuReturnTagLook.vabc = sys-ctrl.char-fld.
                       ELSE
                          ttLabelMenuReturnTagLook.vabc = "".
                    END.
                 END.
              END.
        END.
        ELSE
        IF INT(ttLabelMenuReturnTagLook.ordno) EQ 0 AND
           INT(ttLabelMenuReturnTagLook.ordno) EQ 0 THEN
           DO:
              FIND FIRST shipto WHERE
                   shipto.company EQ cocode AND
                   shipto.cust-no EQ v-cust-no AND
                   shipto.ship-id EQ v-cust-no
                   NO-LOCK NO-ERROR.

              IF AVAIL shipto THEN DO:
                 
                 FIND FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company      EQ cocode AND
                      sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                      sys-ctrl-shipto.cust-vend    EQ YES AND
                      sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                      sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                      sys-ctrl-shipto.char-fld     NE ''
                      NO-LOCK NO-ERROR.

                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                   ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto WHERE
                         sys-ctrl-shipto.company      EQ cocode AND
                         sys-ctrl-shipto.NAME         EQ "BARDIR" AND 
                         sys-ctrl-shipto.cust-vend    EQ YES AND
                         sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                         sys-ctrl-shipto.char-fld     NE ''
                         NO-LOCK NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             ttLabelMenuReturnTagLook.vabc = sys-ctrl.char-fld.
                          ELSE
                             ttLabelMenuReturnTagLook.vabc = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       ttLabelMenuReturnTagLook.vabc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          ttLabelMenuReturnTagLook.vabc = sys-ctrl.char-fld.
                       ELSE
                          ttLabelMenuReturnTagLook.vabc = "".
                    END.
                 END.
              END.


           END. /*begin_ord-no and end_ord-no eq 0*/
END.

FOR EACH ttLabelMenuReturnTagLook NO-LOCK:
        ttLabelMenuReturnTagLook.vabc = REPLACE(ttLabelMenuReturnTagLook.vabc,'\','/').
END.

/**************************************************************************************/


