/*------------------------------------------------------------------------
    File        : loadtg_look.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLabelMenuLoadTgLook NO-UNDO 
    FIELD tag             AS CHAR  
    FIELD vPoNo            AS INT
    FIELD vOrdQty          AS DEC
    FIELD vINo             AS CHARACTER
    FIELD vIName           AS CHARACTER
    FIELD vJobNo           AS CHARACTER
    FIELD vJob2            AS INT
    FIELD vLoc             AS CHARACTER
    FIELD vLocBin          AS CHAR
    FIELD ordno           AS INT
    FIELD qtycase         AS INT
    FIELD plltcnt         AS INT
    FIELD abc             AS CHAR
     .

DEFINE DATASET dsLabelMenuLoadTgLook FOR ttLabelMenuLoadTgLook .

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmitmtype      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcurval       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsLabelMenuLoadTgLook.
   
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
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmitmtype   = ? THEN ASSIGN prmitmtype   = "".
IF prmcurval    = ? THEN ASSIGN prmcurval    = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = prmComp .



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

    FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ prmComp
        AND sys-ctrl.name    EQ "CASETAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = prmComp
     sys-ctrl.name     = "CASETAG"
     sys-ctrl.descrip  = "Case Label Format?   Use Case Label as LoadTag?".
  END.
  ll-casetag = sys-ctrl.log-fld.
  
  


  if prmAction = "loadtg" then do:

    FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO 
                     and loadtag.is-case-tag NO-LOCK BY loadtag.tag-no:

       

        
   CREATE ttLabelMenuLoadTgLook.

   ASSIGN
    ttLabelMenuLoadTgLook.tag        = loadtag.tag-no
    ttLabelMenuLoadTgLook.vPoNo      = loadtag.po-no
    ttLabelMenuLoadTgLook.vJobNo     = loadtag.job-no 
    ttLabelMenuLoadTgLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuLoadTgLook.vINo       = loadtag.i-no 
    ttLabelMenuLoadTgLook.vIName     = loadtag.i-name
    ttLabelMenuLoadTgLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuLoadTgLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuLoadTgLook.ordno      = loadtag.ord-no
    ttLabelMenuLoadTgLook.vOrdQty    = loadtag.qty
    ttLabelMenuLoadTgLook.plltcnt    = loadtag.pallet-count
    .
    
    
   
   

  END. /* for each loadtag */



END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "tagSearch" then do:
    if prmField = "Tag#"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO
                AND loadtag.tag-no = prmText
                and loadtag.is-case-tag NO-LOCK BY loadtag.tag-no:
   
   CREATE ttLabelMenuLoadTgLook.

  ASSIGN
    ttLabelMenuLoadTgLook.tag        = loadtag.tag-no
    ttLabelMenuLoadTgLook.vPoNo      = loadtag.po-no
    ttLabelMenuLoadTgLook.vJobNo     = loadtag.job-no 
    ttLabelMenuLoadTgLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuLoadTgLook.vINo       = loadtag.i-no 
    ttLabelMenuLoadTgLook.vIName     = loadtag.i-name
    ttLabelMenuLoadTgLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuLoadTgLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuLoadTgLook.ordno      = loadtag.ord-no
    ttLabelMenuLoadTgLook.vOrdQty    = loadtag.qty
    ttLabelMenuLoadTgLook.plltcnt    = loadtag.pallet-count
    .

   
            END. /* for each loadtag */
        END.

        
        IF prmCondition = "BEGIN" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO 
                AND loadtag.tag-no BEGINS prmText
                and loadtag.is-case-tag NO-LOCK BY loadtag.tag-no:

               ASSIGN
    ttLabelMenuLoadTgLook.tag        = loadtag.tag-no
    ttLabelMenuLoadTgLook.vPoNo      = loadtag.po-no
    ttLabelMenuLoadTgLook.vJobNo     = loadtag.job-no 
    ttLabelMenuLoadTgLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuLoadTgLook.vINo       = loadtag.i-no 
    ttLabelMenuLoadTgLook.vIName     = loadtag.i-name
    ttLabelMenuLoadTgLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuLoadTgLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuLoadTgLook.ordno      = loadtag.ord-no
    ttLabelMenuLoadTgLook.vOrdQty    = loadtag.qty
    ttLabelMenuLoadTgLook.plltcnt    = loadtag.pallet-count
    .   
            END. /* for each loadtag */
        END.
     END.  /* if prmField = state  */


     if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH loadtag WHERE loadtag.company = prmComp 
                 and loadtag.item-type = NO 
                 AND loadtag.i-no EQ prmText
                 and loadtag.is-case-tag  NO-LOCK BY loadtag.tag-no:

                ASSIGN
    ttLabelMenuLoadTgLook.tag        = loadtag.tag-no
    ttLabelMenuLoadTgLook.vPoNo      = loadtag.po-no
    ttLabelMenuLoadTgLook.vJobNo     = loadtag.job-no 
    ttLabelMenuLoadTgLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuLoadTgLook.vINo       = loadtag.i-no 
    ttLabelMenuLoadTgLook.vIName     = loadtag.i-name
    ttLabelMenuLoadTgLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuLoadTgLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuLoadTgLook.ordno      = loadtag.ord-no
    ttLabelMenuLoadTgLook.vOrdQty    = loadtag.qty
    ttLabelMenuLoadTgLook.plltcnt    = loadtag.pallet-count
    .   

             END. /* for each loadtag */              
         END. /*FOR EACH prmcondition*/

        IF prmCondition = "BEGIN" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO 
                AND loadtag.i-no BEGINS prmText
                and loadtag.is-case-tag  NO-LOCK BY loadtag.tag-no:

                ASSIGN
    ttLabelMenuLoadTgLook.tag        = loadtag.tag-no
    ttLabelMenuLoadTgLook.vPoNo      = loadtag.po-no
    ttLabelMenuLoadTgLook.vJobNo     = loadtag.job-no 
    ttLabelMenuLoadTgLook.vJob2      = int(loadtag.job-no2)
    ttLabelMenuLoadTgLook.vINo       = loadtag.i-no 
    ttLabelMenuLoadTgLook.vIName     = loadtag.i-name
    ttLabelMenuLoadTgLook.vLoc       = STRING(loadtag.loc)
    ttLabelMenuLoadTgLook.vLocBin    = string(loadtag.loc-bin)
    ttLabelMenuLoadTgLook.ordno      = loadtag.ord-no
    ttLabelMenuLoadTgLook.vOrdQty    = loadtag.qty
    ttLabelMenuLoadTgLook.plltcnt    = loadtag.pallet-count
    .

   

            END. /* for each loadtag */
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */


FOR EACH ttLabelMenuLoadTgLook NO-LOCK:

     FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-bardir-chr = sys-ctrl.char-fld.


    FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ cocode
        AND job-hdr.job-no  GE ttLabelMenuLoadTgLook.vJobNo
        AND job-hdr.job-no  LE ttLabelMenuLoadTgLook.vJobNo
         NO-ERROR.

    IF AVAIL job-hdr THEN
       v-cust-no = job-hdr.cust-no.
    ELSE DO:
       FIND FIRST oe-ord WHERE 
            oe-ord.company EQ cocode AND
            oe-ord.ord-no  EQ INT(ttLabelMenuLoadTgLook.ordno)
            NO-LOCK NO-ERROR.

       IF AVAIL oe-ord THEN
          v-cust-no = oe-ord.cust-no.
     END.     

     IF v-cust-no NE "" THEN
        FIND FIRST reftable WHERE
             reftable.reftable EQ "cp-lab-p" AND
             reftable.company  EQ cocode AND
             reftable.loc      GE ttLabelMenuLoadTgLook.vINo AND
             reftable.loc      LE ttLabelMenuLoadTgLook.vINo AND
             reftable.CODE     EQ v-cust-no
             NO-LOCK NO-ERROR.

     IF AVAIL reftable AND reftable.dscr NE "" THEN
        ttLabelMenuLoadTgLook.abc = (IF reftable.dscr <> "" THEN reftable.dscr ELSE v-bardir-chr).
     ELSE
        IF INT(ttLabelMenuLoadTgLook.ordno) NE 0 AND
           INT(ttLabelMenuLoadTgLook.ordno) NE 0 THEN
        DO:
           FIND FIRST oe-rel WHERE
                oe-rel.company EQ cocode AND
                oe-rel.i-no    GE ttLabelMenuLoadTgLook.vINo AND
                oe-rel.i-no    LE ttLabelMenuLoadTgLook.vINo AND
                oe-rel.ord-no  GE INT(ttLabelMenuLoadTgLook.ordno) AND
                oe-rel.ord-no  LE INT(ttLabelMenuLoadTgLook.ordno)
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
                    ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             ttLabelMenuLoadTgLook.abc = sys-ctrl.char-fld.
                          ELSE
                             ttLabelMenuLoadTgLook.abc = "".
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
                    ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          ttLabelMenuLoadTgLook.abc = sys-ctrl.char-fld.
                       ELSE
                          ttLabelMenuLoadTgLook.abc = "".
                    END.
                 END.
              END.
        END.
        ELSE
        IF INT(ttLabelMenuLoadTgLook.ordno) EQ 0 AND
           INT(ttLabelMenuLoadTgLook.ordno) EQ 0 THEN
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
                    ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
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
                       ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             ttLabelMenuLoadTgLook.abc = sys-ctrl.char-fld.
                          ELSE
                             ttLabelMenuLoadTgLook.abc = "".
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
                    ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       ttLabelMenuLoadTgLook.abc = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          ttLabelMenuLoadTgLook.abc = sys-ctrl.char-fld.
                       ELSE
                          ttLabelMenuLoadTgLook.abc = "".
                    END.
                 END.
              END.


           END. /*begin_ord-no and end_ord-no eq 0*/
           ttLabelMenuLoadTgLook.abc = REPLACE(ttLabelMenuLoadTgLook.abc,'\','/'). 

END.


/**************************************************************************************/


