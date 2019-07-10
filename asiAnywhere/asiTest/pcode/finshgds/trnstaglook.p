/*------------------------------------------------------------------------
    File        : fgtaglook.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tttrnstagLook NO-UNDO 
    FIELD  tag             AS CHAR  
    FIELD vPoNo            AS INT
    FIELD vOrdQty          AS DEC
    FIELD vINo             AS CHARACTER
    FIELD vIName           AS CHARACTER
    FIELD vJobNo           AS CHARACTER
    FIELD vJob2            AS INT
    FIELD vLoc             AS CHARACTER
    FIELD vLocBin          AS CHAR
    FIELD vunitCount       AS INT
    FIELD vqty             AS INT
    FIELD  unit            AS INT
    FIELD  ordno           AS INT
    FIELD  qtycase         AS INT
    FIELD  partial         AS INT
    FIELD vdate            AS CHAR  
    FIELD vtime            AS CHAR
    FIELD vTag2             AS CHAR
    FIELD vLoc2            AS CHAR
    FIELD vLocBin2         AS CHAR
    FIELD vcust            AS CHAR
    FIELD vrfid            AS CHAR
    FIELD vpltcnt          AS INT
     .

DEFINE DATASET dstrnstagLook FOR tttrnstagLook .

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTag          AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcurval       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dstrnstagLook.
   
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

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmTag       = ? THEN ASSIGN prmTag       = "".
IF prmcurval    = ? THEN ASSIGN prmcurval    = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF VAR lv-fields AS CHAR INIT "job-no,job-no2,loc,loc-bin,tag,cust-no" NO-UNDO.
DEF VAR li-field# AS INT NO-UNDO.
DEF VAR li-fieldc AS CHAR NO-UNDO.

DEF BUFFER bf-tmp FOR fg-rctd.
  DEFINE BUFFER b-fg-rctd FOR fg-rctd.



{sys/inc/VAR.i NEW SHARED}

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEF BUFFER b-fg-bin FOR fg-bin .
DEFINE VARIABLE unitsOH LIKE fg-rctd.t-qty NO-UNDO.


ASSIGN cocode = prmComp.

{sys/inc/sstransf.i}




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

  
IF prmcurval NE "cons" THEN do: /*Data for Transfer Screen*/

    
if prmAction = "Select" then do:

  FOR EACH loadtag WHERE loadtag.company = prmComp and 
       loadtag.item-type = NO NO-LOCK, 
       FIRST fg-bin WHERE fg-bin.company = loadtag.company 
       AND fg-bin.i-no = loadtag.i-no 
       AND fg-bin.job-no = loadtag.job-no 
       AND fg-bin.job-no2 = loadtag.job-no2 
       /*  AND fg-bin.loc = loadtag.loc ~
       AND fg-bin.loc-bin = loadtag.loc-bin ~
       */ 
       AND fg-bin.tag = loadtag.tag-no 
       /*AND fg-bin.qty > 0*/ NO-LOCK:
       
      

     
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty     .
     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.

      li-field# = 5.

     IF li-field# EQ 0 THEN li-field# = 9999.

     

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
         
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN /*tttrnstagLook.tag   = fg-bin.tag*/
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
                 tttrnstagLook.vTag2 = fg-bin.tag
                .
     END.
     RELEASE fg-bin.

    
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   

    FIND FIRST b-fg-bin
        WHERE b-fg-bin.company  EQ cocode
          AND b-fg-bin.i-no     EQ tttrnstagLook.vINo
          AND b-fg-bin.job-no   EQ tttrnstagLook.vJobno
          AND (b-fg-bin.job-no2 EQ INT(tttrnstagLook.vJob2) OR
               li-field#      LT 2)
          AND (b-fg-bin.loc     EQ tttrnstagLook.vLoc          OR
               li-field#      LT 3)
          AND (b-fg-bin.loc-bin EQ tttrnstagLook.vLocBin        OR
               li-field#      LT 4)
          AND (b-fg-bin.tag     EQ tttrnstagLook.vTag          OR
               li-field#      LT 5)
          AND (b-fg-bin.cust-no EQ tttrnstagLook.vcust          OR
               li-field#      LT 6)
        NO-LOCK NO-ERROR.

    IF li-field# EQ 5 AND AVAIL b-fg-bin THEN
      tttrnstagLook.partial = b-fg-bin.partial-count.

    IF AVAIL b-fg-bin AND
       b-fg-bin.qty GE (DEC(tttrnstagLook.unit) *
                      DEC(tttrnstagLook.qtycase)) +
                      DEC(tttrnstagLook.partial)
    THEN DO:
      ASSIGN
       tttrnstagLook.qtycase = b-fg-bin.case-count
       tttrnstagLook.vLoc      = CAPS(b-fg-bin.loc)
       tttrnstagLook.vLocBin  = CAPS(b-fg-bin.loc-bin)
       tttrnstagLook.tag      = CAPS(b-fg-bin.tag)
       tttrnstagLook.vcust  = CAPS(b-fg-bin.cust-no)
       tttrnstagLook.vTag2     = CAPS(b-fg-bin.tag) .
    END.


    
    IF sstransf-int = 1 THEN
    DO:
        FOR LAST  b-fg-rctd FIELDS (b-fg-rctd.loc2 b-fg-rctd.loc-bin2) 
          WHERE b-fg-rctd.company = cocode
          AND b-fg-rctd.i-no = tttrnstagLook.vINo 
          AND b-fg-rctd.rita-code = "T" 
          AND ROWID(b-fg-rctd) NE ROWID (fg-rctd)
          NO-LOCK.

          ASSIGN tttrnstagLook.vLoc2 = b-fg-rctd.loc2
                 tttrnstagLook.vLocBin2 = b-fg-rctd.loc-bin2.
          
/*           DISPLAY b-fg-rctd.loc2 @ fg-rctd.loc2          */
/*                   b-fg-rctd.loc-bin2  @ fg-rctd.loc-bin2 */
/*                   WITH FRAME {&FRAME-NAME}.              */

         

        END.
            
    END.    


   

  END. /* for each loadtag */



END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "PoSearch" then do:
     if prmField = "Tag#"  then do:
         if prmCondition = "EQUAL" then do:
             
       FOR EACH loadtag WHERE loadtag.company = prmComp and
           loadtag.tag-no = prmText AND
           loadtag.item-type = NO NO-LOCK, 
           FIRST fg-bin WHERE fg-bin.company = loadtag.company 
           AND fg-bin.i-no = loadtag.i-no 
           AND fg-bin.job-no = loadtag.job-no 
           AND fg-bin.job-no2 = loadtag.job-no2 
           /*  AND fg-bin.loc = loadtag.loc ~
           AND fg-bin.loc-bin = loadtag.loc-bin ~
           */ 
           AND fg-bin.tag = loadtag.tag-no 
           /*AND fg-bin.qty > 0*/ NO-LOCK:

           

   
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty   
    .

     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.
    
     li-field# = 5.
     IF li-field# EQ 0 THEN li-field# = 9999.

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN tttrnstagLook.tag   = fg-bin.tag
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
                tttrnstagLook.vTag2 = fg-bin.tag
                .
     END.
     RELEASE fg-bin.

    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   

    FIND FIRST b-fg-bin
        WHERE b-fg-bin.company  EQ cocode
          AND b-fg-bin.i-no     EQ tttrnstagLook.vINo
          AND b-fg-bin.job-no   EQ tttrnstagLook.vJobno
          AND (b-fg-bin.job-no2 EQ INT(tttrnstagLook.vJob2) OR
               li-field#      LT 2)
          AND (b-fg-bin.loc     EQ tttrnstagLook.vLoc          OR
               li-field#      LT 3)
          AND (b-fg-bin.loc-bin EQ tttrnstagLook.vLocBin        OR
               li-field#      LT 4)
          AND (b-fg-bin.tag     EQ tttrnstagLook.vTag          OR
               li-field#      LT 5)
          AND (b-fg-bin.cust-no EQ tttrnstagLook.vcust          OR
               li-field#      LT 6)
        NO-LOCK NO-ERROR.

    IF li-field# EQ 5 AND AVAIL b-fg-bin THEN
      tttrnstagLook.partial = b-fg-bin.partial-count.

    IF AVAIL fg-bin AND
       b-fg-bin.qty GE (DEC(tttrnstagLook.unit) *
                      DEC(tttrnstagLook.qtycase)) +
                      DEC(tttrnstagLook.partial)
    THEN DO:
      ASSIGN
       tttrnstagLook.qtycase = b-fg-bin.case-count
       tttrnstagLook.vLoc      = CAPS(b-fg-bin.loc)
       tttrnstagLook.vLocBin  = CAPS(b-fg-bin.loc-bin)
       tttrnstagLook.tag      = CAPS(b-fg-bin.tag)
       tttrnstagLook.vcust  = CAPS(b-fg-bin.cust-no)
       tttrnstagLook.vTag2     = CAPS(b-fg-bin.tag) .
    END.
    
    IF sstransf-int = 1 THEN
    DO:
        FOR LAST  b-fg-rctd FIELDS (b-fg-rctd.loc2 b-fg-rctd.loc-bin2) 
          WHERE b-fg-rctd.company = cocode
          AND b-fg-rctd.i-no = tttrnstagLook.vINo 
          AND b-fg-rctd.rita-code = "T" 
          AND ROWID(b-fg-rctd) NE ROWID (fg-rctd)
          NO-LOCK.

          ASSIGN tttrnstagLook.vLoc2 = b-fg-rctd.loc2
                 tttrnstagLook.vLocBin2 = b-fg-rctd.loc-bin2.
          
        END.

    END.


   

  END. /* for each loadtag */


             
             
 END.
 
 IF prmCondition = "BEGIN" then do:
     
     FOR EACH loadtag WHERE loadtag.company = prmComp and
         loadtag.tag-no BEGINS prmText AND
         loadtag.item-type = NO NO-LOCK, 
         FIRST fg-bin WHERE fg-bin.company = loadtag.company 
         AND fg-bin.i-no = loadtag.i-no 
         AND fg-bin.job-no = loadtag.job-no 
         AND fg-bin.job-no2 = loadtag.job-no2 
         /*  AND fg-bin.loc = loadtag.loc ~
         AND fg-bin.loc-bin = loadtag.loc-bin ~
         */ 
         AND fg-bin.tag = loadtag.tag-no 
         /*AND fg-bin.qty > 0*/ NO-LOCK:
         

         CREATE tttrnstagLook.
         ASSIGN
             tttrnstagLook.tag        = loadtag.tag-no
             tttrnstagLook.vJobNo     = loadtag.job-no 
             tttrnstagLook.vJob2      = int(loadtag.job-no2)
             tttrnstagLook.vINo       = loadtag.i-no 
             tttrnstagLook.vIName     = loadtag.i-name
             tttrnstagLook.vLoc       = loadtag.loc
             tttrnstagLook.vLocBin    = loadtag.loc-bin
             tttrnstagLook.vunitCount = loadtag.qty-case
             tttrnstagLook.unit       = loadtag.tot-cases
             tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                        STRING(TODAY) ELSE tttrnstagLook.vdate
             tttrnstagLook.ordno        = loadtag.ord-no
             tttrnstagLook.vPoNo        = loadtag.po-no
             tttrnstagLook.vOrdQty      = loadtag.qty  .




           ASSIGN
               li-fieldc = TRIM(tttrnstagLook.vJobNo)
               li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
               tttrnstagLook.vJobNo = li-fieldc.

          li-field# = 5.

           IF li-field# EQ 0 THEN li-field# = 9999.
           
           IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
               FIND FIRST itemfg WHERE itemfg.company = cocode
                   AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.

               ASSIGN tttrnstagLook.tag   = fg-bin.tag
                   tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                   tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                   tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                   tttrnstagLook.vIName =  itemfg.i-name         
                   tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                       ELSE loadtag.qty-case
                           tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                               ELSE loadtag.tot-cases
                   /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                   fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                   ELSE string(loadtag.case-bundle)*/
                   tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                   tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                   tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                   tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                   ELSE loadtag.partial
                   tttrnstagLook.vTag2 = fg-bin.tag .
           END.

           RELEASE fg-bin.

           /* To cover previous Transfer Posting Bugs
           if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
           
           IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
               FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
               AND b-fg-bin.tag = loadtag.tag-no
               AND b-fg-bin.loc = loadtag.loc
               AND b-fg-bin.loc-bin = loadtag.loc-bin
               AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.

           IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
               ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   

           FIND FIRST b-fg-bin
               WHERE b-fg-bin.company  EQ cocode
               AND b-fg-bin.i-no     EQ tttrnstagLook.vINo
               AND b-fg-bin.job-no   EQ tttrnstagLook.vJobno
               AND (b-fg-bin.job-no2 EQ INT(tttrnstagLook.vJob2) OR
                    li-field#      LT 2)
               AND (b-fg-bin.loc     EQ tttrnstagLook.vLoc          OR
                    li-field#      LT 3)
               AND (b-fg-bin.loc-bin EQ tttrnstagLook.vLocBin        OR
                    li-field#      LT 4)
               AND (b-fg-bin.tag     EQ tttrnstagLook.vTag          OR
                    li-field#      LT 5)
               AND (b-fg-bin.cust-no EQ tttrnstagLook.vcust          OR
                    li-field#      LT 6)
               NO-LOCK NO-ERROR.

           IF li-field# EQ 5 AND AVAIL b-fg-bin THEN
               tttrnstagLook.partial = b-fg-bin.partial-count.

           
           IF AVAIL fg-bin AND
               b-fg-bin.qty GE (DEC(tttrnstagLook.unit) *
                                DEC(tttrnstagLook.qtycase)) +
               DEC(tttrnstagLook.partial) THEN DO:

               ASSIGN
                   tttrnstagLook.qtycase = b-fg-bin.case-count
                   tttrnstagLook.vLoc      = CAPS(b-fg-bin.loc)
                   tttrnstagLook.vLocBin  = CAPS(b-fg-bin.loc-bin)
                   tttrnstagLook.tag      = CAPS(b-fg-bin.tag)
                   tttrnstagLook.vcust  = CAPS(b-fg-bin.cust-no)
                   tttrnstagLook.vTag2     = CAPS(b-fg-bin.tag) .
            END.

            IF sstransf-int = 1 THEN
                DO:
                FOR LAST  b-fg-rctd FIELDS (b-fg-rctd.loc2 b-fg-rctd.loc-bin2) 
                    WHERE b-fg-rctd.company = cocode
                    AND b-fg-rctd.i-no = tttrnstagLook.vINo 
                    AND b-fg-rctd.rita-code = "T" 
                    AND ROWID(b-fg-rctd) NE ROWID (fg-rctd)
                    NO-LOCK.

                  ASSIGN tttrnstagLook.vLoc2 = b-fg-rctd.loc2
                      tttrnstagLook.vLocBin2 = b-fg-rctd.loc-bin2.
            END.
    END.




END. /* for each loadtag */

              
         END.
     
     END.  /* if prmField = state  */


     if prmField = "Item"  then do:
          if prmCondition = "EQUAL" then do:


        FOR EACH loadtag WHERE loadtag.company = prmComp and
           loadtag.i-no = prmText AND
           loadtag.item-type = NO NO-LOCK, 
           FIRST fg-bin WHERE fg-bin.company = loadtag.company 
           AND fg-bin.i-no = loadtag.i-no 
           AND fg-bin.job-no = loadtag.job-no 
           AND fg-bin.job-no2 = loadtag.job-no2 
           /*  AND fg-bin.loc = loadtag.loc ~
           AND fg-bin.loc-bin = loadtag.loc-bin ~
           */ 
           AND fg-bin.tag = loadtag.tag-no 
           /*AND fg-bin.qty > 0*/ NO-LOCK:

   
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty   
    .

     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.

      li-field# = 5.

     IF li-field# EQ 0 THEN li-field# = 9999.

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN tttrnstagLook.tag   = fg-bin.tag
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
                tttrnstagLook.vTag2 = fg-bin.tag
                .
     END.
     RELEASE fg-bin.

    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   

    FIND FIRST b-fg-bin
        WHERE b-fg-bin.company  EQ cocode
          AND b-fg-bin.i-no     EQ tttrnstagLook.vINo
          AND b-fg-bin.job-no   EQ tttrnstagLook.vJobno
          AND (b-fg-bin.job-no2 EQ INT(tttrnstagLook.vJob2) OR
               li-field#      LT 2)
          AND (b-fg-bin.loc     EQ tttrnstagLook.vLoc          OR
               li-field#      LT 3)
          AND (b-fg-bin.loc-bin EQ tttrnstagLook.vLocBin        OR
               li-field#      LT 4)
          AND (b-fg-bin.tag     EQ tttrnstagLook.vTag          OR
               li-field#      LT 5)
          AND (b-fg-bin.cust-no EQ tttrnstagLook.vcust          OR
               li-field#      LT 6)
        NO-LOCK NO-ERROR.

    IF li-field# EQ 5 AND AVAIL b-fg-bin THEN
      tttrnstagLook.partial = b-fg-bin.partial-count.

    IF AVAIL fg-bin AND
       b-fg-bin.qty GE (DEC(tttrnstagLook.unit) *
                      DEC(tttrnstagLook.qtycase)) +
                      DEC(tttrnstagLook.partial)
    THEN DO:
      ASSIGN
       tttrnstagLook.qtycase = b-fg-bin.case-count
       tttrnstagLook.vLoc      = CAPS(b-fg-bin.loc)
       tttrnstagLook.vLocBin  = CAPS(b-fg-bin.loc-bin)
       tttrnstagLook.tag      = CAPS(b-fg-bin.tag)
       tttrnstagLook.vcust  = CAPS(b-fg-bin.cust-no)
       tttrnstagLook.vTag2     = CAPS(b-fg-bin.tag) .
    END.
    
    IF sstransf-int = 1 THEN
    DO:
        FOR LAST  b-fg-rctd FIELDS (b-fg-rctd.loc2 b-fg-rctd.loc-bin2) 
          WHERE b-fg-rctd.company = cocode
          AND b-fg-rctd.i-no = tttrnstagLook.vINo 
          AND b-fg-rctd.rita-code = "T" 
          AND ROWID(b-fg-rctd) NE ROWID (fg-rctd)
          NO-LOCK.

          ASSIGN tttrnstagLook.vLoc2 = b-fg-rctd.loc2
                 tttrnstagLook.vLocBin2 = b-fg-rctd.loc-bin2.
          
        END.

    END.


   

  END. /* for each loadtag */


                           
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
                         FOR EACH loadtag WHERE loadtag.company = prmComp and
           loadtag.i-no BEGINS prmText AND
           loadtag.item-type = NO NO-LOCK, 
           FIRST fg-bin WHERE fg-bin.company = loadtag.company 
           AND fg-bin.i-no = loadtag.i-no 
           AND fg-bin.job-no = loadtag.job-no 
           AND fg-bin.job-no2 = loadtag.job-no2 
           /*  AND fg-bin.loc = loadtag.loc ~
           AND fg-bin.loc-bin = loadtag.loc-bin ~
           */ 
           AND fg-bin.tag = loadtag.tag-no 
           /*AND fg-bin.qty > 0*/ NO-LOCK:

   
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty   
    .

     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.

      li-field# = 5.

     IF li-field# EQ 0 THEN li-field# = 9999.

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN tttrnstagLook.tag   = fg-bin.tag
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
                tttrnstagLook.vTag2 = fg-bin.tag
                .
     END.
     RELEASE fg-bin.

    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   

    FIND FIRST b-fg-bin
        WHERE b-fg-bin.company  EQ cocode
          AND b-fg-bin.i-no     EQ tttrnstagLook.vINo
          AND b-fg-bin.job-no   EQ tttrnstagLook.vJobno
          AND (b-fg-bin.job-no2 EQ INT(tttrnstagLook.vJob2) OR
               li-field#      LT 2)
          AND (b-fg-bin.loc     EQ tttrnstagLook.vLoc          OR
               li-field#      LT 3)
          AND (b-fg-bin.loc-bin EQ tttrnstagLook.vLocBin        OR
               li-field#      LT 4)
          AND (b-fg-bin.tag     EQ tttrnstagLook.vTag          OR
               li-field#      LT 5)
          AND (b-fg-bin.cust-no EQ tttrnstagLook.vcust          OR
               li-field#      LT 6)
        NO-LOCK NO-ERROR.

    IF li-field# EQ 5 AND AVAIL b-fg-bin THEN
      tttrnstagLook.partial = b-fg-bin.partial-count.

    IF AVAIL fg-bin AND
       b-fg-bin.qty GE (DEC(tttrnstagLook.unit) *
                      DEC(tttrnstagLook.qtycase)) +
                      DEC(tttrnstagLook.partial)
    THEN DO:
      ASSIGN
       tttrnstagLook.qtycase = b-fg-bin.case-count
       tttrnstagLook.vLoc      = CAPS(b-fg-bin.loc)
       tttrnstagLook.vLocBin  = CAPS(b-fg-bin.loc-bin)
       tttrnstagLook.tag      = CAPS(b-fg-bin.tag)
       tttrnstagLook.vcust  = CAPS(b-fg-bin.cust-no)
       tttrnstagLook.vTag2     = CAPS(b-fg-bin.tag) .
    END.
    
    IF sstransf-int = 1 THEN
    DO:
        FOR LAST  b-fg-rctd FIELDS (b-fg-rctd.loc2 b-fg-rctd.loc-bin2) 
          WHERE b-fg-rctd.company = cocode
          AND b-fg-rctd.i-no = tttrnstagLook.vINo 
          AND b-fg-rctd.rita-code = "T" 
          AND ROWID(b-fg-rctd) NE ROWID (fg-rctd)
          NO-LOCK.

          ASSIGN tttrnstagLook.vLoc2 = b-fg-rctd.loc2
                 tttrnstagLook.vLocBin2 = b-fg-rctd.loc-bin2.
          
        END.

    END.


   

  END. /* for each loadtag */
              
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */


END. /*Data for Transfer lookup*/

ELSE DO: /*Data Consolidate Lookup*/

    if prmAction = "Select" then do:

  FOR EACH loadtag WHERE loadtag.company = prmComp and 
       loadtag.item-type = NO NO-LOCK, 
       FIRST fg-bin WHERE fg-bin.company = loadtag.company 
       AND fg-bin.i-no = loadtag.i-no 
       AND fg-bin.job-no = loadtag.job-no 
       AND fg-bin.job-no2 = loadtag.job-no2 
       /*  AND fg-bin.loc = loadtag.loc ~
       AND fg-bin.loc-bin = loadtag.loc-bin ~
       */ 
       AND fg-bin.tag = loadtag.tag-no 
       /*AND fg-bin.qty > 0*/ NO-LOCK,
      
      EACH rfidtag OF loadtag NO-LOCK: 
      
      

     
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.vrfid      = rfidtag.rfidtag 
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty  
    tttrnstagLook.vpltcnt      = loadtag.pallet-count  .
     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.

      li-field# = 5.

     IF li-field# EQ 0 THEN li-field# = 9999.

     

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
         
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN /*tttrnstagLook.tag   = fg-bin.tag*/
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
                 
                .
     END.
     RELEASE fg-bin.

    
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   
   

  END. /* for each loadtag */



END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "PoSearch" then do:
     if prmField = "Tag#"  then do:
         if prmCondition = "EQUAL" then do:
             
       FOR EACH loadtag WHERE loadtag.company = prmComp and
           loadtag.tag-no = prmText AND
           loadtag.item-type = NO NO-LOCK, 
           FIRST fg-bin WHERE fg-bin.company = loadtag.company 
           AND fg-bin.i-no = loadtag.i-no 
           AND fg-bin.job-no = loadtag.job-no 
           AND fg-bin.job-no2 = loadtag.job-no2 
           /*  AND fg-bin.loc = loadtag.loc ~
           AND fg-bin.loc-bin = loadtag.loc-bin ~
           */ 
           AND fg-bin.tag = loadtag.tag-no 
           /*AND fg-bin.qty > 0*/ NO-LOCK,

           EACH rfidtag OF loadtag NO-LOCK: 

   
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.vrfid      = rfidtag.rfidtag 
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty  
    tttrnstagLook.vpltcnt      = loadtag.pallet-count  .

     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.
    
     li-field# = 5.
     IF li-field# EQ 0 THEN li-field# = 9999.

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN tttrnstagLook.tag   = fg-bin.tag
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
               
                .
     END.
     RELEASE fg-bin.

    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   
    
  END. /* for each loadtag */
 END.
 
 IF prmCondition = "BEGIN" then do:
     
     FOR EACH loadtag WHERE loadtag.company = prmComp and
         loadtag.tag-no BEGINS prmText AND
         loadtag.item-type = NO NO-LOCK, 
         FIRST fg-bin WHERE fg-bin.company = loadtag.company 
         AND fg-bin.i-no = loadtag.i-no 
         AND fg-bin.job-no = loadtag.job-no 
         AND fg-bin.job-no2 = loadtag.job-no2 
         /*  AND fg-bin.loc = loadtag.loc ~
         AND fg-bin.loc-bin = loadtag.loc-bin ~
         */ 
         AND fg-bin.tag = loadtag.tag-no 
         /*AND fg-bin.qty > 0*/ NO-LOCK,

         EACH rfidtag OF loadtag NO-LOCK: 
         

         CREATE tttrnstagLook.
         ASSIGN
             tttrnstagLook.vrfid      = rfidtag.rfidtag
             tttrnstagLook.tag        = loadtag.tag-no
             tttrnstagLook.vJobNo     = loadtag.job-no 
             tttrnstagLook.vJob2      = int(loadtag.job-no2)
             tttrnstagLook.vINo       = loadtag.i-no 
             tttrnstagLook.vIName     = loadtag.i-name
             tttrnstagLook.vLoc       = loadtag.loc
             tttrnstagLook.vLocBin    = loadtag.loc-bin
             tttrnstagLook.vunitCount = loadtag.qty-case
             tttrnstagLook.unit       = loadtag.tot-cases
             tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                        STRING(TODAY) ELSE tttrnstagLook.vdate
             tttrnstagLook.ordno        = loadtag.ord-no
             tttrnstagLook.vPoNo        = loadtag.po-no
             tttrnstagLook.vOrdQty      = loadtag.qty  
             tttrnstagLook.vpltcnt      = loadtag.pallet-count  .




           ASSIGN
               li-fieldc = TRIM(tttrnstagLook.vJobNo)
               li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
               tttrnstagLook.vJobNo = li-fieldc.

          li-field# = 5.

           IF li-field# EQ 0 THEN li-field# = 9999.
           
           IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
               FIND FIRST itemfg WHERE itemfg.company = cocode
                   AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.

               ASSIGN tttrnstagLook.tag   = fg-bin.tag
                   tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                   tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                   tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                   tttrnstagLook.vIName =  itemfg.i-name         
                   tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                       ELSE loadtag.qty-case
                           tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                               ELSE loadtag.tot-cases
                   /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                   fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                   ELSE string(loadtag.case-bundle)*/
                   tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                   tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                   tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                   tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                   ELSE loadtag.partial .
                   
           END.

           RELEASE fg-bin.

           /* To cover previous Transfer Posting Bugs
           if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
           
           IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
               FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
               AND b-fg-bin.tag = loadtag.tag-no
               AND b-fg-bin.loc = loadtag.loc
               AND b-fg-bin.loc-bin = loadtag.loc-bin
               AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.

           IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
               ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   

          




END. /* for each loadtag */

              
         END.
     
     END.  /* if prmField = state  */


     if prmField = "Item"  then do:
          if prmCondition = "EQUAL" then do:


        FOR EACH loadtag WHERE loadtag.company = prmComp and
           loadtag.i-no = prmText AND
           loadtag.item-type = NO NO-LOCK, 
           FIRST fg-bin WHERE fg-bin.company = loadtag.company 
           AND fg-bin.i-no = loadtag.i-no 
           AND fg-bin.job-no = loadtag.job-no 
           AND fg-bin.job-no2 = loadtag.job-no2 
           /*  AND fg-bin.loc = loadtag.loc ~
           AND fg-bin.loc-bin = loadtag.loc-bin ~
           */ 
           AND fg-bin.tag = loadtag.tag-no 
           /*AND fg-bin.qty > 0*/ NO-LOCK,

            EACH rfidtag OF loadtag NO-LOCK: 

   
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.vrfid      = rfidtag.rfidtag 
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty  
    tttrnstagLook.vpltcnt      = loadtag.pallet-count  .

     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.

      li-field# = 5.

     IF li-field# EQ 0 THEN li-field# = 9999.

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN tttrnstagLook.tag   = fg-bin.tag
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
               
                .
     END.
     RELEASE fg-bin.

    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   

    


   

  END. /* for each loadtag */


                           
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
                         FOR EACH loadtag WHERE loadtag.company = prmComp and
           loadtag.i-no BEGINS prmText AND
           loadtag.item-type = NO NO-LOCK, 
           FIRST fg-bin WHERE fg-bin.company = loadtag.company 
           AND fg-bin.i-no = loadtag.i-no 
           AND fg-bin.job-no = loadtag.job-no 
           AND fg-bin.job-no2 = loadtag.job-no2 
           /*  AND fg-bin.loc = loadtag.loc ~
           AND fg-bin.loc-bin = loadtag.loc-bin ~
           */ 
           AND fg-bin.tag = loadtag.tag-no 
           /*AND fg-bin.qty > 0*/ NO-LOCK,

          EACH rfidtag OF loadtag NO-LOCK: 

   
   CREATE tttrnstagLook.

   ASSIGN
    tttrnstagLook.vrfid      = rfidtag.rfidtag 
    tttrnstagLook.tag        = loadtag.tag-no
    tttrnstagLook.vJobNo     = loadtag.job-no 
    tttrnstagLook.vJob2      = int(loadtag.job-no2)
    tttrnstagLook.vINo       = loadtag.i-no 
    tttrnstagLook.vIName     = loadtag.i-name
    tttrnstagLook.vLoc       = loadtag.loc
    tttrnstagLook.vLocBin    = loadtag.loc-bin
    tttrnstagLook.vunitCount = loadtag.qty-case
    tttrnstagLook.unit       = loadtag.tot-cases
    tttrnstagLook.vdate      = IF tttrnstagLook.vdate = "" THEN 
                                  STRING(TODAY) ELSE tttrnstagLook.vdate
    tttrnstagLook.ordno        = loadtag.ord-no
    tttrnstagLook.vPoNo        = loadtag.po-no
    tttrnstagLook.vOrdQty      = loadtag.qty  
    tttrnstagLook.vpltcnt      = loadtag.pallet-count  .

     
     
     ASSIGN
      li-fieldc = TRIM(tttrnstagLook.vJobNo)
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      tttrnstagLook.vJobNo = li-fieldc.

      li-field# = 5.

     IF li-field# EQ 0 THEN li-field# = 9999.

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        
         ASSIGN tttrnstagLook.tag   = fg-bin.tag
                tttrnstagLook.vJobNo = fg-bin.job-no /*loadtag.job-no */
                tttrnstagLook.vJob2 = fg-bin.job-no2 /*loadtag.job-no2*/
                tttrnstagLook.vINo = fg-bin.i-no /*loadtag.i-no */
                tttrnstagLook.vIName =  itemfg.i-name         
                tttrnstagLook.qtycase = IF AVAIL fg-bin THEN fg-bin.case-count
                                                ELSE loadtag.qty-case
                tttrnstagLook.unit = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                                                     ELSE loadtag.tot-cases
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                tttrnstagLook.vLoc = fg-bin.loc /*loadtag.loc*/
                tttrnstagLook.vLocBin = fg-bin.loc-bin /*loadtag.loc-bin*/
                tttrnstagLook.vdate = IF tttrnstagLook.vdate = "" THEN STRING(TODAY) ELSE tttrnstagLook.vdate  
                tttrnstagLook.partial = IF AVAIL fg-bin THEN fg-bin.partial-count
                                               ELSE loadtag.partial
                tttrnstagLook.vTag2 = fg-bin.tag
                .
     END.
     RELEASE fg-bin.

    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
    
    IF AVAIL loadtag AND INT(tttrnstagLook.unit) = 0 AND li-field# = 5 THEN
       FIND FIRST b-fg-bin WHERE b-fg-bin.company = cocode
                           AND b-fg-bin.tag = loadtag.tag-no
                           AND b-fg-bin.loc = loadtag.loc
                           AND b-fg-bin.loc-bin = loadtag.loc-bin
                           AND b-fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL b-fg-bin THEN  ASSIGN tttrnstagLook.unit = 
         ROUND((b-fg-bin.qty - b-fg-bin.partial-count) / loadtag.qty-case,0).   
   

  END. /* for each loadtag */
              
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */


END. /*Data Consolidate Lookup*/








