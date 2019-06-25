

/*------------------------------------------------------------------------
    File        : BrwsCEstimate.p
    Purpose     : Corrugated Box
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTrnsViewFGrece NO-UNDO
        FIELD extra           AS CHAR 
        FIELD vDate           AS CHAR                  
        FIELD vTransTime      AS CHAR                  
        FIELD vTag            AS CHAR FORMAT "x(20)"   
        FIELD vJobno          AS CHAR FORMAT "x(6)"    
        FIELD vJobno2         AS INT                   
        FIELD vItem           AS CHAR FORMAT "x(15)"   
        FIELD vItemName       AS CHAR FORMAT "x(30)"   
        FIELD vLoc            AS CHAR  FORMAT "x(5)"   
        FIELD vLocBin         AS CHAR  FORMAT "x(8)"   
        FIELD vCases          AS INT                   
        FIELD vQtyCas         AS INT                   
        FIELD vCasUnit        AS INT                   
        FIELD vPartial        AS INT                   
        FIELD vcust           AS CHAR                  
        FIELD vLoc2           AS CHAR  FORMAT "x(5)"   
        FIELD vLocBin2        AS CHAR  FORMAT "x(8)"   
        FIELD vTag2           AS CHAR FORMAT "x(20)"   
        FIELD blan            AS INT
        FIELD usrcrt          AS CHAR
        FIELD usrupdt         AS CHAR               
        FIELD vRecKey         AS CHAR. 
        

DEFINE DATASET dsTrnsViewFGrece FOR ttTrnsViewFGrece .

DEFINE INPUT PARAMETER prmUser           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno          AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmTransTime      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJob_no2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmName           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLoc            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLocBin         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCases          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty_Cas        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCasUnit        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPartial        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLoc2           AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmLocBin2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno2         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmcontrans       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeq            AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmOut            AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError           AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTrnsViewFGrece.

DEFINE NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
/*DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.*/
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-next-tag AS cha NO-UNDO.
{sys/inc/var.i "new shared"}
 {fg/invrecpt.i NEW}


IF prmUser         = ? THEN ASSIGN prmUser        = "".
IF prmAction       = ? THEN ASSIGN prmAction      = "Select".
IF prmFgItem       = ? THEN ASSIGN prmFgItem      = "".
IF prmJobno        = ? THEN ASSIGN prmJobno       = "".  
IF prmRcptDate     = ? THEN ASSIGN prmRcptDate    = "".
IF prmTagno        = ? THEN ASSIGN prmTagno       = "".
IF prmTransTime    = ? THEN ASSIGN prmTransTime   = "".
IF prmJob_no2      = ? THEN ASSIGN prmJob_no2     = "0".
IF prmName         = ? THEN ASSIGN prmName        = "".
IF prmLoc          = ? THEN ASSIGN prmLoc         = "".
IF prmLocBin       = ? THEN ASSIGN prmLocBin      = "".
IF prmCases        = ? THEN ASSIGN prmCases       = "0".
IF prmQty_Cas      = ? THEN ASSIGN prmQty_Cas     = "0".
IF prmCasUnit      = ? THEN ASSIGN prmCasUnit     = "".
IF prmPartial      = ? THEN ASSIGN prmPartial     = "0".
IF prmLoc2         = ? THEN ASSIGN prmLoc2        = "".
IF prmLocBin2      = ? THEN ASSIGN prmLocBin2     = "".
IF prmTagno2       = ? THEN ASSIGN prmTagno2      = "".
IF prmRecKey       = ? THEN ASSIGN prmRecKey      = "".
IF prmSeq          = ? THEN ASSIGN prmSeq         = 0.
IF prmOut          = ? THEN ASSIGN prmOut         = "".
IF cError          = ? THEN ASSIGN cError         = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    g_company = prmComp
    vuser = prmUser .


def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEF BUFFER bf-rctd FOR fg-rctd.
DEFINE VARIABLE unitsOH LIKE fg-rctd.t-qty NO-UNDO.
 DEF BUFFER bf-tmp FOR fg-rctd.
DEF VAR lv-tag2 LIKE fg-rctd.tag2 NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
{sys/inc/sstransf.i}


IF prmAction = "validupdate" THEN DO: 

 /*IF prmcontrans = "Yes" THEN DO:

     FIND FIRST fg-rctd WHERE fg-rctd.company = cocode 
      AND fg-rctd.rita-code = "T"
      AND fg-rctd.rec_key   = prmRecKey NO-LOCK NO-ERROR.

     FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
                             bf-tmp.rita-code = "T" AND
                             bf-tmp.tag = prmTagno AND 
         RECID(bf-tmp) <> RECID(fg-rctd) NO-LOCK NO-ERROR.
     IF AVAIL bf-tmp THEN DO:
        cError =  "This Tag Number Has Already Been Used.  " +
                "Please Enter A Unique Tag Number."           .
                       RETURN .
     END. */

     FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
   /*  IF NOT AVAIL loadtag THEN DO:
        FIND rfidtag WHERE rfidtag.company = g_company
                       AND rfidtag.rfidtag = prmTagno NO-LOCK NO-ERROR.
        IF AVAIL rfidtag THEN
           FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = rfidtag.tag-no NO-LOCK NO-ERROR.

        IF NOT AVAIL rfidtag OR NOT AVAIL loadtag THEN DO:
           cError = "Invalid Loadtag#. " .
           RETURN.
        END.
     END. */

     FIND FIRST fg-bin WHERE fg-bin.company = cocode
                            AND fg-bin.i-no = loadtag.i-no
                            AND fg-bin.tag = loadtag.tag-no
                            /*AND fg-bin.job-no = loadtag.job-no
                            AND fg-bin.job-no2 = loadtag.job-no2*/
                            AND fg-bin.qty > 0
                            NO-LOCK NO-ERROR.
     IF NOT AVAIL fg-bin THEN DO:
        cError = "No On-Hand Qty for the tag." .
        RETURN .
     END.


  /* FIND FIRST loadtag WHERE loadtag.company = cocode 
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = prmTagno2 NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag THEN DO:
        FIND rfidtag WHERE rfidtag.company = cocode
                       AND rfidtag.rfidtag = prmTagno2 NO-LOCK NO-ERROR.
        IF AVAIL rfidtag THEN
           FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = rfidtag.tag-no NO-LOCK NO-ERROR.

        IF NOT AVAIL rfidtag and NOT AVAIL loadtag THEN DO:
           cError = "Invalid Loadtag2#. "  .
           RETURN.
        END.
        
     END.
     IF prmFgItem <> loadtag.i-no THEN DO:
                  cError = "To Item is different from From Item." .
                      RETURN .
     END.
     FIND FIRST fg-bin WHERE fg-bin.company = g_company
                            AND fg-bin.i-no = loadtag.i-no
                            AND fg-bin.tag = loadtag.tag-no
                            /*AND fg-bin.job-no = loadtag.job-no
                            AND fg-bin.job-no2 = loadtag.job-no2
                            AND fg-bin.qty > 0 */
                            NO-LOCK NO-ERROR.
     IF NOT AVAIL fg-bin THEN DO:
        cError = "Invalid Inventory for the tag2." .
        RETURN .
     END. */



/*
  
 END. /* prmcontrans cons screen  = yes  */
 ELSE do: /* prmcontrans <> yes   transfer screen */ 
   
  FIND FIRST fg-rctd WHERE fg-rctd.company = cocode 
      AND fg-rctd.rita-code = "T"
      AND fg-rctd.rec_key   = prmRecKey NO-LOCK NO-ERROR.

     IF prmTagno = "" THEN do:
         ASSIGN cError = "Tag No Can not Blank ".
      RETURN.
     END. */


  IF int(prmCases) <= 0 THEN DO:
     cError = "Unit Count must be greater than 0." .
     RETURN .
  END.

  
 /* FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
                             bf-tmp.rita-code = "T" AND
                             bf-tmp.tag = prmTagno
                         AND RECID(bf-tmp) <> RECID(fg-rctd) NO-LOCK NO-ERROR.
     IF AVAIL bf-tmp THEN DO:
        cError = "This Tag Number Has Already Been Used.  Please Enter A Unique Tag Number."  .
        RETURN .
     END.

     FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag THEN DO:
        cError = "Invalid Loadtag#. " .
        RETURN .
     END.
     FIND FIRST fg-bin WHERE fg-bin.company = cocode
                            AND fg-bin.i-no = loadtag.i-no
                            AND fg-bin.tag = loadtag.tag-no
                            /*AND fg-bin.job-no = loadtag.job-no
                            AND fg-bin.job-no2 = loadtag.job-no2*/
                            AND fg-bin.qty > 0
                            NO-LOCK NO-ERROR.
     IF NOT AVAIL fg-bin THEN DO:
        cError =  "Invalid Inventory for the tag." .
        RETURN .
     END. */

     

      FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ prmFgItem
        NO-LOCK NO-ERROR.
      IF NOT AVAIL itemfg THEN DO:
          cError = "Invalid entry, try help..." .
          RETURN .
      END.

      IF lv-msg EQ "" THEN
          IF prmLoc2 EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST loc
          WHERE loc.company EQ cocode
            AND loc.loc     EQ prmLoc2
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loc THEN lv-msg = "Invalid entry, try help".
    END.

    IF lv-msg NE "" THEN DO:
      cError = TRIM(lv-msg) + "..." .
      RETURN .
    END.
     

    
  IF lv-msg EQ "" THEN
      IF prmLocBin2 EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN
      IF prmLoc EQ prmLoc2   AND prmLocBin  EQ prmLocBin2 THEN
        lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ ""
            AND fg-bin.loc     EQ prmLoc2
            AND fg-bin.loc-bin EQ prmLocBin2
        USE-INDEX co-ino NO-LOCK NO-ERROR.

      IF NOT AVAIL fg-bin THEN lv-msg = "Invalid To Bin, try help...".
    END.

    IF lv-msg NE "" THEN DO:
      cError = TRIM(lv-msg) + "..."  .
      
      RETURN .
    END.

/* END.  /* prmcontrans <> yes   transfer screen */ */
              
END.  /*** update*/  

IF prmAction = "Update" THEN DO:

      
    FIND FIRST fg-rctd WHERE fg-rctd.company eq cocode and
                    fg-rctd.rita-code eq "T" AND
                    fg-rctd.rec_key = prmRecKey
                    EXCLUSIVE-LOCK NO-ERROR.
      
      IF AVAIL fg-rctd THEN
          ASSIGN
          fg-rctd.tag      = prmTagno                  
          fg-rctd.job-no   = prmJobno                 
          fg-rctd.job-no2  = int(prmJob_no2)
          fg-rctd.i-no     = prmFgItem                    
          fg-rctd.i-name   = prmName                 
          fg-rctd.loc      = prmLoc                 
          fg-rctd.loc-bin  = prmLocBin                 
          fg-rctd.cases    = int(prmCases)
          fg-rctd.qty-case = int(prmQty_Cas)
          fg-rctd.partial  = int(prmPartial)                   
          fg-rctd.cust-no  = prmCasUnit                 
          fg-rctd.loc2     = prmLoc2                  
          fg-rctd.loc-bin2 = prmLocBin2                 
          lv-tag2     = prmTagno2   .

      ASSIGN
          fg-rctd.t-qty = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial
          fg-rctd.tag2  = lv-tag2.

      

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    ASSIGN
     fg-rctd.pur-uom  = itemfg.prod-uom
     fg-rctd.cost-uom = itemfg.prod-uom
     fg-rctd.std-cost = itemfg.std-tot-cost.

  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ fg-rctd.i-no
        AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.cust-no EQ fg-rctd.cust-no
      NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
    ASSIGN
     fg-rctd.pur-uom  = fg-bin.pur-uom
     fg-rctd.cost-uom = fg-bin.pur-uom
     fg-rctd.std-cost = fg-bin.std-tot-cost
     fg-rctd.units-pallet = fg-bin.units-pallet
     fg-rctd.cases-unit   = fg-bin.cases-unit.

  ld = fg-rctd.std-cost.

  IF fg-rctd.pur-uom NE "EA" THEN
    RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                           ld, OUTPUT ld).

  fg-rctd.ext-cost = fg-rctd.t-qty * ld.


  IF AVAIL fg-bin                AND
     fg-bin.qty GT fg-rctd.t-qty AND
     fg-bin.tag NE ""            AND
     fg-bin.tag EQ fg-rctd.tag2  THEN 
    RUN fg/mkloadtg.p (ROWID(fg-rctd), 0, INPUT-OUTPUT fg-rctd.tag2).


      
      ASSIGN prmAction = "Select" .


END.  /**** update  ***/ 


IF prmAction = "validadd" THEN DO:
MESSAGE "come " .
    IF prmcontrans = "Yes" THEN DO:

     
     FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
                             bf-tmp.rita-code = "T" AND
                             bf-tmp.tag = prmTagno   NO-LOCK NO-ERROR.
     IF AVAIL bf-tmp THEN DO:
        cError =  "This Tag Number Has Already Been Used.  " +
                "Please Enter A Unique Tag Number."           .
                       RETURN .
     END.

     FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag THEN DO:
        FIND rfidtag WHERE rfidtag.company = g_company
                       AND rfidtag.rfidtag = prmTagno NO-LOCK NO-ERROR.
        IF AVAIL rfidtag THEN
           FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = rfidtag.tag-no NO-LOCK NO-ERROR.

        IF NOT AVAIL rfidtag OR NOT AVAIL loadtag THEN DO:
           cError = "Invalid Loadtag#. " .
           RETURN.
        END.
     END.

     FIND FIRST fg-bin WHERE fg-bin.company = cocode
                            AND fg-bin.i-no = loadtag.i-no
                            AND fg-bin.tag = loadtag.tag-no
                            /*AND fg-bin.job-no = loadtag.job-no
                            AND fg-bin.job-no2 = loadtag.job-no2*/
                            AND fg-bin.qty > 0
                            NO-LOCK NO-ERROR.
     IF NOT AVAIL fg-bin THEN DO:
        cError = "No On-Hand Qty for the tag." .
        RETURN .
     END.


   FIND FIRST loadtag WHERE loadtag.company = cocode 
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = prmTagno2 NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag THEN DO:
        FIND rfidtag WHERE rfidtag.company = cocode
                       AND rfidtag.rfidtag = prmTagno2 NO-LOCK NO-ERROR.
        IF AVAIL rfidtag THEN
           FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = rfidtag.tag-no NO-LOCK NO-ERROR.

        IF NOT AVAIL rfidtag and NOT AVAIL loadtag THEN DO:
           cError = "Invalid Loadtag2#. "  .
           RETURN.
        END.
        
     END.
    

     IF prmFgItem <> loadtag.i-no THEN DO:
                  cError = "To Item is different from From Item." .
                      RETURN .
     END.
     FIND FIRST fg-bin WHERE fg-bin.company = g_company
                            AND fg-bin.i-no = loadtag.i-no
                            AND fg-bin.tag = loadtag.tag-no
                            /*AND fg-bin.job-no = loadtag.job-no
                            AND fg-bin.job-no2 = loadtag.job-no2
                            AND fg-bin.qty > 0 */
                            NO-LOCK NO-ERROR.
     IF NOT AVAIL fg-bin THEN DO:
        cError = "Invalid Inventory for the tag2." .
        RETURN .
     END.




  
 END. /* prmcontrans cons screen  = yes  */
 ELSE do: /* prmcontrans <> yes   transfer screen */

    IF prmTagno = "" THEN do:
         ASSIGN cError = "Tag No Can not Blank ".
      RETURN.
     END.


  IF int(prmCases) <= 0 THEN DO:
     cError = "Unit Count must be greater than 0." .
     RETURN .
  END.

  FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
                             bf-tmp.rita-code = "T" AND
                             bf-tmp.tag = prmTagno NO-LOCK NO-ERROR.
     IF AVAIL bf-tmp THEN DO:
        cError = "This Tag Number Has Already Been Used.  Please Enter A Unique Tag Number."  .
        RETURN .
     END.

     FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag THEN DO:
        cError = "Invalid Loadtag#. " .
        RETURN .
     END.
     FIND FIRST fg-bin WHERE fg-bin.company = cocode
                            AND fg-bin.i-no = loadtag.i-no
                            AND fg-bin.tag = loadtag.tag-no
                            /*AND fg-bin.job-no = loadtag.job-no
                            AND fg-bin.job-no2 = loadtag.job-no2*/
                            AND fg-bin.qty > 0
                            NO-LOCK NO-ERROR.
     IF NOT AVAIL fg-bin THEN DO:
        cError =  "Invalid Inventory for the tag." .
        RETURN .
     END.


      FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ prmFgItem
        NO-LOCK NO-ERROR.
      IF NOT AVAIL itemfg THEN DO:
          cError = "Invalid entry, try help..." .
          RETURN .
      END.

      IF lv-msg EQ "" THEN
      IF prmLoc2 EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST loc
          WHERE loc.company EQ cocode
            AND loc.loc     EQ prmLoc2
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loc THEN lv-msg = "Invalid entry, try help".
    END.

    IF lv-msg NE "" THEN DO:
      cError = TRIM(lv-msg) + "..." .
      RETURN .
    END.
     

    IF lv-msg EQ "" THEN
      IF prmLocBin2 EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN
      IF prmLoc EQ prmLoc2   AND prmLocBin  EQ prmLocBin2 THEN
        lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ ""
            AND fg-bin.loc     EQ prmLoc2
            AND fg-bin.loc-bin EQ prmLocBin2
        USE-INDEX co-ino NO-LOCK NO-ERROR.

      IF NOT AVAIL fg-bin THEN lv-msg = "Invalid To Bin, try help...".
    END.

    IF lv-msg NE "" THEN DO:
      cError = TRIM(lv-msg) + "..."  .
      
      RETURN .
    END.
 END. /* prmcontrans <> yes   transfer screen */

END. /* validate add  */

IF prmAction = "Addnewrcpt" THEN DO:
 DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  /*DEF BUFFER b-fg-rctd FOR fg-rctd.*/
 
  FOR EACH bf-rctd WHERE bf-rctd.company EQ cocode NO-LOCK
      USE-INDEX fg-rctd
      BY bf-rctd.r-no DESC:
    lv-rno = bf-rctd.r-no.
    LEAVE.
  END.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  CREATE fg-rctd .
  
    ASSIGN 
     fg-rctd.rct-date    = TODAY
     fg-rctd.trans-time   = TIME
     fg-rctd.s-num        = 0
     fg-rctd.units-pallet = 1
     fg-rctd.cases-unit   = 1
     fg-rctd.ext-cost     = 0
     fg-rctd.partial      = 0
     fg-rctd.qty          = 0
     fg-rctd.qty-case     = 0.

  ASSIGN
   fg-rctd.company   = cocode
   fg-rctd.r-no      = lv-rno + 1
   fg-rctd.rita-code = "T"
   /* gdm - */
   fg-rctd.trans-time   = TIME .

      FIND FIRST reftable
         WHERE reftable.reftable EQ "fg-rctd.user-id"
           AND reftable.company  EQ fg-rctd.company
           AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
         NO-ERROR.
     IF NOT AVAIL reftable THEN DO:
       CREATE reftable.
       ASSIGN
        reftable.reftable = "fg-rctd.user-id"
        reftable.company  = fg-rctd.company
        reftable.loc      = STRING(fg-rctd.r-no,"9999999999")
        reftable.code     = prmUser.
     END.
     ASSIGN
      reftable.code2        = prmUser
      fg-rctd.upd-date = TODAY
      fg-rctd.upd-time = TIME.

      IF AVAIL fg-rctd THEN
          ASSIGN
          fg-rctd.tag      = prmTagno                  
          fg-rctd.job-no   = prmJobno                 
          fg-rctd.job-no2  = int(prmJob_no2)
          fg-rctd.i-no     = prmFgItem                    
          fg-rctd.i-name   = prmName                 
          fg-rctd.loc      = prmLoc                 
          fg-rctd.loc-bin  = prmLocBin                 
          fg-rctd.cases    = int(prmCases)
          fg-rctd.qty-case = int(prmQty_Cas)
          fg-rctd.partial  = int(prmPartial)                   
          fg-rctd.cust-no  = prmCasUnit                 
          fg-rctd.loc2     = prmLoc2                  
          fg-rctd.loc-bin2 = prmLocBin2                 
          lv-tag2     = prmTagno2   .

      ASSIGN
   fg-rctd.t-qty = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial
   fg-rctd.tag2  = lv-tag2.

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    ASSIGN
     fg-rctd.pur-uom  = itemfg.prod-uom
     fg-rctd.cost-uom = itemfg.prod-uom
     fg-rctd.std-cost = itemfg.std-tot-cost.

  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ fg-rctd.i-no
        AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.cust-no EQ fg-rctd.cust-no
      NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
    ASSIGN
     fg-rctd.pur-uom  = fg-bin.pur-uom
     fg-rctd.cost-uom = fg-bin.pur-uom
     fg-rctd.std-cost = fg-bin.std-tot-cost
     fg-rctd.units-pallet = fg-bin.units-pallet
     fg-rctd.cases-unit   = fg-bin.cases-unit.

  ld = fg-rctd.std-cost.

  IF fg-rctd.pur-uom NE "EA" THEN
    RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                           ld, OUTPUT ld).

  fg-rctd.ext-cost = fg-rctd.t-qty * ld.


  IF AVAIL fg-bin                AND
     fg-bin.qty GT fg-rctd.t-qty AND
     fg-bin.tag NE ""            AND
     fg-bin.tag EQ fg-rctd.tag2  THEN 
    RUN fg/mkloadtg.p (ROWID(fg-rctd), 0, INPUT-OUTPUT fg-rctd.tag2).


     ASSIGN prmAction = "Select"
         prmRecKey  = string(fg-rctd.rec_key) .

END.



IF prmAction = "Delete" THEN DO:
     DEF VAR v-trans-tag-no AS cha NO-UNDO.
    
    FIND FIRST fg-rctd WHERE fg-rctd.company eq cocode and 
           fg-rctd.rita-code eq "T" AND
           fg-rctd.rec_key  EQ prmRecKey EXCLUSIVE-LOCK NO-ERROR.
    v-trans-tag-no = "".
    IF fg-rctd.tag <> fg-rctd.tag2 THEN DO:
        v-trans-tag-no = fg-rctd.tag2.
    END.
      
      IF AVAIL fg-rctd THEN 
          DELETE fg-rctd .

    IF v-trans-tag-no <> "" THEN DO:
     FIND FIRST loadtag WHERE loadtag.company = cocode
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = v-trans-tag-no EXCLUSIVE-LOCK NO-ERROR.      

     IF AVAIL loadtag AND loadtag.sts = "Transfered" THEN DELETE loadtag.
    END.

      
        FIND LAST fg-rctd WHERE 
            fg-rctd.company eq cocode and 
            fg-rctd.rita-code eq "T"
            NO-LOCK NO-ERROR .
        IF AVAIL fg-rctd THEN
            ASSIGN
            prmRecKey  = fg-rctd.rec_key 
            prmAction = "Select".

END.  /* end of delete */


 IF prmAction = "Select" THEN DO:
  
  
  FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and
       fg-rctd.rita-code eq "T" AND
       fg-rctd.rec_key  EQ prmRecKey  NO-LOCK,
      EACH reftable WHERE reftable.company EQ fg-rctd.company 
        AND reftable.reftable EQ "fg-rctd.user-id"  
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.loc EQ STRING(fg-rctd.r-no,"9999999999") 
        AND reftable.code EQ prmUser
        AND reftable.reftable EQ "fg-rctd.user-id" 
        AND reftable.company  EQ fg-rctd.company 
        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
        AND ( NOT reftable.dscr begins "fg-rctd: ") NO-LOCK :

            create ttTrnsViewFGrece.
            assign
                ttTrnsViewFGrece.vDate           = string(fg-rctd.rct-date)
                ttTrnsViewFGrece.vTransTime      = STRING(fg-rctd.trans-time,'HH:MM')
                ttTrnsViewFGrece.vTag            = fg-rctd.tag
                ttTrnsViewFGrece.vJobno          = fg-rctd.job-no
                ttTrnsViewFGrece.vJobno2         = fg-rctd.job-no2
                ttTrnsViewFGrece.vItem           = fg-rctd.i-no
                ttTrnsViewFGrece.vItemName       = fg-rctd.i-name
                ttTrnsViewFGrece.vLoc            = fg-rctd.loc 
                ttTrnsViewFGrece.vLocBin         = fg-rctd.loc-bin
                ttTrnsViewFGrece.vCases          = fg-rctd.cases     
                ttTrnsViewFGrece.vQtyCas         = fg-rctd.qty-case
                ttTrnsViewFGrece.vPartial        = fg-rctd.partial
                ttTrnsViewFGrece.vcust           = fg-rctd.cust-no  
                ttTrnsViewFGrece.vLoc2           = fg-rctd.loc2
                ttTrnsViewFGrece.vLocBin2        = fg-rctd.loc-bin2
                ttTrnsViewFGrece.vTag2           = fg-rctd.tag2
                ttTrnsViewFGrece.blan            = fg-rctd.b-num
                ttTrnsViewFGrece.usrcrt          = reftable.code
                ttTrnsViewFGrece.usrupdt         = reftable.code2
                ttTrnsViewFGrece.vRecKey         = fg-rctd.rec_key
                .
                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/


