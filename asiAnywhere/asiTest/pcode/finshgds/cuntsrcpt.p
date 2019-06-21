

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

DEFINE TEMP-TABLE ttCountsRcpt NO-UNDO
        FIELD vRno            AS INT 
        FIELD vcntDate        AS CHAR  
        FIELD vcntTime        AS CHAR   
        FIELD vTag            AS CHAR FORMAT "x(20)"
        FIELD vPono           AS CHAR FORMAT "x(9)"
        FIELD vJobno          AS CHAR FORMAT "x(6)"  
        FIELD vJobno2         AS INT
        FIELD vItem           AS CHAR FORMAT "x(15)"
        FIELD vItemName       AS CHAR FORMAT "x(30)"
        FIELD vLoc            AS CHAR FORMAT "x(5)" 
        FIELD vLocBin         AS CHAR FORMAT "x(8)"  
        FIELD vCases          AS INT 
        FIELD vQtyCas         AS INT 
        FIELD vCasUnit        AS INT    
        FIELD vPartial        AS INT    
        FIELD vTQty           AS DEC
        FIELD vCreatedBy      AS CHAR FORMAT "x(9)"
        FIELD vCreate2        AS CHAR FORMAT "x(9)"
        FIELD vRecKey         AS CHAR 
        .

DEFINE DATASET dsCountsRcpt FOR ttCountsRcpt .

DEFINE INPUT PARAMETER prmUser        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmName        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno2      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmPono        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqno       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmloc         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmlocbin      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTqty        AS DEC         NO-UNDO.
DEFINE INPUT PARAMETER prmCases       AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmQty_Cas     AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCasUnit     AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmPartial     AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmllSetParts  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTransTime   AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError        AS CHAR        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCountsRcpt.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.

IF prmUser        = ? THEN ASSIGN prmUser     = "".
IF prmAction      = ? THEN ASSIGN prmAction   = "Select".
IF prmFgItem      = ? THEN ASSIGN prmFgItem   = "".
IF prmName        = ? THEN ASSIGN prmName     = "".
IF prmJobno       = ? THEN ASSIGN prmJobno    = "". 
IF prmJobno2      = ? THEN ASSIGN prmJobno2   = 0.
IF prmPono        = ? THEN ASSIGN prmPono     = "".
IF prmSeqno       = ? THEN ASSIGN prmSeqno    = "".
IF prmRcptDate    = ? THEN ASSIGN prmRcptDate = "".
IF prmTagno       = ? THEN ASSIGN prmTagno    = "".
IF prmloc         = ? THEN ASSIGN prmloc      = "".
IF prmlocbin      = ? THEN ASSIGN prmlocbin   = "".
IF prmTqty        = ? THEN ASSIGN prmTqty     = 0.
IF prmCases       = ? THEN ASSIGN prmCases    = 0.
IF prmQty_Cas     = ? THEN ASSIGN prmQty_Cas  = 0.
IF prmCasUnit     = ? THEN ASSIGN prmCasUnit  = 0.
IF prmPartial     = ? THEN ASSIGN prmPartial  = 0.
IF prmRecKey      = ? THEN ASSIGN prmRecKey   = "".
IF prmllSetParts  = ? THEN ASSIGN prmllSetParts = "".
IF prmTransTime   = ? THEN ASSIGN prmTransTime  = "".
IF cError          = ? THEN ASSIGN cError         = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp 
    vuser = prmUser.

DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
DEF BUFFER bf-tmp FOR fg-rctd.
DEF VAR lv-wh-list AS cha  NO-UNDO.
DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b2-fg-rctd FOR fg-rctd.
DEF BUFFER b-reftable FOR reftable .
def var lv-recid as recid no-undo.

   
   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 
                                                  

 IF prmAction = "GridSelect" THEN DO:
  
 
    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
        fg-rctd.rita-code eq "C" 
        NO-LOCK, 
        EACH reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                NO-LOCK: 
             
        
            create ttCountsRcpt.
            assign
                ttCountsRcpt.vRno            = fg-rctd.r-no
                ttCountsRcpt.vcntDate        = string(fg-rctd.rct-date)
                ttCountsRcpt.vcntTime        = STRING(fg-rctd.trans-time,'HH:MM')
                ttCountsRcpt.vTag            = fg-rctd.tag
                ttCountsRcpt.vPono           = fg-rctd.po-no
                ttCountsRcpt.vJobno          = fg-rctd.job-no
                ttCountsRcpt.vJobno2         = fg-rctd.job-no2
                ttCountsRcpt.vItem           = fg-rctd.i-no
                ttCountsRcpt.vItemName       = fg-rctd.i-name
                ttCountsRcpt.vLoc            = fg-rctd.loc 
                ttCountsRcpt.vLocBin         = fg-rctd.loc-bin
                ttCountsRcpt.vCases          = fg-rctd.cases 
                ttCountsRcpt.vQtyCas         = fg-rctd.qty-case 
                ttCountsRcpt.vCasUnit        = fg-rctd.cases-unit  
                ttCountsRcpt.vPartial        = fg-rctd.partial
                ttCountsRcpt.vTQty           = fg-rctd.t-qty
                ttCountsRcpt.vCreatedBy      = reftable.code
                ttCountsRcpt.vCreate2        = reftable.code2  
                ttCountsRcpt.vRecKey         = fg-rctd.rec_key .
            


                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
  
 IF prmAction = "GridSearch" THEN DO:

 
    FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.rita-code eq "C" AND
           (fg-rctd.r-no = int(prmSeqno) OR prmSeqno = "" ) AND
           (fg-rctd.i-no BEGINS prmFgItem OR prmFgItem = "" ) AND
           (fg-rctd.job-no = prmJobno OR prmJobno = "" ) AND
           (fg-rctd.rct-date = DATE(prmRcptDate) OR prmRcptDate = "" ) AND
           (fg-rctd.tag BEGINS prmTagno OR prmTagno = "" )
           use-index fg-rctd NO-LOCK, 
       EACH reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")  NO-LOCK: 
             

            create ttCountsRcpt.
            assign
                ttCountsRcpt.vRno            = fg-rctd.r-no
                ttCountsRcpt.vcntDate           = string(fg-rctd.rct-date)
                ttCountsRcpt.vcntTime      = STRING(fg-rctd.trans-time,'HH:MM')
                ttCountsRcpt.vTag            = fg-rctd.tag
                ttCountsRcpt.vPono           = fg-rctd.po-no
                ttCountsRcpt.vJobno          = fg-rctd.job-no
                ttCountsRcpt.vJobno2         = fg-rctd.job-no2
                ttCountsRcpt.vItem           = fg-rctd.i-no
                ttCountsRcpt.vItemName       = fg-rctd.i-name
                ttCountsRcpt.vLoc            = fg-rctd.loc 
                ttCountsRcpt.vLocBin         = fg-rctd.loc-bin
                ttCountsRcpt.vCases          = fg-rctd.cases 
                ttCountsRcpt.vQtyCas         = fg-rctd.qty-case 
                ttCountsRcpt.vCasUnit        = fg-rctd.cases-unit  
                ttCountsRcpt.vPartial        = fg-rctd.partial
                ttCountsRcpt.vTQty           = fg-rctd.t-qty
                ttCountsRcpt.vCreatedBy      = reftable.code
                ttCountsRcpt.vCreate2        = reftable.code2  
                ttCountsRcpt.vRecKey         = fg-rctd.rec_key .
               
                  
                END.   /* end of for loop*/
            
            
 END. /* end search */


IF prmAction = "Update" THEN DO:

    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
        fg-rctd.rita-code eq "C" AND
        fg-rctd.r-no  EQ int(prmSeqno)
        NO-LOCK, 
        EACH reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                NO-LOCK:


        IF prmTagno = "" THEN do:
            ASSIGN cError = "Tag No Can not Blank ".
            RETURN.
        END.


        FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
            bf-tmp.rita-code = "C" AND
            bf-tmp.tag = prmTagno
            AND RECID(bf-tmp) <> RECID(fg-rctd) 
            NO-LOCK NO-ERROR.

        
        IF AVAIL bf-tmp THEN DO:
            cError = "This Tag Number Has Already Been Used. Please Enter A Unique Tag Number." .
            RETURN .
        END.
        FIND FIRST loadtag WHERE loadtag.company = cocode
            AND loadtag.ITEM-type = NO
            AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
        IF NOT AVAIL loadtag THEN DO:
            cError = "Invalid Loadtag#. ".
            RETURN .
        END.
        
         IF NOT CAN-FIND(FIRST itemfg
                         where itemfg.company  = cocode 
                         AND itemfg.i-no EQ prmFgItem)
             THEN DO:
             cError = "Invalid entry, try help...".
             RETURN.
         END.

         prmJobno = FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno).
             

         IF prmJobno NE "" THEN DO:
             FIND FIRST job-hdr
                 WHERE job-hdr.company EQ fg-rctd.company
                 AND job-hdr.job-no    EQ prmJobno
                 AND job-hdr.i-no      EQ prmFgItem
                 NO-LOCK NO-ERROR.
             
             IF NOT AVAIL job-hdr THEN DO:
                 cError = "Invalid Job#. Try Help...".
                 RETURN.
             END.
         END.

         IF prmJobno NE "" THEN DO:
             FIND FIRST job-hdr
                 WHERE job-hdr.company EQ fg-rctd.company
                 AND job-hdr.job-no  EQ prmJobno
                 AND job-hdr.job-no2 EQ prmJobno2
                 AND job-hdr.i-no    EQ prmFgItem
                 NO-LOCK NO-ERROR.

             IF NOT AVAIL job-hdr THEN DO:
                 cError = "Invalid Job#. Try Help..." .
                 RETURN.
             END.
         END.

         IF NOT CAN-FIND(FIRST loc
                         WHERE loc.company EQ cocode
                         AND loc.loc       EQ prmloc) THEN DO:
             
             cError = "Invalid Warehouse, try help..." .
             RETURN.
         END.

         IF NOT CAN-FIND(FIRST fg-bin
                         WHERE fg-bin.company EQ cocode 
                         AND fg-bin.i-no      EQ ""
                         AND fg-bin.loc       EQ prmloc
                         AND fg-bin.loc-bin   EQ prmlocbin
                         USE-INDEX co-ino) THEN DO:

             cError = "Invalid Bin#, try help...".
             RETURN.
         END.

         prmTqty = (prmCases * prmQty_Cas) + (prmPartial).


         FIND FIRST loadtag WHERE loadtag.company = cocode
             AND loadtag.ITEM-type = NO
             AND loadtag.tag-no = prmTagno
             NO-LOCK NO-ERROR.

         IF CAN-FIND(FIRST fg-bin
                     WHERE fg-bin.company EQ cocode 
                     AND fg-bin.i-no      EQ prmFgItem
                     AND fg-bin.tag       EQ prmTagno
                     AND fg-bin.job-no    EQ fg-rctd.job-no
                     AND fg-bin.job-no2   EQ fg-rctd.job-no2
                     AND (fg-bin.loc      NE prmloc
                          OR  fg-bin.loc-bin NE prmlocbin)
                     /*AND fg-bin.qty > 0*/
                     USE-INDEX tag)
             AND AVAIL loadtag AND (loadtag.loc <> prmloc OR 
                                    loadtag.loc-bin <> prmlocbin)
             
             THEN RUN crt-transfer.
    END.


END. /*IF prmAction = "Update"*/


IF prmAction = "Update" THEN DO:
    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
        fg-rctd.rita-code eq "C"  AND
        fg-rctd.r-no  EQ int(prmSeqno)
        EXCLUSIVE-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                EXCLUSIVE-LOCK:

     
          ASSIGN
            fg-rctd.r-no        = INT(prmSeqno)
            fg-rctd.rct-date    = DATE(prmRcptDate)   
            fg-rctd.trans-time  = TIME           
            fg-rctd.tag         = prmTagno            
            fg-rctd.po-no       = prmPono             
            fg-rctd.job-no      = prmJobno            
            fg-rctd.job-no2     = prmJobno2           
            fg-rctd.i-no        = prmFgItem           
            fg-rctd.i-name      = prmName             
            fg-rctd.loc         = prmloc              
            fg-rctd.loc-bin     = prmlocbin           
            fg-rctd.cases       = prmCases            
            fg-rctd.qty-case    = prmQty_Cas          
            fg-rctd.cases-unit  = prmCasUnit          
            fg-rctd.partial     = prmPartial 
            fg-rctd.t-qty       = prmTqty    
            reftable.code2      = prmUser    .

      
    END.
      ASSIGN prmAction = "Select" .


END.  /**** update  ***/ 


IF prmAction = "Addtrns" THEN Do:

    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
        fg-rctd.rita-code eq "C" 
        NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                NO-LOCK:


        IF prmTagno = "" THEN do:
            ASSIGN cError = "Tag No Can not Blank ".
            RETURN.
        END.


        FIND FIRST bf-tmp WHERE bf-tmp.company = cocode AND
            bf-tmp.rita-code = "C" AND
            bf-tmp.tag = prmTagno
            AND RECID(bf-tmp) <> RECID(fg-rctd)
            NO-LOCK NO-ERROR.
        IF AVAIL bf-tmp THEN DO:
            cError = "This Tag Number Has Already Been Used. Please Enter A Unique Tag Number." .
            RETURN .
        END.
        FIND FIRST loadtag WHERE loadtag.company = cocode
            AND loadtag.ITEM-type = NO
            AND loadtag.tag-no = prmTagno NO-LOCK NO-ERROR.
        IF NOT AVAIL loadtag THEN DO:
            cError = "Invalid Loadtag#. ".
            RETURN .
        END.
        


         IF NOT CAN-FIND(FIRST itemfg
                         where itemfg.company  = cocode 
                         AND itemfg.i-no EQ prmFgItem)
             THEN DO:
             cError = "Invalid entry, try help...".
             RETURN.
         END.

         prmJobno = FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno).
             

         IF prmJobno NE "" THEN DO:
             FIND FIRST job-hdr
                 WHERE job-hdr.company EQ fg-rctd.company
                 AND job-hdr.job-no    EQ prmJobno
                 AND job-hdr.i-no      EQ prmFgItem
                 NO-LOCK NO-ERROR.
             
             IF NOT AVAIL job-hdr THEN DO:
                 cError = "Invalid Job#. Try Help...".
                 RETURN.
             END.
         END.

         IF prmJobno NE "" THEN DO:
             FIND FIRST job-hdr
                 WHERE job-hdr.company EQ fg-rctd.company
                 AND job-hdr.job-no  EQ prmJobno
                 AND job-hdr.job-no2 EQ prmJobno2
                 AND job-hdr.i-no    EQ prmFgItem
                 NO-LOCK NO-ERROR.

             IF NOT AVAIL job-hdr THEN DO:
                 cError = "Invalid Job#. Try Help..." .
                 RETURN.
             END.
         END.

         IF NOT CAN-FIND(FIRST loc
                         WHERE loc.company EQ cocode
                         AND loc.loc       EQ prmloc) THEN DO:
             
             cError = "Invalid Warehouse, try help..." .
             RETURN.
         END.

         IF NOT CAN-FIND(FIRST fg-bin
                         WHERE fg-bin.company EQ cocode 
                         AND fg-bin.i-no      EQ ""
                         AND fg-bin.loc       EQ prmloc
                         AND fg-bin.loc-bin   EQ prmlocbin
                         USE-INDEX co-ino) THEN DO:

             cError = "Invalid Bin#, try help...".
             RETURN.
         END.

         prmTqty = (prmCases * prmQty_Cas) + (prmPartial).


         /*FIND FIRST loadtag WHERE loadtag.company = cocode
             AND loadtag.ITEM-type = NO
             AND loadtag.tag-no = prmTagno
             NO-LOCK NO-ERROR.

         IF CAN-FIND(FIRST fg-bin
                     WHERE fg-bin.company EQ cocode 
                     AND fg-bin.i-no      EQ prmFgItem
                     AND fg-bin.tag       EQ prmTagno
                     AND fg-bin.job-no    EQ fg-rctd.job-no
                     AND fg-bin.job-no2   EQ fg-rctd.job-no2
                     AND (fg-bin.loc      NE prmloc
                          OR  fg-bin.loc-bin NE prmlocbin
                     /*AND fg-bin.qty > 0*/
                     USE-INDEX tag)
             AND AVAIL loadtag AND (loadtag.loc <> prmloc OR 
                                    loadtag.loc-bin <> prmlocbin)
             
             THEN /*MESSAGE "Reduce All Existing Bin Location To Zero Qty?"
                 VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-crt-transfer*/ */

    END.
         
END. /* if prmaction="addrcpt"*/


IF prmAction = "Addnewrcpt" THEN DO:

    lv-rno = 0.
 
    FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

    
    DO WHILE TRUE:
        lv-rno = lv-rno + 1.
        FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth THEN NEXT.
        FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL b-fg-rctd THEN NEXT.
        LEAVE.
    END.
 CREATE fg-rctd .
    assign fg-rctd.company = cocode
        fg-rctd.r-no    = lv-rno
        fg-rctd.rita-code = "C"
        fg-rctd.s-num  = 0
        fg-rctd.rct-date = today
        fg-rctd.trans-time = TIME 
        fg-rctd.cases-unit = 1.


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
     
    lv-recid = recid(fg-rctd).  

    ASSIGN
        prmAction = "Select" 
        prmSeqno  =  string(fg-rctd.r-no).





END. /*IF prmAction = "Addnewrcpt"*/


IF prmAction = "Addrcpt" THEN DO:
    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
        fg-rctd.rita-code eq "C" AND
        fg-rctd.r-no  EQ int(prmSeqno)
        EXCLUSIVE-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                 EXCLUSIVE-LOCK:
      
          ASSIGN
            fg-rctd.r-no        = int(prmSeqno)
            fg-rctd.rct-date    = date(prmRcptDate)   
            fg-rctd.trans-time  = TIME                
            fg-rctd.tag         = prmTagno            
            fg-rctd.po-no       = prmPono             
            fg-rctd.job-no      = prmJobno            
            fg-rctd.job-no2     = prmJobno2           
            fg-rctd.i-no        = prmFgItem           
            fg-rctd.i-name      = prmName             
            fg-rctd.loc         = prmloc              
            fg-rctd.loc-bin     = prmlocbin           
            fg-rctd.cases       = prmCases            
            fg-rctd.qty-case    = prmQty_Cas          
            fg-rctd.cases-unit  = prmCasUnit          
            fg-rctd.partial     = prmPartial 
            fg-rctd.t-qty       = prmTqty    
            reftable.code2      = prmUser    .

    END.


    ASSIGN prmAction = "Select".

END. /*IF prmAction = "Addrcpt"*/


IF prmAction = "Delete" THEN DO:

    FIND FIRST fg-rctd WHERE 
        fg-rctd.company eq cocode and
        fg-rctd.rita-code eq "C"  AND
        fg-rctd.r-no  EQ int(prmRecKey)
        EXCLUSIVE-LOCK NO-ERROR. 
    FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                     NO-LOCK.

    IF AVAIL fg-rctd THEN 
          DELETE fg-rctd .

    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and
        fg-rctd.rita-code eq "C" 
        NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                              NO-LOCK:


        ASSIGN 
            prmAction = "Select"
            prmSeqno  = string(fg-rctd.r-no).
        LEAVE.
    END.

END. /*IF prmAction = "Deletercpt"*/


IF prmAction = "Select" THEN DO:
    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and
        fg-rctd.rita-code eq "C" AND
        fg-rctd.r-no  EQ int(prmSeqno)
        NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                 NO-LOCK:

       create ttCountsRcpt.
          ASSIGN
                ttCountsRcpt.vRno            = fg-rctd.r-no
                ttCountsRcpt.vcntDate        = string(fg-rctd.rct-date)
                ttCountsRcpt.vcntTime        = STRING(fg-rctd.trans-time,'HH:MM')
                ttCountsRcpt.vTag            = fg-rctd.tag
                ttCountsRcpt.vPono           = fg-rctd.po-no
                ttCountsRcpt.vJobno          = fg-rctd.job-no
                ttCountsRcpt.vJobno2         = fg-rctd.job-no2
                ttCountsRcpt.vItem           = fg-rctd.i-no
                ttCountsRcpt.vItemName       = fg-rctd.i-name
                ttCountsRcpt.vLoc            = fg-rctd.loc 
                ttCountsRcpt.vLocBin         = fg-rctd.loc-bin
                ttCountsRcpt.vCases          = fg-rctd.cases 
                ttCountsRcpt.vQtyCas         = fg-rctd.qty-case 
                ttCountsRcpt.vCasUnit        = fg-rctd.cases-unit  
                ttCountsRcpt.vPartial        = fg-rctd.partial
                ttCountsRcpt.vTQty           = fg-rctd.t-qty
                ttCountsRcpt.vCreatedBy      = reftable.code
                ttCountsRcpt.vCreate2        = reftable.code2  
                ttCountsRcpt.vRecKey         = fg-rctd.rec_key .
    END.



END. /* IF prmAction = "Select" THEN DO:*/


IF prmAction = "Transfer" THEN DO:
     
    lv-wh-list = prmLoc + ","  + prmLocBin + "," +  
                 prmllSetParts + "," + prmTransTime + "," .


    
    FOR EACH fg-rctd WHERE 
        fg-rctd.company eq cocode and 
        fg-rctd.rita-code eq "C" AND
        fg-rctd.r-no  EQ int(prmSeqno)
        NO-LOCK, 
        EACH reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
                             reftable.company  EQ fg-rctd.company AND 
                             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
                 NO-LOCK:
    
        
        DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.
        
        lv-rno = 0.
        FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.
    
        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
    
        DO WHILE TRUE:
          lv-rno = lv-rno + 1.
          FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
          IF AVAIL fg-rcpth THEN NEXT.
          FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
          IF AVAIL b-fg-rctd THEN NEXT.
          LEAVE.
        END.
    
        /*FOR EACH b-fg-rctd WHERE recid(b-fg-rctd) <> RECID(fg-rctd) 
                           AND b-fg-rctd.i-no = fg-rctd.i-no
                           AND b-fg-rctd.tag = fg-rctd.tag:
          DELETE b-fg-rctd.
        END.
        */
        FOR EACH b-fg-rctd WHERE b-fg-rctd.company = cocode 
                             AND b-fg-rctd.rita-code = "C" 
                             AND b-fg-rctd.i-no = fg-rctd.i-no
                             AND b-fg-rctd.loc = ENTRY(1,lv-wh-list)
                             AND b-fg-rctd.loc-bin = ENTRY(2,lv-wh-list)
                      :
            /*,
          EACH fg-bin WHERE fg-bin.company EQ cocode 
                          AND fg-bin.i-no    EQ b-fg-rctd.i-no
                          AND fg-bin.job-no = b-fg-rctd.job-no
                          AND fg-bin.job-no2 = b-fg-rctd.job-no2 
                          AND fg-bin.tag     EQ b-fg-rctd.tag
                          /*AND fg-bin.qty > 0*/  NO-LOCK:  
         IF fg-bin.loc NE b-fg-rctd.loc OR  fg-bin.loc-bin NE b-fg-rctd.loc-bin
         THEN DO:
         
             CREATE b2-fg-rctd.
             BUFFER-COPY b-fg-rctd EXCEPT b-fg-rctd.r-no TO b2-fg-rctd.
             ASSIGN b2-fg-rctd.r-no = lv-rno
                    b2-fg-rctd.loc = fg-bin.loc
                    b2-fg-rctd.loc-bin = fg-bin.loc-bin
                    b2-fg-rctd.cases = 0
                    b2-fg-rctd.qty-case = 0
                    b2-fg-rctd.cases-unit = 0
                    b2-fg-rctd.partial = 0
                    b2-fg-rctd.t-qty = 0.
             lv-rno = lv-rno + 1.
             
         END. */
             CREATE b2-fg-rctd.
             BUFFER-COPY b-fg-rctd EXCEPT b-fg-rctd.r-no TO b2-fg-rctd.
             ASSIGN b2-fg-rctd.r-no = lv-rno
                    b2-fg-rctd.loc = entry(3,lv-wh-list)
                    b2-fg-rctd.loc-bin = ENTRY(4,lv-wh-list)
                    b-fg-rctd.cases = 0
                    b-fg-rctd.partial = 0
                    b-fg-rctd.t-qty = 0.
             lv-rno = lv-rno + 1.
            
             FIND FIRST b-reftable WHERE b-reftable.reftable EQ "fg-rctd.user-id" AND 
                             b-reftable.company  EQ b2-fg-rctd.company AND 
                             b-reftable.loc      EQ STRING(b2-fg-rctd.r-no,"9999999999") EXCLUSIVE-LOCK NO-ERROR.

               IF NOT AVAIL b-reftable THEN DO:
                   CREATE b-reftable.
                   ASSIGN
                       b-reftable.reftable = "fg-rctd.user-id"
                       b-reftable.company  = b2-fg-rctd.company
                       b-reftable.loc      = STRING(b2-fg-rctd.r-no,"9999999999")
                       b-reftable.code     = prmUser   .
                   END.
                   ASSIGN
                       b-reftable.code2        = prmUser  .

        END.  /* for each fg-bin*/
    
    END.
  




END. /*IF prmAction = "Transfer" THEN DO:*/





PROCEDURE crt-transfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rno = 0.
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  FOR EACH b-fg-rctd WHERE recid(b-fg-rctd) <> RECID(fg-rctd) 
                       AND b-fg-rctd.i-no = fg-rctd.i-no
                       AND b-fg-rctd.tag = fg-rctd.tag:
      DELETE b-fg-rctd.
  END.

  FOR EACH fg-bin WHERE fg-bin.company EQ cocode 
                      AND fg-bin.i-no    EQ prmFgItem
                      AND fg-bin.job-no = fg-rctd.job-no
                      AND fg-bin.job-no2 = fg-rctd.job-no2 
                      AND fg-bin.tag     EQ prmTagno
                      /*AND fg-bin.qty > 0*/  NO-LOCK:

     IF fg-bin.loc NE prmloc
        OR  fg-bin.loc-bin NE prmlocbin
     THEN DO:
         CREATE b-fg-rctd.
         BUFFER-COPY fg-rctd EXCEPT fg-rctd.r-no TO b-fg-rctd.
         ASSIGN b-fg-rctd.r-no = lv-rno
                b-fg-rctd.loc = fg-bin.loc
                b-fg-rctd.loc-bin = fg-bin.loc-bin
                b-fg-rctd.cases = 0
                b-fg-rctd.qty-case = 0
                b-fg-rctd.cases-unit = 0
                b-fg-rctd.partial = 0
                b-fg-rctd.t-qty = 0.
         lv-rno = lv-rno + 1.
     END.
  END.  /* for each fg-bin*/

  lv-rctd-rowid = ROWID(fg-rctd).
  

END PROCEDURE.
