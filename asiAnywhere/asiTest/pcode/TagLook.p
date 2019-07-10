


/*------------------------------------------------------------------------
    File        : TagLook.p
    Purpose     : tag

    Syntax      :

    Description : Return a Dataset of all tags

    Author(s)   : Jyoti
    Created     : dec 05 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTagLook NO-UNDO 
    FIELD vTagNo   AS CHARACTER    
    FIELD vIno   AS CHARACTER
    FIELD vIName     AS CHARACTER
    FIELD vJobNo    AS CHARACTER
    FIELD vJobNo2    AS INTEGER
    FIELD vLoc      AS CHARACTER
    FIELD vLocBin      AS CHARACTER
    FIELD vOrdNo      AS INTEGER
    FIELD vPoNo      AS CHARACTER
    FIELD vQty      AS INTEGER
    FIELD vQtyCase      AS INTEGER
    FIELD vPalletCount      AS INTEGER
    FIELD vLine     AS INTEGER
    FIELD vUnits     AS INTEGER
    FIELD vCustomer     AS CHARACTER
    FIELD vCasesUnit     AS INTEGER
    FIELD vPartial     AS DECIMAL
    FIELD vTagNo2   AS CHARACTER
    .

DEF TEMP-TABLE tt-ldtag LIKE loadtag
           FIELD tag-recid AS RECID.
DEF TEMP-TABLE tt-rell LIKE oe-rell
           FIELD rell-recid AS RECID.

DEFINE DATASET dsTagLook FOR ttTagLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRelease   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmTag       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cError      AS CHARACTER NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTagLook .
       
IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmRelease   = ? THEN ASSIGN prmRelease      = 0.
IF prmTag      = ? THEN ASSIGN prmTag      = "".

DEF VAR lv-do-leave-tag AS LOG NO-UNDO.
DEF VAR v-ord-no AS INT NO-UNDO.

{sys/inc/var.i new shared}

DEF TEMP-TABLE tt-relbol NO-UNDO 
                         FIELD release# LIKE oe-relh.release#
                         FIELD tag# AS cha
                         FIELD i-no AS cha FORM "x(15)"
                         FIELD i-name AS cha FORM "x(30)"
                         FIELD ord-no LIKE oe-ord.ord-no
                         FIELD job-no LIKE oe-rell.job-no
                         FIELD job-no2 LIKE oe-rell.job-no2
                         FIELD loc LIKE oe-rell.loc
                         FIELD loc-bin LIKE oe-rell.loc-bin
                         FIELD cust-no LIKE oe-rell.cust-no
                         FIELD cases LIKE oe-rell.cases
                         FIELD qty-case LIKE oe-rell.qty-case
                         FIELD cases-unit LIKE fg-rctd.cases-unit
                         FIELD partial LIKE oe-rell.partial
                         FIELD qty LIKE oe-rell.qty
                         FIELD t-qty LIKE oe-rell.qty
                         FIELD line LIKE oe-rell.line
                         FIELD seq AS INT
                         FIELD warned AS LOG
                         FIELD po-no LIKE oe-boll.po-no
                         /* gdm - 10160906 */
                         FIELD trailer# LIKE oe-relh.trailer
                         INDEX release# release# ord-no i-no po-no.

DEF BUFFER bf-tmp FOR tt-relbol.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    locode = usercomp.loc.

    
if prmAction <> "search" then do:
    
    RUN build-query.

    FOR EACH ttTagLook NO-LOCK:
        DELETE ttTagLook. 
    END.
    
    FOR EACH tt-ldtag NO-LOCK :         
    create ttTagLook.
            assign
                ttTagLook.vTagNo        =  tt-ldtag.tag-no
                ttTagLook.vTagNo2        =  tt-ldtag.tag-no
                ttTagLook.vIno          =  tt-ldtag.i-no
                ttTagLook.vIName        =  tt-ldtag.i-name   
                ttTagLook.vJobNo        =  tt-ldtag.job-no
                ttTagLook.vJobNo2       =  tt-ldtag.job-no2
                ttTagLook.vLoc          =  tt-ldtag.loc
                ttTagLook.vLocBin       =  tt-ldtag.loc-bin
                ttTagLook.vOrdNo        =  tt-ldtag.ord-no   
                ttTagLook.vPoNo         =  STRING(tt-ldtag.po-no)  
                ttTagLook.vQty          =  tt-ldtag.qty
                ttTagLook.vQtyCase      =  tt-ldtag.qty-case                
                ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                                 
                .               
    END.          
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "tag"  then do:
        if prmCondition = "EQUAL" then do:             

            RUN build-query.

            FOR EACH ttTagLook NO-LOCK:
                DELETE ttTagLook. 
            END.
    
            FOR EACH tt-ldtag WHERE tt-ldtag.tag-no = prmText  NO-LOCK :                 
            create ttTagLook.
                assign
                    ttTagLook.vTagNo        =  tt-ldtag.tag-no
                    ttTagLook.vTagNo2        =  tt-ldtag.tag-no
                    ttTagLook.vIno          =  tt-ldtag.i-no
                    ttTagLook.vIName        =  tt-ldtag.i-name                
                    ttTagLook.vJobNo        =  tt-ldtag.job-no
                    ttTagLook.vJobNo2       =  tt-ldtag.job-no2
                    ttTagLook.vLoc          =  tt-ldtag.loc
                    ttTagLook.vLocBin       =  tt-ldtag.loc-bin
                    ttTagLook.vOrdNo        =  tt-ldtag.ord-no                  
                    ttTagLook.vPoNo         =  STRING(tt-ldtag.po-no)               
                    ttTagLook.vQty          =  tt-ldtag.qty
                    ttTagLook.vQtyCase      =  tt-ldtag.qty-case
                    ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                     
                    .
            END.          
        END. /*if prmCondition = EQUAL */ 

        IF prmCondition = "BEGIN" THEN DO:
             RUN build-query.

            FOR EACH ttTagLook NO-LOCK:
                DELETE ttTagLook. 
            END.
    
            FOR EACH tt-ldtag WHERE tt-ldtag.tag-no BEGINS prmText  NO-LOCK :                 
            create ttTagLook.
                assign
                    ttTagLook.vTagNo        =  tt-ldtag.tag-no
                    ttTagLook.vTagNo2        =  tt-ldtag.tag-no
                    ttTagLook.vIno          =  tt-ldtag.i-no
                    ttTagLook.vIName        =  tt-ldtag.i-name                
                    ttTagLook.vJobNo        =  tt-ldtag.job-no
                    ttTagLook.vJobNo2       =  tt-ldtag.job-no2
                    ttTagLook.vLoc          =  tt-ldtag.loc
                    ttTagLook.vLocBin       =  tt-ldtag.loc-bin
                    ttTagLook.vOrdNo        =  tt-ldtag.ord-no                  
                    ttTagLook.vPoNo         =  STRING(tt-ldtag.po-no)               
                    ttTagLook.vQty          =  tt-ldtag.qty
                    ttTagLook.vQtyCase      =  tt-ldtag.qty-case
                    ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                     
                    .
            END.   
        END.
     END.  /* if prmField = tag  */
          
     if prmField = "inum"  then do:
        if prmCondition = "EQUAL" then do:             

            RUN build-query.

            FOR EACH ttTagLook NO-LOCK:
                DELETE ttTagLook. 
            END.
    
            FOR EACH tt-ldtag WHERE tt-ldtag.i-no = prmText  NO-LOCK :                 
            create ttTagLook.
                assign
                    ttTagLook.vTagNo        =  tt-ldtag.tag-no
                    ttTagLook.vTagNo2        =  tt-ldtag.tag-no
                    ttTagLook.vIno          =  tt-ldtag.i-no
                    ttTagLook.vIName        =  tt-ldtag.i-name                
                    ttTagLook.vJobNo        =  tt-ldtag.job-no
                    ttTagLook.vJobNo2       =  tt-ldtag.job-no2
                    ttTagLook.vLoc          =  tt-ldtag.loc
                    ttTagLook.vLocBin       =  tt-ldtag.loc-bin
                    ttTagLook.vOrdNo        =  tt-ldtag.ord-no                  
                    ttTagLook.vPoNo         =  STRING(tt-ldtag.po-no)               
                    ttTagLook.vQty          =  tt-ldtag.qty
                    ttTagLook.vQtyCase      =  tt-ldtag.qty-case
                    ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                     
                    .
            END.          
        END. /*if prmCondition = EQUAL */ 

        IF prmCondition = "BEGIN" THEN DO:
             RUN build-query.

            FOR EACH ttTagLook NO-LOCK:
                DELETE ttTagLook. 
            END.
    
            FOR EACH tt-ldtag WHERE tt-ldtag.i-no BEGINS prmText  NO-LOCK :                 
            create ttTagLook.
                assign
                    ttTagLook.vTagNo        =  tt-ldtag.tag-no
                    ttTagLook.vTagNo2        =  tt-ldtag.tag-no
                    ttTagLook.vIno          =  tt-ldtag.i-no
                    ttTagLook.vIName        =  tt-ldtag.i-name                
                    ttTagLook.vJobNo        =  tt-ldtag.job-no
                    ttTagLook.vJobNo2       =  tt-ldtag.job-no2
                    ttTagLook.vLoc          =  tt-ldtag.loc
                    ttTagLook.vLocBin       =  tt-ldtag.loc-bin
                    ttTagLook.vOrdNo        =  tt-ldtag.ord-no                  
                    ttTagLook.vPoNo         =  STRING(tt-ldtag.po-no)               
                    ttTagLook.vQty          =  tt-ldtag.qty
                    ttTagLook.vQtyCase      =  tt-ldtag.qty-case
                    ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                     
                    .
            END.   
        END.
     END.  /* if prmField = inum  */

       
     if prmField = "job"  then do:
        if prmCondition = "EQUAL" then do:             

            RUN build-query.

            FOR EACH ttTagLook NO-LOCK:
                DELETE ttTagLook. 
            END.
    
            FOR EACH tt-ldtag WHERE tt-ldtag.job-no = prmText  NO-LOCK :                 
            create ttTagLook.
                assign
                    ttTagLook.vTagNo        =  tt-ldtag.tag-no
                    ttTagLook.vTagNo2        =  tt-ldtag.tag-no
                    ttTagLook.vIno          =  tt-ldtag.i-no
                    ttTagLook.vIName        =  tt-ldtag.i-name                
                    ttTagLook.vJobNo        =  tt-ldtag.job-no
                    ttTagLook.vJobNo2       =  tt-ldtag.job-no2
                    ttTagLook.vLoc          =  tt-ldtag.loc
                    ttTagLook.vLocBin       =  tt-ldtag.loc-bin
                    ttTagLook.vOrdNo        =  tt-ldtag.ord-no                  
                    ttTagLook.vPoNo         =  STRING(tt-ldtag.po-no)               
                    ttTagLook.vQty          =  tt-ldtag.qty
                    ttTagLook.vQtyCase      =  tt-ldtag.qty-case
                    ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                     
                    .
            END.          
        END. /*if prmCondition = EQUAL */ 

        IF prmCondition = "BEGIN" THEN DO:
             RUN build-query.

            FOR EACH ttTagLook NO-LOCK:
                DELETE ttTagLook. 
            END.
    
            FOR EACH tt-ldtag WHERE tt-ldtag.job-no BEGINS prmText  NO-LOCK :                 
            create ttTagLook.
                assign
                    ttTagLook.vTagNo        =  tt-ldtag.tag-no
                    ttTagLook.vTagNo2        =  tt-ldtag.tag-no
                    ttTagLook.vIno          =  tt-ldtag.i-no
                    ttTagLook.vIName        =  tt-ldtag.i-name                
                    ttTagLook.vJobNo        =  tt-ldtag.job-no
                    ttTagLook.vJobNo2       =  tt-ldtag.job-no2
                    ttTagLook.vLoc          =  tt-ldtag.loc
                    ttTagLook.vLocBin       =  tt-ldtag.loc-bin
                    ttTagLook.vOrdNo        =  tt-ldtag.ord-no                  
                    ttTagLook.vPoNo         =  STRING(tt-ldtag.po-no)               
                    ttTagLook.vQty          =  tt-ldtag.qty
                    ttTagLook.vQtyCase      =  tt-ldtag.qty-case
                    ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                     
                    .
            END.   
        END.
     END.  /* if prmField = job  */


END.  /* IF prmAction = search then do: */




IF prmAction = "TagLeave" then do:

    DEF VAR lv-qty-rel AS INT NO-UNDO.
    DEF VAR lv-qty-tag AS INT NO-UNDO.
    DEF VAR ll AS LOG NO-UNDO.

    /*IF (LASTKEY = -1 OR LASTKEY = 27 /*ESC*/) AND NOT lv-do-leave-tag  THEN RETURN.
   lv-do-leave-tag = NO.
   */


    /*RUN build-query.*/    
   
   FOR EACH tt-ldtag WHERE tt-ldtag.tag-no = prmTag  NO-LOCK :
    create tt-relbol.
        assign
            tt-relbol.tag#          =  tt-ldtag.tag-no
            tt-relbol.i-no          =  tt-ldtag.i-no
            tt-relbol.i-name        =  tt-ldtag.i-name                
            tt-relbol.job-no        =  tt-ldtag.job-no
            tt-relbol.job-no2       =  tt-ldtag.job-no2
            tt-relbol.loc           =  tt-ldtag.loc
            tt-relbol.loc-bin       =  tt-ldtag.loc-bin
            tt-relbol.ord-no        =  tt-ldtag.ord-no                  
            tt-relbol.po-no         =  STRING(tt-ldtag.po-no)
            tt-relbol.qty           =  tt-ldtag.qty
            tt-relbol.qty-case      =  tt-ldtag.qty-case
            
            /*ttTagLook.vPalletCount  =  tt-ldtag.pallet-count                     */
            .
   END.         


   FIND FIRST oe-relh
       WHERE oe-relh.company  EQ prmComp
         AND oe-relh.release# EQ INT(prmRelease)
       NO-LOCK NO-ERROR.
   
   FIND FIRST loadtag
       WHERE loadtag.company   EQ prmComp
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ prmTag NO-LOCK NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
        cError =  "Invalid Loadtag for the Release..." .
        RETURN.
   END.
   v-ord-no = 0.

   IF loadtag.ord-no = 0 THEN DO: /* stock box */
      FIND first oe-rell WHERE oe-rell.company   EQ prmComp
                           AND oe-rell.r-no      EQ oe-relh.r-no
                           AND oe-rell.tag      EQ loadtag.tag-no                           
                           USE-INDEX r-no NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-rell THEN do:
         DEF VAR v-ord-no-list AS cha NO-UNDO.
         DEF VAR v-tag-no-list AS cha NO-UNDO.
         DEF VAR v-i-qty AS INT NO-UNDO.
         DEF VAR v-i-rel-qty AS INT NO-UNDO.
         ASSIGN
         v-ord-no-list = ""
         v-tag-no-list = "".
         FOR EACH bf-tmp NO-LOCK WHERE bf-tmp.i-no = loadtag.i-no 
                                 BREAK BY bf-tmp.ord-no:
             IF FIRST-OF(bf-tmp.ord-no) THEN v-ord-no-list = v-ord-no-list + STRING(bf-tmp.ord-no) + ",".
             v-tag-no-list = v-tag-no-list + bf-tmp.tag + ",".
             v-i-qty = v-i-qty + bf-tmp.qty.
         END.
         FOR EACH oe-rell FIELDS(qty) NO-LOCK WHERE
             oe-rell.company = prmComp AND
             oe-rell.r-no  = oe-relh.r-no AND
             oe-rell.i-no = loadtag.i-no
             USE-INDEX r-no:
             v-i-rel-qty = v-i-rel-qty + oe-rell.qty.
         END.
         /*IF adm-new-record THEN
            FIND FIRST oe-rell WHERE oe-rell.company   EQ prmComp
                        AND oe-rell.r-no      EQ oe-relh.r-no
                        AND oe-rell.i-no      EQ loadtag.i-no 
                        AND (LOOKUP(string(oe-rell.ord-no),v-ord-no-list) <= 0 OR
                             lookup(oe-rell.tag,v-tag-no-list) <= 0 OR
                             v-i-rel-qty > v-i-qty)
                        USE-INDEX r-no NO-LOCK NO-ERROR.
         ELSE*/ FIND FIRST oe-rell WHERE oe-rell.company   EQ prmComp
                        AND oe-rell.r-no      EQ oe-relh.r-no
                        AND oe-rell.i-no      EQ loadtag.i-no 
                        AND (LOOKUP(string(oe-rell.ord-no),v-ord-no-list) > 0 OR
                             lookup(oe-rell.tag,v-tag-no-list) > 0)
                        USE-INDEX r-no NO-LOCK NO-ERROR.
      END.
      IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
      ELSE DO:
          IF v-i-rel-qty >= v-i-qty THEN DO:
             FIND FIRST oe-rell NO-LOCK WHERE oe-rell.company   EQ prmComp
                        AND oe-rell.r-no      EQ oe-relh.r-no
                        AND oe-rell.i-no      EQ loadtag.i-no
                        USE-INDEX r-no NO-ERROR.
             IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.             
          END.
          ELSE DO:
             /*MESSAGE "Loadtag has no order number and FG Item not on any order." SKIP
                  "FG Item must be added to order entry line with Sell Price"         
                  VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
             */
                cError =  "Loadtag has no order number and FG Item not on any order." .
                RETURN.
          END.
      END.
   END.
   ELSE DO:
      FIND first oe-rell WHERE oe-rell.company   EQ prmComp
                           AND oe-rell.r-no      EQ oe-relh.r-no
                           AND oe-rell.ord-no    EQ loadtag.ord-no     
                           AND oe-rell.i-no EQ  loadtag.i-no
                         USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL oe-rell THEN v-ord-no = loadtag.ord-no.
      ELSE DO:
       {bol/bolstock.i}
       IF v-ord-no = 0 THEN DO:
         FIND FIRST oe-ord WHERE oe-ord.company = cocode
                             AND oe-ord.ord-no = loadtag.ord-no
                             AND oe-ord.OPENed = YES NO-LOCK NO-ERROR.
         IF AVAIL oe-ord THEN DO:
            IF oe-ord.cust-no = oe-relh.cust-no THEN DO: /* have ord# from release*/
               FIND first oe-rell WHERE oe-rell.company   EQ cocode
                                    AND oe-rell.r-no      EQ oe-relh.r-no
                                    AND oe-rell.tag EQ  loadtag.tag-no
                                  USE-INDEX r-no NO-LOCK NO-ERROR.
               IF NOT AVAIL oe-rell THEN DO:
                  {bol/loadcust.i}
                  IF AVAIL cust AND cust.active = "X" THEN  DO: 
                     FIND FIRST oe-rell NO-LOCK WHERE oe-rell.company   EQ cocode
                              AND oe-rell.r-no      EQ oe-relh.r-no
                              AND oe-rell.i-no      EQ loadtag.i-no                           
                              USE-INDEX r-no NO-ERROR.
                  END.
               END.
               IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
               ELSE v-ord-no = loadtag.ord-no.
            END.
            ELSE DO:
              {bol/loadcust.i}
              IF AVAIL cust AND cust.active = "X" THEN  DO: /* same as stock box ord-no = 0*/
                 FIND first oe-rell WHERE oe-rell.company   EQ cocode
                               AND oe-rell.r-no      EQ oe-relh.r-no
                               AND oe-rell.tag      EQ loadtag.tag-no                           
                               USE-INDEX r-no NO-LOCK NO-ERROR.
                 IF NOT AVAIL oe-rell THEN /* only one line for the item*/
                    FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                              AND oe-rell.r-no      EQ oe-relh.r-no
                              AND oe-rell.i-no      EQ loadtag.i-no                           
                              USE-INDEX r-no NO-LOCK NO-ERROR.
                 IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
              END.
            END.
         END. /* avail oe-ord */
         ELSE DO: /* loadtag.ord-no is invalid */
             FIND first oe-rell WHERE oe-rell.company   EQ cocode
                                  AND oe-rell.r-no      EQ oe-relh.r-no
                                  AND oe-rell.tag EQ  loadtag.tag-no
                                  USE-INDEX r-no NO-LOCK NO-ERROR.
             IF NOT AVAIL oe-rell THEN
                FIND first oe-rell WHERE oe-rell.company   EQ cocode
                                     AND oe-rell.r-no      EQ oe-relh.r-no
                                     AND oe-rell.i-no EQ  loadtag.i-no
                                     USE-INDEX r-no NO-LOCK NO-ERROR.
             IF AVAIL oe-rell THEN  v-ord-no = oe-rell.ord-no.
             ELSE DO:
                /*MESSAGE "Order " loadtag.ord-no " is Closed. Reopen it and try."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.                      
                */

                cError =  "Order is Closed. Reopen it and try." .
                RETURN.
             END.
         END.
       END.  /* v-ord-no = 0*/
      END.  /* not avail oe-rell*/
   END. /* loadtag.ord-no <> 0 */   

   DEF VAR lv-po-cnt AS INT NO-UNDO.
   DEF VAR lv-po-no AS cha NO-UNDO.
   DEF VAR lv-po-from-rell AS LOG NO-UNDO.
   ASSIGN lv-po-cnt = 0
          lv-po-no  = "".
   
   FOR EACH oe-rell
         WHERE oe-rell.company   EQ cocode
           AND oe-rell.r-no      EQ oe-relh.r-no
           AND oe-rell.ord-no    EQ v-ord-no
           AND oe-rell.i-no      EQ loadtag.i-no
         USE-INDEX r-no NO-LOCK BREAK BY oe-rell.po-no:
     IF FIRST-OF(oe-rell.po-no) THEN lv-po-cnt = lv-po-cnt + 1.
     ASSIGN
     lv-po-no = oe-rell.po-no
     lv-po-from-rell = YES.
   END.

   /* IF lv-po-cnt > 1 THEN RUN addon/bol/d-selpo.w (RECID(oe-relh),RECID(loadtag),OUTPUT lv-po-no). 
      
   ELSE IF NOT lv-po-from-rell and CAN-FIND (FIRST itemfg WHERE itemfg.company = cocode
                          AND itemfg.i-no = loadtag.i-no AND itemfg.i-code = "C") THEN DO:
       {addon/bol/bolpoord.i}
   END.
    */
   IF CAN-FIND(FIRST bf-tmp WHERE bf-tmp.tag# = prmTag
                   AND RECID(bf-tmp) <> RECID(tt-relbol) ) 
   THEN DO:
     /*IF NOT  g-sharpshooter THEN MESSAGE "Tag# already scanned..." VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Tag# already scanned...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-apply.
     */

       cError =  "Tag# already scanned..." .
       RETURN.
   END.

   IF NOT CAN-FIND(FIRST fg-bin
                   WHERE fg-bin.company EQ cocode
                     AND fg-bin.tag     EQ prmTag
                     AND fg-bin.i-no    EQ loadtag.i-no
                     AND fg-bin.job-no  EQ loadtag.job-no
                     AND fg-bin.job-no2 EQ loadtag.job-no2
                     AND fg-bin.qty     GT 0)
   THEN DO:
     /*IF NOT  g-sharpshooter THEN MESSAGE "Tag# has no inventory..." VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Tag# has no inventory...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-APPLY.
     */
       cError =  "Tag# has no inventory..." .
       RETURN.
   END.
   
   /*IF relmerge-int EQ 0 AND
      NOT CAN-FIND(FIRST oe-ordl
                   WHERE oe-ordl.company  EQ cocode
                     AND oe-ordl.ord-no   EQ v-ord-no
                     AND oe-ordl.i-no     EQ loadtag.i-no
                     AND ((oe-ordl.job-no EQ loadtag.job-no AND
                           oe-ordl.job-no2 EQ loadtag.job-no2) OR
                          TRIM(oe-ordl.job-no) EQ ""))
   THEN DO:
     IF NOT  g-sharpshooter THEN MESSAGE "Job# not on Order..." v-ord-no VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Job# not on Order...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-APPLY.
   END.
   */
   FOR EACH oe-ordl
       WHERE oe-ordl.company  EQ cocode
         AND oe-ordl.ord-no   EQ v-ord-no
         AND oe-ordl.i-no     EQ loadtag.i-no
       NO-LOCK
       BREAK BY oe-ordl.job-no  DESC
             BY oe-ordl.job-no2 DESC:
     IF LAST(oe-ordl.job-no) OR
        (oe-ordl.job-no EQ loadtag.job-no AND
         oe-ordl.job-no2 EQ loadtag.job-no2) THEN
       LEAVE.
   END.

   IF NOT AVAIL oe-ordl AND loadtag.ord-no <> 0 THEN DO:
     /*IF NOT  g-sharpshooter THEN MESSAGE "Tag# Order/FG# invalid..." VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Tag# Order#/FG# invalid...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-apply.
     */
       cError =  "Tag# Order#/FG# invalid..." .
       RETURN.
   END.

   IF NOT CAN-FIND(FIRST oe-rell
                   WHERE oe-rell.company EQ oe-relh.company
                     AND oe-rell.r-no    EQ oe-relh.r-no
                     AND oe-rell.ord-no  EQ v-ord-no
                     AND oe-rell.i-no    EQ loadtag.i-no
                   USE-INDEX r-no)     
   THEN DO:
     ll = NO.
     /*IF ssbol-log THEN  DO:
       IF NOT  g-sharpshooter THEN
          MESSAGE "This Order# " + TRIM(STRING(v-ord-no),">>>>>>>>") + " FG# " 
                + TRIM(loadtag.i-no) +
                " is not on release, do you want to ADD TO RELEASE? "
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll. 
       ELSE do:
            RUN custom/d-msg.w ("Question","This Order# " + TRIM(STRING(v-ord-no),">>>>>>>>") + " FG#" + TRIM(loadtag.i-no) +
               " is not on release." , "Do you want to ADD TO RELEASE? ","",2,"Yes,No",OUTPUT v-msgreturn).
            IF v-msgreturn = 1 THEN ll = YES.
       END.
     END.
     ELSE DO:
        IF NOT  g-sharpshooter THEN MESSAGE "Invalid Tag# for the Release#. Try again..." VIEW-AS ALERT-BOX ERROR.
        ELSE RUN custom/d-msg.w ("Error","","Invalid Tag# for this Release#.  Try again...","",1,"OK", OUTPUT v-msgreturn).
     END.
     */
     /*IF NOT ll THEN RETURN NO-APPLY.*/
     tt-relbol.warned = YES.
   END.

   IF NOT tt-relbol.warned THEN DO:
     lv-qty-rel = 0.
     FOR EACH oe-rell
         WHERE oe-rell.company   EQ cocode
           AND oe-rell.r-no      EQ oe-relh.r-no
           AND (oe-rell.ord-no   EQ v-ord-no)
           AND oe-rell.i-no      EQ loadtag.i-no
         USE-INDEX r-no NO-LOCK:
       lv-qty-rel = lv-qty-rel + oe-rell.qty.
     END.
     lv-qty-tag = loadtag.pallet-count.
     FOR EACH bf-tmp
         WHERE bf-tmp.release# EQ INT(prmRelease)
           AND (bf-tmp.ord-no   EQ v-ord-no)
           AND bf-tmp.i-no     EQ loadtag.i-no
           AND bf-tmp.warned   EQ NO
           AND ROWID(bf-tmp)   NE ROWID(tt-relbol):
       lv-qty-tag = lv-qty-tag + bf-tmp.qty.
     END.
   END.
   ll = NO.

   IF lv-qty-tag GT lv-qty-rel THEN DO:
      /*IF NOT  g-sharpshooter THEN
         MESSAGE "Qty scanned exceeds qty released for Order# " + TRIM(STRING(v-ord-no),">>>>>>>>")
              + " FG# "  + TRIM(loadtag.i-no) +
             ", accept this tag anyway?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
     ELSE DO:
        RUN custom/d-msg.w ("Question","Qty scanned exceeds qty released for Order# " +
                      TRIM(STRING(v-ord-no),">>>>>>>>") + " FG# " +
                      TRIM(loadtag.i-no) + "." , "Accept this tag anyway? ","",2,"Yes,No",OUTPUT v-msgreturn).
        IF v-msgreturn = 1 THEN ll = YES.        
     END.
     IF NOT ll THEN DO:
       APPLY "entry" TO tt-relbol.tag# .
       RETURN NO-APPLY.
     END.
     */
     FOR FIRST bf-tmp 
         WHERE bf-tmp.release# EQ INT(prmRelease)
           AND bf-tmp.i-no     EQ loadtag.i-no
           AND ROWID(bf-tmp)   NE ROWID(tt-relbol):
       bf-tmp.warned = YES.
     END.
   END.

   RELEASE oe-ord.
   IF v-ord-no NE 0 THEN
   FIND FIRST oe-ord NO-LOCK
       WHERE oe-ord.company EQ cocode
         AND oe-ord.ord-no  EQ v-ord-no
       NO-ERROR.

   RELEASE fg-bin.
   FOR EACH fg-bin NO-LOCK
       WHERE fg-bin.company  EQ cocode
         AND fg-bin.tag      EQ prmTag
         AND fg-bin.i-no     EQ loadtag.i-no
         AND fg-bin.job-no   EQ loadtag.job-no
         AND fg-bin.job-no2  EQ loadtag.job-no2
         AND fg-bin.qty      GT 0
         AND ((AVAIL oe-ord AND fg-bin.cust-no EQ oe-ord.cust-no) OR
              fg-bin.cust-no EQ "")
       USE-INDEX tag
       BREAK BY fg-bin.cust-no DESC
             BY fg-bin.qty:

     IF (fg-bin.cust-no EQ oe-relh.cust-no AND LAST-OF(fg-bin.cust-no)) OR
        LAST(fg-bin.cust-no) THEN LEAVE.
   END.   


    ASSIGN tt-relbol.tag# = prmTag
          tt-relbol.i-no = loadtag.i-no
          tt-relbol.i-name = loadtag.i-name          
          tt-relbol.ord-no = v-ord-no 
          tt-relbol.job-no = loadtag.job-no
          tt-relbol.job-no2 = loadtag.job-no2
          tt-relbol.loc = IF AVAIL fg-bin THEN fg-bin.loc ELSE loadtag.loc
          tt-relbol.loc-bin = IF AVAIL fg-bin THEN fg-bin.loc-bin ELSE loadtag.loc-bin
          tt-relbol.cust-no = IF AVAIL fg-bin THEN fg-bin.cust-no ELSE ""  
          tt-relbol.qty = IF AVAIL fg-bin THEN fg-bin.qty ELSE loadtag.pallet-count  
          tt-relbol.cases  = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) ELSE loadtag.case-bundle
          tt-relbol.qty-case = IF AVAIL fg-bin THEN fg-bin.case-count ELSE loadtag.qty-case
          tt-relbol.cases-unit = IF AVAIL fg-bin THEN fg-bin.cases-unit ELSE loadtag.case-bundle
          tt-relbol.partial = IF AVAIL fg-bin THEN fg-bin.partial-count ELSE loadtag.partial
          tt-relbol.line = IF AVAIL oe-ordl THEN oe-ordl.LINE ELSE 0
          tt-relbol.po-no = lv-po-no
          tt-relbol.warned = ll.
          .

          FOR EACH ttTagLook NO-LOCK:
              DELETE ttTagLook. 
          END.

    create ttTagLook.
            assign
                ttTagLook.vTagNo        =  tt-relbol.tag#
                ttTagLook.vTagNo2        =  tt-relbol.tag#
                ttTagLook.vIno          =  tt-relbol.i-no
                ttTagLook.vIName        =  tt-relbol.i-name   
                ttTagLook.vJobNo        =  tt-relbol.job-no
                ttTagLook.vJobNo2       =  tt-relbol.job-no2
                ttTagLook.vLoc          =  tt-relbol.loc
                ttTagLook.vLocBin       =  tt-relbol.loc-bin
                ttTagLook.vOrdNo        =  tt-relbol.ord-no   
                ttTagLook.vPoNo         =  tt-relbol.po-no  
                ttTagLook.vQty          =  tt-relbol.qty
                ttTagLook.vQtyCase      =  tt-relbol.qty-case         
                ttTagLook.vLine         =  tt-relbol.line
                ttTagLook.vUnits        =  tt-relbol.cases
                ttTagLook.vCustomer     =  tt-relbol.cust-no
                ttTagLook.vCasesUnit    =  tt-relbol.cases-unit
                ttTagLook.vPartial      =  tt-relbol.partial 
                ttTagLook.vPalletCount  =  tt-ldtag.pallet-count 
                .   
          
   
   /*IF g-sharpshooter = YES THEN */ /* DO:*/
      /*FIND FIRST itemfg WHERE itemfg.company = cocode
                          AND itemfg.i-no = tt-relbol.i-no:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF AVAIL ITEMfg AND itemfg.ship-meth THEN DO: /* case */
                           
        /* gdm - 10160906 */
        IF TRIM(ssbolscan-cha) EQ "" THEN DO:
          APPLY "entry" TO tt-relbol.cases IN BROWSE {&browse-name}. 
          DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
          RETURN.
        END.
        ELSE DO:
          APPLY "entry" TO tt-relbol.trailer IN BROWSE {&browse-name}.
          DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
          RETURN.
        END.          
      END.
      ELSE  do:
          IF int(tt-relbol.qty:screen-value) /*loadtag.qty, pallet-count*/ = 0 THEN DO:
             APPLY "entry" TO tt-relbol.qty.
             DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
             RETURN NO-APPLY.
          END.
          ELSE APPLY "row-leave" TO BROWSE {&browse-name}.
      END.
   END.
   RETURN NO-APPLY.
   
END.
*/

END.  /* IF prmAction = TagLeave then do: */




/*------------------------------------------------------------------------------*/
PROCEDURE build-query :

FOR EACH tt-ldtag:
    DELETE tt-ldtag.
END.
FOR EACH oe-relh WHERE oe-relh.company = prmComp and
                       oe-relh.release# = int(prmRelease) NO-LOCK,
    EACH oe-rell
    WHERE oe-rell.company EQ oe-relh.company
      AND oe-rell.r-no    EQ oe-relh.r-no
    USE-INDEX r-no NO-LOCK 
    BREAK BY oe-rell.i-no:
    IF FIRST-OF(oe-rell.i-no) THEN DO:
       CREATE tt-rell.
       BUFFER-COPY oe-rell TO tt-rell.
       tt-rell.rell-recid = RECID(oe-rell).
    END.
END.
FOR EACH tt-rell NO-LOCK,
      EACH loadtag WHERE loadtag.company = prmComp
                     and loadtag.item-type = no
                     and loadtag.i-no = tt-rell.i-no
                     NO-LOCK,
      EACH fg-bin WHERE fg-bin.company = loadtag.company
                    and fg-bin.i-no = loadtag.i-no
                    and fg-bin.loc = loadtag.loc
                    and fg-bin.loc-bin = loadtag.loc-bin
                    and fg-bin.tag = loadtag.tag-no
                    and fg-bin.qty > 0 NO-LOCK :
                 
       CREATE tt-ldtag.
       BUFFER-COPY loadtag TO tt-ldtag.
       tt-ldtag.tag-recid = RECID(loadtag).

END.
END PROCEDURE.


FOR EACH ttTagLook NO-LOCK:
       IF INDEX(ttTagLook.vTagNo2 ,'"',1) > 0 THEN ASSIGN
            ttTagLook.vTagNo2  = REPLACE(ttTagLook.vTagNo2 ,'"',":").        
END.
