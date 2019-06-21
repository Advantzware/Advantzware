
/*------------------------------------------------------------------------
    File        : Rellord_view.p
    Purpose     : Release
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttViewReleaseOrder NO-UNDO
    FIELD ordno     AS INT 
    FIELD ino       AS CHAR
    FIELD pono      AS CHAR
    FIELD qty       AS INT
    FIELD tag       AS CHAR
    FIELD loc       AS CHAR
    FIELD locbin    AS CHAR
    FIELD jobno     AS CHAR
    FIELD jobno2    AS INT
    FIELD custno    AS CHAR
    FIELD cases     AS INT
    FIELD qtycas    AS INT
    FIELD partial   AS INT
    FIELD relno     AS INT
    FIELD bordno    AS INT
    FIELD scod      AS CHAR
    FIELD partno    AS CHAR
    FIELD linkno    AS INT
    FIELD posted    AS CHAR
    FIELD reckey    AS CHAR
    FIELD extra     AS CHAR 
    .

DEFINE DATASET dsViewReleaseOrder FOR ttViewReleaseOrder.
    
DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmordno2   AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmino2     AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpono2    AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmqty     AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmtag     AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmloc     AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmlocbin  AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmjobno   AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmjobno2  AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmcustno  AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmcases   AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmqtycas  AS INT           NO-UNDO.
DEFINE INPUT PARAMETER prmpartial AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmrelno2   AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmbordno  AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmscod    AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpartno  AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER prmlinkno  AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmreckey  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmextra2   AS CHAR        NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewReleaseOrder .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttViewReleaseOrder:
        DELETE ttViewReleaseOrder .
    END.

IF prmAction        = ?  THEN ASSIGN prmAction    = "Search".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmordno2         = ?  THEN ASSIGN prmordno2   = 0.
IF prmino2           = ?  THEN ASSIGN prmino2     = "".
IF prmpono2          = ?  THEN ASSIGN prmpono2    = "".
IF prmqty           = ?  THEN ASSIGN prmqty       = 0.
IF prmtag           = ?  THEN ASSIGN prmtag       = "".
IF prmloc           = ?  THEN ASSIGN prmloc       = "". 
IF prmlocbin        = ?  THEN ASSIGN prmlocbin    = "". 
IF prmjobno         = ?  THEN ASSIGN prmjobno     = "".
IF prmjobno2        = ?  THEN ASSIGN prmjobno2    = 0.
IF prmcustno        = ?  THEN ASSIGN prmcustno    = "".
IF prmcases         = ?  THEN ASSIGN prmcases     = 0.
IF prmqtycas        = ?  THEN ASSIGN prmqtycas    = 0.
IF prmpartial       = ?  THEN ASSIGN prmpartial   = 0.
IF prmrelno2         = ?  THEN ASSIGN prmrelno2   = 0.
IF prmbordno        = ?  THEN ASSIGN prmbordno    = 0.
IF prmscod          = ?  THEN ASSIGN prmscod      = "".
IF prmpartno        = ?  THEN ASSIGN prmpartno    = "".
IF prmlinkno        = ?  THEN ASSIGN prmlinkno    = 0.
IF prmextra2        = ?  THEN ASSIGN prmextra2    = "".



DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
 
  DEF VAR old-po-no LIKE oe-rell.po-no NO-UNDO.

  
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .

  {sys/inc/apsecure.i}

DEF VAR lv-s-codes AS CHAR NO-UNDO.
DEF VAR lv-s-dscrs AS CHAR NO-UNDO.
RUN sys/ref/s-codes.p (OUTPUT lv-s-codes, OUTPUT lv-s-dscrs).
/*
FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.*/

DEF BUFFER io-shipto FOR shipto.
DEF BUFFER b-oe-ordl-2 FOR oe-ordl.
DEF BUFFER b-oe-rell-2 FOR oe-rell.
DEF BUFFER bf-rell FOR oe-rell.
DEF BUFFER bf-ordl FOR oe-ordl.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF VAR v-ship-from AS LOG INIT YES NO-UNDO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company = g_company NO-LOCK NO-ERROR.
IF AVAIL oe-ctrl THEN v-ship-from = oe-ctrl.ship-from.

find first sys-ctrl where sys-ctrl.company = g_company
                        and sys-ctrl.name = "ADDRELSE" no-lock no-error.
  if not avail sys-ctrl then do:
     create sys-ctrl.
     assign sys-ctrl.company = g_company
            sys-ctrl.name = "ADDRELSE"
            sys-ctrl.descrip = "Method for Adding Releases"
            sys-ctrl.char-fld = "ASI".
     message "Sys Ctrl record not found. " 
             "Enter default method for adding releases."
             update sys-ctrl.char-fld.
  end.


IF prmAction = "Gridview" THEN DO:

    FIND oe-relh WHERE oe-relh.company = prmComp 
        AND oe-relh.r-no eq int(prmextra2)
         /*AND oe-relh.release# eq 999999999*/ NO-LOCK NO-ERROR.
    
     FOR EACH oe-rell WHERE oe-rell.company eq oe-relh.company 
         AND oe-rell.r-no EQ oe-relh.r-no
         use-index r-no NO-LOCK,
         FIRST oe-ordl WHERE oe-ordl.company eq oe-rell.company 
         AND oe-ordl.ord-no eq oe-rell.ord-no 
         AND oe-ordl.i-no eq oe-rell.i-no 
         AND oe-ordl.line eq oe-rell.line NO-LOCK BY oe-rell.ord-no:

        CREATE ttViewReleaseOrder.
           ASSIGN 
                 
                 ttViewReleaseOrder.ordno     = oe-rell.ord-no
                 ttViewReleaseOrder.ino       = oe-rell.i-no
                 ttViewReleaseOrder.pono      = oe-rell.po-no
                 ttViewReleaseOrder.qty       = oe-rell.qty 
                 ttViewReleaseOrder.tag       = oe-rell.tag 
                 ttViewReleaseOrder.loc       = oe-rell.loc
                 ttViewReleaseOrder.locbin    = oe-rell.loc-bin
                 ttViewReleaseOrder.jobno     = oe-rell.job-no
                 ttViewReleaseOrder.jobno2    = oe-rell.job-no2 
                 ttViewReleaseOrder.custno    = oe-rell.cust-no         
                 ttViewReleaseOrder.cases     = oe-rell.cases
                 ttViewReleaseOrder.qtycas    = oe-rell.qty-case
                 ttViewReleaseOrder.partial   = oe-rell.partial    
                 ttViewReleaseOrder.relno     = oe-rell.rel-no   
                 ttViewReleaseOrder.bordno    = oe-rell.b-ord-no  
                 ttViewReleaseOrder.scod      = oe-rell.s-code   
                 ttViewReleaseOrder.partno    = oe-ordl.part-no
                 ttViewReleaseOrder.linkno    = oe-rell.link-no 
                 ttViewReleaseOrder.reckey    = oe-rell.rec_key .
           
            
    END. /*FOR EACH oe-relh  */
END. /*IF prmAction = "Select" THEN DO:*/


 
/********************************Add **********************************/

IF prmAction = "CreateAdd" THEN DO:

    FIND oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra2) NO-LOCK NO-ERROR.
     /* Dispatch standard ADM method.                             */
  CREATE oe-rell .

  /* Code placed here will execute AFTER standard behavior.    */
  assign oe-rell.company = oe-relh.company
         oe-rell.r-no = oe-relh.r-no.

  FIND FIRST b-oe-rell NO-LOCK
      WHERE b-oe-rell.r-no   EQ oe-relh.r-no
        AND b-oe-rell.s-code NE ""
        AND ROWID(b-oe-rell) NE ROWID(oe-rell)
      NO-ERROR.
  IF AVAIL b-oe-rell THEN oe-rell.s-code = b-oe-rell.s-code.
  ASSIGN
      prmAction = "Viewadd"
      prmReckey = oe-rell.rec_key .
MESSAGE "prmAction " prmAction prmReckey .

END.  /* end create add */


IF prmAction = "ValidateAdd" THEN DO:

    FIND oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra2) NO-LOCK NO-ERROR.


     
   /* IF prmloc EQ "" THEN
      ttViewReleaseOrder.loc = g_loc.*/

    /*IF prmscod EQ "" THEN
      ttViewReleaseOrder.scod   = 
          IF v-ship-from THEN "B" ELSE "I".*/

    IF prmordno2 NE 0 THEN
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ prmComp
          AND oe-ord.ord-no  EQ prmordno2
        NO-LOCK NO-ERROR.

    IF NOT AVAIL oe-ord THEN do: 
        cError = "Order is invalid. Try help...".
        RETURN.
    END.

    IF AVAIL oe-ord AND oe-ord.cust-no <> oe-relh.cust-no THEN do:
         cError = "Sorry, order must match current customer. Try help..."  .
         RETURN.
    END.

    FIND FIRST bf-ordl
        WHERE bf-ordl.company EQ cocode
          AND bf-ordl.ord-no  EQ INT(prmordno2)
          AND bf-ordl.i-no    EQ prmino2
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-ordl THEN DO:
      cError =  " Item not on Order, try help..." .
      RETURN .
    END.

    ASSIGN
     prmjobno = FILL(" ",6 - LENGTH(prmjobno)) + prmjobno.

    FIND FIRST bf-ordl
          WHERE bf-ordl.company  EQ cocode
            AND bf-ordl.ord-no   EQ INT(prmordno2)
            AND bf-ordl.i-no     EQ prmino2
            AND (TRIM(bf-ordl.job-no) EQ "" OR
                 (bf-ordl.job-no EQ prmjobno AND
                  bf-ordl.job-no2 EQ INT(prmjobno2)))
          NO-LOCK NO-ERROR.
      IF NOT AVAIL bf-ordl THEN DO:
        cError = "Job not for Order/FG, try again..." .
        RETURN .
      END.

      IF prmcustno NE "" AND
       prmcustno NE oe-relh.cust-no
       /*NOT CAN-FIND(FIRST fg-bin 
                     WHERE fg-bin.company EQ cocode
                       AND fg-bin.i-no    EQ prmino2
                       AND fg-bin.job-no  EQ prmjobno
                       AND fg-bin.job-no2 EQ INT(prmjobno2)
                       AND fg-bin.loc     EQ prmloc
                       AND fg-bin.loc-bin EQ prmlocbin
                       AND fg-bin.tag     EQ prmtag
                       AND fg-bin.cust-no EQ prmcustno)*/
        THEN DO:
      cError =  "Invalid Customer#, try help..." .
          RETURN.
      END.

      FIND FIRST b-oe-rell
          WHERE b-oe-rell.r-no   EQ oe-relh.r-no
          AND b-oe-rell.s-code NE prmscod
          AND ROWID(b-oe-rell) NE ROWID(oe-rell)
       NO-LOCK NO-ERROR.
      IF AVAIL b-oe-rell THEN do:
          cError = "Invalid S/I. try help..." .
          RETURN .
      END.
    
    

END.  /* end of validate add*/

IF prmAction = "Add" THEN DO:

    FIND oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra2) NO-LOCK NO-ERROR.
  MESSAGE "prmReckey  " prmAction STRING(prmReckey) .
     FIND FIRST oe-rell WHERE oe-rell.company eq oe-relh.company 
         AND asi.oe-rell.r-no eq oe-relh.r-no AND oe-rell.rec_key EQ prmReckey EXCLUSIVE-LOCK NO-ERROR.
     FIND FIRST oe-ordl WHERE oe-ordl.company eq oe-rell.company 
         AND oe-ordl.ord-no eq oe-rell.ord-no 
         AND oe-ordl.i-no eq oe-rell.i-no 
         AND oe-ordl.line eq oe-rell.line EXCLUSIVE-LOCK NO-ERROR .

          /* Code placed here will execute PRIOR to standard behavior. */
  old-po-no = IF AVAIL oe-rell THEN oe-rell.po-no ELSE "".

  IF oe-rell.line eq 0 THEN DO:
    FIND FIRST bf-ordl
        WHERE bf-ordl.company EQ oe-rell.company
          AND bf-ordl.ord-no  EQ oe-rell.ord-no
          AND bf-ordl.i-no    EQ oe-rell.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL bf-ordl THEN oe-rell.line = bf-ordl.line.
  END.

     
    ASSIGN
       oe-rell.ord-no    =  prmordno2 
       oe-rell.i-no      =  prmino2   
       oe-rell.po-no     =  prmpono2  
       oe-rell.qty       =  prmqty    
       oe-rell.tag       =  prmtag    
       oe-rell.loc       =  prmloc    
       oe-rell.loc-bin   =  prmlocbin 
       oe-rell.job-no    =  prmjobno  
       oe-rell.job-no2   =  prmjobno2 
       oe-rell.cust-no   =  prmcustno 
       oe-rell.cases     =  prmcases  
       oe-rell.qty-case  =  prmqtycas 
       oe-rell.partial   =  prmpartial
       oe-rell.rel-no    =  prmrelno2 
       oe-rell.b-ord-no  =  prmbordno 
       oe-rell.s-code    =  prmscod   
       /*oe-rell.part-no   =  prmpartno */
       oe-rell.link-no   =  prmlinkno   .

    FIND FIRST oe-rel
      WHERE oe-rel.company EQ oe-rell.company
        AND oe-rel.ord-no  EQ oe-rell.ord-no
        AND oe-rel.ship-id EQ oe-relh.ship-id
        AND oe-rel.i-no    EQ oe-rell.i-no
        AND oe-rel.line    EQ oe-rell.line
        AND oe-rel.po-no   EQ old-po-no
        AND oe-rel.link-no EQ 0
        AND NOT CAN-FIND(FIRST b-oe-rell
                         WHERE b-oe-rell.company  EQ oe-rel.company
                           AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                           AND b-oe-rell.i-no     EQ oe-rel.i-no
                           AND b-oe-rell.line     EQ oe-rel.line
                           AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                           AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                           AND b-oe-rell.po-no    EQ oe-rel.po-no
                           AND ROWID(b-oe-rell)   NE ROWID(oe-rell))
      NO-ERROR.
  IF AVAIL oe-rel THEN 
      ASSIGN 
        oe-rel.qty = oe-rell.qty
        oe-rel.rel-no = oe-rell.rel-no.

  IF oe-rell.po-no NE old-po-no THEN
  FOR EACH oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.ship-id  EQ oe-relh.ship-id
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.po-no    EQ old-po-no
        AND oe-rel.link-no  EQ 0:
    oe-rel.po-no = oe-rell.po-no.
  END.

  RELEASE oe-rel.

  /*lv-rowid = ROWID(oe-rell).*/

      ASSIGN
      prmAction = "View"
       .
 

END.


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:

      FIND oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra2) NO-LOCK NO-ERROR.

      IF AVAIL oe-relh AND oe-relh.posted THEN DO:
      cError =  "This release has already been posted, no update allowed." .
      RETURN .
     END.
     
    /*IF prmloc EQ "" THEN
      ttViewReleaseOrder.loc = g_loc.

    IF prmscod EQ "" THEN
      ttViewReleaseOrder.scod   = 
          IF v-ship-from THEN "B" ELSE "I".*/

    IF prmordno2 NE 0 THEN
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ prmComp
          AND oe-ord.ord-no  EQ prmordno2
        NO-LOCK NO-ERROR.

    IF NOT AVAIL oe-ord THEN do: 
        cError = "Order is invalid. Try help...".
        RETURN.
    END.

    IF AVAIL oe-ord AND oe-ord.cust-no <> oe-relh.cust-no THEN do:
         cError = "Sorry, order must match current customer. Try help..."  .
         RETURN.
    END.

    FIND FIRST bf-ordl
        WHERE bf-ordl.company EQ cocode
          AND bf-ordl.ord-no  EQ INT(prmordno2)
          AND bf-ordl.i-no    EQ prmino2
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-ordl THEN DO:
      cError =  " Item not on Order, try help..." .
      RETURN .
    END.

    ASSIGN
     prmjobno = FILL(" ",6 - LENGTH(prmjobno)) + prmjobno.

    FIND FIRST bf-ordl
          WHERE bf-ordl.company  EQ cocode
            AND bf-ordl.ord-no   EQ INT(prmordno2)
            AND bf-ordl.i-no     EQ prmino2
            AND (TRIM(bf-ordl.job-no) EQ "" OR
                 (bf-ordl.job-no EQ prmjobno AND
                  bf-ordl.job-no2 EQ INT(prmjobno2)))
          NO-LOCK NO-ERROR.
      IF NOT AVAIL bf-ordl THEN DO:
        cError = "Job not for Order/FG, try again..." .
        RETURN .
      END.

      IF prmcustno NE "" AND
       prmcustno NE oe-relh.cust-no 
       /*NOT CAN-FIND(FIRST fg-bin 
                     WHERE fg-bin.company EQ cocode
                       AND fg-bin.i-no    EQ prmino2
                       AND fg-bin.job-no  EQ prmjobno
                       AND fg-bin.job-no2 EQ INT(prmjobno2)
                       AND fg-bin.loc     EQ prmloc
                       AND fg-bin.loc-bin EQ prmlocbin
                       AND fg-bin.tag     EQ prmtag
                       AND fg-bin.cust-no EQ prmcustno) */
        THEN DO:
          cError =  "Invalid Customer#, try help..." .
          RETURN.
      END.

      /*FIND FIRST b-oe-rell
          WHERE b-oe-rell.r-no   EQ oe-relh.r-no
          AND b-oe-rell.s-code NE prmscod
          AND ROWID(b-oe-rell) NE ROWID(oe-rell)
       NO-LOCK NO-ERROR.
      IF AVAIL b-oe-rell THEN do:
          cError = "Invalid S/I. try help..." .
          RETURN .
      END. */

      IF LOOKUP(prmscod,lv-s-codes) LE 0 /*OR
       CAN-FIND(FIRST b-oe-rell
                WHERE b-oe-rell.r-no   EQ oe-relh.r-no
                  AND b-oe-rell.s-code NE prmscod
                  AND ROWID(b-oe-rell) NE ROWID(oe-rell))*/
    THEN DO:
      cError = "Invalid S/I. try help..." .
          RETURN .
    END.
     
END.


IF prmAction = "Update" THEN DO:
     FIND oe-relh WHERE oe-relh.company = prmComp 
            AND oe-relh.r-no eq int(prmextra2) NO-LOCK NO-ERROR.
    
     FIND FIRST oe-rell WHERE oe-rell.company eq prmComp 
         AND oe-rell.r-no eq oe-relh.r-no AND oe-rell.rec_key EQ prmReckey EXCLUSIVE-LOCK NO-ERROR.
     FIND FIRST oe-ordl WHERE oe-ordl.company eq oe-rell.company 
         AND oe-ordl.ord-no eq oe-rell.ord-no 
         AND oe-ordl.i-no eq oe-rell.i-no 
         AND oe-ordl.line eq oe-rell.line EXCLUSIVE-LOCK NO-ERROR .

     /* Code placed here will execute PRIOR to standard behavior. */
  old-po-no = IF AVAIL oe-rell THEN oe-rell.po-no ELSE "".

  IF oe-rell.line eq 0 THEN DO:
    FIND FIRST bf-ordl
        WHERE bf-ordl.company EQ oe-rell.company
          AND bf-ordl.ord-no  EQ oe-rell.ord-no
          AND bf-ordl.i-no    EQ oe-rell.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL bf-ordl THEN oe-rell.line = bf-ordl.line.
  END.
     
    ASSIGN
       oe-rell.ord-no    =  prmordno2 
       oe-rell.i-no      =  prmino2   
       oe-rell.po-no     =  prmpono2  
       oe-rell.qty       =  prmqty    
       oe-rell.tag       =  prmtag    
       oe-rell.loc       =  prmloc    
       oe-rell.loc-bin   =  prmlocbin 
       oe-rell.job-no    =  prmjobno  
       oe-rell.job-no2   =  prmjobno2 
       oe-rell.cust-no   =  prmcustno 
       oe-rell.cases     =  prmcases  
       oe-rell.qty-case  =  prmqtycas 
       oe-rell.partial   =  prmpartial
       oe-rell.rel-no    =  prmrelno2 
       oe-rell.b-ord-no  =  prmbordno 
       oe-rell.s-code    =  prmscod   
       oe-ordl.part-no   =  prmpartno 
       oe-rell.link-no   =  prmlinkno   .

    FIND FIRST oe-rel
      WHERE oe-rel.company EQ oe-rell.company
        AND oe-rel.ord-no  EQ oe-rell.ord-no
        AND oe-rel.ship-id EQ oe-relh.ship-id
        AND oe-rel.i-no    EQ oe-rell.i-no
        AND oe-rel.line    EQ oe-rell.line
        AND oe-rel.po-no   EQ old-po-no
        AND oe-rel.link-no EQ 0
        AND NOT CAN-FIND(FIRST b-oe-rell
                         WHERE b-oe-rell.company  EQ oe-rel.company
                           AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                           AND b-oe-rell.i-no     EQ oe-rel.i-no
                           AND b-oe-rell.line     EQ oe-rel.line
                           AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                           AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                           AND b-oe-rell.po-no    EQ oe-rel.po-no
                           AND ROWID(b-oe-rell)   NE ROWID(oe-rell))
      NO-ERROR.
  IF AVAIL oe-rel THEN 
      ASSIGN 
        oe-rel.qty = oe-rell.qty
        oe-rel.rel-no = oe-rell.rel-no.

  IF oe-rell.po-no NE old-po-no THEN
  FOR EACH oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.ship-id  EQ oe-relh.ship-id
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.po-no    EQ old-po-no
        AND oe-rel.link-no  EQ 0:
    oe-rel.po-no = oe-rell.po-no.
  END.

  RELEASE oe-rel.

  /*lv-rowid = ROWID(oe-rell).*/


      ASSIGN
      prmAction = "View"
       .
        

END.  

/*********************************delete ******************************/

IF prmAction = "DeleteValidate" THEN DO:

     FIND FIRST  oe-relh WHERE oe-relh.company = prmComp
         AND oe-relh.r-no eq int(prmextra2) NO-LOCK NO-ERROR.

     IF AVAIL oe-relh AND oe-relh.posted THEN DO:
      cError =  "This release has already been posted, no delete allowed." .
      RETURN .
     END.
END. /* DeleteValidate  */


IF prmAction = "DataDelete"  THEN DO:

     FIND oe-relh WHERE oe-relh.company = prmComp
        AND oe-relh.r-no eq int(prmextra2) NO-LOCK NO-ERROR.

  FIND FIRST oe-rell WHERE oe-rell.company eq prmComp 
         AND oe-rell.rec_key EQ prmreckey EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL oe-rell THEN
      DELETE oe-rell  .

  FIND FIRST oe-rell WHERE oe-rell.company = prmComp 
      AND oe-rell.r-no EQ oe-relh.r-no NO-LOCK NO-ERROR.
  IF AVAIL oe-rell THEN ASSIGN
      prmAction = "View"
      prmReckey = oe-rell.rec_key .

END.  

/*******************************View************************************/


IF prmAction = "View" THEN DO:
  
    /*FIND oe-relh WHERE oe-relh.company = prmComp NO-LOCK NO-ERROR.*/
    
     FOR EACH oe-rell WHERE oe-rell.company eq prmComp 
         AND oe-rell.rec_key EQ prmreckey
         use-index r-no NO-LOCK,
         FIRST oe-ordl WHERE oe-ordl.company eq oe-rell.company 
         AND oe-ordl.ord-no eq oe-rell.ord-no 
         AND oe-ordl.i-no eq oe-rell.i-no 
         AND oe-ordl.line eq oe-rell.line NO-LOCK BY oe-rell.ord-no:

        CREATE ttViewReleaseOrder.
           ASSIGN 
                 ttViewReleaseOrder.ordno     = oe-rell.ord-no
                 ttViewReleaseOrder.ino       = oe-rell.i-no
                 ttViewReleaseOrder.pono      = oe-rell.po-no
                 ttViewReleaseOrder.qty       = oe-rell.qty 
                 ttViewReleaseOrder.tag       = oe-rell.tag 
                 ttViewReleaseOrder.loc       = oe-rell.loc
                 ttViewReleaseOrder.locbin    = oe-rell.loc-bin
                 ttViewReleaseOrder.jobno     = oe-rell.job-no
                 ttViewReleaseOrder.jobno2    = oe-rell.job-no2 
                 ttViewReleaseOrder.custno    = oe-rell.cust-no         
                 ttViewReleaseOrder.cases     = oe-rell.cases
                 ttViewReleaseOrder.qtycas    = oe-rell.qty-case
                 ttViewReleaseOrder.partial   = oe-rell.partial    
                 ttViewReleaseOrder.relno     = oe-rell.rel-no   
                 ttViewReleaseOrder.bordno    = oe-rell.b-ord-no  
                 ttViewReleaseOrder.scod      = oe-rell.s-code   
                 ttViewReleaseOrder.partno    = oe-ordl.part-no
                 ttViewReleaseOrder.linkno    = oe-rell.link-no 
                 ttViewReleaseOrder.reckey    = oe-rell.rec_key .
     END.
      
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "Viewadd" THEN DO:
  
    /*FIND oe-relh WHERE oe-relh.company = prmComp NO-LOCK NO-ERROR.*/
    
     FOR EACH oe-rell WHERE oe-rell.company eq prmComp 
         AND oe-rell.rec_key EQ prmreckey
         use-index r-no NO-LOCK:

        CREATE ttViewReleaseOrder.
           ASSIGN 
                 ttViewReleaseOrder.ordno     = oe-rell.ord-no
                 ttViewReleaseOrder.ino       = oe-rell.i-no
                 ttViewReleaseOrder.pono      = oe-rell.po-no
                 ttViewReleaseOrder.qty       = oe-rell.qty 
                 ttViewReleaseOrder.tag       = oe-rell.tag 
                 ttViewReleaseOrder.loc       = oe-rell.loc
                 ttViewReleaseOrder.locbin    = oe-rell.loc-bin
                 ttViewReleaseOrder.jobno     = oe-rell.job-no
                 ttViewReleaseOrder.jobno2    = oe-rell.job-no2 
                 ttViewReleaseOrder.custno    = oe-rell.cust-no         
                 ttViewReleaseOrder.cases     = oe-rell.cases
                 ttViewReleaseOrder.qtycas    = oe-rell.qty-case
                 ttViewReleaseOrder.partial   = oe-rell.partial    
                 ttViewReleaseOrder.relno     = oe-rell.rel-no   
                 ttViewReleaseOrder.bordno    = oe-rell.b-ord-no  
                 ttViewReleaseOrder.scod      = oe-rell.s-code   
                 /*ttViewReleaseOrder.partno    = oe-ordl.part-no*/
                 ttViewReleaseOrder.linkno    = oe-rell.link-no 
                 ttViewReleaseOrder.reckey    = oe-rell.rec_key .
           MESSAGE "add view " ttViewReleaseOrder.reckey .
     END.
      
END. /*IF prmAction = "Select" THEN DO:*/


