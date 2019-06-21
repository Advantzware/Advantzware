/*------------------------------------------------------------------------
    File        : Est_Goto.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 02  march 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttCorrGoTo NO-UNDO
    FIELD formno        AS INT
    FIELD blankno       AS INT
    FIELD partno        AS CHARACTER
    FIELD blqty         AS INT
    FIELD yldqty        AS INT
    FIELD price         AS CHAR
    FIELD wid           AS DECIMAL
    FIELD len           AS DECIMAL
    FIELD numup         AS INT
    
    FIELD vLine        AS INT
    FIELD vtype        AS INT
        .

DEFINE DATASET dsCorrGoTo FOR ttCorrGoTo.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlankNo     AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPartno       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBlQty        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmYldQty       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPrice        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmWid          AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLen          AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUp           AS DECIMAL NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrGoTo .
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser      = ?  THEN ASSIGN    prmUser       = "".
IF prmAction    = ?  THEN ASSIGN    prmAction     = "".
IF prmComp      = ?  THEN ASSIGN    prmComp       = "".
IF prmEstNum    = ?  THEN ASSIGN    prmEstNum     = "".
IF prmFormNo    = ?  THEN ASSIGN    prmFormNo     = 0.
IF prmBlankNo   = ?  THEN ASSIGN    prmBlankNo    = 0.
IF prmPartno    = ?  THEN ASSIGN    prmPartno     = "".
IF prmBlQty     = ?  THEN ASSIGN    prmBlQty      = 0.
IF prmYldQty    = ?  THEN ASSIGN    prmYldQty     = 0.
IF prmPrice     = ?  THEN ASSIGN    prmPrice      = "".
IF prmWid       = ?  THEN ASSIGN    prmWid        = 0.
IF prmLen       = ?  THEN ASSIGN    prmLen        = 0.
IF prmUp        = ?  THEN ASSIGN    prmUp         = 0.

DEFINE VAR vEstimate AS CHAR NO-UNDO.
DEFINE VAR prmLoc AS CHAR NO-UNDO.
DEF VAR ld-part-qty AS DEC NO-UNDO.
DEF VAR lv-bl-qty LIKE eb.bl-qty NO-UNDO.
DEF VAR lv-yld-qty LIKE eb.yld-qty NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.


DEF BUFFER xest FOR est.
DEF BUFFER xef FOR ef.
DEF BUFFER xeb FOR eb.
DEF BUFFER multbl FOR reftable.

DEF BUFFER b-ref FOR reftable.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR ll-new-form AS LOG NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR hld-yld-qty LIKE eb.yld-qty NO-UNDO.
DEF VAR hld-num-up  LIKE eb.num-up NO-UNDO.
DEF VAR ll-change AS LOG NO-UNDO.
DEF VAR v-form-no LIKE eb.form-no NO-UNDO.
DEF VAR v-blank-no LIKE eb.blank-no NO-UNDO.
DEF VAR v-num-up LIKE eb.num-up NO-UNDO.
DEF VAR lv-prev-val-1 AS CHAR NO-UNDO.
DEFINE VAR wid AS DECIMAL NO-UNDO.

{sys/inc/var.i NEW SHARED}

    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmLoc  =  "MAIN" .
cocode = prmComp.
locode = prmLoc.

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).
/*************************main block****************************/

 FIND FIRST  est WHERE est.company = prmComp  AND (est.est-no = vEstimate ) NO-LOCK NO-ERROR.
 
 IF AVAIL est THEN DO:
 
  FOR EACH multbl  WHERE multbl.reftable EQ "est\d-multbl.w" 
                              AND multbl.company  EQ est.company      
                              AND multbl.loc      EQ est.loc          
                              AND multbl.code     EQ est.est-no  USE-INDEX reftable:
    DELETE multbl.
   
  END.


  FOR EACH ef NO-LOCK
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no,
      EACH eb NO-LOCK
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no:
       
    CREATE multbl.
    ASSIGN
     multbl.reftable = "est\d-multbl.w"
     multbl.company  = est.company
     multbl.loc      = est.loc
     multbl.code     = est.est-no
     multbl.code2    = ef.board
     multbl.dscr     = ef.brd-dscr
     multbl.val[1]   = eb.form-no
     multbl.val[2]   = eb.blank-no
     multbl.val[3]   = DEC(RECID(eb)).
  END.
   
  
END.

/***********************end main block******************************/

/*************************************function***************************************************/  

FUNCTION display-bl-qty RETURNS INTEGER  ( )  :

  
  IF est.est-type EQ 2 OR est.est-type EQ 6 THEN DO:
    IF est.est-type EQ 2 THEN
      ASSIGN
       lv-bl-qty  = eb.bl-qty
       lv-yld-qty = eb.cust-%.
    ELSE
      ASSIGN
       lv-bl-qty  = est.est-qty[1]
       lv-yld-qty = eb.yld-qty.

    {sys/inc/partqty1.i ld-part-qty lv-yld-qty}

    lv-bl-qty = lv-bl-qty * ld-part-qty.
  END.

  ELSE lv-bl-qty = eb.bl-qty.
  
  RETURN lv-bl-qty.   /* Function return value. */

END FUNCTION.


FUNCTION display-yld-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  DEF VAR lv-yld-qty LIKE eb.yld-qty NO-UNDO.


  lv-yld-qty = IF est.est-type EQ 2 THEN eb.cust-%  ELSE
               IF est.est-type EQ 6 THEN eb.yld-qty ELSE eb.yld-qty.
  
  RETURN lv-yld-qty.   /* Function return value. */

END FUNCTION.

FUNCTION display-num-wid RETURNS INTEGER
  ( /* parameter-definitions */ ) :

  RETURN IF est.est-type GE 5 THEN eb.num-len ELSE eb.num-wid. /* Function return value. */

END FUNCTION.

FUNCTION display-num-len RETURNS INTEGER
  ( /* parameter-definitions */ ) :

  RETURN IF est.est-type GE 5 THEN eb.num-wid ELSE eb.num-len. /* Function return value. */

END FUNCTION.
/*****************end function*****************/

IF prmAction = "Update" THEN DO:
    FIND FIRST est WHERE est.est-no = vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.

    FIND FIRST b-ref NO-LOCK
        WHERE b-ref.reftable EQ "est\d-multbl.w"
          AND b-ref.company  EQ est.company
          AND b-ref.loc      EQ est.loc
          AND b-ref.code     EQ est.est-no
          AND b-ref.val[1]   EQ INT(prmFormNo)
        NO-ERROR.
    IF NOT AVAIL b-ref THEN DO:
       cError =  "Form# does not exist on this estimate.. " .
                RETURN .
    END.

    IF INT(prmBlankNo) EQ 0 THEN DO:
      cError =  "Blank Number may not be zero..." .
      RETURN .
    END.
  
      IF est.est-type NE 2 AND
      est.est-type NE 5 AND
      est.est-type NE 6 THEN DO :
          ASSIGN
              prmUp = prmWid * prmLen  .
      END.

  IF prmWid > prmLen THEN DO:
      ASSIGN
          wid = prmLen 
          prmlen = prmWid
          prmWid = wid .
  END.


END.

IF prmAction = "Update"  THEN DO:

    FIND FIRST est WHERE est.est-no = vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FOR EACH eb WHERE eb.company = est.company  AND eb.loc = est.loc 
       AND eb.est-no = est.est-no AND eb.form-no = prmFormNo AND eb.Blank-no = prmBlankNo EXCLUSIVE-LOCK, 
       FIRST ef WHERE ef.company = eb.company AND ef.loc = eb.loc 
       AND ef.est-no = eb.est-no 
       AND ef.form-no = eb.form-no EXCLUSIVE-LOCK, 
     FIRST reftable WHERE reftable.reftable eq "est\d-multbl.w" and 
       reftable.company  eq eb.company       and 
       reftable.loc      eq eb.loc           and 
       reftable.code     eq eb.est-no        and 
       reftable.val[3]   eq dec(recid(eb)) EXCLUSIVE-LOCK :


      

       ASSIGN
           ll-change  = YES
           v-num-up   = prmUp
           v-form-no  = prmFormNo
           v-blank-no = prmBlankNo .

            IF est.est-type EQ 2 AND
                est.est-type EQ 6 THEN DO :
                    ll-change  = NO .
            END.

            IF est.est-type NE 2 AND
                est.est-type NE 5 AND
                est.est-type NE 6 THEN DO :
                IF DEC(prmBlQty) EQ 0 THEN DO:
                    cError = "Request Qty may not be zero..."  .
                    RETURN .
                END.
            END.
                IF est.est-type EQ 8 OR est.est-type EQ 4 THEN DO:
                IF DEC(prmUp) LT 1 THEN
                    ASSIGN
                    prmWid  = 1
                    prmLen  = 1
                    prmUp   = 1 .
                   END.

                IF DEC(prmYldQty) NE hld-yld-qty AND
                    DEC(prmYldQty) MODULO
                    DEC(prmUp) GT 0          AND
                    v-qty NE 0         THEN
                    prmUp =  (TRUNC(prmYldQty / v-qty,0) +
                                    INT(prmYldQty MODULO v-qty GT 0)).

                IF prmUp NE hld-num-up THEN DO:
                ll-ans = YES .
               
                IF ll-ans THEN DO:
                v-qty = DEC(prmYldQty) / DEC(prmUp).
                {sys/inc/roundup.i v-qty}
                prmYldQty = (v-qty * DEC(prmUp)).
                END.
                END.

                ASSIGN
                    hld-yld-qty = prmYldQty
                    hld-num-up  = prmUp .

                
                v-qty = hld-yld-qty / hld-num-up.
                {sys/inc/roundup.i v-qty}  

                    

                    IF AVAIL eb  THEN DO:
                        ASSIGN
                            reftable.val[1] = prmFormNo
                            reftable.val[2] = prmBlankNo
                            eb.part-no      = prmPartno
                            eb.yrprice      = IF prmPrice = "Yield" THEN TRUE ELSE FALSE .
                            
                            IF est.est-type NE 2 AND est.est-type NE 5 AND
                                est.est-type NE 6 THEN DO :

                                ASSIGN
                                    eb.bl-qty       = prmBlQty 
                                    eb.yld-qty      = prmYldQty
                                    eb.num-wid      = prmWid
                                    eb.num-len      = prmLen
                                    eb.num-up       = prmUp  .
                            END.
                           
                    END.
                  
                    RUN finish-assign NO-ERROR.
                    ASSIGN 
                        prmAction = "View" .
   END.

END.


IF prmAction = "Select"  THEN DO:
 FIND FIRST est WHERE est.est-no = vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH eb WHERE eb.company = est.company  AND eb.loc = est.loc 
        AND eb.est-no = est.est-no NO-LOCK, 
        FIRST ef WHERE ef.company = eb.company AND ef.loc = eb.loc 
        AND ef.est-no = eb.est-no 
        AND ef.form-no = eb.form-no NO-LOCK, 
      FIRST reftable WHERE reftable.reftable eq "est\d-multbl.w" and 
        reftable.company  eq eb.company       and 
        reftable.loc      eq eb.loc           and 
        reftable.code     eq eb.est-no        and 
        reftable.val[3]   eq dec(recid(eb)) NO-LOCK :

        IF AVAIL eb  THEN DO:

            CREATE ttCorrGoTo.
            ASSIGN
                 ttCorrGoTo.formno      = reftable.val[1]   
                 ttCorrGoTo.blankno     = reftable.val[2]
                 ttCorrGoTo.partno      = eb.part-no 
                 ttCorrGoTo.blqty       = display-bl-qty()   /*eb.bl-qty*/
                 ttCorrGoTo.yldqty      = display-yld-qty()  /*eb.yld-qty*/
                 ttCorrGoTo.price       = IF eb.yrprice = YES THEN "Yield" ELSE "Request" 
                 ttCorrGoTo.wid         = eb.num-wid  /*eb.num-wid*/
                 ttCorrGoTo.len         = eb.num-len  /*eb.num-len*/
                 ttCorrGoTo.numup       = eb.num-up 
                 ttCorrGoTo.vtype       = est.est-type                    .   
                
        END.
    END.
END.

IF prmAction = "View"  THEN DO:
 FIND FIRST est WHERE est.est-no = vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH eb WHERE eb.company = est.company  AND eb.loc = est.loc 
        AND eb.est-no = est.est-no AND eb.form-no = prmFormNo AND eb.Blank-no = prmBlankNo NO-LOCK, 
        FIRST ef WHERE ef.company = eb.company AND ef.loc = eb.loc 
        AND ef.est-no = eb.est-no 
        AND ef.form-no = eb.form-no NO-LOCK, 
      FIRST reftable WHERE reftable.reftable eq "est\d-multbl.w" and 
        reftable.company  eq eb.company       and 
        reftable.loc      eq eb.loc           and 
        reftable.code     eq eb.est-no        and 
        reftable.val[3]   eq dec(recid(eb)) NO-LOCK :

        IF AVAIL eb  THEN DO:

            CREATE ttCorrGoTo.
            ASSIGN
                 ttCorrGoTo.formno      = reftable.val[1]   
                 ttCorrGoTo.blankno     = reftable.val[2]
                 ttCorrGoTo.partno      = eb.part-no 
                 ttCorrGoTo.blqty       = display-bl-qty()   /*eb.bl-qty*/
                 ttCorrGoTo.yldqty      = display-yld-qty()  /*eb.yld-qty*/
                 ttCorrGoTo.price       = IF eb.yrprice = YES THEN "Yield" ELSE "Request"
                 ttCorrGoTo.wid         = eb.num-wid  /*eb.num-wid*/
                 ttCorrGoTo.len         = eb.num-len  /*eb.num-len*/
                 ttCorrGoTo.numup       = eb.num-up  .
                
        END.
    END.
END.


/*********************ptocdure***********************/
PROCEDURE finish-assign :

  DEF VAR lv-die-in LIKE ef.die-in NO-UNDO.
  DEF VAR ll-die-changed AS LOG NO-UNDO.
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR li-qty AS INT NO-UNDO.
  DEF VAR lv-frm LIKE eb.form-no INIT 0 NO-UNDO.
  DEF VAR lv-blk LIKE eb.blank-no INIT 0 NO-UNDO.

  DEF BUFFER multbl FOR reftable.


  FIND CURRENT eb.
  FIND CURRENT est.

  

  lv-die-in = eb.die-in.
  IF eb.die-in NE 0 THEN eb.die-in = (eb.die-in / v-num-up) * eb.num-up.
  IF lv-die-in NE eb.die-in THEN ll-die-changed = YES.



  RELEASE xef.
  IF ll-change         AND
     est.est-type NE 2 AND
     est.est-type NE 5 AND
     est.est-type NE 6 THEN
  FIND FIRST xef
      WHERE xef.company EQ eb.company
        AND xef.est-no  EQ eb.est-no
        AND xef.form-no EQ reftable.val[1]
      NO-LOCK NO-ERROR.

  IF AVAIL xef THEN DO:
    v-qty = eb.yld-qty / eb.num-up.
    {sys/inc/roundup.i v-qty}
    ASSIGN
     eb.yld-qty = v-qty * eb.num-up
     /*xef.die-in = eb.die-in*/.

    ll-ans = NO.
    
    

    FOR EACH xeb
        WHERE xeb.company EQ xef.company
          AND xeb.est-no  EQ xef.est-no
          AND xeb.form-no EQ xef.form-no
          AND RECID(xeb)  NE RECID(eb)
          AND xeb.yld-qty NE v-qty * xeb.num-up:
      ll-ans = YES .
      /*MESSAGE "For all other Blanks on this Form..." SKIP
              "Click the YES button to calculate the layout based on the Request Qty" SKIP
              "Click the NO  button to calculate the layout based on the Yield Qty"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-ans.*/
      LEAVE.
    END.
    RELEASE xeb.

    FOR EACH xeb
        WHERE xeb.company EQ xef.company
          AND xeb.est-no  EQ xef.est-no
          AND xeb.form-no EQ xef.form-no
          AND ROWID(xeb)  NE ROWID(eb)
          AND xeb.yld-qty NE v-qty * xeb.num-up
        BY xeb.blank-no:
      /*IF xeb.yld-qty LT xeb.bl-qty THEN xeb.yld-qty = xeb.bl-qty.*/
      IF xeb.yld-qty EQ 0 THEN xeb.yld-qty = xeb.bl-qty.
      
      
        
      ASSIGN
       li-qty      = IF ll-ans THEN xeb.bl-qty ELSE xeb.yld-qty
       lv-die-in   = xeb.die-in
       xeb.die-in  = xeb.die-in / xeb.num-up
       xeb.num-up  = TRUNC(li-qty / v-qty,0) + INT(li-qty MODULO v-qty GT 0)
       xeb.die-in  = xeb.die-in * xeb.num-up
       xeb.yld-qty = v-qty * xeb.num-up
       /*xef.die-in  = xef.die-in + xeb.die-in*/.

      IF lv-die-in NE xeb.die-in THEN ll-die-changed = YES.

      IF xeb.num-wid * xeb.num-len NE xeb.num-up THEN
        ASSIGN
         xeb.num-wid = xeb.num-up
         xeb.num-len = 1.
      
    END.
    RELEASE xeb.
  END.

  FIND CURRENT eb  NO-LOCK.
  FIND CURRENT est NO-LOCK.

  IF ll-die-changed THEN RUN est/updefdie.p (ROWID(ef)).

  FOR EACH multbl
      WHERE multbl.reftable EQ "est\d-multbl.w"
        AND multbl.company  EQ est.company
        AND multbl.loc      EQ est.loc
        AND multbl.code     EQ est.est-no
      BY multbl.val[1] DESC
      BY multbl.val[2] DESC:

    ASSIGN
     multbl.val[1] = (multbl.val[1] * 1000) +
                     (1 * (IF multbl.val[1] LT v-form-no THEN -1 ELSE 1))
     multbl.val[2] = (multbl.val[2] * 1000) +
                     (1 * (IF multbl.val[2] LT v-blank-no THEN -1 ELSE 1)).
  END.

  FOR EACH multbl
      WHERE multbl.reftable EQ "est\d-multbl.w"
        AND multbl.company  EQ est.company
        AND multbl.loc      EQ est.loc
        AND multbl.code     EQ est.est-no
      BREAK BY multbl.val[1]
            BY multbl.val[2]:

    IF FIRST-OF(multbl.val[1]) THEN lv-frm = lv-frm + 1.

    ASSIGN
     lv-blk        = lv-blk + 1
     multbl.val[1] = lv-frm
     multbl.val[2] = lv-blk.

    IF LAST-OF(multbl.val[1]) THEN lv-blk = 0.
  END.
  

END PROCEDURE.
