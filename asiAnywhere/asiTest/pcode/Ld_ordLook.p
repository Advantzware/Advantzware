
/*------------------------------------------------------------------------
    File         : Ld_ordlook
    Purpose     : Load Tag Order lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrderLoadTagLookup NO-UNDO 
    FIELD ord_no          AS INT
    FIELD est_no           AS CHARACTER 
    FIELD job             AS CHAR
    FIELD job2            AS INT
    FIELD i_no            AS CHAR
    FIELD part_no         AS CHAR
    FIELD qty             AS DEC
    FIELD inv_qty         AS INT
    FIELD ship_qty        AS INT
    FIELD qty_prod        AS INT
    FIELD bal_hand        AS INT
    FIELD ord             AS INT
    FIELD ext_lodtg       AS CHAR.


DEFINE DATASET dsOrderLoadTagLookup FOR ttOrderLoadTagLookup .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderLoadTagLookup.
       
DEF VAR prmComp AS CHAR NO-UNDO.


def var lv-ord-ok as cha init "R,I,S,P,A,N,U" no-undo.

def var lv-first-time as log init yes no-undo.
DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR li-prod AS INT NO-UNDO.
DEF VAR li-bal AS INT NO-UNDO.


IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmUser       = ? THEN ASSIGN prmUser       = "".
IF prmField      = ? THEN ASSIGN prmField      = "".
IF prmCondition  = ? THEN ASSIGN prmCondition  = "".
IF prmText       = ? THEN ASSIGN prmText       = "".

/*ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .*/


DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.
DEF VAR v-bardir-chr AS CHAR NO-UNDO.


IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FUNCTION get-bal RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  /*li = li-bal - (IF AVAIL oe-ordl THEN oe-ordl.ship-qty ELSE 0).*/

  IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
  FOR EACH fg-bin
      WHERE fg-bin.company EQ prmComp
        AND fg-bin.job-no  EQ oe-ordl.job-no
        AND fg-bin.job-no2 EQ oe-ordl.job-no2
        AND fg-bin.i-no    EQ oe-ordl.i-no
      NO-LOCK:
    li = li + fg-bin.qty.
  END.

  RETURN li.    /* Function return value. */ 

END FUNCTION.



FUNCTION get-ord-no RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF AVAIL oe-ordl THEN oe-ordl.ord-no ELSE 0.
                                          /* Function return value. */

END FUNCTION.



FUNCTION get-prod RETURNS INTEGER
  ( /* OUTPUT op-bal AS INT */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
  FOR EACH fg-act
      WHERE fg-act.company EQ prmComp
        AND fg-act.job-no  EQ oe-ordl.job-no
        AND fg-act.job-no2 EQ oe-ordl.job-no2
        AND fg-act.i-no    EQ oe-ordl.i-no
      NO-LOCK:
    li = li + fg-act.qty.
  END.
 
  

  RETURN li.   /* Function return value. */

END FUNCTION.


if prmAction <> "search" then do:
        
        FOR EACH oe-ordl WHERE oe-ordl.company eq prmComp AND
            ASI.oe-ordl.opened eq yes 
            use-index opened NO-LOCK:

            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    
        create ttOrderLoadTagLookup.
        assign                                     
            ttOrderLoadTagLookup.ord_no   = oe-ordl.ord-no
            ttOrderLoadTagLookup.est_no   = oe-ordl.est-no 
            ttOrderLoadTagLookup.job      = oe-ordl.job-no
            ttOrderLoadTagLookup.job2     = oe-ordl.job-no2 
            ttOrderLoadTagLookup.i_no     = oe-ordl.i-no
            ttOrderLoadTagLookup.part_no  = oe-ordl.part-no
            ttOrderLoadTagLookup.qty      = oe-ordl.qty     
            ttOrderLoadTagLookup.inv_qty  = oe-ordl.inv-qty 
            ttOrderLoadTagLookup.ship_qty = oe-ordl.ship-qty
            ttOrderLoadTagLookup.qty_prod = get-prod()
            ttOrderLoadTagLookup.bal_hand = get-bal()
            ttOrderLoadTagLookup.ord      = get-ord-no().
            
        END.
END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
if prmField = "ord-no" then do:
     if prmCondition = "EQUAL" then do:
          FOR EACH oe-ordl WHERE oe-ordl.company eq prmComp 
              AND ASI.oe-ordl.opened eq yes 
              AND oe-ordl.ord-no = int(prmText)
            use-index opened NO-LOCK:
            
            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    
        create ttOrderLoadTagLookup.
        assign                                     
            ttOrderLoadTagLookup.ord_no   = oe-ordl.ord-no
            ttOrderLoadTagLookup.est_no   = oe-ordl.est-no 
            ttOrderLoadTagLookup.job      = oe-ordl.job-no
            ttOrderLoadTagLookup.job2     = oe-ordl.job-no2
            ttOrderLoadTagLookup.i_no     = oe-ordl.i-no
            ttOrderLoadTagLookup.part_no  = oe-ordl.part-no
            ttOrderLoadTagLookup.qty      = oe-ordl.qty     
            ttOrderLoadTagLookup.inv_qty  = oe-ordl.inv-qty 
            ttOrderLoadTagLookup.ship_qty = oe-ordl.ship-qty
            /*ttOrderLoadTagLookup.qty_prod = get-prod()
            ttOrderLoadTagLookup.bal_hand = get-bal()
            ttOrderLoadTagLookup.ord      = get-ord-no()*/.
            
        END. 
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-ordl WHERE oe-ordl.company eq prmComp 
              AND ASI.oe-ordl.opened eq yes 
              AND oe-ordl.ord-no = int(prmText)
            use-index opened NO-LOCK:
            
            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    
        create ttOrderLoadTagLookup.
        assign                                     
            ttOrderLoadTagLookup.ord_no   = oe-ordl.ord-no
            ttOrderLoadTagLookup.est_no   = oe-ordl.est-no 
            ttOrderLoadTagLookup.job      = oe-ordl.job-no
            ttOrderLoadTagLookup.job2     = oe-ordl.job-no2
            ttOrderLoadTagLookup.i_no     = oe-ordl.i-no
            ttOrderLoadTagLookup.part_no  = oe-ordl.part-no
            ttOrderLoadTagLookup.qty      = oe-ordl.qty     
            ttOrderLoadTagLookup.inv_qty  = oe-ordl.inv-qty 
            ttOrderLoadTagLookup.ship_qty = oe-ordl.ship-qty
           /* ttOrderLoadTagLookup.qty_prod = get-prod()
            ttOrderLoadTagLookup.bal_hand = get-bal()
            ttOrderLoadTagLookup.ord      = get-ord-no() */.
            
        END. 
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "ord-no" then do:*/



if prmField = "job-no" then do:
     if prmCondition = "EQUAL" then do:
          FOR EACH oe-ordl WHERE oe-ordl.company eq prmComp 
              AND ASI.oe-ordl.opened eq yes 
              AND oe-ordl.job-no = (prmText)
            use-index opened NO-LOCK:
            
            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    
        create ttOrderLoadTagLookup.
        assign                                     
            ttOrderLoadTagLookup.ord_no   = oe-ordl.ord-no
            ttOrderLoadTagLookup.est_no   = oe-ordl.est-no 
            ttOrderLoadTagLookup.job      = oe-ordl.job-no
            ttOrderLoadTagLookup.job2     = oe-ordl.job-no2
            ttOrderLoadTagLookup.i_no     = oe-ordl.i-no
            ttOrderLoadTagLookup.part_no  = oe-ordl.part-no
            ttOrderLoadTagLookup.qty      = oe-ordl.qty     
            ttOrderLoadTagLookup.inv_qty  = oe-ordl.inv-qty 
            ttOrderLoadTagLookup.ship_qty = oe-ordl.ship-qty
            /*ttOrderLoadTagLookup.qty_prod = get-prod()
            ttOrderLoadTagLookup.bal_hand = get-bal()
            ttOrderLoadTagLookup.ord      = get-ord-no()*/.
            
        END. 
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-ordl WHERE oe-ordl.company eq prmComp 
              AND ASI.oe-ordl.opened eq yes 
              AND oe-ordl.job-no BEGINS (prmText)
            use-index opened NO-LOCK:
            
            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    
        create ttOrderLoadTagLookup.
        assign                                     
            ttOrderLoadTagLookup.ord_no   = oe-ordl.ord-no
            ttOrderLoadTagLookup.est_no   = oe-ordl.est-no 
            ttOrderLoadTagLookup.job      = oe-ordl.job-no
            ttOrderLoadTagLookup.job2     = oe-ordl.job-no2
            ttOrderLoadTagLookup.i_no     = oe-ordl.i-no
            ttOrderLoadTagLookup.part_no  = oe-ordl.part-no
            ttOrderLoadTagLookup.qty      = oe-ordl.qty     
            ttOrderLoadTagLookup.inv_qty  = oe-ordl.inv-qty 
            ttOrderLoadTagLookup.ship_qty = oe-ordl.ship-qty
           /* ttOrderLoadTagLookup.qty_prod = get-prod()
            ttOrderLoadTagLookup.bal_hand = get-bal()
            ttOrderLoadTagLookup.ord      = get-ord-no()*/.
            
        END. 
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "ord-no" then do:*/




END. /* IF prmAction = search then do: */


FOR EACH ttOrderLoadTagLookup  NO-LOCK:

    FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ prmComp
      AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-bardir-chr = sys-ctrl.char-fld.


    FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ prmComp
        AND job-hdr.job-no  GE ttOrderLoadTagLookup.job
        AND job-hdr.job-no  LE ttOrderLoadTagLookup.job
         NO-ERROR.

    IF AVAIL job-hdr THEN
       v-cust-no = job-hdr.cust-no.
    ELSE DO:
       FIND FIRST oe-ord WHERE 
            oe-ord.company EQ prmComp AND
            oe-ord.ord-no  EQ INT(ttOrderLoadTagLookup.ord)
            NO-LOCK NO-ERROR.

       IF AVAIL oe-ord THEN
          v-cust-no = oe-ord.cust-no.
     END.     

     IF v-cust-no NE "" THEN
        FIND FIRST reftable WHERE
             reftable.reftable EQ "cp-lab-p" AND
             reftable.company  EQ prmComp AND
             reftable.loc      GE ttOrderLoadTagLookup.i_no AND
             reftable.loc      LE ttOrderLoadTagLookup.i_no AND
             reftable.CODE     EQ v-cust-no
             NO-LOCK NO-ERROR.

     IF AVAIL reftable AND reftable.dscr NE "" THEN do:
        ttOrderLoadTagLookup.ext_lodtg = (IF reftable.dscr <> "" THEN reftable.dscr ELSE v-bardir-chr).
        MESSAGE "ttOrderLoadTagLookup.ext_lodtg " ttOrderLoadTagLookup.ext_lodtg .
     END.
     ELSE
        IF INT(ttOrderLoadTagLookup.ord) NE 0 AND
           INT(ttOrderLoadTagLookup.ord) NE 0 THEN
        DO:
           FIND FIRST oe-rel WHERE
                oe-rel.company EQ prmComp AND
                oe-rel.i-no    GE ttOrderLoadTagLookup.i_no AND
                oe-rel.i-no    LE ttOrderLoadTagLookup.i_no AND
                oe-rel.ord-no  GE INT(ttOrderLoadTagLookup.ord) AND
                oe-rel.ord-no  LE INT(ttOrderLoadTagLookup.ord)
                NO-LOCK NO-ERROR.
           
           IF AVAIL oe-rel THEN 
              FIND FIRST shipto NO-LOCK 
               WHERE shipto.company EQ prmComp 
                 AND shipto.cust-no EQ oe-rel.cust-no 
                 AND shipto.ship-id EQ oe-rel.ship-id 
               USE-INDEX ship-id NO-ERROR.
           ELSE
              FIND FIRST shipto NO-LOCK
               WHERE shipto.company EQ prmComp 
                 AND shipto.cust-no EQ v-cust-no 
                 AND shipto.ship-id EQ v-cust-no
                  USE-INDEX ship-id NO-ERROR.
           
              IF AVAIL shipto THEN DO:
                 IF AVAIL oe-rel THEN
                    v-cust-no = oe-rel.cust-no.
           
                 FIND FIRST sys-ctrl-shipto NO-LOCK
                   WHERE sys-ctrl-shipto.company      EQ prmComp 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ prmComp 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ prmComp 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ prmComp AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                            ttOrderLoadTagLookup.ext_lodtg = sys-ctrl.char-fld.
                          ELSE
                             ttOrderLoadTagLookup.ext_lodtg = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ prmComp 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ prmComp 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ prmComp AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          ttOrderLoadTagLookup.ext_lodtg = sys-ctrl.char-fld.
                       ELSE
                          ttOrderLoadTagLookup.ext_lodtg = "".
                    END.
                 END.
              END.
        END.
        ELSE
        IF INT(ttOrderLoadTagLookup.ord) EQ 0 AND
           INT(ttOrderLoadTagLookup.ord) EQ 0 THEN
           DO:
              FIND FIRST shipto WHERE
                   shipto.company EQ prmComp AND
                   shipto.cust-no EQ v-cust-no AND
                   shipto.ship-id EQ v-cust-no
                   NO-LOCK NO-ERROR.

              IF AVAIL shipto THEN DO:
                 
                 FIND FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company      EQ prmComp AND
                      sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                      sys-ctrl-shipto.cust-vend    EQ YES AND
                      sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                      sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                      sys-ctrl-shipto.char-fld     NE ''
                      NO-LOCK NO-ERROR.

                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                   ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto WHERE
                         sys-ctrl-shipto.company      EQ prmComp AND
                         sys-ctrl-shipto.NAME         EQ "BARDIR" AND 
                         sys-ctrl-shipto.cust-vend    EQ YES AND
                         sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                         sys-ctrl-shipto.char-fld     NE ''
                         NO-LOCK NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ prmComp 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ prmComp AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             ttOrderLoadTagLookup.ext_lodtg = sys-ctrl.char-fld.
                          ELSE
                             ttOrderLoadTagLookup.ext_lodtg = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ prmComp 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ prmComp 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       ttOrderLoadTagLookup.ext_lodtg = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ prmComp AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          ttOrderLoadTagLookup.ext_lodtg = sys-ctrl.char-fld.
                       ELSE
                          ttOrderLoadTagLookup.ext_lodtg = "".
                    END.
                 END.
              END.


           END. /*begin_ord-no and end_ord-no eq 0*/
    ttOrderLoadTagLookup.ext_lodtg = REPLACE(ttOrderLoadTagLookup.ext_lodtg,'\','/'). 
END.

