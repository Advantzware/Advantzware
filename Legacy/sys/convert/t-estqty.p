/* t-estqty create est-qty */
DISABLE TRIGGERS FOR LOAD OF eb.
DISABLE TRIGGERS FOR LOAD OF ef.
disable triggers for load of est-qty.
disable triggers for load of est-op.
disable triggers for load of est-inst.
disable triggers for load of est-flm.
disable triggers for load of est-prep.

SESSION:SET-WAIT-STATE("general").

DISPLAY "Creating est-qty......  " STRING(TIME,"hh:mm:ss")  SKIP.
PAUSE 0.
FOR EACH est NO-LOCK.
    FIND FIRST est-qty WHERE est-qty.company = est.company
                         AND est-qty.est-no = est.est-no NO-ERROR.
    IF NOT AVAIL est-qty THEN CREATE est-qty.
    ASSIGN est-qty.company = est.company
           est-qty.est-no = est.est-no
           est-qty.eqty = est.est-qty[1]
           est-qty.qty[1] = est.est-qty[1]
           est-qty.qty[2] = est.est-qty[2]
           est-qty.qty[3] = est.est-qty[3]
           est-qty.qty[4] = est.est-qty[4]
           .
END.

DISPLAY "Updating ef ...........  " STRING(TIME,"hh:mm:ss:") SKIP .
PAUSE 0.
FOR EACH ef.
    FIND FIRST est-qty WHERE est-qty.company = ef.company AND
                             est-qty.est-no = ef.est-no NO-LOCK NO-ERROR.
    IF AVAIL est-qty THEN ef.eqty = est-qty.eqty.
END.

DISPLAY "Updating eb ..........  " STRING(TIME,"hh:mm:ss:") SKIP.
PAUSE 0.
FOR EACH eb.
    FIND FIRST est-qty WHERE est-qty.company = eb.company AND
                             est-qty.est-no = eb.est-no NO-LOCK NO-ERROR.
    IF AVAIL est-qty THEN eb.eqty = est-qty.eqty.
    IF eb.num-len = 0 THEN eb.num-len = 1.
    IF eb.num-wid = 0 THEN eb.num-wid = 1.
    
    IF eb.est-type = 1 THEN eb.bl-qty = eb.eqty.
    /* need for est-type 2 and 3 ???*/
END.

DISPLAY "Updating est-op .....  " STRING(TIME,"hh:mm:ss:") SKIP.
PAUSE 0.
FOR EACH est-op WHERE est-op.LINE < 500 . 
    IF est-op.company = "" THEN est-op.company = "001".
    FIND FIRST ef WHERE ef.company = est-op.company
                    AND ef.est-no = est-op.est-no  NO-LOCK NO-ERROR.

    IF AVAIL ef THEN ASSIGN est-op.eqty = ef.eqty.
    
    IF ef.est-type = 1 AND est-op.qty <> est-op.eqty THEN est-op.qty = est-op.eqty.

    DISP est-op.est-no FORM "x(8)".
    PAUSE 0.            
END.

DISPLAY "Updating est-prep ... " STRING(TIME,"hh:mm:ss:") SKIP.
PAUSE 0.
FOR EACH est-prep  .
    IF est-prep.company = "" THEN est-prep.company = "001".
    FIND FIRST ef WHERE ef.company = est-prep.company 
                    AND ef.est-no = est-prep.est-no NO-LOCK NO-ERROR.
    IF AVAIL ef THEN ASSIGN est-prep.eqty = ef.eqty.
    DISP est-prep.est-no FORM "x(8)".  
    PAUSE 0. 
END.
DISPLAY "Updating est-inst ... " STRING(TIME,"hh:mm:ss:") SKIP.
PAUSE 0.
FOR EACH est-inst:
    FIND FIRST ef WHERE ef.company = est-inst.company 
                    AND ef.est-no = est-inst.est-no NO-LOCK NO-ERROR.
    IF AVAIL ef THEN ASSIGN est-inst.eqty = ef.eqty.
END.
DISPLAY "Updating est-flm ... " STRING(TIME,"hh:mm:ss:") SKIP.
PAUSE 0.
FOR EACH est-flm  :
    FIND FIRST ef WHERE ef.company = est-flm.company 
                    AND ef.est-no = est-flm.est-no NO-LOCK NO-ERROR.
    IF AVAIL ef THEN ASSIGN est-flm.eqty = ef.eqty.
       
END.

SESSION:SET-WAIT-STATE("").
MESSAGE "Completed" VIEW-AS ALERT-BOX.


/* not using now 
FOR EACH est-qty NO-LOCK:
    FOR EACH eb WHERE eb.company = est-qty.company AND
                      eb.est-no = est-qty.est-no :
        eb.eqty = est-qty.e-qty.
    END.
    FOR EACH ef WHERE ef.company = est-qty.company AND
                  ef.est-no = est-qty.est-no :
        ef.eqty = est-qty.e-qty.
    END.
    
    FOR EACH est-op WHERE est-op.company = est-qty.company AND
                          est-op.est-no = est-qty.est-no :
        est-op.eqty = est-qty.e-qty.
    END.
    FOR EACH est-inst WHERE est-inst.company = est-qty.company AND
                            est-inst.est-no = est-qty.est-no :
        est-inst.eqty = est-qty.e-qty.
    END.
    FOR EACH est-prep WHERE est-prep.company = est-qty.company AND
                            est-prep.est-no = est-qty.est-no :
        est-prep.eqty = est-qty.e-qty.
    END.
    FOR EACH est-flm WHERE est-op.company = est-qty.company AND
                          eest-op.est-no = est-qty.est-no :
        est-op.eqty = est-qty.e-qty.
    END.
END.
*/
