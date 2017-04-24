

DEF BUFFER b-mach FOR mach.

RELEASE mach.

IF tb_slsmn OR tb_comm THEN
for each xest
    where xest.company eq cocode
      and xest.est-no  ge fest
      and xest.est-no  le test
      AND CAN-FIND(FIRST xeb
                   where xeb.company eq xest.company
                     and xeb.est-no  eq xest.est-no
                     and xeb.cust-no ge fcus
                     and xeb.cust-no le tcus)
    NO-LOCK:

  v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Updating Estimate " + TRIM(xest.est-no).

  FOR each xeb
      where xeb.company eq xest.company
        and xeb.est-no  eq xest.est-no
        and xeb.cust-no ge fcus
        and xeb.cust-no le tcus
      USE-INDEX est-no,
      
      first xef
      WHERE xef.company EQ xeb.company
        AND xef.est-no  EQ xeb.est-no
        AND xef.eqty    EQ xeb.eqty
        AND xef.form-no EQ xeb.form-no
       no-lock,

      first cust
      where cust.company eq xeb.company
        and cust.cust-no eq xeb.cust-no
      no-lock:

    IF tb_slsmn OR tb_comm THEN DO:
      IF tb_slsmn THEN xeb.sman = "".

      RUN ce/markup.p (xeb.company, ROWID(xeb), OUTPUT ld).

      RUN sys/inc/getsmncm.p (xeb.cust-no,
                              INPUT-OUTPUT xeb.sman,
                              xeb.procat,
                              ld,
                              OUTPUT ld).

      IF tb_comm THEN xeb.comm = ld.
    END.
  END.
  

  IF tb_slsmn THEN
  FOR EACH quotehd WHERE quotehd.company EQ xest.company
                     AND quotehd.loc EQ xest.loc
                     AND quotehd.est-no EQ xest.est-no
                     AND quotehd.cust-no GE fcus
                     AND quotehd.cust-no LE tcus,
      first cust where cust.company eq quotehd.company
                   and cust.cust-no eq quotehd.cust-no NO-LOCK:
    IF quotehd.sman <> cust.sman THEN quotehd.sman = cust.sman.
  END.

  
end.  /* end est*/



  

IF tb_order THEN 
      FOR EACH oe-ord WHERE oe-ord.company EQ cocode
                     AND oe-ord.cust-no GE fcus
                     AND oe-ord.cust-no LE tcus
                     AND oe-ord.opened EQ YES,
      first cust where cust.company eq oe-ord.company
                   and cust.cust-no eq oe-ord.cust-no EXCLUSIVE-LOCK:
    
    v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Updating Order " + string(oe-ord.ord-no).

    FOR EACH oe-ordl WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no EQ oe-ord.ord-no
        AND oe-ordl.opened EQ YES 
        AND oe-ordl.stat NE "C" EXCLUSIVE-LOCK:
        IF oe-ordl.s-man[1] <> cust.sman  THEN oe-ordl.s-man[1] = cust.sman .
    END.

        IF oe-ord.sman[1] <> cust.sman  THEN do:
            ASSIGN oe-ord.sman[1] = cust.sman .
            FIND FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ cust.sman
                NO-LOCK NO-ERROR.
            IF AVAIL sman THEN
                oe-ord.sname[1] = sman.sNAME.
        END.
   END.


  IF tb_inv THEN 
      FOR EACH inv-head WHERE inv-head.company = cocode and 
      inv-head.multi-invoice = NO AND
      inv-head.cust-no GE fcus AND
      inv-head.cust-no LE tcus NO-LOCK,
      EACH inv-line OF inv-head ,
     first cust where cust.company eq cocode
                   and cust.cust-no eq inv-head.cust-no EXCLUSIVE-LOCK:
         v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Updating Invoice " + string(inv-head.inv-no).
      IF inv-line.sman[1] <> cust.sman  THEN do:
       inv-line.sman[1] = cust.sman .
        FIND FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ cust.sman
                NO-LOCK NO-ERROR.
            IF AVAIL sman THEN
                inv-line.sname[1] = sman.sNAME.
      END.
  END.
 

  IF tb_shipto THEN 
      FOR EACH shipto WHERE shipto.company = cocode and 
      shipto.cust-no GE fcus AND
      shipto.cust-no LE tcus EXCLUSIVE-LOCK,
     first cust where cust.company eq cocode
                   and cust.cust-no eq shipto.cust-no NO-LOCK:
         v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Updating Shipto " + string(shipto.ship-id).
      IF shipto.spare-char-1 <> cust.sman  THEN do:
       shipto.spare-char-1 = cust.sman .
      END.
  END.
