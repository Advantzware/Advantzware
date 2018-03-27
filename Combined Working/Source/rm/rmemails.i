/****************************************************************************************************
      Program Name : rm/rmemails.i  
      Author       : Amit Jadon  
      Date         : 06/19/2008 
      Description  : rmemails = "NONE,Overrun,Receipts"
                     None: don't email 
                     Overrun:  email only qty is overrun/underrun
                     receipts: email all the time  
      Modify       :                                                                   
****************************************************************************************************/
IF rmemails <> "NONE" THEN DO:
   
   FIND FIRST po-ord WHERE
        po-ord.company = rm-rctd.company AND
        po-ord.po-no = int(rm-rctd.po-no)
        NO-LOCK NO-ERROR.

   FIND FIRST po-ordl WHERE
        po-ordl.company = rm-rctd.company AND
        po-ordl.po-no = int(rm-rctd.po-no) AND
        po-ordl.i-no = rm-rctd.i-no
        NO-LOCK NO-ERROR.
   
   IF AVAIL po-ord THEN
   DO:
      FIND FIRST tt-email WHERE
           tt-email.po-no = po-ord.po-no AND
           tt-email.item-no = ITEM.i-no
           NO-ERROR.

      IF NOT AVAIL tt-email THEN
      DO:
         CREATE tt-email.
         ASSIGN tt-email.vend-no    =  IF AVAIL po-ord THEN po-ord.vend-no
                                       ELSE ""
                tt-email.po-no      =  po-ord.po-no
                tt-email.item-no    =  ITEM.i-no
                tt-email.item-name  =  item.i-name
                tt-email.po-qty     =  IF AVAIL po-ordl THEN po-ordl.ord-qty
                                       ELSE 0
                tt-email.cons-uom   =  ITEM.cons-uom.
            
         IF AVAIL po-ordl AND
            ITEM.cons-uom NE po-ordl.pr-qty-uom THEN
            RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                   ITEM.cons-uom, ITEM.basis-w,
                                   po-ordl.s-len,  po-ordl.s-wid, item.s-dep,
                                   tt-email.po-qty, OUTPUT tt-email.po-qty).

         FIND FIRST vend WHERE
              vend.company = rm-rctd.company AND
              vend.vend-no = item.vend-no
              NO-LOCK NO-ERROR.

         ASSIGN 
            tt-email.overrun-pct = IF AVAIL po-ordl THEN po-ordl.over-pct ELSE
                                    IF AVAIL po-ord  THEN po-ord.over-pct  ELSE
                                    IF AVAIL vend    THEN vend.over-pct    ELSE 0
      
            tt-email.underrun-pct = IF AVAIL po-ordl THEN po-ordl.under-pct ELSE
                                     IF AVAIL po-ord  THEN po-ord.under-pct  ELSE
                                     IF AVAIL vend    THEN vend.under-pct    ELSE 0
            tt-email.allow-qty = tt-email.po-qty + (tt-email.po-qty * tt-email.overrun-pct / 100)
            tt-email.under-qty = tt-email.po-qty - (tt-email.po-qty * tt-email.underrun-pct / 100).
      END. /*NOT AVAIL tt-email*/

      ASSIGN
         tt-email.total-recvd-qty = po-ordl.t-rec-qty
         tt-email.recvd-qty = tt-email.recvd-qty + rm-rctd.qty.

      IF AVAIL po-ordl THEN
      DO:
         IF ITEM.cons-uom NE rm-rctd.pur-uom THEN
            RUN sys/ref/convquom.p(rm-rctd.pur-uom,
                                   ITEM.cons-uom, ITEM.basis-w,
                                   po-ordl.s-len,  po-ordl.s-wid, item.s-dep,
                                   tt-email.recvd-qty, OUTPUT tt-email.recvd-qty).

         IF po-ordl.cons-uom NE ITEM.cons-uom THEN
            RUN sys/ref/convquom.p(po-ordl.cons-uom,
                                   ITEM.cons-uom, ITEM.basis-w,
                                   po-ordl.s-len,  po-ordl.s-wid, item.s-dep,
                                   tt-email.total-recvd-qty, OUTPUT tt-email.total-recvd-qty).
      END.
   END.
END.

