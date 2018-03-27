/* ------------------------------------------------- ar/rep/custinvoice.i */
/* -----------------------------------------------------------------------*/
   
   IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(fi_file).

      IF tb_contact-sort EQ NO THEN
      DO:
         excelheader = "Cust #,Name,First Inv. Date".
         PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
      END.
   END.
   
 for EACH cust
       WHERE cust.company       EQ cocode
         AND cust.cust-no GE fcust
         AND cust.cust-no LE tcust
         AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
         AND ttCustList.log-fld no-lock) else true)
         AND cust.type          GE begin_cust-type
         AND cust.type          LE end_cust-type
         AND cust.sman          GE begin_slsmn
         AND cust.sman          LE end_slsmn
         AND (cust.cust-level   EQ price-level OR price-level EQ 99)
         AND (cust.disc         NE 0 OR NOT tb_disc-only)
         AND (cust.active       NE "I" AND tb_active OR
              cust.active       EQ "I" AND tb_inactive)
       NO-LOCK
       {2}
       BREAK BY cust.{1} BY cust.cust-no:
       
       {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

       FOR EACH ar-inv FIELDS(inv-date) WHERE
           ar-inv.company EQ cust.company AND
           ar-inv.cust-no EQ cust.cust-no AND
           ar-inv.inv-date NE ?
           NO-LOCK
           USE-INDEX inv-date:
       
          

           IF ar-inv.inv-date GE begin_date AND
              ar-inv.inv-date LE END_date THEN
           DO:
           
              DISPLAY cust.cust-no LABEL "Cust #"
                      cust.name LABEL "Name"
                      ar-inv.inv-date FORMAT "99/99/9999" LABEL "First Inv. Date"
                      SKIP.
              DOWN.
             
              IF tb_excel THEN
                 PUT STREAM excel UNFORMATTED
                     '"' cust.cust-no '",'
                     '"' cust.name  '",'
                     '"' STRING(ar-inv.inv-date,"99/99/9999") '",'
                     SKIP.
           END.

           LEAVE.
       END.
   end.
