/* ------------------------------------------------- ar/rep/custcontact.i */
/* -----------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tt-cust.
   EMPTY TEMP-TABLE tt-contact.
   
   ASSIGN
      v-seq = 0
      v-page-break = NO.

   IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(fi_file).

      IF tb_contact-sort EQ NO THEN
      DO:
         excelheader = "Cust #,Name,Contact".
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
         AND cust.date-field[1] GE begin_date
         AND cust.date-field[1] LE end_date
         AND (cust.cust-level   EQ price-level OR price-level EQ 99)
         AND (cust.disc         NE 0 OR NOT tb_disc-only)
         AND (cust.active       NE "I" AND tb_active OR
              cust.active       EQ "I" AND tb_inactive)
       NO-LOCK
       {2},
       EACH phone FIELDS(attention) WHERE 
            phone.table_rec_key = cust.rec_key AND
            phone.titlcode = scr-title
           NO-LOCK
       BREAK BY cust.{1} BY cust.cust-no:

       {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

       IF NOT CAN-FIND(FIRST tt-cust WHERE
          tt-cust.cust-no EQ cust.cust-no) THEN
          DO:
             CREATE tt-cust.
             ASSIGN
                tt-cust.cust-no = cust.cust-no
                tt-cust.cust-name = cust.NAME
                tt-cust.seq = v-seq
                v-seq = v-seq + 1.
             RELEASE tt-cust.
          END.

       CREATE tt-contact.
       ASSIGN
          tt-contact.cust-no = cust.cust-no
          tt-contact.attention = phone.attention.
       RELEASE tt-contact.
   end.

   IF tb_contact-sort EQ NO THEN
      FOR EACH tt-cust,
          EACH tt-contact WHERE
               tt-contact.cust-no EQ tt-cust.cust-no
          BREAK BY tt-cust.seq:
          
       {custom/statusMsg.i " 'Processing Customer#  '  + string(tt-cust.cust-no) "}

          DISPLAY tt-cust.cust-no WHEN FIRST-OF(tt-cust.seq) LABEL "Cust #"
                  tt-cust.cust-name WHEN FIRST-OF(tt-cust.seq) LABEL "Name"
                  tt-contact.attention LABEL "Contact"
                  SKIP.
          DOWN.
     
          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
               '"' (IF FIRST-OF(tt-cust.seq) THEN tt-cust.cust-no ELSE "") '",'
               '"' (IF first-of(tt-cust.seq) THEN tt-cust.cust-name ELSE "") '",'
               '"' tt-contact.attention '",'
               SKIP.
      END.
   ELSE
      FOR EACH tt-contact,
          EACH tt-cust WHERE
               tt-cust.cust-no EQ tt-contact.cust-no
          BREAK BY tt-contact.attention
                BY tt-cust.seq:
          
        {custom/statusMsg.i " 'Processing Customer#  '  + string(tt-cust.cust-no) "}

          IF FIRST-OF(tt-contact.attention) THEN
          DO:
             IF v-page-break THEN
                PAGE.
             
             DISPLAY tt-contact.attention LABEL "Contact" WITH FRAME contact-top.

             IF tb_excel THEN
             DO:
                IF v-page-break THEN
                   PUT STREAM excel UNFORMATTED SKIP(1).

                PUT STREAM excel UNFORMATTED
                    '"' REPLACE("Contact",',','","') '"' SKIP
                    '"' tt-contact.attention '",' SKIP(1)
                    '"' REPLACE("Cust #,Name",',','","') '"' SKIP.
             END.

             v-page-break = YES.
          END.

          DISPLAY tt-cust.cust-no LABEL "Cust #"
                  tt-cust.cust-name LABEL "Name"
                  SKIP.
          DOWN.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' tt-cust.cust-no '",' 
                 '"' tt-cust.cust-name '",' SKIP.
      END.
