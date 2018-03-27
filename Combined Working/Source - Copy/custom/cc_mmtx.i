/* cc_mmtx.i - used in custom/companyCopy.i */

    BUFFER-COPY mmtx EXCEPT company mmtx-no rec_key TO bmmtx
      ASSIGN lvMMtxNo = getNextMMtxNo()
             bmmtx.mmtx-no = lvMMtxNo
             bmmtx.company = ipCompanyTo.

    {custom\rec_key.i bmmtx}

    IF CAN-FIND(FIRST mmtx2 OF mmtx) THEN
    DO:
      FIND FIRST mmtx2 OF mmtx NO-LOCK.
      CREATE bmmtx2.
      BUFFER-COPY mmtx2 EXCEPT company mmtx-no rec_key TO bmmtx2
        ASSIGN bmmtx2.mmtx-no = lvMMtxNo
               bmmtx2.company = ipCompanyTo.

      {custom\rec_key.i bmmtx2}
    END.


