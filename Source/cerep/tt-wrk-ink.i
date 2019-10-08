 DEF {1} TEMP-TABLE wrk-ink
   FIELD rm-i-no     LIKE job-mat.rm-i-no
   FIELD i-name      LIKE ITEM.i-name
   FIELD i-code      AS CHAR FORMAT "x(10)"
   FIELD form-no     LIKE eb.form-no
   FIELD blank-no    LIKE eb.blank-no
   FIELD i-dscr      AS CHAR FORMAT "x(20)"
   FIELD i-qty       AS DEC FORMAT ">,>>9.9< " COLUMN-LABEL "LBS"
   FIELD i-pass      AS DEC
   FIELD qty-uom     LIKE job-mat.qty-uom
   FIELD old-qty     LIKE job-mat.qty
   FIELD cnt         AS INT
   FIELD num-sheets  AS DEC
   FIELD yield       AS DEC.
