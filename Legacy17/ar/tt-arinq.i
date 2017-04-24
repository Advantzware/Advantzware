DEF TEMP-TABLE tt-arinq NO-UNDO
  FIELD ref-num AS CHAR FORM "x(15)" LABEL "Ck/Cr/Dr/Po#"
  FIELD inv-no LIKE ar-inv.inv-no LABEL "Invoice#"
  FIELD tr-date AS DATE FORM "99/99/9999" LABEL "Date"
  FIELD tr-dscr LIKE gltrans.tr-dscr LABEL "Description"
  FIELD tr-damt LIKE gltrans.tr-amt LABEL "Debits"
  FIELD tr-camt LIKE gltrans.tr-amt LABEL "Credits"
  FIELD ageapp AS CHAR FORM "x(5)" LABEL "Age App"
  FIELD tr-from AS CHAR LABEL "Inquiry From"
  FIELD balance AS dec FORM "->>>,>>>,>>9.99" LABEL "Balance"
  FIELD applied AS LOG
  FIELD seq AS INT
  FIELD printed LIKE ar-inv.printed
  FIELD posted LIKE ar-inv.posted
  INDEX seq seq
  INDEX applied IS PRIMARY applied seq
  INDEX ref-num ref-num seq
  INDEX inv-no inv-no seq
  INDEX tr-date tr-date seq
  INDEX tr-dscr tr-dscr seq
  INDEX tr-damt tr-damt seq
  INDEX tr-camt tr-camt seq
  INDEX ageapp ageapp seq
  INDEX tr-from tr-from seq
  INDEX balance balance seq.
