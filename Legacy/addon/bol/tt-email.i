DEFINE {1} SHARED TEMP-TABLE tt-email NO-UNDO
     FIELD release# AS INT FORMAT "->,>>>,>>9"
     FIELD ord-no AS INT FORMAT ">>>>>9"
     FIELD i-no AS CHAR FORMAT "X(15)"
     FIELD part-no AS CHAR FORMAT "X(15)"
     FIELD i-name AS CHAR FORMAT "X(30)"
     FIELD done-what AS cha
     FIELD ord-no2 AS INT FORMAT ">>>>>9"
     FIELD job-no AS CHAR FORMAT "X(6)"
     FIELD job-no2 AS INT FORMAT ">9"
     FIELD qty-rel LIKE oe-rell.qty
     FIELD qty-scan LIKE oe-rell.qty
     .
  
