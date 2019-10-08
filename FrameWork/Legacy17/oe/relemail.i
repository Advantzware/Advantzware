/* oe/relemail.i*/

DEF {1} SHARED TEMP-TABLE tt-email
    FIELD ord-no AS INT
    FIELD i-no AS CHAR
    FIELD rel-qty AS DEC
    FIELD rel-date AS DATE
    FIELD po-no AS CHAR
    FIELD cust-no AS cha
    INDEX tt-cust IS PRIMARY ord-no.
