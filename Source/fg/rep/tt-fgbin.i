
DEF {1} {2} VAR age-days AS INT EXTENT 4 NO-UNDO.

DEF {1} {2} TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin 
                                            FIELD first-date AS DATE
                                            FIELD aged-qty   AS DEC EXTENT 6.
