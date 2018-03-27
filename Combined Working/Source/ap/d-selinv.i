
DEF {1} SHARED TEMP-TABLE tt-inv FIELD selekt AS LOG LABEL "Selected"
                                 FIELD rec-id AS RECID 
                                 FIELD tt-key AS CHAR
                                 INDEX tt-key tt-key.
