
DEF {1} SHARED TEMP-TABLE tt-formtext
                          FIELD tt-line-no AS INT
                          FIELD tt-length  AS INT
                          FIELD tt-text    AS CHAR
                          INDEX tt-form-text tt-line-no.
                          
