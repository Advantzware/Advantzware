/* sys/ref/nk1look.p */

DEFINE INPUT PARAMETER ip-cocode              AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-nk1-name            AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-return-type         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-use-shipto          AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER ip-shipto-vendor       AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER ip-shipto-vendor-value AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-shipid-value        AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER op-char-value AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-rec-found  AS LOGICAL   NO-UNDO.

op-rec-found = TRUE.
FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ ip-cocode
       AND sys-ctrl.name    EQ ip-nk1-name
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:    
    RUN sys/inc/create_nk1.p (INPUT ip-cocode, 
        INPUT ip-nk1-name).
    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ ip-cocode
           AND sys-ctrl.name    EQ ip-nk1-name
         NO-ERROR.
    IF NOT AVAIL(sys-ctrl) THEN DO:
        ASSIGN 
            op-rec-found  = NO
            op-char-value = "".
        RETURN.
    END.
END.

CASE ip-return-type:
    WHEN "I" THEN 
        op-char-value = STRING(sys-ctrl.int-fld).
    WHEN "D" THEN 
        op-char-value = STRING(sys-ctrl.dec-fld).
    WHEN "C" THEN 
        op-char-value = sys-ctrl.char-fld.
    WHEN "DT" THEN 
        op-char-value = STRING(sys-ctrl.date-fld).
    WHEN "DS" THEN 
        op-char-value = sys-ctrl.descrip.
    WHEN "L" THEN 
        op-char-value = CAPS(STRING(sys-ctrl.log-fld)).
    OTHERWISE 
    op-char-value = "".
END CASE.

IF ip-use-shipto THEN DO:
    FIND FIRST sys-ctrl-shipto NO-LOCK
         WHERE sys-ctrl-shipto.company      EQ ip-cocode
           AND sys-ctrl-shipto.NAME         EQ ip-nk1-name
           AND sys-ctrl-shipto.cust-vend    EQ ip-shipto-vendor
           AND sys-ctrl-shipto.cust-vend-no EQ ip-shipto-vendor-value
           AND sys-ctrl-shipto.ship-id      EQ ip-shipid-value 
         NO-ERROR.
    IF NOT AVAILABLE sys-ctrl-shipto THEN
    FIND FIRST sys-ctrl-shipto NO-LOCK
         WHERE sys-ctrl-shipto.company      EQ ip-cocode
           AND sys-ctrl-shipto.NAME         EQ ip-nk1-name
           AND sys-ctrl-shipto.cust-vend    EQ ip-shipto-vendor
           AND sys-ctrl-shipto.cust-vend-no EQ ip-shipto-vendor-value
           AND sys-ctrl-shipto.ship-id      EQ ""      
         NO-ERROR.
    IF AVAILABLE sys-ctrl-shipto THEN DO:
        CASE ip-return-type:
            WHEN "I" THEN 
                op-char-value = STRING(sys-ctrl-shipto.int-fld).
            WHEN "D" THEN 
                op-char-value = STRING(sys-ctrl-shipto.dec-fld).
            WHEN "C" THEN 
                op-char-value = sys-ctrl-shipto.char-fld.
            WHEN "DT" THEN 
                op-char-value = STRING(sys-ctrl-shipto.date-fld).
            WHEN "L" THEN 
                op-char-value = IF sys-ctrl-shipto.log-fld THEN "YES" ELSE "NO".
            OTHERWISE 
            op-char-value = "".
        END CASE.
    END.
END.
