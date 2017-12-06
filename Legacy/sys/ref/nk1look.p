/* sys/ref/nk1look.p */
DEF INPUT PARAMETER ip-cocode AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-nk1-name AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-return-type AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-use-shipto AS LOG NO-UNDO.
DEF INPUT PARAMETER ip-shipto-vendor AS LOG NO-UNDO.
DEF INPUT PARAMETER ip-shipto-vendor-value AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-shipid-value AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-char-value AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-rec-found AS LOG NO-UNDO.

ASSIGN 
    op-rec-found = TRUE.
FIND FIRST sys-ctrl NO-LOCK WHERE 
    sys-ctrl.company EQ ip-cocode AND 
    sys-ctrl.name    EQ ip-nk1-name
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:    
    RUN sys/inc/create_nk1.p (INPUT ip-cocode, 
                              INPUT ip-nk1-name).
    FIND FIRST sys-ctrl NO-LOCK WHERE 
        sys-ctrl.company EQ ip-cocode AND 
        sys-ctrl.name    EQ ip-nk1-name
        NO-ERROR.
    IF NOT AVAIL(sys-ctrl) THEN DO:
        ASSIGN 
            op-rec-found  = NO
            op-char-value = "".
        RETURN.
    END.
END.

CASE ip-return-type:
    WHEN "I" THEN op-char-value = STRING(sys-ctrl.int-fld).
    WHEN "D" THEN op-char-value = STRING(sys-ctrl.dec-fld).
    WHEN "C" THEN op-char-value = sys-ctrl.char-fld.
    WHEN "DT" THEN op-char-value = string(sys-ctrl.date-fld).
    WHEN "DS" THEN op-char-value = sys-ctrl.descrip.
    WHEN "L" THEN op-char-value = IF sys-ctrl.log-fld THEN "YES" ELSE "NO".
    OTHERWISE op-char-value = "".
END CASE.

IF ip-use-shipto THEN DO:
    FIND FIRST sys-ctrl-shipto NO-LOCK WHERE
        sys-ctrl-shipto.company = ip-cocode AND
        sys-ctrl-shipto.NAME = ip-nk1-name AND
        sys-ctrl-shipto.cust-vend = ip-shipto-vendor AND
        sys-ctrl-shipto.cust-vend-no = ip-shipto-vendor-value AND
        sys-ctrl-shipto.ship-id = ip-shipid-value AND
        (IF ip-return-type = "C" THEN sys-ctrl-shipto.char-fld GT '' ELSE TRUE) AND
        (IF ip-return-type = "D" THEN sys-ctrl-shipto.dec-fld GT 0 ELSE TRUE)  AND
        (IF ip-return-type = "DT" THEN sys-ctrl-shipto.date-fld NE ? ELSE TRUE) AND
        (IF ip-return-type = "I" THEN sys-ctrl-shipto.int-fld GT 0 ELSE TRUE)  
        NO-ERROR.
    IF NOT AVAIL sys-ctrl-shipto THEN FIND FIRST sys-ctrl-shipto NO-LOCK WHERE
        sys-ctrl-shipto.company = ip-cocode AND
        sys-ctrl-shipto.NAME = ip-nk1-name AND
        sys-ctrl-shipto.cust-vend = ip-shipto-vendor AND
        sys-ctrl-shipto.cust-vend-no = ip-shipto-vendor-value AND          
        (IF ip-return-type = "C" THEN sys-ctrl-shipto.char-fld GT '' ELSE TRUE) AND
        (IF ip-return-type = "D" THEN sys-ctrl-shipto.dec-fld GT 0 ELSE TRUE)  AND
        (IF ip-return-type = "DT" THEN sys-ctrl-shipto.date-fld NE ? ELSE TRUE) AND
        (IF ip-return-type = "I" THEN sys-ctrl-shipto.int-fld GT 0 ELSE TRUE)  
        NO-ERROR.

    IF AVAIL sys-ctrl-shipto THEN DO:
        CASE ip-return-type:
            WHEN "I" THEN op-char-value = STRING(sys-ctrl-shipto.int-fld).
            WHEN "D" THEN op-char-value = STRING(sys-ctrl-shipto.dec-fld).
            WHEN "C" THEN op-char-value = sys-ctrl-shipto.char-fld.
            WHEN "DT" THEN op-char-value = string(sys-ctrl-shipto.date-fld).
            WHEN "L" THEN op-char-value = IF sys-ctrl-shipto.log-fld THEN "YES" ELSE "NO".
            OTHERWISE op-char-value = "".
        END CASE.
    END.
END.
