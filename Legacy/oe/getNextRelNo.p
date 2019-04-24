/* Obtain next available r-no for release creation */
  
    DEF INPUT PARAMETER ipMode AS CHAR NO-UNDO. /* oe-rel or oe-relh */
    DEF OUTPUT PARAMETER opNextRelNo AS INT NO-UNDO.
    DEF VAR iOpNext# AS INT NO-UNDO.
    DEF VAR cCompSuffix AS CHAR NO-UNDO.

    {custom/globdefs.i}
    {custom/gcompany.i}
    {sys/inc/var.i NEW SHARED}
    {sys/inc/varasgn.i}

    DEF VAR iNextRelNo AS INT NO-UNDO.
    DEF VAR iLastRelNo AS INT NO-UNDO.

    CASE ipMode:
        WHEN "oe-rel" THEN DO:
            ASSIGN 
                opNextRelNo = NEXT-VALUE(oerel_rno_seq).
            /* Test to ensure this is the highest r-no */
            FIND FIRST oe-rel NO-LOCK WHERE  
                oe-rel.r-no NE ? AND  
                oe-rel.r-no NE 0 
                USE-INDEX seq-no /* This is a DESC index */
                NO-ERROR.
            /* If it is used, rerun the process */
            IF AVAIL oe-rel THEN DO:
                ASSIGN 
                    CURRENT-VALUE(oerel_rno_seq) = oe-rel.r-no.
                RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT iopNext#).
                opNextRelNo = iopNext#.
            END.
        END.
        WHEN "oe-relh" THEN DO:
            ASSIGN
                opNextRelNo = NEXT-VALUE(oerel_release_seq).
            /* Test to ensure this is the highest r-no */
            FIND LAST oe-relh NO-LOCK WHERE  
                oe-relh.r-no NE ? AND  
                oe-relh.r-no NE 0 
                USE-INDEX r-no /* This is an ASC index */
                NO-ERROR.
            /* If it is used, rerun the process */
            IF AVAIL oe-relh THEN DO:
                ASSIGN 
                    CURRENT-VALUE(oerel_release_seq) = oe-relh.r-no.
                RUN oe/getNextRelNo.p (INPUT "oe-relh", OUTPUT iopNext#).
                opNextRelNo = iopNext#.
            END.
        END.
        WHEN "release#" THEN DO: /* Release # for oe-relh */
            FIND company NO-LOCK WHERE 
                company.company EQ coCode
                NO-ERROR.
            IF NOT AVAIL company THEN DO:
                MESSAGE 
                    "Company " + coCode + " not found in company table."
                    VIEW-AS ALERT-BOX ERROR.
                ASSIGN 
                    opNextRelNo = 0.
                RETURN.
            END.
            ELSE DO:
                ASSIGN 
                    cCompSuffix = company.spare-char-1.
                opNextRelNo = DYNAMIC-NEXT-VALUE("oerel_release_seq" + cCompSuffix, "ASI") NO-ERROR.
                /* Test to ensure this is the highest r-no */
                FIND LAST oe-relh NO-LOCK WHERE 
                    oe-relh.company EQ company.company AND  
                    oe-relh.r-no NE ? AND  
                    oe-relh.r-no NE 0 
                    USE-INDEX r-no /* This is an ASC index */
                    NO-ERROR.
                /* If it is used, rerun the process */
                IF AVAIL oe-relh THEN DO:
                    ASSIGN 
                        CURRENT-VALUE(oerel_release_seq) = oe-relh.r-no.
                    RUN oe/getNextRelNo.p (INPUT "release#", OUTPUT iopNext#).
                    opNextRelNo = iopNext#.
                END.
                    
            END.
        END.
        OTHERWISE DO:
            MESSAGE
                "Entered mode (" + ipMode + ") not supported" SKIP
                "in program oe/getNextRelNo.p. Please" SKIP
                "contact your System Administrator."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END CASE.

