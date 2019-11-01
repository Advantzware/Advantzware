/* Obtain next available r-no for release creation */
  
    DEF INPUT PARAMETER ipMode AS CHAR NO-UNDO. /* oe-rel or oe-relh */
    DEF OUTPUT PARAMETER opNextRelNo AS INT NO-UNDO.

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
            /* This SHOULD take care of situations where an out-of-sequence r-no has been added, */
            /* such that the sequence generator is updated to the latest (correct) r-no.         */
            /* Since this is not a consistent error, there is some sequence of events/processes  */
            /* that create this condition, but all reported cases have been resolved within 1-2  */
            /* sequence numbers                                                                  */
            DO WHILE CAN-FIND(FIRST oe-rel WHERE
                oe-rel.company EQ cocode AND
                oe-rel.r-no EQ opNextRelNo):
                ASSIGN 
                    opNextRelNo = NEXT-VALUE(oerel_rno_seq).
            END.
        END.
        WHEN "release#" THEN DO:
            /* The caller program here (oe/release#.p) contains additional logic, so don't want to change what works */
            RUN sys/ref/asiseq.p (cocode,
                                  "oerel_release_seq",
                                  OUTPUT opNextRelNo).
        END.
        WHEN "oe-relh" THEN DO:
            ASSIGN
                opNextRelNo = NEXT-VALUE(oerel_release_seq).
            /* This SHOULD take care of situations where an out-of-sequence r-no has been added, */
            /* such that the sequence generator is updated to the latest (correct) r-no.         */
            /* Since this is not a consistent error, there is some sequence of events/processes  */
            /* that create this condition, but all reported cases have been resolved within 1-2  */
            /* sequence numbers                                                                  */
            DO WHILE CAN-FIND(FIRST oe-relh WHERE
                oe-relh.company EQ cocode AND
                oe-relh.r-no EQ opNextRelNo):
                ASSIGN 
                    opNextRelNo = NEXT-VALUE(oerel_release_seq).
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

