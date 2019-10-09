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
        WHEN "oe-rel" THEN ASSIGN 
            opNextRelNo = NEXT-VALUE(oerel_rno_seq).
        WHEN "release#" THEN DO:
            RUN sys/ref/asiseq.p (cocode,
                                  "oerel_release_seq",
                                  OUTPUT opNextRelNo).
        END.
        WHEN "oe-relh" THEN ASSIGN
            opNextRelNo = NEXT-VALUE(oerel_release_seq).
        OTHERWISE DO:
            MESSAGE
                "Entered mode (" + ipMode + ") not supported" SKIP
                "in program oe/getNextRelNo.p. Please" SKIP
                "contact your System Administrator."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END CASE.

