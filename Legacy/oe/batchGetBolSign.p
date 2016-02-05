DEF VAR cDefaultCompany AS CHAR INIT '001' NO-UNDO.
DEF VAR cFile AS CHAR NO-UNDO.
    RUN oe/GetBOLSign.p(INPUT cDefaultCompany,
                        INPUT ?,
                        INPUT YES,
                        INPUT YES,
                        OUTPUT cFile).
