DEF VAR cDefaultCompany AS CHAR INIT '001' NO-UNDO.
DEF VAR cFile AS CHAR NO-UNDO.

    RUN oe/BuildBOLSignData.p(INPUT cDefaultCompany,
                              INPUT "XML",
                              INPUT ?,
                              INPUT YES,
                              INPUT YES).
