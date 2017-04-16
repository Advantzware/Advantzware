DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-i-no AS CHAR NO-UNDO.

FOR EACH item-comm WHERE
    item-comm.company EQ ip-company AND
    item-comm.i-no EQ ip-i-no:

    DELETE item-comm.
END.
