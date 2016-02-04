/*addon/bol/find-ssrelbol.p*/

DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-release# AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER op-log AS LOG NO-UNDO.

op-log = CAN-FIND (FIRST ssrelbol WHERE
         ssrelbol.company EQ ip-company AND
         ssrelbol.release# = ip-release#).
