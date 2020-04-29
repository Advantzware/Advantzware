/* itemUOM.i */

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemNo  AS CHARACTER NO-UNDO.

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT cCompany, OUTPUT cItemNo)"}
ASSIGN
    {&FIRST-EXTERNAL-TABLE}.company     = cCompany
    {&FIRST-EXTERNAL-TABLE}.itemType    = "FG"
    {&FIRST-EXTERNAL-TABLE}.itemID      = cItemNo
    .
FIND FIRST itemfg NO-LOCK
     WHERE itemfg.company EQ cCompany
       AND itemfg.i-no    EQ cItemNo
     NO-ERROR.
IF AVAILABLE itemfg THEN
{&FIRST-EXTERNAL-TABLE}.procat = itemfg.procat.
