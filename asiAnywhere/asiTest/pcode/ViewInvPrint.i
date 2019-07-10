


DEFINE TEMP-TABLE ttViewInvPrint NO-UNDO
        FIELD vInvFile        AS CHAR  .
DEFINE DATASET dsViewInvPrint FOR ttViewInvPrint .
DEFINE QUERY q-ViewInvPrintQuery FOR ttViewInvPrint.
DEFINE DATA-SOURCE src-ViewInvPrint  FOR QUERY q-ViewInvPrintQuery.
BUFFER ttViewInvPrint :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewInvPrint  :HANDLE).

