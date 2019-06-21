
DEFINE TEMP-TABLE ttbolInvPrint NO-UNDO
FIELD aFile AS CHAR.
DEFINE DATASET dsbolInvPrint FOR ttbolInvPrint .
DEFINE QUERY q-bolInvPrintQuery FOR ttbolInvPrint.
DEFINE DATA-SOURCE src-bolInvPrint  FOR QUERY q-bolInvPrintQuery.
BUFFER ttbolInvPrint :ATTACH-DATA-SOURCE(DATA-SOURCE src-bolInvPrint  :HANDLE).






