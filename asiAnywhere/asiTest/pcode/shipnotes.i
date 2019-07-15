

DEFINE TEMP-TABLE ttshipnotes NO-UNDO
   BEFORE-TABLE beforeshipnotes
    FIELD i-no      AS CHAR
    FIELD i-name    AS CHAR
    FIELD po-no     AS CHAR
    FIELD rel-date  AS DATE
    FIELD SNote1    AS CHAR
    FIELD SNote2    AS CHAR
    FIELD SNote3    AS CHAR
    FIELD SNote4    AS CHAR
    . 
DEFINE DATASET dsshipnotes FOR ttshipnotes .
DEFINE QUERY q-shipnotesQuery FOR ttshipnotes.
DEFINE DATA-SOURCE src-shipnotes  FOR QUERY q-shipnotesQuery.
BUFFER ttshipnotes :ATTACH-DATA-SOURCE(DATA-SOURCE src-shipnotes  :HANDLE).






