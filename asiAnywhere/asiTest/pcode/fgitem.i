
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttfg NO-UNDO LIKE itemfg.

DEFINE DATASET dsfg FOR ttfg.

DEFINE QUERY q-fgQuery FOR itemfg.

DEFINE DATA-SOURCE src-fg FOR QUERY q-fgQuery.

BUFFER ttfg:ATTACH-DATA-SOURCE(DATA-SOURCE src-fg:HANDLE).

