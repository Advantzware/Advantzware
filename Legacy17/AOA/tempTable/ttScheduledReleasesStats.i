/* ttScheduledReleasesStats.i */

/* Scheduled Releases.rpa */
DEFINE TEMP-TABLE ttScheduledReleasesStats NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD statsLine      AS CHARACTER LABEL "Stats"           FORMAT "x(200)"
    FIELD xxItemFGRecKey AS CHARACTER LABEL "Item FG Rec Key" FORMAT "x(20)" INITIAL "Z"
        INDEX itemRecKey IS PRIMARY xxItemFGRecKey
        .
