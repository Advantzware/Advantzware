/* ttScheduledReleasesNotes.i */

/* Scheduled Releases.rpa */
DEFINE TEMP-TABLE ttScheduledReleasesNotes NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD noteCode       AS CHARACTER LABEL "Spec Code"       FORMAT "x(2)"
    FIELD noteText       AS CHARACTER LABEL "Note"            FORMAT "x(1000)"
    FIELD xxItemFGRecKey AS CHARACTER LABEL "Item FG Rec Key" FORMAT "x(20)" INITIAL "Z"
        INDEX itemRecKey IS PRIMARY xxItemFGRecKey noteCode
        .
