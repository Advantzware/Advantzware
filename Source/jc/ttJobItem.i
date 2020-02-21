
DEF {1} {2} TEMP-TABLE tt-job-item 
    FIELD tt-rowid    AS ROWID
    FIELD frm         LIKE job-mat.frm
    FIELD blank-no    LIKE job-mat.blank-no
    FIELD rm-i-no     AS CHARACTER COLUMN-LABEL "Item ID" 
    FIELD mach-id     AS CHARACTER COLUMN-LABEL "Machine ID" 
    FIELD IS-SELECTED AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX
    FIELD mat-alloc   AS LOG       COLUMN-LABEL "Committed"
    .
