/* cc-code.i */

DEFINE TEMP-TABLE ttblcc-code NO-UNDO
  FIELD cc-code LIKE {1}.cc-code COLUMN-LABEL "Cycle Count Code"
  FIELD rec_key AS CHARACTER
    INDEX ttblcc-code IS PRIMARY UNIQUE
          cc-code.

RUN cc-codeBuild.
