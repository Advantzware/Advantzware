/* ttbldir.i */

DEFINE TEMP-TABLE ttbldir NO-UNDO
  FIELD dirname AS CHARACTER FORMAT "x(20)" LABEL "Directory"
  FIELD rec_key AS CHARACTER
    INDEX pi-ttbldir IS PRIMARY UNIQUE
          dirname.

RUN Build-Directory-Table.
