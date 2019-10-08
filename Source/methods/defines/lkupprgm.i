/* lkupprgm.i */

DEFINE TEMP-TABLE lkupprgm NO-UNDO
  FIELD lkupprgm AS CHARACTER FORMAT "x(20)" LABEL "Lookup Prgm"
  FIELD rec_key AS CHARACTER
    INDEX pi-lkupprgm IS PRIMARY UNIQUE
          lkupprgm.

RUN Build-LkupPrgm-Table.
