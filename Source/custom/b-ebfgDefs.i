/* b-ebfgDefs.i - used in oe/b-ebfg.w and fg/b-ebfg.w */

DEFINE VARIABLE browseTitle AS CHARACTER NO-UNDO INIT 'Estimate % Inks and Coatings'.

DEFINE TEMP-TABLE ebfg NO-UNDO
  FIELD i-row AS INTEGER
  FIELD unit# AS DECIMAL FORMAT '>>>' LABEL 'Unit'
  FIELD i-ps2 AS INTEGER FORMAT '>9' LABEL 'PS'
  FIELD i-code2 AS CHARACTER FORMAT 'X(10)' LABEL 'Code'
  FIELD i-dscr2 AS CHARACTER FORMAT 'X(19)' LABEL 'Description'
  FIELD i-%2 AS INTEGER FORMAT '>>9' LABEL '%'
    INDEX ebfg IS UNIQUE PRIMARY i-row.
