/* listdefs.i */

&IF "{&PROGNAME}" = "audit_." &THEN
DEFINE STREAM s-audit.
&ELSEIF "{&PROGNAME}" = "track_." &THEN
DEFINE STREAM s-purge.

DEFINE TEMP-TABLE ttbl-detail NO-UNDO
  FIELD line-1 AS CHARACTER
  FIELD line-2 AS CHARACTER
    INDEX tmptbl-index IS PRIMARY
      line-1.
&ENDIF

{custom/listdefs.i}

FUNCTION CityState RETURNS CHARACTER (zip-code AS CHARACTER):
  FIND zipcode WHERE zipcode.zipcode = zip-code NO-LOCK NO-ERROR.
  IF AVAILABLE zipcode THEN
  RETURN zipcode.city + ", " + zipcode.state.
  ELSE
  RETURN "No City & State Available for Zipcode".
END FUNCTION.
