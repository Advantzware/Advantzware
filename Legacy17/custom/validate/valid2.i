
{{&prior}}

{&avail} = no.

find first {&file1} WHERE {&file1}.{&FIELD} = {&val-field}:SCREEN-VALUE IN FRAME {&FRAME-NAME} and {&file1}.{&FIELD} <> ""
    {{&where}}
    no-lock no-error.

IF NOT AVAIL {&file1}                                           AND
   ( ({&val-field}:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" and "{&val-field}" BEGINS "ar-ctrl." ) OR
    "{&val-field}" BEGINS "ap-ctrl." OR
    "{&val-field}" BEGINS "ar-ctrl.")                           THEN DO:
  MESSAGE {&messg} VIEW-AS ALERT-BOX ERROR.
  APPLY "entry" to {&val-field} IN FRAME {&FRAME-NAME}.
  {{&error}}
  RETURN NO-APPLY.
end.

{{&after}}
