/* listobjs.i */

&IF "{&FIRST-EXTERNAL-TABLE}" NE "" &THEN
&Scoped-define DEFINETYPE Global
&IF "{&SCOPDEFS}" = "" &THEN
{methods/listobjs/scopdefs/{&FIRST-EXTERNAL-TABLE}.i}
&ELSE
{methods/listobjs/scopdefs/{&SCOPDEFS}.i}
&ENDIF
&ENDIF

{methods/high_low.i}

{methods/listobjs/settings.i BEGIN 1}
{methods/listobjs/settings.i BEGIN 2}
{methods/listobjs/settings.i BEGIN 3}
{methods/listobjs/settings.i BEGIN 4}
{methods/listobjs/settings.i BEGIN 5}
{methods/listobjs/settings.i END 1}
{methods/listobjs/settings.i END 2}
{methods/listobjs/settings.i END 3}
{methods/listobjs/settings.i END 4}
{methods/listobjs/settings.i END 5}

&Scoped-define BEGINVALUES ~
{&BEGINVALUE1} ~
{&BEGINVALUE2} ~
{&BEGINVALUE3} ~
{&BEGINVALUE4} ~
{&BEGINVALUE5}

&Scoped-define ENDVALUES ~
{&ENDVALUE1} ~
{&ENDVALUE2} ~
{&ENDVALUE3} ~
{&ENDVALUE4} ~
{&ENDVALUE5}

&Scoped-define DISPLAYVALUES ~
{&BEGINFLD1} {&ENDFLD1} ~
{&BEGINFLD2} {&ENDFLD2} ~
{&BEGINFLD3} {&ENDFLD3} ~
{&BEGINFLD4} {&ENDFLD4} ~
{&BEGINFLD5} {&ENDFLD5} ~
{&DISPLAYFLDS}

{methods/listobjs/addsttgs.i 1}
{methods/listobjs/addsttgs.i 2}
{methods/listobjs/addsttgs.i 3}
{methods/listobjs/addsttgs.i 4}
{methods/listobjs/addsttgs.i 5}
{methods/listobjs/addsttgs.i 6}

PROCEDURE Initialize-Values:
  DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
  DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    DO cnt = 1 TO NUM-ENTRIES("{&LISTORDER}"):
      ASSIGN
        cdummy = "~&" + STRING(cnt) + " By " +
            ENTRY(cnt,"{&LISTORDER}") + "," + STRING(cnt).
        list-order:RADIO-BUTTONS = IF cnt = 1 THEN cdummy
             ELSE list-order:RADIO-BUTTONS + "," + cdummy.
    END.
    IF NUM-ENTRIES("{&LISTORDER}") LT 2 THEN
    DISABLE list-order.
    &IF "{&F1}" NE "" &THEN
    DISPLAY {&F1}.
    &ENDIF
    RUN Get-Display-Values.
  END.
END PROCEDURE.

&Scoped-define FIELDS1 {&BEGINFLD1} {&ENDFLD1}
&Scoped-define FIELDS2 {&BEGINFLD2} {&ENDFLD2}
&Scoped-define FIELDS3 {&BEGINFLD3} {&ENDFLD3}
&Scoped-define FIELDS4 {&BEGINFLD4} {&ENDFLD4}
&Scoped-define FIELDS5 {&BEGINFLD5} {&ENDFLD5}

PROCEDURE Get-Display-Values:
  DO WITH FRAME {&FRAME-NAME}:
    CASE list-order:SCREEN-VALUE:
      WHEN "1" THEN
      DO:
        ENABLE {&FIELDS1}.
        DISABLE {&FIELDS2} {&FIELDS3} {&FIELDS4} {&FIELDS5}.
      END.
      WHEN "2" THEN
      DO:
        ENABLE {&FIELDS2}.
        DISABLE {&FIELDS1} {&FIELDS3} {&FIELDS4} {&FIELDS5}.
      END.
      WHEN "3" THEN
      DO:
        ENABLE {&FIELDS3}.
        DISABLE {&FIELDS1} {&FIELDS2} {&FIELDS4} {&FIELDS5}.
      END.
      WHEN "4" THEN
      DO:
        ENABLE {&FIELDS4}.
        DISABLE {&FIELDS1} {&FIELDS2} {&FIELDS3} {&FIELDS5}.
      END.
      WHEN "5" THEN
      DO:
        ENABLE {&FIELDS5}.
        DISABLE {&FIELDS1} {&FIELDS2} {&FIELDS3} {&FIELDS4}.
      END.
    END.
  END.
  RUN Get-Begin-Values.
  IF RETURN-VALUE = "EMPTY" THEN
  RETURN.
  RUN Get-End-Values.
  RUN Display-Values.
END PROCEDURE.

&Scoped-define FINDTYPE FIRST
&Scoped-define VALUTYPE BEGIN
{methods/listobjs/listtype.i}

&Scoped-define FINDTYPE LAST
&Scoped-define VALUTYPE END
{methods/listobjs/listtype.i}

PROCEDURE Display-Values:
  &IF "{&DISPLAYVALUES}" NE "" &THEN
  DISPLAY
    {&DISPLAYVALUES}
      WITH FRAME {&FRAME-NAME}.
  &ENDIF
END PROCEDURE.

PROCEDURE Display-Field-Value:
  DEFINE INPUT PARAMETER fldhandle AS WIDGET-HANDLE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    CASE fldhandle:NAME:
      &Scoped-define FINDTYPE FIRST
      &Scoped-define VALUTYPE BEGIN
      {methods/listobjs/dispvalu.i 1}
      {methods/listobjs/dispvalu.i 2}
      {methods/listobjs/dispvalu.i 3}
      {methods/listobjs/dispvalu.i 4}
      {methods/listobjs/dispvalu.i 5}
      &Scoped-define FINDTYPE LAST
      &Scoped-define VALUTYPE END
      {methods/listobjs/dispvalu.i 1}
      {methods/listobjs/dispvalu.i 2}
      {methods/listobjs/dispvalu.i 3}
      {methods/listobjs/dispvalu.i 4}
      {methods/listobjs/dispvalu.i 5}
      {custom/flddisp.i}
    END CASE.
    &IF "{&DISPLAYVALUES}" NE "" &THEN
    ASSIGN {&DISPLAYVALUES}.
    &ENDIF
    RUN Display-Values.
  END.
END PROCEDURE.
