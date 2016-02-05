/* btnlabel.i */

&IF '{&BUTTON-INCLUDE}' = 'JOBS' &THEN
&Scoped-define BUTTON-COUNT 30
&ELSE
&Scoped-define BUTTON-COUNT 10
&ENDIF

DEFINE INPUT-OUTPUT PARAMETER j AS INTEGER NO-UNDO.

DEFINE VARIABLE k AS INTEGER NO-UNDO.
DEFINE VARIABLE x AS INTEGER NO-UNDO.

ENABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
IF j + {&BUTTON-COUNT} GT NUM-ENTRIES(itemlist,'@') OR
   j GT NUM-ENTRIES(itemlist,'@') THEN
j = NUM-ENTRIES(itemlist,'@') - {&BUTTON-COUNT} + 1.
IF j LT 1 THEN
j = 1.
DO WITH FRAME {&FRAME-NAME}:
  DO k = j TO NUM-ENTRIES(itemlist,'@'):
    x = x + 1.
    IF x GT {&BUTTON-COUNT} THEN
    LEAVE.
    CASE x:
      WHEN 1 THEN
      Btn_Button-1:LABEL = ENTRY(k,itemlist,'@').
      WHEN 2 THEN
      Btn_Button-2:LABEL = ENTRY(k,itemlist,'@').
      WHEN 3 THEN
      Btn_Button-3:LABEL = ENTRY(k,itemlist,'@').
      WHEN 4 THEN
      Btn_Button-4:LABEL = ENTRY(k,itemlist,'@').
      WHEN 5 THEN
      Btn_Button-5:LABEL = ENTRY(k,itemlist,'@').
      WHEN 6 THEN
      Btn_Button-6:LABEL = ENTRY(k,itemlist,'@').
      WHEN 7 THEN
      Btn_Button-7:LABEL = ENTRY(k,itemlist,'@').
      WHEN 8 THEN
      Btn_Button-8:LABEL = ENTRY(k,itemlist,'@').
      WHEN 9 THEN
      Btn_Button-9:LABEL = ENTRY(k,itemlist,'@').
      WHEN 10 THEN
      Btn_Button-10:LABEL = ENTRY(k,itemlist,'@').
&IF '{&BUTTON-INCLUDE}' = 'JOBS' &THEN
      WHEN 11 THEN
      Btn_Button-11:LABEL = ENTRY(k,itemlist,'@').
      WHEN 12 THEN
      Btn_Button-12:LABEL = ENTRY(k,itemlist,'@').
      WHEN 13 THEN
      Btn_Button-13:LABEL = ENTRY(k,itemlist,'@').
      WHEN 14 THEN
      Btn_Button-14:LABEL = ENTRY(k,itemlist,'@').
      WHEN 15 THEN
      Btn_Button-15:LABEL = ENTRY(k,itemlist,'@').
      WHEN 16 THEN
      Btn_Button-16:LABEL = ENTRY(k,itemlist,'@').
      WHEN 17 THEN
      Btn_Button-17:LABEL = ENTRY(k,itemlist,'@').
      WHEN 18 THEN
      Btn_Button-18:LABEL = ENTRY(k,itemlist,'@').
      WHEN 19 THEN
      Btn_Button-19:LABEL = ENTRY(k,itemlist,'@').
      WHEN 20 THEN
      Btn_Button-20:LABEL = ENTRY(k,itemlist,'@').
      WHEN 21 THEN
      Btn_Button-21:LABEL = ENTRY(k,itemlist,'@').
      WHEN 22 THEN
      Btn_Button-22:LABEL = ENTRY(k,itemlist,'@').
      WHEN 23 THEN
      Btn_Button-23:LABEL = ENTRY(k,itemlist,'@').
      WHEN 24 THEN
      Btn_Button-24:LABEL = ENTRY(k,itemlist,'@').
      WHEN 25 THEN
      Btn_Button-25:LABEL = ENTRY(k,itemlist,'@').
      WHEN 26 THEN
      Btn_Button-26:LABEL = ENTRY(k,itemlist,'@').
      WHEN 27 THEN
      Btn_Button-27:LABEL = ENTRY(k,itemlist,'@').
      WHEN 28 THEN
      Btn_Button-28:LABEL = ENTRY(k,itemlist,'@').
      WHEN 29 THEN
      Btn_Button-29:LABEL = ENTRY(k,itemlist,'@').
      WHEN 30 THEN
      Btn_Button-30:LABEL = ENTRY(k,itemlist,'@').
&ENDIF
    END CASE.
  END.
  j = k.
  DO k = x + 1 TO {&BUTTON-COUNT}:
    CASE k:
      WHEN 1 THEN
      Btn_Button-1:HIDDEN = TRUE.
      WHEN 2 THEN
      Btn_Button-2:HIDDEN = TRUE.
      WHEN 3 THEN
      Btn_Button-3:HIDDEN = TRUE.
      WHEN 4 THEN
      Btn_Button-4:HIDDEN = TRUE.
      WHEN 5 THEN
      Btn_Button-5:HIDDEN = TRUE.
      WHEN 6 THEN
      Btn_Button-6:HIDDEN = TRUE.
      WHEN 7 THEN
      Btn_Button-7:HIDDEN = TRUE.
      WHEN 8 THEN
      Btn_Button-8:HIDDEN = TRUE.
      WHEN 9 THEN
      Btn_Button-9:HIDDEN = TRUE.
      WHEN 10 THEN
      Btn_Button-10:HIDDEN = TRUE.
&IF '{&BUTTON-INCLUDE}' = 'JOBS' &THEN
      WHEN 11 THEN
      Btn_Button-11:HIDDEN = TRUE.
      WHEN 12 THEN
      Btn_Button-12:HIDDEN = TRUE.
      WHEN 13 THEN
      Btn_Button-13:HIDDEN = TRUE.
      WHEN 14 THEN
      Btn_Button-14:HIDDEN = TRUE.
      WHEN 15 THEN
      Btn_Button-15:HIDDEN = TRUE.
      WHEN 16 THEN
      Btn_Button-16:HIDDEN = TRUE.
      WHEN 17 THEN
      Btn_Button-17:HIDDEN = TRUE.
      WHEN 18 THEN
      Btn_Button-18:HIDDEN = TRUE.
      WHEN 19 THEN
      Btn_Button-19:HIDDEN = TRUE.
      WHEN 20 THEN
      Btn_Button-20:HIDDEN = TRUE.
      WHEN 21 THEN
      Btn_Button-21:HIDDEN = TRUE.
      WHEN 22 THEN
      Btn_Button-22:HIDDEN = TRUE.
      WHEN 23 THEN
      Btn_Button-23:HIDDEN = TRUE.
      WHEN 24 THEN
      Btn_Button-24:HIDDEN = TRUE.
      WHEN 25 THEN
      Btn_Button-25:HIDDEN = TRUE.
      WHEN 26 THEN
      Btn_Button-26:HIDDEN = TRUE.
      WHEN 27 THEN
      Btn_Button-27:HIDDEN = TRUE.
      WHEN 28 THEN
      Btn_Button-28:HIDDEN = TRUE.
      WHEN 29 THEN
      Btn_Button-29:HIDDEN = TRUE.
      WHEN 30 THEN
      Btn_Button-30:HIDDEN = TRUE.
&ENDIF
    END CASE.
  END.
END.
