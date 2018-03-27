/* sys/convert/txt-to-ln.p */
DEF INPUT PARAMETER ip-text AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-size AS INT  NO-UNDO.
DEF OUTPUT PARAMETER op-text AS CHAR NO-UNDO.

DEF  VAR editor-1 AS CHAR VIEW-AS EDITOR SIZE 30 BY 3.
DEF VAR v-out-text AS CHAR.


  RUN get-text
  ( INPUT ip-text,
   INPUT ip-size,
   OUTPUT op-text).

/*   RUN get-text                                                                                 */
/*   ( INPUT "This is a test to see if the editor widget can be used to cut the text into lines", */
/*    INPUT 20,                                                                                   */
/*    OUTPUT v-out-text).                                                                         */

PROCEDURE get-text:
  DEF INPUT PARAMETER ip-text AS CHAR NO-UNDO. /* input text */
  DEF INPUT PARAMETER ip-size AS INT NO-UNDO. /* input width-chars */
  DEF OUTPUT PARAMETER op-text AS CHAR NO-UNDO.
  DEF VAR v-out-text AS CHAR.

  DEF VAR i AS INT.
  DEF VAR j AS INT.
  DEF VAR k AS INT.
  DEF VAR prior-line-end AS INT.
  DEF VAR prior-line-start AS INT.

  FORM
      EDITOR-1 WITH FRAME X.

  editor-1:WIDTH-CHARS = ip-size.
  EDITOR-1:SCREEN-VALUE = ip-text.

  prior-line-start = 1.
  ASSIGN editor-1.
  DO i = 1 TO EDITOR-1:LENGTH:
    EDITOR-1:CURSOR-OFFSET = i.
    j = editor-1:CURSOR-LINE.

    IF j NE k THEN DO:
        /* New Line */
        IF k NE 0 THEN DO:
            prior-line-end = i - 1.
            editor-1:SET-SELECTION(prior-line-start, prior-line-end).
            prior-line-start = i.

            v-out-text = v-out-text + "|" +  editor-1:SELECTION-TEXT.
        END.
        k = j.
    END.

  END.
      prior-line-end = editor-1:LENGTH + 2.
    editor-1:SET-SELECTION(prior-line-start, prior-line-end).
    prior-line-start = i.

    v-out-text = v-out-text + "|" + editor-1:SELECTION-TEXT.
    v-out-text = TRIM(v-out-text, "|").
    op-text = v-out-text.
END.
