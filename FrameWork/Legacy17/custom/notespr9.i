/* custom/notespr2.i for job ticket 
                     {1} = table name 
                     {2} = note variable name with extent 
                     {3} = variable's extent 
                     {4} = where block */
                    
DEF VAR v-notes AS CHAR NO-UNDO.
DEF VAR v-lntxt AS CHAR NO-UNDO.
DEF VAR v-text  AS CHAR FORMAT "x(100)" NO-UNDO.

DEF VAR v-prvcd LIKE notes.note_code NO-UNDO.

DEF VAR v-spcnt AS INT NO-UNDO.
DEF VAR v-cnt1  AS INT NO-UNDO INIT 0.
DEF VAR v-cnt2  AS INT NO-UNDO INIT 0.
DEF VAR v-excnt AS INT NO-UNDO INIT 1.


FOR EACH notes WHERE {4} NO-LOCK BREAK BY notes.note_code:
  
  ASSIGN v-notes = TRIM(notes.note_text)
         v-notes = REPLACE(v-notes,CHR(10)," ")
         v-notes = REPLACE(v-notes,CHR(13)," ").

  ASSIGN v-cnt1 = 0.  

  DO v-cnt1 = 1 TO 100:
   
    ASSIGN v-excnt = v-excnt + 1
           v-spcnt = 0
           v-notes = TRIM(v-notes).    

    IF LENGTH(v-notes) GT 80 THEN DO:
      IF SUBSTR(v-notes,79,1) NE "" AND 
         SUBSTR(v-notes,81,1) NE ""
        THEN ASSIGN v-spcnt = R-INDEX(SUBSTR(v-notes,1,95)," ")
                    v-lntxt = SUBSTR(v-notes,1,v-spcnt)
                    v-notes = SUBSTR(v-notes,v-spcnt + 1).
        ELSE
         IF SUBSTR(v-notes,79,1) EQ "" AND 
            SUBSTR(v-notes,81,1) NE ""
           THEN ASSIGN v-spcnt = R-INDEX(SUBSTR(v-notes,1,80)," ")
                       v-lntxt = SUBSTR(v-notes,1,v-spcnt)
                       v-notes = SUBSTR(v-notes,v-spcnt + 1).
           ELSE ASSIGN v-lntxt = SUBSTR(v-notes,1,80)
                       v-notes = SUBSTR(v-notes,81).
         
       ASSIGN {2}[v-excnt] = {2}[v-excnt] + v-lntxt.

    END.
    ELSE  ASSIGN {2}[v-excnt] = {2}[v-excnt] + v-notes
                 v-notes        = "".           
    IF v-notes EQ ""  THEN LEAVE.

  END.

END.
