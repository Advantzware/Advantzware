/* custom/compile2.p       YSK */
DEF INPUT PARAM ip-comp-list AS cha .

DEF VAR v-pco-name AS cha FORM "x(300)" NO-UNDO.
DEF VAR v-rco-name AS cha FORM "x(300)" NO-UNDO.

OUTPUT TO c:\tmp\COMPILEADD.LOG.

INPUT FROM VALUE(ip-comp-list) NO-ECHO.
REPEAT WITH STREAM-IO:
    IMPORT v-pco-name.    
    ASSIGN
      v-rco-name = REPLACE(v-pco-name,'pco','rco')
      v-rco-name = SUBSTR(v-rco-name,1,R-INDEX(v-rco-name,'\') - 1).
    PUT UNFORMATTED 'Compiling ' v-pco-name ' ...' SKIP.

    COMPILE VALUE(v-pco-name) SAVE INTO VALUE(v-rco-name).
    /*PROCESS EVENTS.*/
    /*STATUS INPUT "Compiling " + v-pco-name .*/
  END. /* repeat */
