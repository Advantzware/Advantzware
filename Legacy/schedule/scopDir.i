/* scopDir.i */

&GLOBAL-DEFINE WinKitDontEmbed
&GLOBAL-DEFINE startDir schedule
&GLOBAL-DEFINE backup c:\sb.dat.backup
&GLOBAL-DEFINE data {&startDir}/data
&GLOBAL-DEFINE scenarios {&data}/scenarios
&GLOBAL-DEFINE updates {&data}/updates
&GLOBAL-DEFINE images {&startDir}/images
&GLOBAL-DEFINE objects {&startDir}/objects
&GLOBAL-DEFINE includes {&objects}/includes
&GLOBAL-DEFINE exports {&includes}/exports
&GLOBAL-DEFINE loads {&objects}/loads
&GLOBAL-DEFINE print {&objects}/print
&GLOBAL-DEFINE printPrompts {&print}/prompts
&GLOBAL-DEFINE prompts {&objects}/prompts
&GLOBAL-DEFINE viewers {&objects}/viewers

/* cellColumn = udfExtent + userExtent + 30 */
&GLOBAL-DEFINE cellColumn 173
&GLOBAL-DEFINE statusExtent 30
&GLOBAL-DEFINE udfExtent 20
&GLOBAL-DEFINE userExtent 123
/*******************************************************************
when expanding userExtent, changes need to be made in the following:
--------------------------------------------------------------------
   0. loads/ID/loadPro.p (loadUserFieldLabelWidth & setUseFields)
   1. scopDir.i (userExtent)
   2. loads/jobText.i
   3. includes/ttblJobIndex.i
   4. includes/ttblJobFields.i
   5. includes/Pro/boardProc.i (saveSenario)
   6. print/includes/rptLayout.i
   7. viewers/includes/setFilterFlag.i
   8. viewers/includes/viewersInclude.i (reopenBrowse)
   9. config.w [fieldsFrame] (defs)
*******************************************************************/
&GLOBAL-DEFINE version1 v3.003
&GLOBAL-DEFINE version2 v3.004
&GLOBAL-DEFINE version3 v3.005
&GLOBAL-DEFINE version4 v3.006
&GLOBAL-DEFINE version5 v4.000
&GLOBAL-DEFINE version6 v4.001
&GLOBAL-DEFINE version7 v4.002
&GLOBAL-DEFINE version8 v4.003
&GLOBAL-DEFINE version9 v4.004
&GLOBAL-DEFINE version v4.005

&IF DEFINED(installDir) EQ 0 &THEN
DEFINE VARIABLE clientDat AS CHARACTER NO-UNDO.
DEFINE VARIABLE codeDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE staticDat AS CHARACTER NO-UNDO.
DEFINE VARIABLE sbUser    AS CHARACTER NO-UNDO.

IF &IF DEFINED(FWD-VERSION) > 0 &THEN RT-OPSYS &ELSE OPSYS &ENDIF = "unix" THEN DO:

ASSIGN
  clientDat = SEARCH('{&data}/validID.dat')
  clientDat = REPLACE(clientDat,REPLACE('{&data}/validID.dat','\','/') ,'')
  codeDir   = SEARCH('{&startDir}/sbPro.r')
  codeDir   = REPLACE(codeDir,'{&startDir}/sbPro.r','')
  staticDat = SEARCH('{&startDir}/about.txt')
  staticDat = REPLACE(staticDat,'{&startDir}/about.txt','')
  sbUser    = USERID('{&sbDB}')
  .
IF codeDir EQ ? THEN
ASSIGN
  codeDir = SEARCH('{&startDir}/sbPro.p')
  codeDir = REPLACE(codeDir,'{&startDir}/sbPro.p','')
  .
END.
ELSE DO:
ASSIGN
  clientDat = SEARCH('{&data}\validID.dat')
  clientDat = REPLACE(clientDat,REPLACE('{&data}\validID.dat','/','\') ,'')
  codeDir   = SEARCH('{&startDir}\sbPro.r')
  codeDir   = REPLACE(codeDir,'{&startDir}\sbPro.r','')
  staticDat = SEARCH('{&startDir}\about.txt')
  staticDat = REPLACE(staticDat,'{&startDir}\about.txt','')
  sbUser    = USERID('{&sbDB}')
  .

IF codeDir EQ ? THEN
ASSIGN
  codeDir = SEARCH('{&startDir}\sbPro.p')
  codeDir = REPLACE(codeDir,'{&startDir}\sbPro.p','')
  .
END.

PROCEDURE noEmbeddedWindowForm :
    /* here simply as a dummy procedure to prevent folder.w  */
    /* from attempting to create tabs in non embedded window */
END PROCEDURE.

&GLOBAL-DEFINE installDir
&ENDIF
