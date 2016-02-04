/* scopDir.i */

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

/* cellColumn = userExtent + 30 */
&GLOBAL-DEFINE cellColumn 128
&GLOBAL-DEFINE statusExtent 30
&GLOBAL-DEFINE userExtent 98
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
&GLOBAL-DEFINE version1 v2.007
&GLOBAL-DEFINE version2 v2.008
&GLOBAL-DEFINE version3 v2.009
&GLOBAL-DEFINE version4 v2.010
&GLOBAL-DEFINE version5 v2.011
&GLOBAL-DEFINE version6 v2.012
&GLOBAL-DEFINE version7 v2.013
&GLOBAL-DEFINE version8 v2.014
&GLOBAL-DEFINE version9 v2.015
&GLOBAL-DEFINE version v2.016

&IF DEFINED(installDir) EQ 0 &THEN
DEFINE VARIABLE installDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE sbUser AS CHARACTER NO-UNDO.

ASSIGN
  installDir = SEARCH('{&startDir}\about.txt')
  installDir = REPLACE(installDir,'{&startDir}\about.txt','')
  sbUser = USERID('{&sbDB}').

&GLOBAL-DEFINE installDir
&ENDIF
