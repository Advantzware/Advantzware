/*------------------------------------------------------------------------
  File: buildDumpFiles.p
  Description: utility to create .d files for a patch
  Input Parameters:  <none>
  Output Parameters: <none>
  Author: MYT
  Created: 08/31/18
  Change History:
------------------------------------------------------------------------*/
&SCOPED-DEFINE cDir N:\Repositories\Advantzware

DEF VAR cOutDir AS CHAR INIT "{&cDir}\Template\Patch\DataFiles" NO-UNDO.

&SCOPED-DEFINE cFile audittbl

OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile emailcod
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile lookups
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile module
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgrms
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgmxref
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile translation
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile userLanguage
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.


&SCOPED-DEFINE cFile utilities
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile xUserMenu
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile} WHERE xUserMenu.user_id EQ "AddonUsr":
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile cueCard
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile cueCardText
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.



