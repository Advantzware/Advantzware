/*------------------------------------------------------------------------
  File: buildDumpFiles.p
  Description: utility to create .d files for a patch
  Input Parameters:  <none>
  Output Parameters: <none>
  Author: MYT
  Created: 08/31/18
  Change History:
------------------------------------------------------------------------*/

DEF VAR cOutDir AS CHAR INIT "N:\Repositories\Advantzware\PatchTemplate\DataFiles" NO-UNDO.

&SCOPED-DEFINE cFile emailcod
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d"
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile lookups
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d"
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile module
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d"
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgrms
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d"
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgmxref
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d"
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

OUTPUT TO VALUE(cOutDir + "\reftable.d"
FOR EACH reftable NO-LOCK WHERE 
    reftable.reftable EQ 'Utilities':
    EXPORT reftable.
END. 
OUTPUT CLOSE.

OUTPUT TO VALUE(cOutDir + "\notes.d"
FOR EACH reftable NO-LOCK WHERE 
    reftable.reftable EQ 'Utilities':
    FOR EACH notes EXCLUSIVE WHERE 
        notes.rec_key EQ reftable.rec_key:
        EXPORT notes.
    END. 
END. 
OUTPUT CLOSE.
