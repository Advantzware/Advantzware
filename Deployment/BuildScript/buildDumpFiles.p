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

DEFINE STREAM s0.
DEFINE STREAM s1.
DEFINE STREAM s2.
DEFINE STREAM s3.
DEFINE STREAM s4.

DEF VAR cOutDir AS CHAR INIT "{&cDir}\Deployment\Patch\DataFiles" NO-UNDO.

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

&SCOPED-DEFINE cFile sys-ctrl
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile} WHERE
    sys-ctrl.module EQ "val":
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile zMessage
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

OUTPUT STREAM s0 TO VALUE(cOutDir + "\dynSubject.d").
OUTPUT STREAM s1 TO VALUE(cOutDir + "\dynSubjectTable.d").
OUTPUT STREAM s2 TO VALUE(cOutDir + "\dynSubjectWhere.d").
OUTPUT STREAM s3 TO VALUE(cOutDir + "\dynSubjectColumn.d").
OUTPUT STREAM s4 TO VALUE(cOutDir + "\dynSubjectParamSet.d").
FOR EACH dynSubject NO-LOCK WHERE 
    dynSubject.subjectID LT 5000
    BY dynSubject.subjectid:
    EXPORT STREAM s0 dynSubject.
    FOR EACH dynSubjectTable OF dynSubject NO-LOCK:
        EXPORT STREAM s1 dynSubjectTable.
    END. /* each dynSubjectTable */
    FOR EACH dynSubjectWhere OF dynSubject NO-LOCK:
        EXPORT STREAM s2 dynSubjectWhere.
    END. /* each dynSubjectWhere */
    FOR EACH dynSubjectColumn OF dynSubject NO-LOCK:
        EXPORT STREAM s3 dynSubjectColumn.
    END. /* each dynSubjectColumn */
    FOR EACH dynSubjectParamSet OF dynSubject NO-LOCK:
        EXPORT STREAM s4 dynSubjectParamSet.
    END. /* each dynSubjectParamSet */
END. /* each dynSubject */
OUTPUT STREAM s0 CLOSE.
OUTPUT STREAM s1 CLOSE.
OUTPUT STREAM s2 CLOSE.
OUTPUT STREAM s3 CLOSE.
OUTPUT STREAM s4 CLOSE.

OUTPUT TO VALUE(cOutDir + "\dynParam.d").
FOR EACH dynParam NO-LOCK WHERE 
    dynParam.paramType EQ "system" AND 
    dynParam.paramID LT 5000:
    EXPORT dynParam.
END. /* each dynParam */
OUTPUT CLOSE.

OUTPUT STREAM s0 TO VALUE(cOutDir + "\dynParamSet.d").
OUTPUT STREAM s1 TO VALUE(cOutDir + "\dynParamSetDtl.d").
FOR EACH dynParamSet NO-LOCK WHERE 
    dynParamSet.paramSetType EQ "system" AND 
    dynParamSet.paramSetID LT 5000:
    EXPORT STREAM s0 dynParamSet.
    FOR EACH dynParamSetDtl OF dynParamSet NO-LOCK:
        EXPORT STREAM s1 dynParamSetDtl.
    END. /* each dynParamSetDtl */
END. /* each dynParamSet */
OUTPUT STREAM s0 CLOSE.
OUTPUT STREAM s1 CLOSE.

OUTPUT TO VALUE(cOutDir + "\dynParamValue.d").
FOR EACH dynParamValue NO-LOCK WHERE 
    dynParamValue.user-id EQ "_default" AND 
    dynParamValue.subjectID LT 5000:
    EXPORT dynParamValue.
END. /* each dynParamValue */
OUTPUT CLOSE.

OUTPUT TO VALUE(cOutDir + "\dynPrgrmsPage.d").
FOR EACH dynPrgrmsPage NO-LOCK :
    EXPORT dynPrgrmsPage.
END. /* each dynParamValue */
OUTPUT CLOSE.

OUTPUT TO VALUE(cOutDir + "\dynPrgrmsPage.d").
FOR EACH dynPrgrmsPage NO-LOCK :
    EXPORT dynPrgrmsPage.
END. /* each dynParamValue */
OUTPUT CLOSE.

OUTPUT TO VALUE(cOutDir + "\dynLookup.d").
FOR EACH dynLookup NO-LOCK :
    EXPORT dynLookup.
END. /* each dynLookup */
OUTPUT CLOSE.

&SCOPED-DEFINE cFile estCostCategory
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile estCostGroup
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile estCostGroupLevel
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile naics
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile inventoryStatusType
OUTPUT TO VALUE(cOutDir + "\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile apiInbound
OUTPUT TO VALUE(cOutDir + "\APIData\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile apiInboundDetail
OUTPUT TO VALUE(cOutDir + "\APIData\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile apiOutbound
OUTPUT TO VALUE(cOutDir + "\APIData\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile apiOutboundDetail
OUTPUT TO VALUE(cOutDir + "\APIData\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile apiOutboundTrigger
OUTPUT TO VALUE(cOutDir + "\APIData\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile emailConfig
OUTPUT TO VALUE(cOutDir + "\APIData\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.

&SCOPED-DEFINE cFile serverResource
OUTPUT TO VALUE(cOutDir + "\APIData\{&cFile}.d").
FOR EACH {&cFile}:
    EXPORT {&cFile}.
END.
OUTPUT CLOSE.




