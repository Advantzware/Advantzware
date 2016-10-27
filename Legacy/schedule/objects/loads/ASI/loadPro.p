/* loadPro.p - ASI as of 2.3.2016 @ 5:45pm */

&SCOPED-DEFINE sbDB nosweat
&SCOPED-DEFINE ID ASI/ALL
&SCOPED-DEFINE HOP ASI/HOP
&SCOPED-DEFINE Fleetwood ASI/Fleetwood
/* add new fields to procedures loadUserFieldLabelWidth & setUseFields below */
/* add userField to rptFields.dat, see config.w definitions section to enable field */
&SCOPED-DEFINE nextUserField 88

/* when expanding userFields mod the following:
   1. scopDir.i (userExtent)
   2. loads/jobText.i
   3. includes/ttblJobIndex.i
   4. includes/ttblJobFields.i
   5. includes/Pro/boardProc.i (saveSenario)
   6. print/includes/rptLayout.i
   7. viewers/includes/setFilterFlag.i
   8. viewers/includes/viewersInclude.i (reopenBrowse)
   9. config.w [fieldsFrame] (defs) */

{schedule/scopDir.i}
{{&loads}/loadPro.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

RUN get{&version}.

DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE customLabelList AS CHARACTER NO-UNDO.
DEFINE VARIABLE ufCust AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufBoardName AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufEB AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufEF AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufEst AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufGetSalesRep AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufIPJobMaterial AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufIPJobMatField AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufIPJobSet AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufItemFG AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufJobMch AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufOEOrdl AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufOERel AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufPOOrdl AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufProdQty AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE ufPrep AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE useField AS LOGICAL NO-UNDO EXTENT {&userExtent}.
DEFINE VARIABLE useNotes AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE useSalesRep AS LOGICAL NO-UNDO INIT YES.
DEFINE VARIABLE useStatus AS LOGICAL NO-UNDO INIT YES.

DEFINE BUFFER bJobMch FOR job-mch.

DEFINE TEMP-TABLE rptLayout NO-UNDO
  FIELD rptID AS CHARACTER
  FIELD rptName AS CHARACTER
  FIELD rptFormat AS CHARACTER
  FIELD fieldLabel AS CHARACTER
  FIELD fieldName AS CHARACTER
  FIELD rptLine AS INTEGER
  FIELD rptColumn AS INTEGER
  FIELD excelColumn AS INTEGER
  FIELD exclude AS LOGICAL
  FIELD rptAltName AS CHARACTER
    INDEX rptLayout IS PRIMARY rptName rptFormat fieldLabel fieldName.

DEFINE TEMP-TABLE ttblRptLayout NO-UNDO LIKE rptLayout.
CREATE ttblRptLayout.
  
FUNCTION comma RETURNS CHARACTER (ipValue AS CHARACTER):
  RETURN IF ipValue NE '' THEN ',' ELSE ''.
END FUNCTION.

FUNCTION fDueQty RETURNS CHARACTER (ipiRunQty AS INTEGER,ipiProdQty AS INTEGER,
                                    ipdUnder% AS DECIMAL,ipdOver% AS DECIMAL):
    DEFINE VARIABLE dDueQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMinQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMaxQty AS DECIMAL NO-UNDO.

    ASSIGN
        dMinQty = IF ipdUnder% NE 0 THEN ipiRunQty * ipdUnder% ELSE ipiRunQty
        dMaxQty = IF ipdOver%  NE 0 THEN ipiRunQty * ipdOver%  ELSE ipiRunQty
        .
    
    IF ipiProdQty LT dMinQty THEN
    dDueQty = ipiRunQty - ipiProdQty.

    IF dDueQty LT 0 THEN dDueQty = 0.
    
    RETURN LEFT-TRIM(STRING(dDueQty,'zzz,zzz,zz9')).
END FUNCTION.

FUNCTION getItemName RETURNS CHARACTER (ipCompany AS CHARACTER,ipJobNo AS CHARACTER,
                                        ipJob AS INTEGER,ipJobNo2 AS INTEGER,
                                        ipForm AS INTEGER,ipBlank AS INTEGER):
  DEFINE VARIABLE itemName AS CHARACTER NO-UNDO.

  DEFINE BUFFER bItemFG FOR itemfg.

  IF traceON THEN
  PUT UNFORMATTED 'Function getItemName @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.

  IF AVAILABLE itemfg THEN DO:
    FOR FIRST fg-set NO-LOCK
        WHERE fg-set.company EQ itemfg.company
          AND fg-set.set-no EQ itemfg.i-no,
        FIRST bItemFG NO-LOCK
        WHERE bItemFG.company EQ fg-set.company
          AND bItemFG.i-no EQ fg-set.part-no
          AND bItemFG.i-name GT ''.
      itemName = bItemFG.i-name.
    END. /* for first */
  
    IF itemName EQ '' THEN DO:
      FOR EACH fg-set NO-LOCK
          WHERE fg-set.company EQ itemfg.company
            AND fg-set.set-no EQ itemfg.i-no:
        FIND FIRST bItemFG NO-LOCK
             WHERE bItemFG.company EQ fg-set.company
               AND bItemFG.i-no EQ fg-set.part-no NO-ERROR.
        IF AVAILABLE bItemFG AND bItemFG.i-name NE '' THEN DO:
          itemName = bItemFG.i-name.
          LEAVE.
        END. /* avail bitemfg */
      END. /* each fg-set */
    END. /* itemname blank */
  END. /* avail itemfg */

  RETURN itemName.
END FUNCTION.
/*
FUNCTION getItemNo RETURNS CHARACTER (ipCompany AS CHARACTER,ipJobNo AS CHARACTER,
                                      ipJobNo2 AS INTEGER,ipForm AS INTEGER,
                                      ipItemNo AS CHARACTER):
  DEFINE VARIABLE itemNo AS CHARACTER NO-UNDO.

  FOR EACH bJobMch NO-LOCK
     WHERE bJobMch.company EQ ipCompany
       AND bJobMch.job-no EQ ipJobNo
       AND bJobMch.job-no2 EQ ipJobNo2
       AND bJobMch.frm EQ ipForm
       AND bJobMch.i-no GT '':
    IF bJobMch.i-no NE '' AND bJobMch.i-no NE itemNo THEN
    itemNo = IF itemNo EQ '' THEN bJobMch.i-no
        ELSE IF itemNo NE bJobMch.i-no THEN '<Multi Item>'
        ELSE ''.
    IF itemNo EQ '<Multi Item>' THEN LEAVE.
  END. /* each bjobmch */
  IF itemNo EQ '' THEN itemNo = ipItemNo.
  RETURN itemNo.
END FUNCTION.
*/
FUNCTION getSetPOQtyRec RETURNS CHARACTER (ipCompany AS CHARACTER,ipJobNo AS CHARACTER,
                                           ipJobNo2 AS INTEGER,ipForm AS INTEGER,
                                           ipINo AS CHARACTER):
  DEFINE VARIABLE qty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE rtnQty AS DECIMAL NO-UNDO.

  IF traceON THEN
  PUT UNFORMATTED 'Function getSetPOQtyRec @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.

  FOR EACH rm-rcpth NO-LOCK
      WHERE rm-rcpth.company EQ ipCompany
        AND rm-rcpth.job-no EQ ipJobNo
        AND rm-rcpth.job-no2 EQ ipJobNo2
        AND rm-rcpth.i-no EQ ipINo
        AND rm-rcpth.rita-code EQ 'R'
        AND CAN-FIND(FIRST item
                     WHERE item.company EQ ipCompany
                       AND item.i-no EQ rm-rcpth.i-no
                       AND CAN-DO('B,P,R,1,2,3,4',item.mat-type)),
      FIRST job-mat NO-LOCK
      WHERE job-mat.company EQ ipCompany
        AND job-mat.rm-i-no EQ rm-rcpth.i-no
        AND job-mat.job-no EQ ipJobNo
        AND job-mat.job-no2 EQ ipJobNo2
        AND job-mat.frm EQ ipForm,
      EACH rm-rdtlh NO-LOCK
      WHERE rm-rdtlh.r-no EQ rm-rcpth.r-no
        AND (rm-rdtlh.s-num EQ ipForm OR rm-rdtlh.s-num EQ 0):
    IF rm-rcpth.pur-uom EQ 'EA' THEN qty = rm-rdtlh.qty.
    ELSE RUN sys/ref/convquom.p(rm-rcpth.pur-uom,'EA',job-mat.basis-w,job-mat.len,
                                job-mat.wid,job-mat.dep,rm-rdtlh.qty,OUTPUT qty).
    rtnQty = rtnQty + qty.
  END.
  RETURN STRING(rtnQty).
END FUNCTION.

FUNCTION prodQty RETURNS CHARACTER (ipCompany AS CHARACTER,ipResource AS CHARACTER,
                                    ipJobNo AS CHARACTER,ipJobNo2 AS INTEGER,
                                    ipFrm AS INTEGER,ipBlankNo AS INTEGER,
                                    ipPass AS INTEGER,ipProdQtyProgram AS CHARACTER):
  DEFINE VARIABLE prodQty AS INTEGER NO-UNDO.

  IF NOT ufProdQty THEN RETURN ''.

  IF traceON THEN
  PUT UNFORMATTED 'Function prodQty @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
  
  RUN VALUE(ipProdQtyProgram) (ipCompany,ipResource,ipJobNo,ipJobNo2,ipFrm,ipBlankNo,ipPass,OUTPUT prodQty).
  RETURN LEFT-TRIM(STRING(prodQty,'zzz,zzz,zz9')).
END FUNCTION.

FUNCTION setUserField RETURNS CHARACTER (ipIdx AS INTEGER, ipValue AS CHARACTER):
  RETURN IF useField[ipIdx] THEN ipValue ELSE ''.
END FUNCTION.

FUNCTION statusCheckOff RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipJob AS INTEGER,ipJobNo AS CHARACTER,
   ipJobNo2 AS INTEGER,ipForm AS INTEGER,ipMatType AS CHARACTER):

  DEFINE VARIABLE rtnValue AS LOGICAL NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF traceON THEN
  PUT UNFORMATTED 'Function statusCheckOff @ Checking: ' AT 15 ipMatType ' ' STRING(TIME,'hh:mm:ss') ' ' ETIME.
  FOR EACH job-mat NO-LOCK WHERE job-mat.company EQ ipCompany
                             AND job-mat.job EQ ipJob
                             AND job-mat.job-no EQ ipJobNo
                             AND job-mat.job-no2 EQ ipJobNo2
                             AND job-mat.frm EQ ipForm,
      FIRST item OF job-mat NO-LOCK WHERE item.mat-type EQ ipMatType:
    IF traceON THEN
    PUT UNFORMATTED 'Mat Type: ' AT 20 item.mat-type ' '
      job-mat.company ' '
      job-mat.job ' '
      job-mat.job-no ' '
      job-mat.job-no2 ' '
      job-mat.i-no ' '
      job-mat.frm ' '
      job-mat.blank-no.
    IF item.mat-type EQ ipMatType THEN
      rtnvalue = CAN-FIND(FIRST mat-act
         WHERE mat-act.company EQ job-mat.company
           AND mat-act.job EQ job-mat.job
           AND mat-act.job-no EQ job-mat.job-no
           AND mat-act.job-no2 EQ job-mat.job-no2
           AND mat-act.i-no EQ job-mat.i-no
           AND mat-act.s-num EQ job-mat.frm
           AND mat-act.b-num EQ job-mat.blank-no USE-INDEX job).
  END. /* each job-mat */
  IF traceON THEN
  PUT UNFORMATTED ipMatType AT 20 ' ' rtnValue ' ' ETIME SKIP.
  RETURN rtnValue.
END FUNCTION.

RUN customValueList (OUTPUT customValueList,OUTPUT customLabelList).

IF ID EQ '' THEN ID = 'ASI/ALL'.
departmentList = ''.
FOR EACH dept NO-LOCK:
  departmentList = departmentList + comma(departmentList) + dept.code.
END.

&IF '{&Board}' NE 'View' &THEN
DEFINE VARIABLE altResSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE asiCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE asiLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE beginEstType AS INTEGER NO-UNDO.
DEFINE VARIABLE boardLength AS DECIMAL NO-UNDO.
DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE boardWidth AS DECIMAL NO-UNDO.
DEFINE VARIABLE custNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE custName AS CHARACTER NO-UNDO.
DEFINE VARIABLE customVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE dimFormat AS CHARACTER NO-UNDO.
DEFINE VARIABLE decimalFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE department AS CHARACTER NO-UNDO.
DEFINE VARIABLE dueDate AS DATE NO-UNDO.
DEFINE VARIABLE endDate AS DATE NO-UNDO.
DEFINE VARIABLE endEstType AS INTEGER NO-UNDO.
DEFINE VARIABLE endTime AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE itemDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobBoard AS LOGICAL NO-UNDO.
DEFINE VARIABLE jobDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobSort AS CHARACTER NO-UNDO.
DEFINE VARIABLE keyValues AS CHARACTER NO-UNDO.
DEFINE VARIABLE kFrac AS DECIMAL NO-UNDO.
DEFINE VARIABLE lagTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvCode2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvNoteKey AS CHARACTER NO-UNDO.
DEFINE VARIABLE noDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE prodDate AS DATE NO-UNDO.
DEFINE VARIABLE prodQtyProgram AS CHARACTER NO-UNDO INITIAL ?.
DEFINE VARIABLE resourceDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE resSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE runMSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE salesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE salesRepFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE scheduleResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE sortOrder AS INTEGER NO-UNDO.
DEFINE VARIABLE startDate AS DATE NO-UNDO.
DEFINE VARIABLE startTime AS INTEGER NO-UNDO.
DEFINE VARIABLE strRowID AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobText AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobToolTip AS CHARACTER NO-UNDO.
DEFINE VARIABLE timeSpan AS INTEGER NO-UNDO.
DEFINE VARIABLE unitFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE useDeptSort AS LOGICAL NO-UNDO.

DEFINE BUFFER bMach FOR mach.

DEFINE TEMP-TABLE tResource NO-UNDO
  FIELD dSeq AS INTEGER
  FIELD mSeq AS INTEGER
  FIELD resource AS CHARACTER
  FIELD mDscr AS CHARACTER
  FIELD department AS CHARACTER
    INDEX tResource IS PRIMARY dSeq mSeq resource.

DISABLE TRIGGERS FOR LOAD OF reftable.

FUNCTION boardName RETURNS CHARACTER (ipMatType1 AS CHARACTER):
  IF traceON THEN
  PUT UNFORMATTED 'Function boardName @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.

  IF NOT ufBoardName THEN RETURN ''.
  
  FIND FIRST item NO-LOCK WHERE item.company EQ asiCompany
                            AND item.i-no EQ ipMatType1 NO-ERROR.
  RETURN IF AVAILABLE item THEN item.i-name ELSE ''.
END FUNCTION.

FUNCTION calcJobTime RETURNS INTEGER (ipMr AS DECIMAL, ipRun AS DECIMAL):
  IF ipMR EQ ? THEN ipMR = 0.
  IF ipRun EQ ? THEN ipRun = 0.
  RETURN INTEGER(ipMR * 3600 + ipRun * 3600).
END FUNCTION.

FUNCTION convBase16 RETURNS DECIMAL (ipValue AS DECIMAL):
  RETURN TRUNCATE(ipValue,0) + ((ipValue - TRUNCATE(ipValue,0)) / 6.25).
END FUNCTION.

FUNCTION fixTime RETURNS INTEGER (ipTime AS INTEGER):
  IF ipTime EQ ? THEN ipTime = 0.
  RETURN INTEGER(ipTime - TRUNCATE(ipTime / 86400,0) * 86400).
END FUNCTION.

FUNCTION getLiveUpdate RETURNS LOGICAL
        (ipCompany AS CHARACTER, ipJobNo AS CHARACTER, ipJobNo2 AS INTEGER,
         ipForm AS INTEGER, ipMCode AS CHARACTER):

  DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.

  IF traceON THEN
  PUT UNFORMATTED 'Function getLiveUpdate @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
  
  lvCode = ipJobNo + ',' + STRING(ipJobNo2) + ',' + STRING(ipForm) + ',' + ipMCode.
  FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ 'sbLiveUpdate'
                                AND reftable.company EQ ipCompany
                                AND reftable.loc EQ ''
                                AND reftable.code EQ lvCode NO-ERROR.
  IF AVAILABLE reftable THEN
  RETURN reftable.code2 EQ 'Yes'.
  ELSE RETURN YES.
END FUNCTION.

FUNCTION getSalesRep RETURNS CHARACTER
        (ipCompany AS CHARACTER,ipSalesRep AS CHARACTER):
  IF NOT ufGetSalesRep THEN RETURN ''.
  FIND FIRST sman NO-LOCK WHERE sman.company EQ ipCompany
                            AND sman.sman EQ ipSalesRep NO-ERROR.
  RETURN IF AVAILABLE sman THEN sman.sname ELSE ''.
END FUNCTION.

FUNCTION jobBoard RETURN LOGICAL (ipCompany AS CHARACTER, ipJob AS INTEGER,
                                  ipJobNo AS CHARACTER, ipJobNo2 AS INTEGER,
                                  ipForm AS INTEGER,
                                  OUTPUT opBoardLength AS DECIMAL,
                                  OUTPUT opBoardWidth AS DECIMAL):
  IF traceON THEN
  PUT UNFORMATTED 'Function jobBoard @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
  
  ASSIGN
    opBoardLength = 0
    opBoardWidth = 0.
  FOR EACH job-mat NO-LOCK
      WHERE job-mat.company EQ ipCompany
        AND job-mat.job EQ ipJob
        AND job-mat.job-no EQ ipJobNo
        AND job-mat.job-no2 EQ ipJobNo2
        AND job-mat.frm EQ ipForm,
      FIRST item OF job-mat NO-LOCK WHERE item.mat-type EQ 'B':
    ASSIGN
      opBoardLength = job-mat.len
      opBoardWidth = job-mat.wid.
    IF CAN-FIND(FIRST mat-act
       WHERE mat-act.company EQ job-mat.company
         AND mat-act.job EQ job-mat.job
         AND mat-act.job-no EQ job-mat.job-no
         AND mat-act.job-no2 EQ job-mat.job-no2
         AND mat-act.i-no EQ job-mat.i-no
         AND mat-act.s-num EQ job-mat.frm
         AND mat-act.b-num EQ job-mat.blank-no USE-INDEX job) THEN
    RETURN YES.
  END. /* each job-mat */
  RETURN NO.
END FUNCTION.

FUNCTION k16 RETURN CHARACTER (ipArrayValue AS DECIMAL,
                               ipKFrac AS DECIMAL,
                               ipDecimalFormat AS INTEGER):
  RETURN STRING(ROUND(TRUNC(ipArrayValue,0) +
              ((ipArrayValue - TRUNC(ipArrayValue,0)) / ipKFrac),
                ipDecimalFormat)).
END FUNCTION.

FUNCTION noDate RETURN LOGICAL (ipCompany AS CHARACTER):
  RETURN CAN-FIND(FIRST sys-ctrl NO-LOCK
                  WHERE sys-ctrl.company EQ ipCompany
                    AND sys-ctrl.name EQ 'Schedule'
                    AND sys-ctrl.char-fld EQ 'NoDate'
                    AND sys-ctrl.int-fld EQ 0
                    AND sys-ctrl.log-fld EQ YES).
END FUNCTION.

IF VALID-HANDLE(ipContainerHandle) THEN DO:
    RUN asiCommaList IN ipContainerHandle ('Company',OUTPUT asiCompany).
    RUN asiCommaList IN ipContainerHandle ('Location',OUTPUT asiLocation).
END.
IF asiCompany EQ '' THEN asiCompany = '001'.
IF asiLocation EQ '' THEN asiLocation = 'Main'.

ASSIGN
  useDeptSort = SEARCH(findProgram('{&data}/',ID,'/useDeptSort.dat')) NE ?
  useSalesRep = SEARCH(findProgram('{&data}/',ID,'/useSalesRep.dat')) NE ?
  .

IF useSalesRep THEN DO:
  FOR EACH sman NO-LOCK WHERE sman.company EQ asiCompany:
    IF NOT CAN-DO(customValueList,sman.sman) THEN
    customValueList = customValueList + ',' + sman.sman.
  END. /* each sman */
END. /* salesman routine */

RUN setUseFields.
RUN moveFontValue.
RUN moveNotes.
RUN fixSBRefTable.
RUN loadRptLayout ('jobText',OUTPUT jobText).
RUN loadRptLayout ('jobToolTip',OUTPUT jobToolTip).

FOR EACH mach NO-LOCK WHERE mach.company EQ asiCompany AND mach.m-code GT ''
    BREAK BY mach.d-seq BY mach.m-seq BY mach.m-code:
  IF FIRST-OF(mach.m-code) THEN DO:
    &IF '{&Board}' EQ 'Pro' &THEN
    IF capacityLoad THEN
    FOR EACH mach-calendar NO-LOCK WHERE mach-calendar.company EQ mach.company
                                     AND mach-calendar.m-code EQ mach.m-code:
      IF mach-calendar.start-time EQ 0 AND
        (mach-calendar.end-time EQ 0 OR mach-calendar.end-time EQ 86400) THEN
      NEXT.
      {{&loads}/resourceUse.i mach.m-code}
      IF mach-calendar.start-time NE 0 THEN
      {{&exports}/capacity.i &streamName=sCapacity
          &dayID=0
          &resource=mach-calendar.m-code
          &startDate=mach-calendar.m-date
          &startTime=0
          &endTime=mach-calendar.start-time}
      IF mach-calendar.end-time NE 86400 THEN
      {{&exports}/capacity.i &streamName=sCapacity
          &dayID=0
          &resource=mach-calendar.m-code
          &startDate=mach-calendar.m-date
          &startTime=mach-calendar.end-time
          &endTime=86400}
    END. /* each mach-calendar */
    &ENDIF
    lvResource = IF mach.sch-m-code NE '' THEN mach.sch-m-code ELSE mach.m-code.
    IF NOT CAN-FIND(FIRST tResource WHERE tResource.resource EQ lvResource) THEN DO:
      FIND FIRST bMach NO-LOCK WHERE bMach.company EQ asiCompany
                                 AND bMach.m-code EQ lvResource NO-ERROR.
      IF AVAILABLE bMach THEN DO:
        CREATE tResource.
        ASSIGN
          tResource.resource = lvResource
          tResource.mDscr = bMach.m-dscr
          tResource.dSeq = bMach.d-seq
          tResource.mSeq = bMach.m-seq
          tResource.department = STRING(bMach.d-seq,'99') + STRING(bMach.m-seq,'99').
        IF NOT useDeptSort THEN tResource.department = '0000'.
      END. /* avail bmach */
    END. /* if not */
  END. /* first-of m-code */
END. /* each mach */

FOR EACH tResource EXCLUSIVE-LOCK:
  sortOrder = sortOrder + 1.
  {{&exports}/resource.i &streamName=sResource
      &resourceDescription=tResource.mDscr
      &resource=tResource.resource
      &sortOrder=sortOrder
      &department=tResource.department}
END. /* each tresource */

IF traceON THEN DO:
  OUTPUT TO 'schedule/load.log' APPEND.
  ETIME(TRUE).
END.

/* set values used for getting internal & end cell Len/Width values in ipJobSet */
ASSIGN
  decimalFormat = 6
  kFrac = 6.25.
FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ asiCompany
       AND sys-ctrl.name EQ 'CECSCRN' NO-ERROR.
IF AVAILABLE sys-ctrl THEN DO:
  CASE sys-ctrl.char-fld:
    WHEN '32nd~'s' THEN kFrac = 3.125.
    WHEN 'Decimal' THEN ASSIGN decimalFormat = 2 kFrac = 1.0.
  END CASE.
END. /* avail sys-ctrl */

ASSIGN
  beginEstType = IF CAN-DO('ASI/ALL*,ASI/Folding*,{&HOP}',ID) THEN 0 ELSE 5
  endEstType = IF CAN-DO('ASI/ALL*,ASI/Corrugated*,{&Fleetwood}',ID) THEN 99 ELSE 4.

IF CONNECTED('emptrack') THEN
prodQtyProgram = SEARCH(findProgram('{&loads}/',ID,'/prodQty.r')).
  
FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company EQ asiCompany
      AND job-hdr.opened EQ YES
      &IF DEFINED(jobNo) NE 0 &THEN
      AND job-hdr.job-no GE '{&jobNo}'
      &ENDIF
   ,EACH job OF job-hdr NO-LOCK
   ,FIRST est OF job NO-LOCK
    WHERE est.est-type GE beginEstType
      AND est.est-type LE endEstType
    BREAK BY est.est-type
          BY job-hdr.job-no
          BY job-hdr.job-no2
          BY job-hdr.frm
          BY job-hdr.blank-no:

  FOR EACH job-mch NO-LOCK
      WHERE job-mch.company EQ job.company
        AND job-mch.job EQ job.job
        AND job-mch.job-no EQ job.job-no
        AND job-mch.job-no2 EQ job.job-no2
        AND job-mch.run-complete EQ NO
     ,FIRST mach NO-LOCK
      WHERE mach.company EQ job.company
        AND mach.loc EQ asiLocation
        AND mach.m-code EQ job-mch.m-code
      BREAK BY job-mch.job
            BY job-mch.frm
            BY job-mch.blank-no
            BY job-mch.line:

    IF est.est-type EQ 3 OR est.est-type EQ 4 OR
       est.est-type EQ 7 OR est.est-type EQ 8 THEN DO:
      IF job-mch.frm NE job-hdr.frm THEN NEXT.
      IF job-hdr.blank-no LE 1 AND job-mch.blank-no GT 1 THEN NEXT.
      IF job-hdr.blank-no GT 1 AND job-hdr.blank-no NE job-mch.blank-no THEN NEXT.
    END. /* tandem or combo */
    
    IF traceON THEN DO:
      debugCount = debugCount + 1.
      PUT UNFORMATTED
        debugCount AT 10
        job-mch.m-code AT 20 ' '
        job-mch.job-no AT 30 '-'
        job-mch.job-no2 '.'
        job-mch.frm ' -- '
        job-mch.line ' : '
        STRING(TIME,'hh:mm:ss') ' ' ETIME
        SKIP
        .
    END.
    
    IF FIRST-OF(job-mch.frm) OR NOT cascadeJob THEN resSeq = 0.
    
    scheduleResource = IF mach.sch-m-code NE '' THEN mach.sch-m-code ELSE mach.m-code.
    {{&loads}/resourceUse.i scheduleResource}
    
    department = '0000'.
    IF useDeptSort THEN DO:
      FIND FIRST bMach NO-LOCK
           WHERE bMach.company EQ asiCompany
             AND bMach.m-code EQ scheduleResource NO-ERROR.
      department = IF AVAILABLE bMach THEN STRING(bMach.d-seq,'99') + STRING(bMach.m-seq,'99')
                                      ELSE STRING(mach.d-seq,'99') + STRING(mach.m-seq,'99').
    END. /* usedeptsort */
    
    ASSIGN
      altResSeq = job-mch.line
      customVal = ''
      dimFormat = '>>9.99999'
      dueDate = IF job.due-date NE ? THEN job.due-date ELSE {{&includes}/lastDate.i}
      jobSort = job-mch.job-no + '-'
              + STRING(job-mch.job-no2,'99')
              + '.' + STRING(job-mch.frm,'99')
      jobNumber = LEFT-TRIM(job-mch.job-no + '-'
                + STRING(job-mch.job-no2)
                + '.' + STRING(job-mch.frm))
      jobStatus = useSalesRep
      resSeq = resSeq + 1
      resourceDescription = mach.m-dscr
      salesRepFound = NO
      startDate = job-mch.start-date-su
      startTime = fixTime(job-mch.start-time-su)
      strRowID = STRING(ROWID(job)) + ',' + STRING(ROWID(job-mch))
      keyValues = job-mch.company + ','
                + STRING(job-mch.line) + ','
                + job-mch.m-code + ','
                + STRING(job-mch.job) + ','
                + job-mch.job-no + ','
                + STRING(job-mch.job-no2) + ','
                + STRING(job-mch.frm) + ','
                + STRING(job-mch.blank-no) + ','
                + STRING(job-mch.pass) + ','
                + job-mch.rec_key
      timeSpan = calcJobTime(job-mch.mr-hr,job-mch.run-hr)
      unitFound = NO
      userField = ''
      .
  
    /* surely, jobs shouldn't be this old, if any, move to pending */
    IF startDate LT TODAY - 365 THEN startDate = ?.
    /* prevent any jobs too far into the future from loading */
    IF startDate GT TODAY + pendingLastDay THEN DO:
      MESSAGE 'Job' jobNumber 'has a Start Date of' startDate SKIP
        'which is over' pendingLastDay 'days into the future,' SKIP
        'it has been returned to Pending.'
        VIEW-AS ALERT-BOX ERROR TITLE 'Return Job to Pending'.
      startDate = ?.
    END.
  
    IF NoDate(job-mch.company) AND (job-mch.end-date-su EQ ? OR job-mch.end-date EQ ?) THEN
    ASSIGN
      startDate = ?
      startTime = timeSpan
      .
    ELSE IF startDate NE ? THEN
    RUN calcEnd (startDate
                ,startTime
                ,job-mch.mr-hr
                ,job-mch.run-hr
                ,OUTPUT endDate
                ,OUTPUT endTime
                 ).
    ELSE /* used for pending job, need to store total job time */
    startTime = timeSpan.

    IF traceON THEN
    PUT UNFORMATTED 'Access oe-ordl @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    FIND FIRST oe-ordl NO-LOCK
         WHERE oe-ordl.company EQ job-mch.company
           AND oe-ordl.job-no EQ job-mch.job-no
           AND oe-ordl.job-no2 EQ job-mch.job-no2 
           AND oe-ordl.i-no EQ job-mch.i-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      IF ufOERel THEN DO:
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no EQ oe-ordl.ord-no
              AND oe-rel.i-no EQ oe-ordl.i-no
              AND oe-rel.line EQ oe-ordl.line
            BREAK BY oe-rel.rel-no:
          IF LAST(oe-rel.rel-no) THEN
          ASSIGN
            userField[37] = setUserField(37,STRING(oe-rel.qty,'->>,>>>,>>9'))
            userField[38] = setUserField(38,STRING(oe-rel.rel-date,'99.99.9999'))
            userField[39] = setUserField(39,oe-rel.ship-addr[1])
            userField[40] = setUserField(40,oe-rel.ship-city + ', '
                          + oe-rel.ship-state + ' '
                          + oe-rel.ship-zip)
            userField[52] = setUserField(52,STRING(oe-rel.qty))
            userField[63] = setUserField(63,oe-ordl.po-no)
            .
        END. /* each oe-rel */
      END. /* ufoerel */
    END. /* avail oe-ordl */
    
    IF NOT AVAILABLE oe-ordl OR oe-ordl.req-date EQ ? THEN DO:
      FIND FIRST oe-ord NO-LOCK
           WHERE oe-ord.company EQ job-mch.company
             AND oe-ord.job-no EQ job-mch.job-no
             AND oe-ord.job-no2 EQ job-mch.job-no2 NO-ERROR.
      IF NOT AVAILABLE oe-ord THEN DO:
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ job-mch.company
               AND oe-ordl.job-no EQ job-mch.job-no
               AND oe-ordl.job-no2 EQ job-mch.job-no2 NO-ERROR.
        IF AVAILABLE oe-ordl THEN
        FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      END. /* not avail oe-ord */
      IF AVAILABLE oe-ord THEN DO:
        IF useSalesRep THEN DO:
          i = LOOKUP(oe-ord.sman[1],customValueList) - 1.
          IF i GT 0 THEN jobStatus[i] = NO.
        END. /* if usesalesrep */
        ASSIGN
          dueDate = oe-ord.due-date
          prodDate = oe-ord.prod-date
          userField[36] = setUserField(36,getSalesRep(oe-ord.company,oe-ord.sman[1]))
          userField[86] = setUserField(86,STRING(oe-ord.under-pct,'>>9.99'))
          userField[87] = setUserField(87,STRING(oe-ord.over-pct,'>>9.99'))
          salesRepFound = YES
          .
      END. /* avail oe-ord */
    END. /* not avail oe-ordl or reg-date eq ? */
    ELSE dueDate = oe-ordl.req-date.

    RELEASE po-ord.
    RELEASE po-ordl.
    
    IF traceON THEN
    PUT UNFORMATTED 'Available oe-ordl @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    IF AVAILABLE oe-ordl THEN DO:
      IF ufOEOrdl THEN
      ASSIGN
        userField[82] = setUserField(82,STRING(oe-ordl.prom-date,'99/99/9999'))
        userField[84] = setUserField(84,STRING(oe-ordl.t-price))
        userField[86] = setUserField(86,STRING(oe-ordl.under-pct,'>>9.99'))
        userField[87] = setUserField(87,STRING(oe-ordl.over-pct,'>>9.99'))
        .
      FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      IF AVAILABLE oe-ord THEN DO:
        IF useSalesRep THEN DO:
          i = LOOKUP(oe-ord.sman[1],customValueList) - 1.
          IF i GT 0 THEN jobStatus[i] = NO.
        END. /* if usesalesrep */
        ASSIGN
          prodDate = oe-ord.prod-date
          userField[36] = setUserField(36,getSalesRep(oe-ord.company,oe-ord.sman[1]))
          salesRepFound = YES
          .
      END. /* avail oe-ord */
      IF ufPOOrdl THEN DO:
        FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ job-mch.company
                                    AND po-ord.po-no EQ oe-ordl.po-no-po NO-ERROR.
        IF traceON THEN
        PUT UNFORMATTED 'Available po-ord @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
        IF AVAILABLE po-ord THEN DO:
          FIND FIRST po-ordl NO-LOCK
               WHERE po-ordl.company EQ po-ord.company
                 AND po-ordl.po-no EQ po-ord.po-no
                 AND po-ordl.job-no EQ job-mch.job-no
                 AND po-ordl.job-no2 EQ job-mch.job-no2
                 AND po-ordl.i-no EQ job-mch.i-no
                 AND po-ordl.s-num EQ job-mch.frm
                 AND po-ordl.b-num EQ job-mch.blank-no NO-ERROR.
          IF NOT AVAILABLE po-ordl THEN
          FIND FIRST po-ordl NO-LOCK
               WHERE po-ordl.company EQ po-ord.company
                 AND po-ordl.po-no EQ po-ord.po-no
                 AND po-ordl.job-no EQ job-mch.job-no
                 AND po-ordl.job-no2 EQ job-mch.job-no2
                 AND po-ordl.i-no EQ job-mch.i-no
                 AND po-ordl.b-num EQ job-mch.blank-no NO-ERROR.
          IF NOT AVAILABLE po-ordl THEN
          FIND FIRST po-ordl NO-LOCK
               WHERE po-ordl.company EQ po-ord.company
                 AND po-ordl.po-no EQ po-ord.po-no
                 AND po-ordl.job-no EQ job-mch.job-no
                 AND po-ordl.job-no2 EQ job-mch.job-no2
                 AND po-ordl.b-num EQ job-mch.blank-no NO-ERROR.
          IF NOT AVAILABLE po-ordl THEN
          FIND FIRST po-ordl NO-LOCK
               WHERE po-ordl.company EQ po-ord.company
                 AND po-ordl.po-no EQ po-ord.po-no
                 AND po-ordl.job-no EQ job-mch.job-no
                 AND po-ordl.job-no2 EQ job-mch.job-no2 NO-ERROR.
          IF NOT AVAILABLE po-ordl THEN
          FIND FIRST po-ordl NO-LOCK
               WHERE po-ordl.company EQ po-ord.company
                 AND po-ordl.po-no EQ po-ord.po-no NO-ERROR.
        END. /* avail po-ord */
      END. /* ufpoordl */
    END. /* avail oe-ordl */
    ELSE IF ufPOOrdl THEN DO:
      IF traceON THEN
      PUT UNFORMATTED 'Access po-ordl @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
      FIND FIRST po-ordl NO-LOCK
           WHERE po-ordl.company EQ job-mch.company
             AND po-ordl.job-no EQ job-mch.job-no
             AND po-ordl.job-no2 EQ job-mch.job-no2
             AND po-ordl.i-no EQ job-mch.i-no
             AND po-ordl.s-num EQ job-mch.frm
             AND po-ordl.b-num EQ job-mch.blank-no NO-ERROR.
      IF NOT AVAILABLE po-ordl THEN
      FIND FIRST po-ordl NO-LOCK
           WHERE po-ordl.company EQ job-mch.company
             AND po-ordl.job-no EQ job-mch.job-no
             AND po-ordl.job-no2 EQ job-mch.job-no2
             AND po-ordl.i-no EQ job-mch.i-no
             AND po-ordl.b-num EQ job-mch.blank-no NO-ERROR.
      IF NOT AVAILABLE po-ordl THEN
      FIND FIRST po-ordl NO-LOCK
           WHERE po-ordl.company EQ job-mch.company
             AND po-ordl.job-no EQ job-mch.job-no
             AND po-ordl.job-no2 EQ job-mch.job-no2
             AND po-ordl.b-num EQ job-mch.blank-no NO-ERROR.
      IF NOT AVAILABLE po-ordl THEN
      FIND FIRST po-ordl NO-LOCK
           WHERE po-ordl.company EQ job-mch.company
             AND po-ordl.job-no EQ job-mch.job-no
             AND po-ordl.job-no2 EQ job-mch.job-no2 NO-ERROR.
      IF AVAILABLE po-ordl THEN
      FIND FIRST po-ord NO-LOCK
           WHERE po-ord.company EQ po-ordl.company
             AND po-ord.po-no EQ po-ordl.po-no NO-ERROR.
    END. /* else avail oe-ordl */
    
    IF traceON THEN
    PUT UNFORMATTED 'Access ef and eb @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    IF ufEF THEN
    FIND FIRST ef NO-LOCK WHERE ef.company EQ job.company
                            AND ef.est-no EQ job.est-no
                            AND ef.form-no EQ job-mch.frm NO-ERROR.
    IF ufEB THEN
    FIND FIRST eb NO-LOCK WHERE eb.company EQ job.company
                            AND eb.est-no EQ job.est-no
                            AND eb.form-no EQ job-mch.frm
                            AND (eb.blank-no EQ job-mch.blank-no
                             OR job-mch.blank-no EQ 0) NO-ERROR.
    IF AVAILABLE eb THEN DO:
      FIND style NO-LOCK WHERE style.company EQ job.company
                           AND style.style EQ eb.style NO-ERROR.
      IF AVAILABLE style THEN DO:
        IF style.industry EQ '2' AND est.est-type LT 5 THEN dimFormat = '->>9.99'.
        RUN ipJobSet (job.company
                     ,job.est-no
                     ,style.dim-df
                     ,kFrac,decimalFormat
                     ,OUTPUT userField[65]
                     ,OUTPUT userField[66]
                     ,OUTPUT userField[67]
                     ,OUTPUT userField[68]
                      ).
      END. /* avail stype */
      
      FIND FIRST reftable NO-LOCK
           WHERE reftable.reftable EQ 'ce/v-est3.w Unit#'
             AND reftable.company EQ eb.company
             AND reftable.loc EQ eb.est-no
             AND reftable.code EQ STRING(eb.form-no,'9999999999')
             AND reftable.code2 EQ STRING(eb.blank-no,'9999999999') NO-ERROR.
      IF AVAILABLE reftable THEN
      DO i = 1 TO 10:
        IF reftable.val[i] GE 1 AND reftable.val[i] LE 10 THEN
        userField[INTEGER(reftable.val[i]) + 40] = setUserField(INTEGER(reftable.val[i]) + 40,eb.i-dscr2[i]).
        unitFound = YES.
      END. /* do i */
    END. /* avail eb */
    
    IF ufCust THEN DO:
      ASSIGN
        custNo = ''
        custName = ''
        salesRep = ''
        .
      FIND FIRST cust NO-LOCK
           WHERE cust.company EQ job-hdr.company
             AND cust.cust-no EQ job-hdr.cust-no
           NO-ERROR.
      IF AVAILABLE cust THEN
      ASSIGN
        custNo = cust.cust-no
        custName = cust.name
        salesRep = cust.sman
        .
    END. /* if ufcust */
    
    IF NOT salesRepFound THEN DO:
      IF useSalesRep THEN DO:
        i = LOOKUP(salesRep,customValueList) - 1.
        IF i GT 0 THEN jobStatus[i] = NO.
      END. /* if usesalesrep */
      userField[36] = setUserField(36,getSalesRep(job.company,salesRep)).
    END. /* not salesrepfound */

    IF ufItemFG THEN DO:
      RELEASE itemfg.
      IF job-mch.i-no NE '' THEN
      FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ job-mch.company
             AND itemfg.i-no EQ job-mch.i-no NO-ERROR.
      ELSE
      FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ job-hdr.company
             AND itemfg.i-no EQ job-hdr.i-no NO-ERROR.
    END. /* if ufitemfg */

    IF traceON THEN
    PUT UNFORMATTED 'Assign Fields @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    
    IF FIRST-OF(job-mch.frm) THEN
    itemDescription = IF job-mch.i-no NE '' THEN job-mch.i-no
                 ELSE IF CAN-FIND(FIRST bJobMch
                                  WHERE bJobMch.company EQ job-mch.company
                                    AND bJobMch.job EQ job-mch.job
                                    AND bJobMch.job-no EQ job-mch.job-no
                                    AND bJobMch.job-no2 EQ job-mch.job-no2
                                    AND bJobMch.frm EQ job-mch.frm
                                    AND bJobMch.i-no NE job-hdr.i-no
                                    AND bJobMch.i-no NE '') THEN '<Multi Item>'
                 ELSE job-hdr.i-no.
              /* ELSE getItemNo(job-mch.company,job-mch.job-no,job-mch.job-no2,job-mch.frm,job-hdr.i-no). */

    ASSIGN
      customVal = SUBSTR(customValueList,2)
      statusTimeStamp = ''
      lagTime = job-mch.lag-time
      liveUpdate = getLiveUpdate(job-mch.company,job-mch.job-no,job-mch.job-no2,
                                 job-mch.frm,job-mch.m-code)
      userField[1] = setUserField(1,custNo)
      userField[2] = setUserField(2,custName)
      userField[5] = setUserField(5,IF AVAILABLE eb THEN eb.die-no ELSE '')
      userField[6] = setUserField(6,IF AVAILABLE eb THEN eb.plate-no ELSE '')
      userField[7] = setUserField(7,IF AVAILABLE po-ordl THEN STRING(po-ordl.po-no,'>>>>>9') ELSE '')
      userField[8] = setUserField(8,IF AVAIL eb AND eb.est-type EQ 6 THEN eb.stock-no /* set */
                                    ELSE IF job-mch.i-no NE '' THEN job-mch.i-no
                                    ELSE itemDescription)
      userField[9] = setUserField(9,IF job-mch.i-name NE '' THEN job-mch.i-name
                               ELSE IF AVAILABLE itemfg AND itemfg.i-name NE '' THEN itemfg.i-name
                               ELSE IF userField[8] EQ '<Multi Item>' THEN '<Multiple Items>'
                               ELSE getItemName(job-mch.company,job-mch.job-no,job-mch.job,
                                                job-mch.job-no2,job-mch.frm,job-mch.blank-no))
      userField[10] = setUserField(10,IF AVAILABLE eb THEN STRING(convBase16(eb.len),dimFormat) ELSE '')
      userField[11] = setUserField(11,IF AVAILABLE eb THEN STRING(convBase16(eb.wid),dimFormat) ELSE '')
      userField[12] = setUserField(12,IF AVAILABLE eb THEN STRING(convBase16(eb.dep),dimFormat) ELSE '')
      userField[13] = setUserField(13,IF AVAILABLE eb THEN eb.style ELSE '')
      userField[15] = setUserField(15,IF job-mch.run-qty EQ ? THEN '' ELSE LEFT-TRIM(STRING(job-mch.run-qty,'zzz,zzz,zz9')))
      userField[16] = setUserField(16,IF AVAILABLE po-ordl THEN STRING(po-ordl.due-date,'99/99/9999') ELSE '')
      userField[17] = setUserField(17,IF AVAILABLE po-ord THEN po-ord.vend-no ELSE '')
      userField[18] = setUserField(18,STRING(job-mch.frm,'zz9'))
      userField[19] = setUserField(19,STRING(job-mch.blank-no,'>>>'))
      userField[20] = setUserField(20,STRING(job-mch.pass,'zz9'))
      userField[21] = setUserField(21,IF AVAILABLE itemfg THEN itemfg.procat ELSE '')
      userField[22] = setUserField(22,IF AVAILABLE ef THEN STRING(ef.cal,'9.99999') ELSE '')
      userField[23] = setUserField(23,IF AVAILABLE ef THEN STRING(ef.gsh-qty,'-zz,zzz,zz9') ELSE '')
      userField[24] = setUserField(24,IF AVAILABLE eb THEN eb.adhesive ELSE '')
      userField[25] = setUserField(25,IF AVAILABLE eb THEN STRING(eb.i-coat,'z9') ELSE '')
      userField[28] = setUserField(28,IF AVAILABLE eb THEN eb.i-coldscr ELSE '')
      userField[34] = setUserField(34,IF AVAILABLE itemfg THEN itemfg.cad-no ELSE '')
      userField[35] = setUserField(35,IF AVAILABLE eb AND eb.est-type EQ 6 THEN getSetPOQtyRec(job-mch.company,job-mch.job-no,job-mch.job-no2,eb.form-no,eb.stock-no)
                                 ELSE IF AVAILABLE po-ordl THEN STRING(po-ordl.t-rec-qty,'->,>>>,>>>,>>9.99<<<')
                                 ELSE '')
      userField[51] = setUserField(51,IF AVAILABLE eb THEN eb.tr-no ELSE '')
      userField[52] = setUserField(52,IF AVAILABLE itemfg THEN STRING(DECIMAL(userField[52]) * itemfg.t-sqft / 1000,'->,>>9.999') ELSE '')
      userField[53] = setUserField(53,IF AVAILABLE eb THEN STRING(eb.tab-in,'In/Out') ELSE '')
      runMSF = 0
      runMSF = job-mch.run-qty * itemfg.t-sqft / 1000 WHEN AVAIL itemfg AND job-mch.run-qty NE ?
      userField[54] = setUserField(54,IF runMSF LT 1000 THEN STRING(runMSF,'->>>,>>9.99999') ELSE '')
      userField[57] = ''
      userField[57] = setUserField(57,prodQty(job-mch.company,job-mch.m-code,job-mch.job-no,
                                              job-mch.job-no2,job-mch.frm,job-mch.blank-no,
                                              job-mch.pass,prodQtyProgram)) WHEN prodQtyProgram NE ?
      userField[58] = setUserField(58,IF AVAILABLE ef THEN STRING(ef.gsh-len,'>>9.9999') ELSE '')
      userField[59] = setUserField(59,IF AVAILABLE ef THEN STRING(ef.gsh-wid,'>>9.9999') ELSE '')
      userField[60] = setUserField(60,IF AVAILABLE eb THEN eb.cas-no ELSE '')
      userField[64] = setUserField(64,IF AVAILABLE itemfg THEN itemfg.part-no ELSE '')
      userField[83] = setUserField(83,job.stat)
      userField[85] = setUserField(85,fDueQty(INT(userField[15]),INT(userField[57]),DEC(userField[86]),DEC(userField[87])))
      jobDescription = jobText
      .
    IF AVAILABLE itemfg AND NOT job-mch.run-qty * itemfg.t-sqft / 1000 LT 1000000 THEN
    MESSAGE 'Job:' jobNumber SKIP
            'Resource:' scheduleResource SKIP
            'Run Qty:' job-mch.run-qty SKIP
            'FG Item:' itemfg.i-no SKIP
            'FG Total SqFt:' itemfg.t-sqft SKIP
            'Run MSF:' job-mch.run-qty * itemfg.t-sqft / 1000
        VIEW-AS ALERT-BOX TITLE 'Run MSF Error'.

    RUN ipJobMaterial (job-mch.company,
                       job-mch.job,
                       job-mch.job-no,
                       job-mch.job-no2,
                       job-mch.frm,
                       job-mch.blank-no,
                       job-mch.i-no,
                       OUTPUT boardLength,
                       OUTPUT boardWidth,
                       OUTPUT jobBoard,
                       OUTPUT userField[3],  /* B */
                       OUTPUT userField[14], /* I */
                       OUTPUT userField[55], /* D */
                       OUTPUT userField[56], /* D */
                       OUTPUT userField[61], /* 5 */
                       OUTPUT userField[62], /* 6 */
                       OUTPUT userField[70], /* V */
                       OUTPUT userField[71], /* A */
                       OUTPUT userField[78], /* C */
                       OUTPUT userField[79], /* C */
                       OUTPUT userField[81]) /* W */
                       .

    RUN ipJobMatField (job-mch.company,
                       job-mch.j-no,
                       job-mch.i-no,
                       job-mch.frm,
                       job-mch.blank-no,
                       '>>,>>>,>>9.99<',
                       '>>9.99<<',
                       '>>>9',
                       OUTPUT userField[29],
                       OUTPUT userField[30],
                       OUTPUT userField[31]).
    ASSIGN
      userField[32] = setUserField(32,STRING(DECIMAL(userField[29]) * (DECIMAL(userField[30]) / 12),'>>,>>>,>>9'))
      userField[33] = setUserField(33,STRING(DECIMAL(userField[29]) * DECIMAL(userField[31]),'>>,>>>,>>9.99<'))
      .

    IF useField[71] THEN
    DO i = 1 TO NUM-ENTRIES(userField[71]):
      userField[71 + i] = ENTRY(i,userField[71]).
    END.
    ELSE userField[71] = ''.

    IF ufPrep AND userField[5] NE '' THEN DO:
      FIND FIRST prep NO-LOCK
           WHERE prep.company EQ job.company
             AND prep.code EQ userField[5]
           NO-ERROR.
      userField[80] = IF AVAILABLE prep THEN prep.loc-bin ELSE ''.
    END. /* if ufprep */

    ASSIGN
      userField[26] = IF est.est-type GE 5 THEN STRING(boardLength,dimFormat)
                      ELSE STRING(convBase16(boardLength),dimFormat)
      userField[27] = IF est.est-type GE 5 THEN STRING(boardWidth,dimFormat)
                      ELSE STRING(convBase16(boardWidth),dimFormat)
      userField[4] = boardName(ENTRY(1,userField[3])).
    IF userField[3] EQ '' THEN
    userField[3] = IF AVAILABLE ef THEN ef.board ELSE ''.
    IF AVAILABLE eb THEN DO:
      IF userField[14] EQ '' THEN
      DO i = 1 TO EXTENT(eb.i-code):
        IF eb.i-code[i] NE '' AND NOT CAN-DO(userField[14],eb.i-code[i]) THEN
        userField[14] = userField[14] + comma(userField[14]) + eb.i-code[i].
      END. /* do i */
      IF NOT unitFound THEN /* ink units not used, load in order found */
      DO i = 1 TO 10:
        userField[i + 40] = eb.i-dscr[i].
      END. /* do i */
    END. /* avail eb */

    IF useNotes THEN DO:
      IF traceON THEN
      PUT UNFORMATTED 'Job Notes @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
      lvCode = scheduleResource + ',' + STRING(job-mch.job) + ',' + job-mch.job-no + ',' + STRING(job-mch.job-no2) + ',' + STRING(job-mch.frm).
      IF CAN-FIND(FIRST reftable
                  WHERE reftable.reftable EQ 'SB: Note'
                    AND reftable.company EQ job-mch.company
                    AND reftable.loc EQ ''
                    AND reftable.code EQ lvCode
                    AND reftable.dscr NE '') THEN
      FOR EACH reftable NO-LOCK
          WHERE reftable.reftable BEGINS 'SB: Note'
            AND reftable.company EQ job-mch.company
            AND reftable.loc EQ ''
            AND reftable.code EQ lvCode
            AND reftable.dscr NE '':
        IF traceON THEN
        PUT UNFORMATTED reftable.dscr AT 20 SKIP.
        lvNoteKey = job-mch.company + ',' + lvCode.
        {{&exports}/jobNotes.i &streamName=sJobNotes
          &jobRowID=ENTRY(2,strRowID)
          &noteDate=ENTRY(1,reftable.code2)
          &noteTime=ENTRY(2,reftable.code2)
          &noteText=reftable.dscr
          &noteKey=lvNoteKey}
      END. /* each reftable */
    END. /* usenotes */
    
    IF useStatus THEN DO:
      IF traceON THEN
      PUT UNFORMATTED 'Job Status @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
      FIND FIRST reftable NO-LOCK
           WHERE reftable.reftable EQ 'SB: Status'
             AND reftable.company EQ job-mch.company
             AND reftable.loc EQ ''
             AND reftable.code EQ lvCode
             AND reftable.dscr NE '' NO-ERROR.
      IF AVAILABLE reftable THEN DO:
        IF traceON THEN
        PUT UNFORMATTED reftable.dscr AT 20 SKIP.
        IF NOT useSalesRep THEN
        DO i = 2 TO NUM-ENTRIES(customValueList):
          IF ENTRY(i,customValueList) EQ 'H' THEN DO:
            jobStatus[i - 1] = job.stat NE 'H'.
            NEXT.
          END. /* check for hold status */
          jobStatus[i - 1] = NOT CAN-DO(reftable.dscr,ENTRY(i,customValueList)).
          IF jobStatus[i - 1] THEN NEXT. /* if already yes, leave it alone */
          FIND FIRST statusCheckOffs
               WHERE statusCheckOffs.statusCheckOffs EQ ENTRY(i,customValueList)
               NO-ERROR.
          IF AVAILABLE statusCheckOffs THEN
          jobStatus[i - 1] = statusCheckOff(job-mch.company,job-mch.job,
                                            job-mch.job-no,job-mch.job-no2,
                                            job-mch.frm,statusCheckOffs.materialType).
        END. /* not usesalesrep */
      END. /* if avail reftable */
      ELSE IF NOT useSalesRep THEN
      ASSIGN
        jobStatus[LOOKUP('Board',customValueList) - 1] = jobBoard
                  WHEN CAN-DO(customValueList,'Board')
        jobStatus[LOOKUP('Hold',customValueList) - 1] = job.stat NE 'H'
                  WHEN CAN-DO(customValueList,'Hold')
        .
    END. /* useStatus */

    /* make sure data is clean, unknown values unacceptable */
    DO i = 1 TO EXTENT(jobStatus):
      IF jobStatus[i] EQ ? THEN jobStatus[i] = NO.
    END. /* do i */
    DO i = 1 TO EXTENT(userField):
      IF userField[i] EQ ? THEN userField[i] = ''.
    END. /* do i */

    IF prodDate EQ ? THEN prodDate = dueDate.

    IF traceON THEN
    PUT UNFORMATTED 'Export Data @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    IF startDate NE ? THEN
    {{&exports}/job.i &streamName=sScenario
      &resource=scheduleResource
      &resourceSequence=resSeq
      &altResSeq=altResSeq
      &resourceDescription=resourceDescription
      &altResource=job-mch.m-code
      &department=department
      &job=jobNumber
      &jobSort=jobSort
      &jobDescription=jobDescription
      &jobToolTip=jobToolTip
      &jobSequence=job-mch.seq-no
      &startDate=startDate
      &startTime=startTime
      &endDate=endDate
      &endTime=endTime
      &timeSpan=timeSpan
      &jobLocked=job-mch.anchored
      &dueDate=dueDate
      &prodDate=prodDate
      &customValue=customVal
      &strRowID=strRowID
      &keyValue=keyValues}
    ELSE
    {{&exports}/job.i &streamName=sPending
      &resource=scheduleResource
      &resourceSequence=resSeq
      &altResSeq=altResSeq
      &resourceDescription=resourceDescription
      &altResource=job-mch.m-code
      &department=department
      &job=jobNumber
      &jobSort=jobSort
      &jobDescription=jobDescription
      &jobToolTip=jobToolTip
      &jobSequence=job-mch.seq-no
      &startTime=startTime
      &timeSpan=timeSpan
      &jobLocked=NO
      &dueDate=dueDate
      &prodDate=prodDate
      &customValue=customVal
      &strRowID=strRowID
      &keyValue=keyValues}
  END. /* each job-mch */
END. /* each job-hdr */

PROCEDURE ipJobSet:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDimDF AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipKFrac AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipDecimalFormat AS INTEGER NO-UNDO.

  DEFINE OUTPUT PARAMETER opInternalLength AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opEndCellLength AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInternalWidth AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opEndCellWidth AS CHARACTER NO-UNDO.

  DEFINE BUFFER bEB1 FOR eb.
  DEFINE BUFFER bEB2 FOR eb.
  
  IF NOT ufIPJobSet THEN RETURN.

  FIND FIRST bEB1 NO-LOCK
       WHERE bEB1.company EQ ipCompany
         AND bEB1.est-no  EQ ipEstNo
         AND bEB1.form-no NE 0
         AND bEB1.blank-no NE 0
       USE-INDEX est-qty NO-ERROR.
  
  IF AVAIL bEB1 THEN DO:
    ASSIGN
      opInternalLength = setUserField(65,k16(bEB1.k-len-array2[2],ipKFrac,ipDecimalFormat))
      opEndCellLength = setUserField(66,k16(bEB1.k-len-array2[ipDimDF + 1],ipKFrac,ipDecimalFormat)).
    FIND FIRST bEB2 NO-LOCK
         WHERE bEB2.company EQ ipCompany
           AND bEB2.est-no  EQ ipEstNo
           AND bEB2.form-no NE 0
           AND bEB2.blank-no NE 0
           AND ROWID(bEB2) NE ROWID(bEB1)
         USE-INDEX est-qty NO-ERROR.
    IF AVAIL bEB2 THEN
    ASSIGN
      opInternalWidth = setUserField(67,k16(bEB2.k-len-array2[2],ipKFrac,ipDecimalFormat))
      opEndCellWidth = setUserField(68,k16(bEB2.k-len-array2[ipDimDF + 1],ipKFrac,ipDecimalFormat)).
  END. /* if avail bEB1 */
END PROCEDURE.

PROCEDURE ipJobMaterial:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipForm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipBlankNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipItemNo AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opBoardLength AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opBoardWidth AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opJobBoard AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opBoard AS CHARACTER NO-UNDO.     /*  3,B */
  DEFINE OUTPUT PARAMETER opInk AS CHARACTER NO-UNDO.       /* 14,I */
  DEFINE OUTPUT PARAMETER opPallet AS CHARACTER NO-UNDO.    /* 55,D */
  DEFINE OUTPUT PARAMETER opTotMRP AS CHARACTER NO-UNDO.    /* 56,D */
  DEFINE OUTPUT PARAMETER opMatType5 AS CHARACTER NO-UNDO.  /* 61,5 */
  DEFINE OUTPUT PARAMETER opMatType6 AS CHARACTER NO-UNDO.  /* 62,6 */
  DEFINE OUTPUT PARAMETER opVarnish AS CHARACTER NO-UNDO.   /* 70,V */
  DEFINE OUTPUT PARAMETER opAdders AS CHARACTER NO-UNDO.    /* 71,A */
  DEFINE OUTPUT PARAMETER opNoCases AS CHARACTER NO-UNDO.   /* 78,C */
  DEFINE OUTPUT PARAMETER opCasesName AS CHARACTER NO-UNDO. /* 79,C */
  DEFINE OUTPUT PARAMETER opFilmName AS CHARACTER NO-UNDO.  /* 81,W */

  DEFINE VARIABLE noCases AS DECIMAL NO-UNDO.
  
  IF NOT ufIPJobMaterial THEN RETURN.

  IF traceON THEN
  PUT UNFORMATTED 'Procedure ipJobMaterial @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME.
  
  FOR EACH job-mat NO-LOCK
      WHERE job-mat.company eq ipCompany
        AND job-mat.job EQ ipJob
        AND job-mat.job-no EQ ipJobNo
        AND job-mat.job-no2 EQ ipJobNo2
        AND job-mat.frm EQ ipForm:
    FOR EACH item OF job-mat NO-LOCK:
      CASE item.mat-type:
        WHEN '5' THEN
          IF NOT CAN-DO(opMatType5,job-mat.i-no) THEN
          opMatType5 = opMatType5 + comma(opMatType5) + job-mat.i-no.
        WHEN '6' THEN
          IF NOT CAN-DO(opMatType6,job-mat.i-no) THEN
          opMatType6 = opMatType6 + comma(opMatType6) + job-mat.i-no.
        WHEN 'A' THEN
          IF NOT CAN-DO(opAdders,job-mat.i-no) THEN
          opAdders = opAdders + comma(opAdders) + job-mat.i-no.
        WHEN 'B' THEN DO:
          IF NOT opJobBoard THEN
          ASSIGN
            opBoardLength = job-mat.len
            opBoardWidth = job-mat.wid
            opJobBoard = CAN-FIND(FIRST mat-act
                                  WHERE mat-act.company EQ job-mat.company
                                    AND mat-act.job EQ job-mat.job
                                    AND mat-act.job-no EQ job-mat.job-no
                                    AND mat-act.job-no2 EQ job-mat.job-no2
                                    AND mat-act.i-no EQ job-mat.i-no
                                    AND mat-act.s-num EQ job-mat.frm
                                    AND mat-act.b-num EQ job-mat.blank-no USE-INDEX job).
          IF NOT CAN-DO(opBoard,job-mat.i-no) THEN
          opBoard = opBoard + comma(opBoard) + job-mat.i-no.
        END. /* B */
        WHEN 'C' THEN
          IF ipBlankNo EQ job-mat.blank-no THEN
          ASSIGN
            noCases = noCases + job-mat.qty
            opCasesName = item.i-name.
        WHEN 'D' THEN
          IF opPallet EQ '' AND opTotMRP EQ '' THEN
          ASSIGN
            opPallet = item.i-name
            opTotMRP = STRING(job-mat.qty,'>,>>>,>>9.9<<<<<').
        WHEN 'I' THEN
          IF NOT CAN-DO(opInk,job-mat.i-no) THEN
          opInk = opInk + comma(opInk) + job-mat.i-no.
        WHEN 'V' THEN
          IF NOT CAN-DO(opVarnish,job-mat.i-no) THEN
          opVarnish = opVarnish + comma(opVarnish) + job-mat.i-no.
        WHEN 'W' THEN
        opFilmName = REPLACE(item.i-name,'"','').
      END CASE.
    END. /* each item */
  END. /* each job-mat */
  opNoCases = STRING(noCases,'>,>>>,>>9.9<<<<<').
  IF traceON THEN
  PUT UNFORMATTED
    '5 ' AT 20 opMatType5
    '6 ' AT 20 opMatType6
    'A ' AT 20 opAdders
    'B ' AT 20 opBoard
    'C ' AT 20 opNoCases ' ' opCasesName
    'D ' AT 20 opPallet ' ' opTotMRP
    'I ' AT 20 opInk
    'V ' AT 20 opVarnish
    'W ' AT 20 opFilmName
    'Time: ' AT 20 ETIME
    SKIP.
END PROCEDURE.

PROCEDURE ipJobMatField:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipINo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipForm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipBlank AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipFormat29 AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFormat30 AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFormat31 AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opUserField29 AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opUserField30 AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opUserField31 AS CHARACTER NO-UNDO.

  IF NOT ufIPJobMatField THEN RETURN ''.

  IF traceON THEN
  PUT UNFORMATTED 'Function ipJobMatField @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
  
  FIND FIRST job-mat NO-LOCK WHERE job-mat.company eq ipCompany
                               AND job-mat.j-no EQ ipJNo
                               AND job-mat.i-no EQ ipINo
                               AND job-mat.frm EQ ipForm
                               AND job-mat.blank-no EQ ipBlank
                               AND job-mat.qty GT 0 NO-ERROR.
  IF AVAILABLE job-mat THEN
  ASSIGN
    opUserField29 = STRING(job-mat.qty,ipFormat29)
    opUserField30 = STRING(job-mat.len,ipFormat30)
    opUserField31 = STRING(job-mat.n-up,ipFormat31)
    .
END PROCEDURE.

{{&loads}/loadProEnd.i}

{{&includes}/{&Board}/calcEnd.i}

&ELSE
RUN loadUserFieldLabelWidth.
&ENDIF

PROCEDURE customValueList:
  DEFINE OUTPUT PARAMETER opCustomValueList AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opCustomLabelList AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE valueList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE labelList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE printPrgm AS CHARACTER NO-UNDO.
  DEFINE VARIABLE printPrgmDat AS CHARACTER NO-UNDO.

  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT ^.
  IMPORT ^.
  IMPORT ^.
  REPEAT:
    IMPORT ^ ^ labelList valueList.
    IF NOT useSalesRep OR valueList NE '' THEN
    opCustomValueList = opCustomValueList + ',' + valueList.
    opCustomLabelList = opCustomLabelList + ',' + labelList.
  END.
  INPUT CLOSE.
  opCustomLabelList = LEFT-TRIM(opCustomLabelList,',').
END PROCEDURE.

PROCEDURE colCheck:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE notUsed AS CHARACTER NO-UNDO INITIAL 'Not Used'.

  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Seq' colCheck.fld2 = 'jobSequence'
         colCheck.fld3 = 'Seq' colCheck.fld4 = 'jobSequence'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Job ID' colCheck.fld2 = 'job'
         colCheck.fld3 = 'Job ID' colCheck.fld4 = 'job'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Resource' colCheck.fld2 = 'resource'
         colCheck.fld3 = 'Resource' colCheck.fld4 = 'resource'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'ResSeq' colCheck.fld2 = 'resourceSequence'
         colCheck.fld3 = 'ResSeq' colCheck.fld4 = 'resourceSequence'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'C' colCheck.fld2 = 'jobCompleted'
         colCheck.fld3 = 'C' colCheck.fld4 = 'jobCompleted'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Alt Res' colCheck.fld2 = 'altResource'
         colCheck.fld3 = 'Alt Res' colCheck.fld4 = 'altResource'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Start Date' colCheck.fld2 = 'startDate'
         colCheck.fld3 = 'Start Date' colCheck.fld4 = 'startDate'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Start Time' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'Start Time' colCheck.fld4 = 'calcTimeField'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'End Date' colCheck.fld2 = 'endDate'
         colCheck.fld3 = 'End Date' colCheck.fld4 = 'endDate'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'End Time' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'End Time' colCheck.fld4 = 'calcTimeField'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Due Date' colCheck.fld2 = 'dueDate'
         colCheck.fld3 = 'Due Date' colCheck.fld4 = 'dueDate'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Due Time' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'Due Time' colCheck.fld4 = 'calcTimeField'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Prod Date' colCheck.fld2 = 'prodDate'
         colCheck.fld3 = 'Prod Date' colCheck.fld4 = 'prodDate'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Orig Start' colCheck.fld2 = 'origStartDate'
         colCheck.fld3 = 'Orig Start' colCheck.fld4 = 'origStartDate'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Orig Start' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'Orig Start' colCheck.fld4 = 'calcTimeField'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Orig End' colCheck.fld2 = 'origEndDate'
         colCheck.fld3 = 'Orig End' colCheck.fld4 = 'origEndDate'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Orig End' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'Orig End' colCheck.fld4 = 'calcTimeField'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[1] colCheck.fld2 = 'userField01'
         colCheck.fld3 = userLabel[1] colCheck.fld4 = 'userField01'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[2] colCheck.fld2 = 'userField02'
         colCheck.fld3 = userLabel[2] colCheck.fld4 = 'userField02'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[3] colCheck.fld2 = 'userField03'
         colCheck.fld3 = userLabel[3] colCheck.fld4 = 'userField03'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField14'
         colCheck.fld3 = userLabel[4] colCheck.fld4 = 'userField04'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[5] colCheck.fld2 = 'userField04'
         colCheck.fld3 = userLabel[5] colCheck.fld4 = 'userField05'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[6] colCheck.fld2 = 'userField05'
         colCheck.fld3 = userLabel[6] colCheck.fld4 = 'userField06'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[7] colCheck.fld2 = 'userField06'
         colCheck.fld3 = userLabel[7] colCheck.fld4 = 'userField07'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[8] colCheck.fld2 = 'userField07'
         colCheck.fld3 = userLabel[8] colCheck.fld4 = 'userField08'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField13'
         colCheck.fld3 = userLabel[9] colCheck.fld4 = 'userField09'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[9] colCheck.fld2 = 'userField08'
         colCheck.fld3 = userLabel[9] colCheck.fld4 = 'userField09'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[10] colCheck.fld2 = 'userField08'
         colCheck.fld3 = userLabel[10] colCheck.fld4 = 'userField10'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[10] colCheck.fld2 = 'userField09'
         colCheck.fld3 = userLabel[10] colCheck.fld4 = 'userField10'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[11] colCheck.fld2 = 'userField09'
         colCheck.fld3 = userLabel[11] colCheck.fld4 = 'userField11'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[11] colCheck.fld2 = 'userField10'
         colCheck.fld3 = userLabel[11] colCheck.fld4 = 'userField11'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[12] colCheck.fld2 = 'userField10'
         colCheck.fld3 = userLabel[12] colCheck.fld4 = 'userField12'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[12] colCheck.fld2 = 'userField11'
         colCheck.fld3 = userLabel[12] colCheck.fld4 = 'userField12'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[13] colCheck.fld2 = 'userField11'
         colCheck.fld3 = userLabel[13] colCheck.fld4 = 'userField13'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[13] colCheck.fld2 = 'userField12'
         colCheck.fld3 = userLabel[13] colCheck.fld4 = 'userField13'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[14] colCheck.fld2 = 'userField12'
         colCheck.fld3 = userLabel[14] colCheck.fld4 = 'userField14'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[14] colCheck.fld2 = 'userField13'
         colCheck.fld3 = userLabel[14] colCheck.fld4 = 'userField14'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[15] colCheck.fld2 = 'extraField01'
         colCheck.fld3 = userLabel[15] colCheck.fld4 = 'userField15'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[16] colCheck.fld2 = 'extraField02'
         colCheck.fld3 = userLabel[16] colCheck.fld4 = 'userField16'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = userLabel[17] colCheck.fld2 = 'extraField03'
         colCheck.fld3 = userLabel[17] colCheck.fld4 = 'userField17'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'User Value' colCheck.fld2 = 'userValue'
         colCheck.fld3 = 'User Value' colCheck.fld4 = 'userValue'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Status' colCheck.fld2 = 'status'
         colCheck.fld3 = 'Status' colCheck.fld4 = 'status'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Job Time' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'Job Time' colCheck.fld4 = 'calcTimeField'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Downtime' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'Downtime' colCheck.fld4 = 'calcTimeField'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Total Time' colCheck.fld2 = 'calcTimeField'
         colCheck.fld3 = 'Total Time' colCheck.fld4 = 'calcTimeField'.
  DO i = 18 TO {&nextUserField} - 1:
    CREATE colCheck.
    ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField' + STRING(i,'99')
           colCheck.fld3 = userLabel[i] colCheck.fld4 = 'userField' + STRING(i,'99').
  END. /* do i */
  DO i = {&nextUserField} TO {&userExtent}:
    CREATE colCheck.
    ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField' + STRING(i,'99')
           colCheck.fld3 = notUsed colCheck.fld4 = 'userField' + STRING(i,'99').
  END. /* do i */
  /* added 6.20.2006 to ttblJob */
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Live' colCheck.fld2 = 'liveUpdate'
         colCheck.fld3 = 'Live' colCheck.fld4 = 'liveUpdate'.
  /* added 11.30.2006 to ttblJob */
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Lag Time' colCheck.fld2 = 'lagTime'
         colCheck.fld3 = 'Lag Time' colCheck.fld4 = 'lagTime'.
END PROCEDURE.

PROCEDURE moveFontValue:
  DEFINE VARIABLE fontValue AS CHARACTER NO-UNDO.

  fontValue = SEARCH('{&print}/' + ID + '/fontValue.dat').
  IF fontValue EQ ? THEN RETURN.
  OS-RENAME VALUE(fontValue) VALUE('{&data}/' + ID + '/fontValue.dat').
END PROCEDURE.

PROCEDURE moveNotes:
  DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvCode2 AS CHARACTER NO-UNDO.

  FOR EACH notes EXCLUSIVE-LOCK WHERE notes.note_title CONTAINS 'SB:':
    FIND FIRST job-mch NO-LOCK WHERE job-mch.rec_key EQ notes.rec_key NO-ERROR.
    IF AVAILABLE job-mch THEN DO:
      ASSIGN
        lvCode = job-mch.m-code + ',' + STRING(job-mch.job) + ',' + job-mch.job-no + ',' + STRING(job-mch.job-no2) + ',' + STRING(job-mch.frm)
        lvCode2 = STRING(notes.note_date) + ',' + STRING(notes.note_time).
      IF NOT CAN-FIND(FIRST reftable NO-LOCK
                      WHERE reftable.reftable EQ notes.note_title
                        AND reftable.company EQ job-mch.company
                        AND reftable.loc EQ ''
                        AND reftable.code EQ lvCode
                        AND reftable.code2 EQ lvCode2) THEN DO:
        CREATE reftable.
        ASSIGN
          reftable.reftable = notes.note_title
          reftable.company = job-mch.company
          reftable.loc = ''
          reftable.code = lvCode
          reftable.code2 = lvCode2
          reftable.dscr = notes.note_text.
      END. /* if not can-find */
    END. /* avail job-mch */
    DELETE notes.
  END. /* each notes */
END PROCEDURE.

PROCEDURE fixSBRefTable:
  DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER bRefTable FOR reftable.
  
  IF NOT CAN-FIND(FIRST reftable WHERE reftable.reftable BEGINS 'SB: Job') THEN
  RETURN.

  OUTPUT TO VALUE('{&data}/' + ID + '/reftable.d').
  FOR EACH reftable EXCLUSIVE-LOCK WHERE reftable.reftable BEGINS 'SB: Job':
    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company EQ reftable.company
          AND job-mch.m-code EQ ENTRY(1,reftable.code)
          AND job-mch.job EQ INTEGER(ENTRY(2,reftable.code))
          AND job-mch.job-no EQ ENTRY(3,reftable.code)
          AND job-mch.job-no2 EQ INTEGER(ENTRY(4,reftable.code))
          AND job-mch.run-complete EQ NO,
        FIRST job-hdr NO-LOCK       
        WHERE job-hdr.company EQ job-mch.company
          AND job-hdr.job EQ job-mch.job
          AND job-hdr.job-no EQ job-mch.job-no
          AND job-hdr.job-no2 EQ job-mch.job-no2
          AND job-hdr.frm EQ job-mch.frm
          AND job-hdr.opened EQ YES
      BREAK BY job-mch.frm:
      IF NOT FIRST(job-mch.frm) THEN NEXT.
      lvCode = reftable.code + ',' + STRING(job-mch.frm).
      IF CAN-FIND(FIRST bRefTable WHERE bRefTable.reftable BEGINS 'SB: Job'
                  AND bRefTable.company EQ reftable.company
                  AND bRefTable.loc EQ ''
                  AND bRefTable.code EQ lvCode) THEN NEXT.
      CREATE bRefTable.
      BUFFER-COPY reftable EXCEPT reftable code TO bRefTable
        ASSIGN
          bRefTable.reftable = REPLACE(reftable.reftable,'SB: Job','SB:')
          bRefTable.code = lvCode.
    END. /* each job-mch */
    EXPORT reftable.
    DELETE reftable.
  END. /* each reftable */
  OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE loadUserFieldLabelWidth:
  ASSIGN
    cascadeJob = SEARCH(findProgram('{&data}/',ID,'/noCascade.dat')) EQ ?
    changeResource = YES
    loginID = USERID('NoSweat') + '.' + STRING(TODAY,'999999') + '.' + STRING(TIME,'99999')
    statusObject[1] = ',Ready/Pending'
    statusObject[2] = 'Machine MR,Completed/Pending'
    statusObject[3] = 'Machine Run,Completed/Pending'
    /* calc width by multiplying length by 1.5 */
    /* add userField to rptFields.dat */
    userLabel[1] = 'Customer'         userWidth[1] = 15
    userLabel[2] = 'Name'             userWidth[2] = 45
    userLabel[3] = 'Board'            userWidth[3] = 15
    userLabel[4] = 'Board Name'       userWidth[4] = 45
    userLabel[5] = 'Die'              userWidth[5] = 23
    userLabel[6] = 'Plate'            userWidth[6] = 23
    userLabel[7] = 'PO'               userWidth[7] = 9
    userLabel[8] = 'FG Item'          userWidth[8] = 23
    userLabel[9] = 'Item Name'        userWidth[9] = 45
    userLabel[10] = 'Length'          userWidth[10] = 12
    userLabel[11] = 'Width'           userWidth[11] = 12
    userLabel[12] = 'Depth'           userWidth[12] = 12
    userLabel[13] = 'Style'           userWidth[13] = 9
    userLabel[14] = 'Color'           userWidth[14] = 100
    userLabel[15] = 'Run Qty'         userWidth[15] = 12
    userLabel[16] = 'PO Due'          userWidth[16] = 15
    userLabel[17] = 'Vendor'          userWidth[17] = 12
    userLabel[18] = 'Form'            userWidth[18] = 5
    userLabel[19] = 'Blank'           userWidth[19] = 6
    userLabel[20] = 'Pass'            userWidth[20] = 5
    userLabel[21] = 'Category'        userWidth[21] = 9
    userLabel[22] = 'Caliper'         userWidth[22] = 8
    userLabel[23] = 'Sheet Qty'       userWidth[23] = 12
    userLabel[24] = 'Adhesive'        userWidth[24] = 12
    userLabel[25] = 'Coats'           userWidth[25] = 6
    userLabel[26] = 'Board L.'        userWidth[26] = 12
    userLabel[27] = 'Board W.'        userWidth[27] = 12
    userLabel[28] = 'Color Desc'      userWidth[28] = 60
    userLabel[29] = 'Total MRP'       userWidth[29] = 13
    userLabel[30] = 'Mat Length'      userWidth[30] = 12
    userLabel[31] = '#Up'             userWidth[31] = 5
    userLabel[32] = 'Linear Feet'     userWidth[32] = 13
    userLabel[33] = 'Total Pieces'    userWidth[33] = 13
    userLabel[34] = 'CAD'             userWidth[34] = 12
    userLabel[35] = 'PO Qty Recd'     userWidth[35] = 20
    userLabel[36] = 'Sales Rep'       userWidth[36] = 30
    userLabel[37] = 'Release Qty'     userWidth[37] = 12
    userLabel[38] = 'Release Date'    userWidth[38] = 15
    userLabel[39] = 'Ship To Addr'    userWidth[39] = 30
    userLabel[40] = 'Ship To City'    userWidth[40] = 30
    .
  DO idx = 41 TO 50:
    ASSIGN
      userLabel[idx] = 'Ink-' + STRING(idx - 40)
      userWidth[idx] = 30.
  END. /* do idx */

  ASSIGN  
    userLabel[51] = 'Pallet/Unit'     userWidth[51] = 12
    userLabel[52] = 'Rel MSF'         userWidth[52] = 9
    userLabel[53] = 'Tab'             userWidth[53] = 5
    userLabel[54] = 'Run MSF'         userWidth[54] = 9
    userLabel[55] = 'Pallet Name'     userWidth[55] = 45
    userLabel[56] = 'Pallet MRP'      userWidth[56] = 13
    userLabel[57] = 'Prod Qty'        userWidth[57] = 12
    userLabel[58] = 'Gross Sht L.'    userWidth[58] = 12
    userLabel[59] = 'Gross Sht W.'    userWidth[59] = 12
    userLabel[60] = 'Pack Code'       userWidth[60] = 15
    userLabel[61] = 'Mat Type5 Item#' userWidth[61] = 20
    userLabel[62] = 'Mat Type6 Item#' userWidth[62] = 20
    userLabel[63] = 'Cust PO#'        userWidth[63] = 20
    userLabel[64] = 'Cust Part#'      userWidth[64] = 15
    userLabel[65] = 'Internal L.'     userWidth[65] = 12
    userLabel[66] = 'End Cell L.'     userWidth[66] = 12
    userLabel[67] = 'Internal W.'     userWidth[67] = 12
    userLabel[68] = 'End Cell W.'     userWidth[68] = 12
    userLabel[69] = 'Current Qty'     userWidth[69] = 12
    userLabel[70] = 'Varnish'         userWidth[70] = 100
    userLabel[71] = 'Adders'          userWidth[71] = 100
    userLabel[72] = 'Adder 1'         userWidth[72] = 20
    userLabel[73] = 'Adder 2'         userWidth[73] = 20
    userLabel[74] = 'Adder 3'         userWidth[74] = 20
    userLabel[75] = 'Adder 4'         userWidth[75] = 20
    userLabel[76] = 'Adder 5'         userWidth[76] = 20
    userLabel[77] = 'Adder 6'         userWidth[77] = 20
    userLabel[78] = 'No. of Cases'    userWidth[78] = 12
    userLabel[79] = 'Cases Name'      userWidth[79] = 45
    userLabel[80] = 'Die Bin'         userWidth[80] = 15
    userLabel[81] = 'Film Name'       userWidth[81] = 45
    userLabel[82] = 'Mfg Date'        userWidth[82] = 15
    userLabel[83] = 'Job Status'      userWidth[83] = 12
    userLabel[84] = 'Total Price'     userWidth[84] = 20
    userLabel[85] = 'Due Qty'         userWidth[85] = 12
    .
  /* add userField to rptFields.dat, see config.w definitions section
     to enable field */
END PROCEDURE.

PROCEDURE setUseFields:
  DEFINE VARIABLE fldName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE useFieldsFile AS CHARACTER NO-UNDO.
/*
  DEFINE VARIABLE licenseError AS LOGICAL NO-UNDO.
  
  licenseError = NOT CAN-FIND(FIRST module
                              WHERE module.db-name EQ 'ASI'
                                AND module.module EQ 'sbFields')
                  OR CAN-FIND(FIRST module
                              WHERE module.db-name EQ 'ASI'
                                AND module.module EQ 'sbFields'
                                AND (NOT module.is-used
                                 OR module.expire-date LT TODAY)).
  IF licenseError THEN DO:
    ASSIGN
      useNotes = YES
      useStatus = YES
      useField = YES.
    RETURN.
  END. /* if licenseerror */
*/
  useFieldsFile = SEARCH('{&data}/' + ID + '/userFields.dat').
  IF useFieldsFile NE ? THEN DO:
    INPUT FROM VALUE(useFieldsFile) NO-ECHO.
    REPEAT:
      IMPORT fldName.
      IF fldName EQ 'useNotes' THEN useNotes = YES.
      ELSE IF fldName EQ 'useStatus' THEN useStatus = YES.
      ELSE
      ASSIGN
        idx = INT(REPLACE(fldName,'userField',''))
        useField[idx] = YES.
    END. /* repeat */
    INPUT CLOSE.
  END. /* if usefieldsfile */
  ELSE
  ASSIGN
    useNotes = YES
    useStatus = YES
    useField = YES.

  ASSIGN
    ufCust = useField[1] OR useField[2]
    ufBoardName = useField[4]
    ufEB = useField[5]  OR useField[6]  OR useField[8]  OR useField[10] OR useField[11] OR useField[12] OR
           useField[13] OR useField[14] OR useField[24] OR useField[25] OR useField[28] OR useField[35] OR
           useField[41] OR useField[42] OR useField[43] OR useField[44] OR useField[45] OR useField[46] OR
           useField[47] OR useField[48] OR useField[49] OR useField[50] OR useField[51] OR useField[53] OR
           useField[60] OR useField[65] OR useField[66] OR useField[67] OR useField[68]
    ufEF = useField[3] OR useField[22] OR useField[23] OR useField[58] OR useField[59]
    ufEst = useField[26] OR useField[27]
    ufGetSalesRep = useField[36]
    ufIPJobMaterial = useField[3] OR useField[14] OR useField[55] OR useField[56] OR useField[61] OR useField[62] OR useField[70] OR useField[71] OR useField[78] OR useField[79] OR useField[81]
    ufIPJobMatField = useField[29] OR useField[30] OR useField[31] OR useField[32] OR useField[33]
    ufIPJobSet = useField[65] OR useField[66] OR useField[67] OR useField[68]
    ufItemFG = useField[21] OR useField[34] OR useField[52] OR useField[54] OR useField[64]
    ufJobMch = useField[9] OR useField[15] OR useField[18] OR useField[19] OR useField[20] OR useField[85]
    ufOEOrdl = useField[82] OR useField[84] OR useField[86] OR useField[87]
    ufOERel = useField[37] OR useField[38] OR useField[39] OR useField[40] OR useField[52] OR useField[63]
    ufPOOrdl = useField[7] OR useField[16] OR useField[17] OR useField[35]
    ufProdQty = useField[57]
    ufPrep = useField[80]
    .
END PROCEDURE.
