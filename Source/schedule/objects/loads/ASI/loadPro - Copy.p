/* loadPro.p - ASI */

&SCOPED-DEFINE sbDB nosweat
&SCOPED-DEFINE ID ASI/ALL
&SCOPED-DEFINE HOP ASI/HOP
&SCOPED-DEFINE Fleetwood ASI/Fleetwood

{schedule/scopDir.i}
{{&loads}/loadPro.i}

DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE useSalesRep AS LOGICAL NO-UNDO.

DEFINE BUFFER bJobHdr FOR job-hdr.
DEFINE BUFFER bJobMch FOR job-mch.
DEFINE BUFFER beb1 FOR eb.
DEFINE BUFFER beb2 FOR eb.

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
  userLabel[66] = 'Internal W.'     userWidth[66] = 12
  userLabel[67] = 'End Cell L.'     userWidth[67] = 12
  userLabel[68] = 'End Cell W.'     userWidth[68] = 12
  userLabel[69] = 'Current Qty'     userWidth[69] = 12.
&SCOPED-DEFINE nextUserField 70
/* add userField to rptFields.dat */
DO idx = 41 TO 50:
  ASSIGN
    userLabel[idx] = 'Ink-' + STRING(idx - 40)
    userWidth[idx] = 30.
END. /* do i */
/* adding additional fields mod:
   1. print/includes/rptLayout.i
   2. viewers/includes/viewersInclude.i (reopenBrowse)
   3. scopDir.i (userExtent)
   4. includes/ttblJobIndex.i
   5. includes/ttblJobFields.i */
  
FUNCTION comma RETURNS CHARACTER (ipValue AS CHARACTER):
  RETURN IF ipValue NE '' THEN ',' ELSE ''.
END FUNCTION.

FUNCTION getItemNo RETURNS CHARACTER (ipCompany AS CHARACTER,ipJobNo AS CHARACTER,
                                      ipJobNo2 AS INTEGER,ipForm AS INTEGER):
  DEFINE VARIABLE itemNo AS CHARACTER NO-UNDO.

  FOR EACH bJobMch NO-LOCK
     WHERE bJobMch.company EQ ipCompany
       AND bJobMch.job-no EQ ipJobNo
       AND bJobMch.job-no2 EQ ipJobNo2
       AND bJobMch.frm EQ ipForm:
    IF bJobMch.i-no NE '' AND bJobMch.i-no NE itemNo THEN
    itemNo = IF itemNo EQ '' THEN bJobMch.i-no
        ELSE IF itemNo NE bJobMch.i-no THEN '<Multi Item>'
        ELSE ''.
    IF itemNo EQ '<Multi Item>' THEN LEAVE.
  END. /* each bjobmch */
  IF itemNo EQ '' THEN itemNo = '<Multi Item>'.
  RETURN itemNo.
END FUNCTION.

FUNCTION getSetPOQtyRec RETURNS CHARACTER (ipCompany AS CHARACTER,ipJobNo AS CHARACTER,
                                           ipJobNo2 AS INTEGER,ipForm AS INTEGER,
                                           ipINo AS CHARACTER):
  DEFINE VARIABLE qty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE rtnQty AS DECIMAL NO-UNDO.

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
                                    ipFrm AS INTEGER,ipBlankNo AS INTEGER):
  DEFINE VARIABLE prodQtyProgram AS CHARACTER NO-UNDO.
  DEFINE VARIABLE prodQty AS INTEGER NO-UNDO.

  IF CONNECTED('emptrack') THEN DO:
    prodQtyProgram = SEARCH(findProgram('{&loads}/',ID,'/prodQty.p')).
    IF prodQtyProgram NE ? THEN
    RUN VALUE(prodQtyProgram) (ipCompany,ipResource,ipJobNo,ipJobNo2,ipFrm,ipBlankNo,OUTPUT prodQty).
  END. /* emptrack connected */
  RETURN LEFT-TRIM(STRING(prodQty,'zzz,zzz,zz9')).
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
      FIRST item OF job-mat NO-LOCK:
    IF traceON THEN
    PUT UNFORMATTED 'Mat Type: ' AT 20 item.mat-type ' '
      job-mat.company ' '
      job-mat.job ' '
      job-mat.job-no ' '
      job-mat.job-no2 ' '
      job-mat.i-no ' '
      job-mat.frm ' '
      job-mat.blank-no.
    DO i = 1 TO NUM-ENTRIES(ipMatType):
      IF item.mat-type EQ ENTRY(i,ipMatType) THEN DO:
        rtnvalue = CAN-FIND(FIRST mat-act
           WHERE mat-act.company EQ job-mat.company
             AND mat-act.job EQ job-mat.job
             AND mat-act.job-no EQ job-mat.job-no
             AND mat-act.job-no2 EQ job-mat.job-no2
             AND mat-act.i-no EQ job-mat.i-no
             AND mat-act.s-num EQ job-mat.frm
             AND mat-act.b-num EQ job-mat.blank-no USE-INDEX job).
        LEAVE.
      END. /* if mattype match */
    END. /* do i */
  END. /* each job-mat */
  IF traceON THEN
  PUT UNFORMATTED ipMatType AT 20 ' ' rtnValue ' ' ETIME SKIP.
  RETURN rtnvalue.
END FUNCTION.

RUN customValueList (OUTPUT customValueList).

IF ID EQ '' THEN ID = 'ASI/ALL'.
departmentList = ''.
FOR EACH dept NO-LOCK:
  departmentList = departmentList + comma(departmentList) + dept.code.
END.

&IF '{&Board}' NE 'View' &THEN
DEFINE VARIABLE altResSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE asiCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE beginEstType AS INTEGER NO-UNDO.
DEFINE VARIABLE boardLength AS DECIMAL NO-UNDO.
DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE boardWidth AS DECIMAL NO-UNDO.
DEFINE VARIABLE custNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE custName AS CHARACTER NO-UNDO.
DEFINE VARIABLE customValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE dimFormat AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE lagTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvCode2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvNoteKey AS CHARACTER NO-UNDO.
DEFINE VARIABLE noDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE prodDate AS DATE NO-UNDO.
DEFINE VARIABLE resourceDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE resSeq AS INTEGER NO-UNDO.
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

DEFINE BUFFER bMach FOR mach.

DEFINE TEMP-TABLE tResource NO-UNDO
  FIELD dSeq AS INTEGER
  FIELD mSeq AS INTEGER
  FIELD resource AS CHARACTER
  FIELD mDscr AS CHARACTER
    INDEX tResource IS PRIMARY dSeq mSeq resource.

DISABLE TRIGGERS FOR LOAD OF reftable.

FUNCTION boardName RETURNS CHARACTER (ipBoard AS CHARACTER):
  IF traceON THEN
  PUT UNFORMATTED 'Function boardName @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
  
  FIND FIRST item NO-LOCK WHERE item.company EQ asiCompany
                            AND item.i-no EQ ipBoard NO-ERROR.
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

FUNCTION jobMaterial RETURN CHARACTER (ipCompany AS CHARACTER, ipJob AS INTEGER,
                                       ipJobNo AS CHARACTER, ipJobNo2 AS INTEGER,
                                       ipForm AS INTEGER,ipMatType AS CHARACTER):
  DEFINE VARIABLE rtnValue AS CHARACTER NO-UNDO.

  IF traceON THEN
  PUT UNFORMATTED 'Function jobMaterial (' AT 15 ipMatType ') @ ' STRING(TIME,'hh:mm:ss') ' ' ETIME.
  
  FOR EACH job-mat NO-LOCK WHERE job-mat.company eq ipCompany
                             AND job-mat.job EQ ipJob
                             AND job-mat.job-no EQ ipJobNo
                             AND job-mat.job-no2 EQ ipJobNo2
                             AND job-mat.frm EQ ipForm,
        FIRST item OF job-mat NO-LOCK WHERE item.mat-type EQ ipMatType:
    IF CAN-DO(rtnValue,job-mat.i-no) THEN NEXT.
    rtnValue = rtnValue + comma(rtnValue) + job-mat.i-no.
  END. /* each job-mat */
  IF traceON THEN
  PUT UNFORMATTED ' (' rtnValue ') ' ETIME SKIP.
  RETURN rtnValue.
END FUNCTION.

FUNCTION jobMatField RETURN CHARACTER (ipCompany AS CHARACTER, ipJNo AS INTEGER, ipINo AS CHARACTER,
                                       ipForm AS INTEGER, ipBlank AS INTEGER,
                                       ipMatField AS CHARACTER, ipFormat AS CHARACTER):
  DEFINE VARIABLE rtnValue AS CHARACTER NO-UNDO.

  IF traceON THEN
  PUT UNFORMATTED 'Function jobMatField @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
  
  FIND FIRST job-mat NO-LOCK WHERE job-mat.company eq ipCompany
                               AND job-mat.j-no EQ ipJNo
                               AND job-mat.i-no EQ ipINo
                               AND job-mat.frm EQ ipForm
                               AND job-mat.blank-no EQ ipBlank
                               AND job-mat.qty GT 0 NO-ERROR.
  IF AVAILABLE job-mat THEN
  CASE ipMatField:
    WHEN 'totalMRP' THEN
    rtnValue = STRING(job-mat.qty,ipFormat).
    WHEN 'matLength' THEN
    rtnValue = STRING(job-mat.len,ipFormat).
    WHEN '#Up' THEN
    rtnValue = STRING(job-mat.n-up,ipFormat).
  END CASE.
  RETURN rtnValue.
END FUNCTION.

FUNCTION noDate RETURN LOGICAL (ipCompany AS CHARACTER):
  RETURN CAN-FIND(FIRST sys-ctrl NO-LOCK
                  WHERE sys-ctrl.company EQ ipCompany
                    AND sys-ctrl.name EQ 'Schedule'
                    AND sys-ctrl.char-fld EQ 'NoDate'
                    AND sys-ctrl.log-fld EQ YES).
END FUNCTION.

IF VALID-HANDLE(ipContainerHandle) THEN
RUN asiCommaList IN ipContainerHandle ('Company',OUTPUT asiCompany).
IF asiCompany EQ '' THEN asiCompany = '001'.

useSalesRep = SEARCH(findProgram('{&data}/',ID,'/useSalesRep.dat')) NE ?.
IF useSalesRep THEN DO:
  FOR EACH sman NO-LOCK WHERE sman.company EQ asiCompany:
    IF NOT CAN-DO(customValueList,sman.sman) THEN
    customValueList = customValueList + ',' + sman.sman.
  END. /* each sman */
END. /* salesman routine */

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
          tResource.mSeq = bMach.m-seq.
      END. /* avail bmach */
    END. /* if not */
  END. /* first-of m-code */
END. /* each mach */

FOR EACH tResource EXCLUSIVE-LOCK:
  sortOrder = sortOrder + 1.
  {{&exports}/resource.i &streamName=sResource
      &resourceDescription=tResource.mDscr
      &resource=tResource.resource
      &sortOrder=sortOrder}
END. /* each tresource */

IF traceON THEN DO:
  OUTPUT TO 'schedule/load.log' APPEND.
  ETIME(TRUE).
END.

ASSIGN
  beginEstType = IF CAN-DO('ASI/ALL,ASI/Folding*,{&HOP}',ID) THEN 0 ELSE 5
  endEstType = IF CAN-DO('ASI/ALL,ASI/Corrugated*,{&Fleetwood}',ID) THEN 99 ELSE 4.
  
FOR EACH job-hdr NO-LOCK WHERE job-hdr.company EQ asiCompany
                           AND job-hdr.opened EQ YES
                         BREAK BY job-hdr.job-no BY job-hdr.job-no2:
  IF FIRST-OF(job-hdr.job-no2) THEN
  FOR EACH job OF job-hdr NO-LOCK {&jobNo} {&joinTable},
      FIRST est OF job NO-LOCK WHERE est.est-type GE beginEstType
                                 AND est.est-type LE endEstType,
      EACH job-mch NO-LOCK WHERE job-mch.company EQ job.company
                             AND job-mch.job EQ job.job
                             AND job-mch.job-no EQ job.job-no
                             AND job-mch.job-no2 EQ job.job-no2
                            {&startDatePhrase}
                             AND job-mch.run-complete EQ NO,
      FIRST mach NO-LOCK WHERE mach.company EQ job.company
                           AND mach.loc EQ job.loc
                           AND mach.m-code EQ job-mch.m-code
      BREAK BY job.job BY job-mch.frm BY job-mch.line:
    
    IF traceON THEN DO:
    debugCount = debugCount + 1.
    PUT UNFORMATTED
      debugCount AT 10
      job-mch.m-code AT 20 ' '
      job-mch.job-no AT 30 '-'
      job-mch.job-no2 '.'
      job-mch.frm ' -- '
      job-mch.line ' : '
      STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    END.
    
    IF FIRST-OF(job-mch.frm) OR NOT cascadeJob THEN resSeq = 0.
    
    IF FIRST-OF(job-mch.frm) THEN
    itemDescription = IF job-mch.i-no NE '' THEN job-mch.i-no
                      ELSE getItemNo(job-mch.company,job-mch.job-no,job-mch.job-no2,job-mch.frm).
    
    scheduleResource = IF mach.sch-m-code NE '' THEN mach.sch-m-code ELSE mach.m-code.
    {{&loads}/resourceUse.i scheduleResource}
    
    ASSIGN
      altResSeq = job-mch.line
      customValue = ''
      dimFormat = '>>9.99999'
      dueDate = IF job.due-date NE ? THEN job.due-date ELSE {{&includes}/lastDate.i}
      jobSort = job-mch.job-no + '-' + STRING(job-mch.job-no2,'99') +
                '.' + STRING(job-mch.frm,'99')
      jobNumber = LEFT-TRIM(job-mch.job-no + '-' + STRING(job-mch.job-no2) +
                  '.' + STRING(job-mch.frm))
      jobStatus = useSalesRep
      resSeq = resSeq + 1
      resourceDescription = mach.m-dscr
      salesRepFound = NO
      startDate = job-mch.start-date-su
      startTime = fixTime(job-mch.start-time-su)
      strRowID = STRING(ROWID(job)) + ',' + STRING(ROWID(job-mch))
      keyValues = job-mch.company + ',' + job-mch.m-code + ',' +
                  STRING(job-mch.job) + ',' + job-mch.job-no + ',' +
                  STRING(job-mch.job-no2)
      timeSpan = calcJobTime(job-mch.mr-hr,job-mch.run-hr)
      unitFound = NO
      userField = ''.
  
    /* surely, jobs shouldn't be this old, if any, move to pending */
    IF startDate LT TODAY - 365 THEN startDate = ?.
  
    IF NoDate(job-mch.company) AND (job-mch.end-date-su EQ ? OR job-mch.end-date EQ ?) THEN
    ASSIGN
      startDate = ?
      startTime = timeSpan.
    ELSE IF startDate NE ? THEN
    RUN calcEnd (startDate,startTime,job-mch.mr-hr,job-mch.run-hr,
                 OUTPUT endDate,OUTPUT endTime).
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
      FOR EACH oe-rel NO-LOCK
          WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no EQ oe-ordl.ord-no
            AND oe-rel.i-no EQ oe-ordl.i-no
            AND oe-rel.line EQ oe-ordl.line
          BREAK BY oe-rel.rel-no:
        IF LAST(oe-rel.rel-no) THEN
        ASSIGN
          userField[37] = STRING(oe-rel.qty,'->>,>>>,>>9')
          userField[38] = STRING(oe-rel.rel-date,'99.99.9999')
          userField[39] = oe-rel.ship-addr[1]
          userField[40] = oe-rel.ship-city + ', ' + oe-rel.ship-state + ' ' + oe-rel.ship-zip
          userField[52] = STRING(oe-rel.qty)
          userField[63] = oe-ordl.po-no.
      END. /* each oe-rel */
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
          userField[36] = getSalesRep(oe-ord.company,oe-ord.sman[1])
          salesRepFound = YES.
      END. /* avail oe-ord */
    END. /* not avail oe-ordl or reg-date eq ? */
    ELSE dueDate = oe-ordl.req-date.

    RELEASE po-ord.
    RELEASE po-ordl.
    
    IF traceON THEN
    PUT UNFORMATTED 'Available oe-ordl @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    IF AVAILABLE oe-ordl THEN DO:
      FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      IF AVAILABLE oe-ord THEN DO:
        IF useSalesRep THEN DO:
          i = LOOKUP(oe-ord.sman[1],customValueList) - 1.
          IF i GT 0 THEN jobStatus[i] = NO.
        END. /* if usesalesrep */
        ASSIGN
          prodDate = oe-ord.prod-date
          userField[36] = getSalesRep(oe-ord.company,oe-ord.sman[1])
          salesRepFound = YES.
      END. /* avail oe-ord */
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
    END. /* avail oe-ordl */
    ELSE DO:
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
    FIND FIRST ef NO-LOCK WHERE ef.company EQ job.company
                            AND ef.est-no EQ job.est-no
                            AND ef.form-no EQ job-mch.frm NO-ERROR.
    FIND FIRST eb NO-LOCK WHERE eb.company EQ job.company
                            AND eb.est-no EQ job.est-no
                            AND eb.form-no EQ job-mch.frm
                            AND (eb.blank-no EQ job-mch.blank-no
                             OR job-mch.blank-no EQ 0) NO-ERROR.
    IF AVAILABLE eb THEN DO:
      FIND style NO-LOCK WHERE style.company EQ job.company
                           AND style.style EQ eb.style NO-ERROR.
      IF AVAILABLE style AND style.industry EQ '2' AND est.est-type LT 5 THEN
      dimFormat = '->>9.99'.
      FIND FIRST reftable NO-LOCK
           WHERE reftable.reftable EQ 'ce/v-est3.w Unit#'
             AND reftable.company EQ eb.company
             AND reftable.loc EQ eb.est-no
             AND reftable.code EQ STRING(eb.form-no,'9999999999')
             AND reftable.code2 EQ STRING(eb.blank-no,'9999999999') NO-ERROR.
      IF AVAILABLE reftable THEN
      DO i = 1 TO 10:
        IF reftable.val[i] GE 1 AND reftable.val[i] LE 10 THEN
        userField[INTEGER(reftable.val[i]) + 40] = eb.i-dscr2[i].
        unitFound = YES.
      END. /* do i */
    END. /* avail eb */
    
    RUN getCustomer (job-mch.company,job-mch.job-no,job-mch.job-no2,job-mch.frm,
                     OUTPUT custNo,OUTPUT custName,OUTPUT salesRep).
    IF NOT salesRepFound THEN DO:
      IF useSalesRep THEN DO:
        i = LOOKUP(salesRep,customValueList) - 1.
        IF i GT 0 THEN jobStatus[i] = NO.
      END. /* if usesalesrep */
      userField[36] = getSalesRep(job.company,salesRep).
    END. /* not salesrepfound */

    FIND itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
                          AND itemfg.i-no EQ job-hdr.i-no NO-ERROR.
    IF traceON THEN
    PUT UNFORMATTED 'Assign Fields @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME SKIP.
    ASSIGN
      customValue = SUBSTR(customValueList,2)
      statusTimeStamp = ''
      lagTime = job-mch.lag-time
      liveUpdate = getLiveUpdate(job-mch.company,job-mch.job-no,job-mch.job-no2,
                                 job-mch.frm,job-mch.m-code)
      userField[1] = custNo
      userField[2] = custName
      userField[5] = IF AVAILABLE eb THEN eb.die-no ELSE ''
      userField[6] = IF AVAILABLE eb THEN eb.plate-no ELSE ''
      userField[7] = IF AVAILABLE po-ordl THEN STRING(po-ordl.po-no,'>>>>>9') ELSE ''
      userField[8] = IF AVAIL eb AND eb.est-type EQ 6 THEN eb.stock-no /* set */
                ELSE IF job-mch.i-no NE '' THEN job-mch.i-no
                ELSE itemDescription
      userField[9] = IF job-mch.i-name NE '' THEN job-mch.i-name
                ELSE IF AVAILABLE itemfg AND job-mch.i-no NE '' THEN itemfg.i-name
                ELSE IF userField[8] EQ '<Multi Item>' THEN '<Multiple Items>' ELSE ''
      userField[10] = IF AVAILABLE eb THEN STRING(convBase16(eb.len),dimFormat) ELSE ''
      userField[11] = IF AVAILABLE eb THEN STRING(convBase16(eb.wid),dimFormat) ELSE ''
      userField[12] = IF AVAILABLE eb THEN STRING(convBase16(eb.dep),dimFormat) ELSE ''
      userField[13] = IF AVAILABLE eb THEN eb.style ELSE ''
      userField[15] = IF job-mch.run-qty EQ ? THEN '' ELSE LEFT-TRIM(STRING(job-mch.run-qty,'zzz,zzz,zz9'))
      userField[16] = IF AVAILABLE po-ordl THEN STRING(po-ordl.due-date,'99/99/9999') ELSE ''
      userField[17] = IF AVAILABLE po-ord THEN po-ord.vend-no ELSE ''
      userField[18] = STRING(job-mch.frm,'zz9')
      userField[19] = STRING(job-mch.blank-no,'zzz')
      userField[20] = STRING(job-mch.pass,'zz9')
      userField[21] = IF AVAILABLE itemfg THEN itemfg.procat ELSE ''
      userField[22] = IF AVAILABLE ef THEN STRING(ef.cal,'9.99999') ELSE ''
      userField[23] = IF AVAILABLE ef THEN STRING(ef.gsh-qty,'-zz,zzz,zz9') ELSE ''
      userField[24] = IF AVAILABLE eb THEN eb.adhesive ELSE ''
      userField[25] = IF AVAILABLE eb THEN STRING(eb.i-coat,'z9') ELSE ''
      userField[28] = IF AVAILABLE eb THEN eb.i-coldscr ELSE ''
      userField[29] = jobMatField(job-mch.company,job-mch.j-no,job-mch.i-no,job-mch.frm,job-mch.blank-no,'totalMrp','>>,>>>,>>9.99<')
      userField[30] = jobMatField(job-mch.company,job-mch.j-no,job-mch.i-no,job-mch.frm,job-mch.blank-no,'matLength','>>9.99<<')
      userField[31] = jobMatField(job-mch.company,job-mch.j-no,job-mch.i-no,job-mch.frm,job-mch.blank-no,'#Up','>>>9')
      userField[32] = STRING(DECIMAL(userField[29]) * (DECIMAL(userField[30]) / 12),'>>,>>>,>>9') /* was >>,>>>,>>9.99< */
      userField[33] = STRING(DECIMAL(userField[29]) * DECIMAL(userField[31]),'>>,>>>,>>9.99<')
      userField[34] = IF AVAILABLE itemfg THEN itemfg.cad-no ELSE ''
      userField[35] = IF AVAILABLE eb AND eb.est-type EQ 6 THEN getSetPOQtyRec(job-mch.company,job-mch.job-no,job-mch.job-no2,eb.form-no,eb.stock-no)
                 ELSE IF AVAILABLE po-ordl THEN STRING(po-ordl.t-rec-qty,'->,>>>,>>>,>>9.99<<<')
                 ELSE ''
      userField[51] = IF AVAILABLE eb THEN eb.tr-no ELSE ''
      userField[52] = IF AVAILABLE itemfg THEN STRING(DECIMAL(userField[52]) * itemfg.t-sqft / 1000,'->,>>9.999') ELSE ''
      userField[53] = IF AVAILABLE eb THEN STRING(eb.tab-in,'In/Out') ELSE ''
      userField[54] = IF AVAILABLE itemfg AND job-mch.run-qty NE ? AND job-mch.run-qty * itemfg.t-sqft / 10000 LT 1000 THEN
                      STRING(job-mch.run-qty * itemfg.t-sqft / 1000,'->>>,>>9.99999') ELSE ''
      userField[57] = prodQty(job-mch.company,job-mch.m-code,job-mch.job-no,
                              job-mch.job-no2,job-mch.frm,job-mch.blank-no)
      userField[58] = IF AVAILABLE ef THEN STRING(ef.gsh-len,'>>9.9999') ELSE ''
      userField[59] = IF AVAILABLE ef THEN STRING(ef.gsh-wid,'>>9.9999') ELSE ''
      userField[60] = IF AVAILABLE eb THEN eb.cas-no ELSE ''
      userField[61] = jobMaterial(job-mch.company,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,'5')
      userField[62] = jobMaterial(job-mch.company,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,'6')
      userField[64] = IF AVAILABLE itemfg THEN itemfg.part-no ELSE ''
   /* jobDescription = custNo + (IF custName NE '' THEN ' - ' + REPLACE(custName,',','') ELSE '') +
                       ',' + REPLACE(userField[8] + ': ' + userField[9],',','') */
      jobDescription = jobText.
    IF NOT job-mch.run-qty * itemfg.t-sqft / 1000 LT 1000000 THEN
    MESSAGE 'Job:' jobNumber SKIP
            'Resource:' scheduleResource SKIP
            'Run Qty:' job-mch.run-qty SKIP
            'FG Item:' itemfg.i-no SKIP
            'FG Total SqFt:' itemfg.t-sqft SKIP
            'Run MSF:' job-mch.run-qty * itemfg.t-sqft / 1000
        VIEW-AS ALERT-BOX TITLE 'Run MSF Error'.
    RUN ipJobMaterial (job-mch.company,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,'B','I','D',
                       OUTPUT userField[3],OUTPUT userField[14],
                       OUTPUT boardLength,OUTPUT boardWidth,OUTPUT jobBoard,
                       OUTPUT userField[55],OUTPUT userField[56]).
    ASSIGN
      userField[26] = IF est.est-type GE 5 THEN STRING(boardLength,dimFormat)
                      ELSE STRING(convBase16(boardLength),dimFormat)
      userField[27] = IF est.est-type GE 5 THEN STRING(boardWidth,dimFormat)
                      ELSE STRING(convBase16(boardWidth),dimFormat).
    IF userField[3] EQ '' THEN
    userField[3] = IF AVAILABLE ef THEN ef.board ELSE ''.
    userField[4] = boardName(ENTRY(1,userField[3])).
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
        FIND FIRST statusCheckOffs
             WHERE statusCheckOffs.statusCheckOffs EQ ENTRY(i,customValueList) NO-ERROR.
        IF AVAILABLE statusCheckOffs THEN
        jobStatus[i - 1] = statusCheckOff(job-mch.company,job-mch.job,
                                          job-mch.job-no,job-mch.job-no2,
                                          job-mch.frm,statusCheckOffs.materialType).
      /*IF ENTRY(i,customValueList) EQ 'Board' AND CAN-DO(reftable.dscr,'Board') THEN
        jobStatus[i - 1] = jobBoard.*/
        ELSE
        jobStatus[i - 1] = NOT CAN-DO(reftable.dscr,ENTRY(i,customValueList)).
      END. /* not usesalesrep */
    END. /* if avail */
    ELSE IF NOT useSalesRep THEN
    jobStatus[LOOKUP('Board',customValueList) - 1] = jobBoard.

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
      &customValue=customValue
      &strRowID=strRowID
      &keyValue=keyValues}
    ELSE
    {{&exports}/job.i &streamName=sPending
      &resource=scheduleResource
      &resourceSequence=resSeq
      &altResSeq=altResSeq
      &resourceDescription=resourceDescription
      &altResource=job-mch.m-code
      &job=jobNumber
      &jobSort=jobSort
      &jobDescription=jobDescription
      &jobToolTip=jobToolTip
      &jobSequence=job-mch.seq-no
      &startTime=startTime
      &timeSpan=timeSpan
      &jobLocked=job-mch.anchored
      &dueDate=dueDate
      &prodDate=prodDate
      &customValue=customValue
      &strRowID=strRowID
      &keyValue=keyValues}
  END. /* each job */
END. /* each job-hdr */

PROCEDURE ipJobSet:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstNo AS CHARACTER NO-UNDO.

  FIND FIRST beb1 NO-LOCK
       WHERE beb1.company EQ ipCompany
         AND beb1.est-no  EQ ipEstNo
         AND beb1.form-no NE 0
         AND beb1.blank-no NE 0
       USE-INDEX est-qty NO-ERROR.
  
  IF AVAIL beb1 THEN DO:
     FIND FIRST beb2 NO-LOCK
          WHERE beb2.company EQ ipCompany
            AND beb2.est-no  EQ ipEstNo
            AND beb2.form-no NE 0
            AND beb2.blank-no NE 0
            AND ROWID(beb2) NE ROWID(beb1)
          USE-INDEX est-qty NO-ERROR.
  END. /* if avail beb1 */
END PROCEDURE.

PROCEDURE ipJobMaterial:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipForm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipMatType1 AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMatType2 AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMatType3 AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opBoard AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInk AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opBoardLength AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opBoardWidth AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opJobBoard AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opPallet AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opTotMRP AS CHARACTER NO-UNDO.

  IF traceON THEN
  PUT UNFORMATTED 'Procedure ipJobMaterial @ ' AT 15 STRING(TIME,'hh:mm:ss') ' ' ETIME.
  
  FOR EACH job-mat NO-LOCK WHERE job-mat.company eq ipCompany
                             AND job-mat.job EQ ipJob
                             AND job-mat.job-no EQ ipJobNo
                             AND job-mat.job-no2 EQ ipJobNo2
                             AND job-mat.frm EQ ipForm,
      FIRST item OF job-mat NO-LOCK:
    IF item.mat-type EQ 'B' AND NOT opJobBoard THEN
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
    IF item.mat-type EQ ipMatType1 AND NOT CAN-DO(opBoard,job-mat.i-no) THEN
    opBoard = opBoard + comma(opBoard) + job-mat.i-no.
    IF item.mat-type EQ ipMatType2 AND NOT CAN-DO(opInk,job-mat.i-no) THEN
    opInk = opInk + comma(opInk) + job-mat.i-no.
    IF item.mat-type EQ ipMatType3 AND opPallet EQ '' AND opTotMRP EQ '' THEN
    ASSIGN
      opPallet = item.i-name
      opTotMRP = STRING(job-mat.qty,'>,>>>,>>9.9<<<<<').
  END. /* each job-mat */
  IF traceON THEN
  PUT UNFORMATTED ipMatType1 AT 20 ' ' opBoard
                  ipMatType2 AT 20 ' ' opInk
                  ipMatType3 AT 20 ' ' opPallet ' ' opTotMRP ' ' ETIME SKIP.
END PROCEDURE.

{{&loads}/loadProEnd.i}

{{&includes}/{&Board}/calcEnd.i}

&ENDIF

PROCEDURE getCustomer:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipFrm AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opCustNo AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opCustName AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opSalesRep AS CHARACTER NO-UNDO.

  FIND FIRST bJobHdr NO-LOCK WHERE bJobHdr.company EQ ipCompany
                               AND bJobHdr.job-no EQ ipJobNo
                               AND bJobHdr.job-no2 EQ ipJobNo2
                               AND bJobHdr.frm EQ ipFrm NO-ERROR.
  IF NOT AVAILABLE bJobHdr THEN
  FIND FIRST bJobHdr NO-LOCK WHERE bJobHdr.company EQ ipCompany
                               AND bJobHdr.job-no EQ ipJobNo
                               AND bJobHdr.job-no2 EQ ipJobNo2 NO-ERROR.
  IF AVAILABLE bJobHdr THEN
  FIND cust NO-LOCK WHERE cust.company EQ bJobHdr.company
                      AND cust.cust-no EQ bJobHdr.cust-no NO-ERROR.
  IF AVAILABLE cust THEN
  ASSIGN
    opCustNo = cust.cust-no
    opCustName = cust.name
    opSalesRep = cust.sman.
END PROCEDURE.

PROCEDURE customValueList:
  DEFINE OUTPUT PARAMETER opCustomValueList AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE valueList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE printPrgm AS CHARACTER NO-UNDO.
  DEFINE VARIABLE printPrgmDat AS CHARACTER NO-UNDO.

  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT ^.
  IMPORT ^.
  IMPORT ^.
  REPEAT:
    IMPORT ^ ^ ^ valueList.
    IF NOT useSalesRep OR valueList NE '' THEN
    opCustomValueList = opCustomValueList + ',' + valueList.
  END.
  INPUT CLOSE.
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
  ASSIGN colCheck.fld1 = 'Customer' colCheck.fld2 = 'userField01'
         colCheck.fld3 = 'Customer' colCheck.fld4 = 'userField01'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Name' colCheck.fld2 = 'userField02'
         colCheck.fld3 = 'Name' colCheck.fld4 = 'userField02'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Board' colCheck.fld2 = 'userField03'
         colCheck.fld3 = 'Board' colCheck.fld4 = 'userField03'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField14'
         colCheck.fld3 = 'Board Name' colCheck.fld4 = 'userField04'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Die' colCheck.fld2 = 'userField04'
         colCheck.fld3 = 'Die' colCheck.fld4 = 'userField05'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Plate' colCheck.fld2 = 'userField05'
         colCheck.fld3 = 'Plate' colCheck.fld4 = 'userField06'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'PO' colCheck.fld2 = 'userField06'
         colCheck.fld3 = 'PO' colCheck.fld4 = 'userField07'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'FG Item' colCheck.fld2 = 'userField07'
         colCheck.fld3 = 'FG Item' colCheck.fld4 = 'userField08'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField13'
         colCheck.fld3 = 'Item Name' colCheck.fld4 = 'userField09'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Item Name' colCheck.fld2 = 'userField08'
         colCheck.fld3 = 'Item Name' colCheck.fld4 = 'userField09'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Length' colCheck.fld2 = 'userField08'
         colCheck.fld3 = 'Length' colCheck.fld4 = 'userField10'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Length' colCheck.fld2 = 'userField09'
         colCheck.fld3 = 'Length' colCheck.fld4 = 'userField10'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Width' colCheck.fld2 = 'userField09'
         colCheck.fld3 = 'Width' colCheck.fld4 = 'userField11'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Width' colCheck.fld2 = 'userField10'
         colCheck.fld3 = 'Width' colCheck.fld4 = 'userField11'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Depth' colCheck.fld2 = 'userField10'
         colCheck.fld3 = 'Depth' colCheck.fld4 = 'userField12'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Depth' colCheck.fld2 = 'userField11'
         colCheck.fld3 = 'Depth' colCheck.fld4 = 'userField12'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Style' colCheck.fld2 = 'userField11'
         colCheck.fld3 = 'Style' colCheck.fld4 = 'userField13'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Style' colCheck.fld2 = 'userField12'
         colCheck.fld3 = 'Style' colCheck.fld4 = 'userField13'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Color' colCheck.fld2 = 'userField12'
         colCheck.fld3 = 'Color' colCheck.fld4 = 'userField14'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Color' colCheck.fld2 = 'userField13'
         colCheck.fld3 = 'Color' colCheck.fld4 = 'userField14'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Run Qty' colCheck.fld2 = 'extraField01'
         colCheck.fld3 = 'Run Qty' colCheck.fld4 = 'userField15'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'PO Due' colCheck.fld2 = 'extraField02'
         colCheck.fld3 = 'PO Due' colCheck.fld4 = 'userField16'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = 'Vendor' colCheck.fld2 = 'extraField03'
         colCheck.fld3 = 'Vendor' colCheck.fld4 = 'userField17'.
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
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField18'
         colCheck.fld3 = 'Form' colCheck.fld4 = 'userField18'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField19'
         colCheck.fld3 = 'Blank' colCheck.fld4 = 'userField19'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField20'
         colCheck.fld3 = 'Pass' colCheck.fld4 = 'userField20'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField21'
         colCheck.fld3 = 'Category' colCheck.fld4 = 'userField21'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField22'
         colCheck.fld3 = 'Caliper' colCheck.fld4 = 'userField22'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField23'
         colCheck.fld3 = 'Sheet Qty' colCheck.fld4 = 'userField23'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField24'
         colCheck.fld3 = 'Adhesive' colCheck.fld4 = 'userField24'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField25'
         colCheck.fld3 = 'Coats' colCheck.fld4 = 'userField25'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField26'
         colCheck.fld3 = 'Board L.' colCheck.fld4 = 'userField26'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField27'
         colCheck.fld3 = 'Board W.' colCheck.fld4 = 'userField27'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField28'
         colCheck.fld3 = 'Color Desc' colCheck.fld4 = 'userField28'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField29'
         colCheck.fld3 = 'Total MRP' colCheck.fld4 = 'userField29'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField30'
         colCheck.fld3 = 'Mat Length' colCheck.fld4 = 'userField30'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField31'
         colCheck.fld3 = '#Up' colCheck.fld4 = 'userField31'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField32'
         colCheck.fld3 = 'Linear Feet' colCheck.fld4 = 'userField32'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField33'
         colCheck.fld3 = 'Total Pieces' colCheck.fld4 = 'userField33'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField34'
         colCheck.fld3 = 'CAD' colCheck.fld4 = 'userField34'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField35'
         colCheck.fld3 = 'PO Qty Recd' colCheck.fld4 = 'userField35'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField36'
         colCheck.fld3 = 'Sales Rep' colCheck.fld4 = 'userField36'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField37'
         colCheck.fld3 = 'Release Qty' colCheck.fld4 = 'userField37'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField38'
         colCheck.fld3 = 'Release Date' colCheck.fld4 = 'userField38'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField39'
         colCheck.fld3 = 'Ship To Addr' colCheck.fld4 = 'userField39'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField40'
         colCheck.fld3 = 'Ship To City' colCheck.fld4 = 'userField40'.
  DO i = 41 TO 50:
    CREATE colCheck.
    ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField' + STRING(i,'99')
           colCheck.fld3 = 'Ink-' + STRING(i - 40) colCheck.fld4 = 'userField' + STRING(i,'99').
  END. /* do i */
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField51'
         colCheck.fld3 = 'Pallet/Unit' colCheck.fld4 = 'userField51'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField52'
         colCheck.fld3 = 'MSF' colCheck.fld4 = 'userField52'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField53'
         colCheck.fld3 = 'Tab' colCheck.fld4 = 'userField53'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField54'
         colCheck.fld3 = 'Run MSF' colCheck.fld4 = 'userField54'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField55'
         colCheck.fld3 = 'Pallet Name' colCheck.fld4 = 'userField55'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField56'
         colCheck.fld3 = 'Pallet MRP' colCheck.fld4 = 'userField56'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField57'
         colCheck.fld3 = 'Prod Qty' colCheck.fld4 = 'userField57'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField58'
         colCheck.fld3 = 'Gross Sht L.' colCheck.fld4 = 'userField58'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField59'
         colCheck.fld3 = 'Gross Sht W.' colCheck.fld4 = 'userField59'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField60'
         colCheck.fld3 = 'Pack Code' colCheck.fld4 = 'userField60'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField61'
         colCheck.fld3 = 'Mat Type5 Item#' colCheck.fld4 = 'userField61'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField62'
         colCheck.fld3 = 'Mat Type6 Item#' colCheck.fld4 = 'userField62'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField63'
         colCheck.fld3 = 'Cust PO#' colCheck.fld4 = 'userField63'.
  CREATE colCheck.
  ASSIGN colCheck.fld1 = notUsed colCheck.fld2 = 'userField64'
         colCheck.fld3 = 'Cust Part#' colCheck.fld4 = 'userField64'.
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
