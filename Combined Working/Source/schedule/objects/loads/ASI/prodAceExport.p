/* prodAceExport.p */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcID      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcDir     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.

DEFINE VARIABLE cMachine  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
DEFINE VARIABLE jdx       AS INTEGER   NO-UNDO.

DO idx = 1 TO NUM-ENTRIES(ipcType):
    ASSIGN
        cFile = ipcDir + '\'.
        cType = ENTRY(idx,ipcType)
        .
    CASE cType:
        WHEN 'Charge Codes' THEN DO:
            cFile = cFile + 'codelist.tmp'.
            OUTPUT TO VALUE(cFile).
            FOR EACH job-code:
                IF job-code.dmiID EQ 0 THEN
                job-code.dmiID = NEXT-VALUE(jobCodeDMIseq).
                IF CAN-DO('DT,NC',job-code.cat) THEN
                PUT UNFORMATTED
                    job-code.dmiID ',"'
                    job-code.code '",'
                    0 /* 0=down, 1=reject */
                    SKIP
                    .
            END. /* each job-code */
            OUTPUT CLOSE.
        END. /* charge codes */
        WHEN 'Employees' THEN DO:
            cFile = cFile + 'oprlist.tmp'.
            OUTPUT TO VALUE(cFile).
            FOR EACH employee NO-LOCK
                WHERE employee.company EQ ipcCompany
                :
                PUT UNFORMATTED '"'
                    employee.employee '","'
                    TRIM(employee.first_name + ' ' +
                    employee.last_name) '"'
                    SKIP 
                    .
            END. /* each employee */
            OUTPUT CLOSE.
        END. /* employees */
        WHEN 'Machines' THEN DO:
            cFile = cFile + 'machdata.tmp'.
            OUTPUT TO VALUE(cFile).    
            FOR EACH mach
                WHERE mach.company EQ ipcCompany
                  AND mach.spare-int-2 NE 0
                   BY mach.spare-int-2
                :
                ASSIGN
                    cMachine = REPLACE(mach.m-dscr,'"','')
                    cMachine = REPLACE(cMachine,'#','')
                    cMachine = REPLACE(cMachine,'/','')
                    cMachine = REPLACE(cMachine,'\','')
                    .
                PUT UNFORMATTED
                    STRING(mach.spare-int-2,'999') ',~"'
                    cMachine '","'
                    ENTRY(NUM-ENTRIES(ipcID,'/'),ipcID,'/') '"'
                    SKIP
                    .
            END. /* each mach */
            OUTPUT CLOSE.
        END. /* resources */
        WHEN 'Machine Codes' THEN DO:
            cFile = cFile + 'machcodelist.tmp'.
            OUTPUT TO VALUE(cFile).    
            FOR EACH mach
                WHERE mach.company EQ ipcCompany
                  AND mach.spare-int-2 NE 0
                   BY mach.spare-int-2
                :
                jdx = 0.
                FOR EACH machchrg NO-LOCK 
                    WHERE machchrg.company EQ mach.company
                      AND machchrg.machine EQ mach.m-code,
                    FIRST job-code NO-LOCK 
                    WHERE job-code.code EQ machchrg.charge_code
                      AND (job-code.cat EQ 'DT' OR job-code.cat EQ 'NC')
                    :
                    jdx = jdx + 1.
                    PUT UNFORMATTED
                        STRING(mach.spare-int-2,'999') ','
                        jdx ','
                        job-code.dmiID
                        SKIP
                        .
                END. /* each machchrg */
            END. /* each mach */
            OUTPUT CLOSE.
        END. /* resources */
        WHEN 'Shifts' THEN DO:
            cFile = cFile + 'shiftlist.tmp'.
            OUTPUT TO VALUE(cFile).
            FOR EACH shifts NO-LOCK 
                WHERE shifts.company EQ ipcCompany
                :
                PUT UNFORMATTED 
                    '"' shifts.shift ',"'
                    shifts.description '","'
                    STRING(shifts.start_time,'HH:MM') '"'
                    .
                FIND FIRST reftable NO-LOCK
                     WHERE reftable.reftable EQ "ShiftDays"
                       AND reftable.CODE     EQ shifts.rec_key
                     NO-ERROR.
                /* indicates all days of the week */
                IF NOT AVAILABLE reftable OR reftable.loc NE "1" THEN
                PUT UNFORMATTED ',1,1,1,1,1,1,1'.
                ELSE
                DO idx = 1 TO NUM-ENTRIES(reftable.code2):
                    PUT UNFORMATTED
                        ',' IF ENTRY(idx,reftable.code2) EQ 'yes' THEN '1' ELSE '0' 
                        .
                END. /* do idx */
                PUT UNFORMATTED SKIP. 
            END. /* each shifts */
            OUTPUT CLOSE.
        END. /* shifts */
    END CASE.
    cFileList = cFileList + cFile + ','.
END. /* do idx */
cFileList = TRIM(cFileList,',').

/* wait till end to rename all files so production ace doesn't pick up too soon */
DO idx = 1 TO NUM-ENTRIES(cFileList):
    cFile = ENTRY(idx,cFileList).
    OS-COPY VALUE(SEARCH(cFile)) VALUE(REPLACE(SEARCH(cFile),'.tmp','.dat')).
    OS-DELETE VALUE(SEARCH(cFile)).
END. /* do idx */
