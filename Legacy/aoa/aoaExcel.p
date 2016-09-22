/* aoaExcel.p */

DEFINE INPUT PARAMETER iphTable AS HANDLE NO-UNDO.

DEFINE VARIABLE iColumn     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iColIdx     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iRow        AS INTEGER    NO-UNDO INITIAL 1.
DEFINE VARIABLE iStatusRow  AS INTEGER    NO-UNDO INITIAL 3.
DEFINE VARIABLE hQuery      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hQueryBuf   AS HANDLE     NO-UNDO.
DEFINE VARIABLE cDynFunc    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cExcelFile  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDataType   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFormat     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chExcel     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRangeRow  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRangeCol  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE idx         AS INTEGER    NO-UNDO.

IF NOT VALID-HANDLE(iphTable) THEN RETURN.

cExcelFile = "c:\tmp\excel.xls".

IF SEARCH(cExcelFile) NE ? THEN
OS-DELETE VALUE(SEARCH(cExcelFile)).

/* Connect to the running Excel session. */
CREATE "Excel.Application" chExcel CONNECT NO-ERROR.
/* Start a new session of Excel. */
IF NOT VALID-HANDLE(chExcel) THEN
CREATE "Excel.Application" chExcel NO-ERROR.
/* Check if Excel got initialized. */
IF NOT VALID-HANDLE(chExcel) THEN DO:
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
/* Open our Excel Template. */
chWorkbook = chExcel:Workbooks:Open(cExcelFile) NO-ERROR.
chExcel:Visible = TRUE.
/* Do not display Excel error messages. */
chExcel:DisplayAlerts = FALSE NO-ERROR.
ASSIGN
    chExcel:SheetsInNewWorkbook = 1
    chWorkbook  = chExcel:Workbooks:Add()
    chWorksheet = chWorkbook:Worksheets(1)
    .
chWorkbook:Worksheets:Add(,chWorksheet).
RELEASE OBJECT chWorksheet.

/* Select a worksheet */
chWorkbook:Worksheets(1):Activate.
ASSIGN
    chWorksheet = chWorkbook:Worksheets(1)
    /* Rename the worksheet */
    chWorkSheet:Name = "Data Dump"
    /* Disable screen updating so it will go faster */
    chExcel:ScreenUpdating = TRUE
    .
/* remove spare worksheet */
chWorkbook:WorkSheets(2):DELETE NO-ERROR.

/* run dynamic function (business subject) */
iphTable = iphTable:DEFAULT-BUFFER-HANDLE.

/* build header row column labels */
DO iColIdx = 1 TO iphTable:NUM-FIELDS:
    IF CAN-DO("rowType,parameters,recDataType",iphTable:BUFFER-FIELD(iColIdx):NAME) THEN NEXT.
    IF iphTable:BUFFER-FIELD(iColIdx):NAME BEGINS "xx" THEN NEXT.
    iColumn = iColumn + 1.
    ASSIGN
        cDataType = iphTable:BUFFER-FIELD(iColIdx):DATA-TYPE
        /* align left (-4131) or right (-4152) */
        chWorkSheet:Cells(iRow,iColumn):HorizontalAlignment = IF cDataType EQ "Character" THEN -4131
                                                                                          ELSE -4152
        chRangeRow = chWorkSheet:Cells(iRow,iColumn)
        chRangeCol = chWorkSheet:Cells(65536,iColumn)
        .
    /* column label */
    chWorkSheet:Cells(iRow,iColumn):Value = iphTable:BUFFER-FIELD(iColIdx):LABEL.
    /* apply column format based on data type */
    CASE cDataType:
        WHEN "Character" THEN
        ASSIGN
            chWorkSheet:Range(chRangeRow,chRangeCol):HorizontalAlignment = -4131
            chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = "General"
            .
        WHEN "Date" THEN
        chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = "mm/dd/yyyy".
        WHEN "Integer" OR WHEN "Decimal" THEN DO:
            ASSIGN
                cFormat = iphTable:BUFFER-FIELD(iColIdx):FORMAT
                cFormat = REPLACE(cFormat,">","#")
                cFormat = REPLACE(cFormat,"<","#")
                cFormat = REPLACE(cFormat,"9","0")
                .
            IF INDEX(cFormat,"-") NE 0 THEN
            ASSIGN
                cFormat = REPLACE(cFormat,"-","")
                cFormat = cFormat + "_);[Red](" + cFormat + ")"
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = cFormat.
        END. /* integer/decimal */
    END CASE.
END. /* do iColIdx */

/* bold and underline header row */
ASSIGN
    chRangeRow = chWorkSheet:Cells(iRow,1)
    chRangeCol = chWorkSheet:Cells(iRow,iColumn)
    chWorkSheet:Range(chRangeRow,chRangeCol):Font:Bold = TRUE
    chWorkSheet:Range(chRangeRow,chRangeCol):Font:Underline = TRUE
    .
chWorkSheet:Range(chRangeRow,chRangeCol):Select.
/* auto size the columns */
chExcel:Selection:Columns:AutoFit.
chWorksheet:Cells(iRow + 1,1):Select.

/* pause to let excel display catch up */
PAUSE 1 NO-MESSAGE.

/* turn off display to run faster and clear status messages */
ASSIGN
    chExcel:ScreenUpdating = FALSE
    chWorkSheet:Cells(iStatusRow,2):Value     = ""
    chWorkSheet:Cells(iStatusRow + 2,2):Value = ""
    chWorkSheet:Cells(iStatusRow + 4,2):Value = ""
    .

/* scroll returned temp-table records */
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(iphTable:HANDLE).
hQuery:QUERY-PREPARE("FOR EACH " + iphTable:NAME).
hQuery:QUERY-OPEN.
hQueryBuf = hQuery:GET-BUFFER-HANDLE(iphTable:NAME).
REPEAT:
    hQuery:GET-NEXT().
    IF hQuery:QUERY-OFF-END THEN LEAVE.
    IF hQueryBuf:BUFFER-FIELD("RowType"):BUFFER-VALUE() NE "Data" THEN NEXT.
    ASSIGN
        iRow = iRow + 1
        iColumn = 0
        .
    DO iColIdx = 1 TO iphTable:NUM-FIELDS:
        IF CAN-DO("rowType,parameters,recDataType",iphTable:BUFFER-FIELD(iColIdx):NAME) THEN NEXT.
        IF iphTable:BUFFER-FIELD(iColIdx):NAME BEGINS "xx" THEN NEXT.
        iColumn = iColumn + 1.
        chWorkSheet:Cells(iRow,iColumn):Value = iphTable:BUFFER-FIELD(iColIdx):BUFFER-VALUE() NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE "Row:" iRow "Column:" iColumn SKIP
                "Value:" iphTable:BUFFER-FIELD(iColIdx):BUFFER-VALUE() SKIP
                "Error:" ERROR-STATUS:GET-MESSAGE(idx)
                VIEW-AS ALERT-BOX ERROR.
        END. /* do idx */
    END. /* do iColIdx */
END. /* repeat */
hQuery:QUERY-CLOSE().
DELETE OBJECT hQuery.

/* calc header and data */
ASSIGN
    chRangeRow = chWorkSheet:Cells(iStatusRow - 2,1)
    chRangeCol = chWorkSheet:Cells(iRow,iphTable:NUM-FIELDS)
    .
/* put data into a table */
chWorkSheet:ListObjects:Add(,chWorkSheet:Range(chRangeRow,chRangeCol),,NO):Name = "TableAOA".
/* select header and data */
chWorkSheet:Range(chRangeRow,chRangeCol):Select.
/* auto size the columns */
chExcel:Selection:Columns:AutoFit.
/* select first none header cell */
chWorksheet:Cells(iStatusRow - 1,1):Select.
/* enable screen updating */
chExcel:ScreenUpdating = TRUE.
/* auto save excel file */
chExcel:ActiveSheet:SaveAs(cExcelFile).

/* Release created objects. */
RELEASE OBJECT chWorkbook  NO-ERROR.
RELEASE OBJECT chWorkSheet NO-ERROR.
RELEASE OBJECT chExcel     NO-ERROR.

