/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : ErrorMessages
    Purpose     : Translation of the List & Label Error Messages
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 07 23:08:17 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlErrorMessages:

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LlErrorMessages ():
        SUPER ().

    END CONSTRUCTOR.

    DEFINE STATIC VARIABLE cCodes AS CHARACTER NO-UNDO EXTENT 106 INIT [
        "LL_ERR_BAD_JOBHANDLE":U,
        "LL_ERR_TASK_ACTIVE":U,
        "LL_ERR_BAD_PROJECTTYPE":U,
        "LL_ERR_PRINTING_JOB":U,
        "LL_ERR_NO_BOX":U,
        "LL_ERR_ALREADY_PRINTING":U,
        "LL_ERR_NOT_YET_PRINTING":U,
        "":U,
        "":U,
        "LL_ERR_NO_PROJECT":U,
        "LL_ERR_NO_PRINTER":U,
        "LL_ERR_PRINTING":U,
        "LL_ERR_EXPORTING":U,
        "LL_ERR_NEEDS_VB":U,
        "LL_ERR_BAD_PRINTER":U,
        "LL_ERR_NO_PREVIEWMODE":U,
        "LL_ERR_NO_PREVIEWFILES":U,
        "LL_ERR_PARAMETER":U,
        "LL_ERR_BAD_EXPRESSION":U,
        "LL_ERR_BAD_EXPRMODE":U,
        "LL_ERR_NO_TABLE":U,
        "LL_ERR_CFGNOTFOUND":U,
        "LL_ERR_EXPRESSION":U,
        "LL_ERR_CFGBADFILE":U,
        "LL_ERR_BADOBJNAME":U,
        "LL_ERR_NOOBJECT":U,
        "LL_ERR_UNKNOWNOBJECT":U,
        "LL_ERR_NO_TABLEOBJECT":U,
        "LL_ERR_NO_OBJECT":U,
        "LL_ERR_NO_TEXTOBJECT":U,
        "LL_ERR_UNKNOWN":U,
        "LL_ERR_BAD_MODE":U,
        "LL_ERR_CFGBADMODE":U,
        "LL_ERR_ONLYWITHONETABLE":U,
        "LL_ERR_UNKNOWNVARIABLE":U,
        "LL_ERR_UNKNOWNFIELD":U,
        "LL_ERR_UNKNOWNSORTORDER":U,
        "LL_ERR_NOPRINTERCFG":U,
        "LL_ERR_SAVEPRINTERCFG":U,
        "LL_ERR_RESERVED":U,
        "LL_ERR_NOVALIDPAGES":U,
        "LL_ERR_NOTINHOSTPRINTERMODE":U,
        "LL_ERR_NOTFINISHED":U,
        "LL_ERR_BUFFERTOOSMALL":U,
        "LL_ERR_BADCODEPAGE":U,
        "LL_ERR_CANNOTCREATETEMPFILE":U,
        "LL_ERR_NODESTINATION":U,
        "LL_ERR_NOCHART":U,
        "LL_ERR_TOO_MANY_CONCURRENT_PRINTJOBS":U,
        "LL_ERR_BAD_WEBSERVER_LICENSE":U,
        "LL_ERR_NO_WEBSERVER_LICENSE":U,
        "LL_ERR_INVALIDDATE":U,
        "LL_ERR_DRAWINGNOTFOUND":U,
        "LL_ERR_NOUSERINTERACTION":U,
        "LL_ERR_BADDATABASESTRUCTURE":U,
        "LL_ERR_UNKNOWNPROPERTY":U,
        "LL_ERR_INVALIDOPERATION":U,
        "LL_ERR_PROPERTY_ALREADY_DEFINED":U,
        "LL_ERR_CFGFOUND":U,
        "LL_ERR_SAVECFG":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "LL_ERR_USER_ABORTED":U,
        "LL_ERR_BAD_DLLS":U,
        "LL_ERR_NO_LANG_DLL":U,
        "LL_ERR_NO_MEMORY":U,
        "":U,
        "LL_ERR_EXCEPTION":U,
        "LL_ERR_LICENSEVIOLATION":U,
        "LL_ERR_NOT_SUPPORTED_IN_THIS_OS":U].

    DEFINE STATIC VARIABLE cMessages AS CHARACTER NO-UNDO EXTENT 106 INIT [
        "(-1) bad jobhandle"{&TRAN},
        "(-2) LlDefineLayout() only once in a job"{&TRAN},
        "(-3) nObjType must be one of the allowed values"{&TRAN},
        "(-4) print job not opened, no print object"{&TRAN},
        "(-5) LlPrintSetBoxText(...) called when no abort box exists!"{&TRAN},
        "(-6) LlPrintWithBoxStart(...): another print job is being done, please wait or try LlPrintStart(...)"{&TRAN},
        "(-7) LlPrintGetOptionString..."{&TRAN},
        "":U,
        "":U,
        "(-10) object with requested name does not exist (former ERR_NO_OBJECT)"{&TRAN},
        "(-11) printer couldn't be opened"{&TRAN},
        "(-12) error while printing"{&TRAN},
        "(-13) error while exporting"{&TRAN},
        "(-14) '11...' needs VB.EXE"{&TRAN},
        "(-15) PrintOptionsDialog(): no printer available"{&TRAN},
        "(-16) Preview functions: not in preview mode"{&TRAN},
        "(-17) PreviewDisplay(): no file found"{&TRAN},
        "(-18) bad parameter (usually NULL pointer)"{&TRAN},
        "(-19) bad expression in LlExprEvaluate() and LlExprType()"{&TRAN},
        "(-20) bad expression mode (LlSetExpressionMode())"{&TRAN},
        "(-21) not used"{&TRAN},
        "(-22) on LlPrintStart(), LlPrintWithBoxStart() [not found]"{&TRAN},
        "(-23) on LlPrintStart(), LlPrintWithBoxStart()"{&TRAN},
        "(-24) on LlPrintStart(), LlPrintWithBoxStart() [read error, bad format]"{&TRAN},
        "(-25) on LlPrintEnableObject() - not a ':' at the beginning"{&TRAN},
        "(-26) on LlPrintEnableObject() - "", and no object in project"{&TRAN},
        "(-27) on LlPrintEnableObject() - object with that name not existing"{&TRAN},
        "(-28) LlPrint...Start() and no list in Project, or:"{&TRAN},
        "(-29) LlPrint...Start() and no object in project"{&TRAN},
        "(-30) LlPrintGetTextCharsPrinted() and no printable text in Project!"{&TRAN},
        "(-31) LlPrintIsVariableUsed(), LlPrintIsFieldUsed()"{&TRAN},
        "(-32) LlPrintFields(), LlPrintIsFieldUsed() called on non-OBJECT_LIST"{&TRAN},
        "(-33) on LlDefineLayout(), LlPrint...Start(): file is in wrong expression mode"{&TRAN},
        "(-34) on LlDefinePageSeparation(), LlDefineGrouping()"{&TRAN},
        "(-35) on LlGetVariableContents()"{&TRAN},
        "(-36) on LlGetFieldContents()"{&TRAN},
        "(-37) on LlGetFieldContents()"{&TRAN},
        "(-38) on LlPrintCopyPrinterConfiguration() - no or bad file"{&TRAN},
        "(-39) on LlPrintCopyPrinterConfiguration() - file could not be saved"{&TRAN},
        "(-40) function not yet implemeted"{&TRAN},
        "(-41) could also be that 16 bit Viewer tries to open 32bit-only storage"{&TRAN},
        "(-42) cannot be done in Host Printer Mode (LlSetPrinterInPrinterFile())"{&TRAN},
        "(-43) appears when a project reset() is done, but the table not finished"{&TRAN},
        "(-44) LlXXGetOptionStr()"{&TRAN},
        "(-45) LL_OPTION_CODEPAGE"{&TRAN},
        "(-46) cannot create temporary file"{&TRAN},
        "(-47) no valid export destination"{&TRAN},
        "(-48) no chart control present"{&TRAN},
        "(-49) WebServer: not enough print process licenses"{&TRAN},
        "(-50) WebServer: bad license file"{&TRAN},
        "(-51) WebServer: no license file"{&TRAN},
        "(-52) LlSystemTimeFromLocaleString(): date not valid!"{&TRAN},
        "(-53) only if LL_OPTION_ERR_ON_FILENOTFOUND set"{&TRAN},
        "(-54) a call is used which would show a dialog, but LL is in Webserver mode"{&TRAN},
        "(-55) the project that is loading has a table that is not supported by the database"{&TRAN},
        "(-56) "{&TRAN},
        "(-57) "{&TRAN},
        "(-58) "{&TRAN},
        "(-59) on LlPrjOpen() with CREATE_NEW disposition, or of project file is r/o and access flag is r/w"{&TRAN},
        "(-60) error while saving (LlProjectSave())"{&TRAN},
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "":U,
        "(-99) user aborted printing"{&TRAN},
        "(-100) DLLs not up to date (CTL, DWG, UTIL)"{&TRAN},
        "(-101) no or out-of-date language resource DLL"{&TRAN},
        "(-102) out of memory"{&TRAN},
        "":U,
        "(-104) there was a GPF during the API execution. Any action that follows might cause problems!"{&TRAN},
        "(-105) your license does not allow this call (see LL_OPTIONSTR_LICENSINGINFO)"{&TRAN},
        "(-106) the OS does not support this function"{&TRAN}] .

    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_JOBHANDLE                 AS INTEGER NO-UNDO INIT -1 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_TASK_ACTIVE                   AS INTEGER NO-UNDO INIT -2 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_OBJECTTYPE                AS INTEGER NO-UNDO INIT -3 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_PROJECTTYPE               AS INTEGER NO-UNDO INIT -3 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PRINTING_JOB                  AS INTEGER NO-UNDO INIT -4 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_BOX                        AS INTEGER NO-UNDO INIT -5 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_ALREADY_PRINTING              AS INTEGER NO-UNDO INIT -6 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOT_YET_PRINTING              AS INTEGER NO-UNDO INIT -7 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PROJECT                    AS INTEGER NO-UNDO INIT -10 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PRINTER                    AS INTEGER NO-UNDO INIT -11 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PRINTING                      AS INTEGER NO-UNDO INIT -12 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_EXPORTING                     AS INTEGER NO-UNDO INIT -13 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NEEDS_VB                      AS INTEGER NO-UNDO INIT -14 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_PRINTER                   AS INTEGER NO-UNDO INIT -15 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PREVIEWMODE                AS INTEGER NO-UNDO INIT -16 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PREVIEWFILES               AS INTEGER NO-UNDO INIT -17 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PARAMETER                     AS INTEGER NO-UNDO INIT -18 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_EXPRESSION                AS INTEGER NO-UNDO INIT -19 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_EXPRMODE                  AS INTEGER NO-UNDO INIT -20 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_TABLE                      AS INTEGER NO-UNDO INIT -21 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGNOTFOUND                   AS INTEGER NO-UNDO INIT -22 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_EXPRESSION                    AS INTEGER NO-UNDO INIT -23 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGBADFILE                    AS INTEGER NO-UNDO INIT -24 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BADOBJNAME                    AS INTEGER NO-UNDO INIT -25 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOOBJECT                      AS INTEGER NO-UNDO INIT -26 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNOBJECT                 AS INTEGER NO-UNDO INIT -27 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_TABLEOBJECT                AS INTEGER NO-UNDO INIT -28 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_OBJECT                     AS INTEGER NO-UNDO INIT -29 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_TEXTOBJECT                 AS INTEGER NO-UNDO INIT -30 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWN                       AS INTEGER NO-UNDO INIT -31 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_MODE                      AS INTEGER NO-UNDO INIT -32 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGBADMODE                    AS INTEGER NO-UNDO INIT -33 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_ONLYWITHONETABLE              AS INTEGER NO-UNDO INIT -34 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNVARIABLE               AS INTEGER NO-UNDO INIT -35 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNFIELD                  AS INTEGER NO-UNDO INIT -36 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNSORTORDER              AS INTEGER NO-UNDO INIT -37 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOPRINTERCFG                  AS INTEGER NO-UNDO INIT -38 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_SAVEPRINTERCFG                AS INTEGER NO-UNDO INIT -39 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_RESERVED                      AS INTEGER NO-UNDO INIT -40 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOVALIDPAGES                  AS INTEGER NO-UNDO INIT -41 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOTINHOSTPRINTERMODE          AS INTEGER NO-UNDO INIT -42 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOTFINISHED                   AS INTEGER NO-UNDO INIT -43 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BUFFERTOOSMALL                AS INTEGER NO-UNDO INIT -44 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BADCODEPAGE                   AS INTEGER NO-UNDO INIT -45 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CANNOTCREATETEMPFILE          AS INTEGER NO-UNDO INIT -46 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NODESTINATION                 AS INTEGER NO-UNDO INIT -47 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOCHART                       AS INTEGER NO-UNDO INIT -48 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_TOO_MANY_CONCURRENT_PRINTJOBS AS INTEGER NO-UNDO INIT -49 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_WEBSERVER_LICENSE         AS INTEGER NO-UNDO INIT -50 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_WEBSERVER_LICENSE          AS INTEGER NO-UNDO INIT -51 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_INVALIDDATE                   AS INTEGER NO-UNDO INIT -52 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_DRAWINGNOTFOUND               AS INTEGER NO-UNDO INIT -53 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOUSERINTERACTION             AS INTEGER NO-UNDO INIT -54 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BADDATABASESTRUCTURE          AS INTEGER NO-UNDO INIT -55 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNPROPERTY               AS INTEGER NO-UNDO INIT -56 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_INVALIDOPERATION              AS INTEGER NO-UNDO INIT -57 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PROPERTY_ALREADY_DEFINED      AS INTEGER NO-UNDO INIT -58 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGFOUND                      AS INTEGER NO-UNDO INIT -59 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_SAVECFG                       AS INTEGER NO-UNDO INIT -60 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_USER_ABORTED                  AS INTEGER NO-UNDO INIT -99 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_DLLS                      AS INTEGER NO-UNDO INIT -100 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_LANG_DLL                   AS INTEGER NO-UNDO INIT -101 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_MEMORY                     AS INTEGER NO-UNDO INIT -102 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_EXCEPTION                     AS INTEGER NO-UNDO INIT -104 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_LICENSEVIOLATION              AS INTEGER NO-UNDO INIT -105 GET .
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOT_SUPPORTED_IN_THIS_OS      AS INTEGER NO-UNDO INIT -106 GET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the formatted error message
        Notes:
        @param piError The error message number
        @return The formatted error message
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER FormattedErrorMessage (piError AS INTEGER):

        RETURN SUBSTITUTE ("&2 / &1":U,
                           GetErrorCode (piError),
                           GetErrorText (piError)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Error Message Code
        Notes:
        @param piError The error message number
        @return The Error Message Code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetErrorCode (piError AS INTEGER):

        DEFINE VARIABLE iError AS INTEGER NO-UNDO.

        ASSIGN iError = ABSOLUTE (piError) .

        IF iError > 0 AND iError <= EXTENT (cCodes) THEN
            RETURN cCodes [iError] .
        ELSE
            RETURN STRING (piError) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Error Message Text
        Notes:
        @param piError The error message number
        @return The Error Message Text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetErrorText (piError AS INTEGER):

        DEFINE VARIABLE iError AS INTEGER NO-UNDO.

        ASSIGN iError = ABSOLUTE (piError) .

        IF iError > 0 AND iError <= EXTENT (cCodes) THEN
            RETURN cMessages [iError] .
        ELSE
            RETURN STRING (piError) .

    END METHOD .

END CLASS.
