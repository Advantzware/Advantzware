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
    File        : DatabaseHelper
    Purpose     : Provides helper routines working with the database
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 18 20:36:21 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Assertion.*       FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH .
USING Consultingwerk.Util.Exceptions.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.Util.DatabaseHelper:

    /**
     * Purpose: Constructor for the DatabaseHelper class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED DatabaseHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the sequence number of a database the ABL session is connected
     *          to. This sequence number can be used for functions like LDBNAME or
     *          the return-value of the GetRecordsReads() method in this class
     * Notes:
     * @param pcDatabaseName The logical database name to return the connect sequence number for
     * @return The sequence number of a database the ABL session is connected to or ? when the DB is not connected
     */
    METHOD PUBLIC STATIC INTEGER DatabaseNumber (pcDatabaseName AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO NUM-DBS:
            IF LDBNAME (i) = pcDatabaseName THEN
                RETURN i .
        END.

        RETURN ?.

    END METHOD.

    /**
     * Purpose: Returns a comma-delimited list of the logical database
     *          names of all connected database names, all alias names
     *          and the logical name of the DICTDB
     * Notes:
     * @return The comma-delimited signature of the connected database
     */
    METHOD PUBLIC STATIC CHARACTER GetDbNameSignature ():

        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cDbNames AS CHARACTER NO-UNDO .

        DO i = 1 TO NUM-DBS:
            ASSIGN cDbNames = cDbNames + LDBNAME (i) + ",":U .
        END.

        DO i = 1 TO NUM-ALIASES:
            ASSIGN cDbNames = cDbNames + ALIAS (i) + ",":U .
        END.

        IF CONNECTED ("DICTDB":U) THEN
            ASSIGN cDbNames = cDbNames + LDBNAME ("DICTDB":U) .

        RETURN TRIM (cDbNames, ",":U).

    END METHOD.

    /**
     * Purpose: Returns the RECID of the _DB record
     * Notes:   Required for invoking some of the data dictionary, data admin API's
     * @return The RECID of the DICTDB _DB record
     */
    METHOD PUBLIC STATIC RECID GetDictDbRecid ():

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        DEFINE VARIABLE iRecid  AS RECID  NO-UNDO.

        Consultingwerk.Assertion.DatabaseAssert:IsConnected ("DICTDB":U) .

        CREATE BUFFER hBuffer FOR TABLE "DICTDB._DB":U .

        hBuffer:FIND-FIRST () .

        iRecid = hbuffer:RECID .

        RETURN iRecid .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hBuffer) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns an int64 array containing the _ActSummary._Summary-RecReads
     *          for every connected database
     * Notes:
     * @return The INT64[] containing the _ActSummary._Summary-RecReads per connected DB
     */
    METHOD PUBLIC STATIC INT64 EXTENT GetRecordReads ():

        DEFINE VARIABLE iReturn AS INT64   EXTENT NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER        NO-UNDO .
        DEFINE VARIABLE hBuffer AS HANDLE         NO-UNDO .

        IF NUM-DBS = 0 THEN
            UNDO, THROW NEW AppError ("Unable to get record statistics, there is no database connected."{&TRAN}, 0) .

        EXTENT (iReturn) = NUM-DBS .

        DO i = 1 TO NUM-DBS ON ERROR UNDO, THROW:

            CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1._ActSummary":U,
                                                        LDBNAME (i)) .

            hBuffer:FIND-FIRST ()  .

            iReturn [i] = hBuffer::_Summary-RecReads .

            FINALLY:
                DELETE OBJECT hBuffer NO-ERROR .
            END FINALLY.
        END.

        RETURN iReturn .

    END METHOD.

    /**
     * Purpose: Returns an int64 array containing the _ActSummary._Summary-RecReads
     *          for every connected database substracted by the values in a start
     *          array.
     * Notes:   This function is useful for computing the actual number of records
     *          read since the GetRecordsReads() (no parameter) methods was called
     * @param iStart An INT64[] containing the start offset for the records read
     * @return The INT64[] containing the _ActSummary._Summary-RecReads per connected DB since the offset was taken
     */
    METHOD PUBLIC STATIC INT64 EXTENT GetRecordReads (iStart AS INT64 EXTENT):

        DEFINE VARIABLE iReturn AS INT64   EXTENT NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER        NO-UNDO .
        DEFINE VARIABLE hBuffer AS HANDLE         NO-UNDO .

        IF NUM-DBS = 0 THEN
            UNDO, THROW NEW AppError ("Unable to get record statistics, there is no database connected."{&TRAN}, 0) .

        IF NUM-DBS = 0 THEN
            UNDO, THROW NEW AppError ("Unable to get record statistics difference, the start array is not of the proper size."{&TRAN}, 0) .

        EXTENT (iReturn) = NUM-DBS .

        DO i = 1 TO NUM-DBS ON ERROR UNDO, THROW:

            CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1._ActSummary":U,
                                                        LDBNAME (i)) .

            hBuffer:FIND-FIRST ()  .

            iReturn [i] = hBuffer::_Summary-RecReads - iStart [i].

            FINALLY:
                DELETE OBJECT hBuffer NO-ERROR .
            END FINALLY.
        END.

        RETURN iReturn .

    END METHOD.

    /**
     * Purpose: Returns the unabbreviated field name based on an abbreviated field
     *          name
     * Notes:
     * @param pcTableName The table name (possibly abbreviated)
     * @param pcFieldName The field name (possibly abbreviated)
     * @return The unabbreviated field name
     */
    METHOD PUBLIC STATIC CHARACTER UnabbreviatedFieldName (pcTableName AS CHARACTER,
                                                           pcFieldName AS CHARACTER):

        DEFINE VARIABLE cTableName   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hFileBuffer  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hFieldBuffer AS HANDLE    NO-UNDO.

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcFieldName "'pcFieldName':U"} .

        ASSIGN cTableName = DatabaseHelper:UnabbreviatedTableName (pcTableName) .

        CREATE BUFFER hFileBuffer FOR TABLE SUBSTITUTE ("&1._file":U, ENTRY (1, cTableName, ".":U)) .

        CREATE BUFFER hFieldBuffer FOR TABLE SUBSTITUTE ("&1._field":U, ENTRY (1, cTableName, ".":U)) .

        hFileBuffer:FIND-UNIQUE (SUBSTITUTE ("where _file-name = &1":U, QUOTER (ENTRY (2, cTableName, ".":U)))) NO-ERROR .

        hFieldBuffer:FIND-UNIQUE (SUBSTITUTE ("where _field._file-recid = &1 and _field-name = &2":U,
                                              hFileBuffer:RECID, QUOTER (pcFieldName))) NO-ERROR .

        IF hFieldBuffer:AVAILABLE THEN
            RETURN hFieldBuffer::_field-name .

        hFieldBuffer:FIND-UNIQUE (SUBSTITUTE ("where _field._file-recid = &1 and _field-name begins &2":U,
                                              hFileBuffer:RECID, QUOTER (pcFieldName))) NO-ERROR .

        IF hFieldBuffer:AVAILABLE THEN
            RETURN hFieldBuffer::_field-name .

        hFieldBuffer:FIND-FIRST (SUBSTITUTE ("where _field._file-recid = &1 and _field-name begins &2":U,
                                             hFileBuffer:RECID, QUOTER (pcFieldName))) NO-ERROR .

        IF hFieldBuffer:AVAILABLE THEN
            UNDO, THROW NEW AmbiguousFieldNameException ("Ambigious field name reference ~"&1~"."{&TRAN},
                                                         pcFieldName) .

        UNDO, THROW NEW InvalidFieldNameException ("Invalid field name reference ~"&1~"."{&TRAN},
                                                   pcFieldName) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hFileBuffer) .
            GarbageCollectorHelper:DeleteObject (hFieldBuffer) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns the unabbreviated table name based on an abbreviated table
     *          name
     * Notes:
     * @param pcDatabaseName The database name
     * @param pcTableName The abbreviated table name
     * @return The unabbreviated buffer name (including database name)
     */
    METHOD PUBLIC STATIC CHARACTER UnabbreviatedTableName (pcDatabaseName AS CHARACTER,
                                                           pcTableName AS CHARACTER):

        DEFINE VARIABLE hFileBuffer AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcDatabaseName "'pcDatabaseName':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcTableName "'pcTableName':U"} .
        DatabaseAssert:IsConnected (pcDatabaseName) .

        CREATE BUFFER hFileBuffer FOR TABLE SUBSTITUTE ("&1._file":U, pcDatabaseName) .

        hFileBuffer:FIND-UNIQUE (SUBSTITUTE ("where _file-name = &1":U, QUOTER (pcTableName))) NO-ERROR .

        IF hFileBuffer:AVAILABLE THEN
            RETURN SUBSTITUTE ("&1.&2":U, pcDatabaseName, hFileBuffer::_file-name) .

        hFileBuffer:FIND-UNIQUE (SUBSTITUTE ("where _file-name begins &1":U, QUOTER (pcTableName))) NO-ERROR .

        IF hFileBuffer:AVAILABLE THEN
            RETURN SUBSTITUTE ("&1.&2":U, pcDatabaseName, hFileBuffer::_file-name) .

        hFileBuffer:FIND-FIRST (SUBSTITUTE ("where _file-name begins &1":U, QUOTER (pcTableName))) NO-ERROR .

        IF hFilebuffer:AVAILABLE THEN
            UNDO, THROW NEW AmbiguousTableNameException ("Ambigious table name reference ~"&1~"."{&TRAN},
                                                         pcTableName) .

        UNDO, THROW NEW InvalidTableNameException ("Invalid table name reference ~"&1~"."{&TRAN},
                                                   pcTableName) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hFileBuffer) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns the unabbreviated table name based on an abbreviated table
     *          name
     * Notes:
     * @param pcTableName The abbreviated table name, optionally with DB prefix
     * @return The unabbreviated buffer name (including database name)
     */
    METHOD PUBLIC STATIC CHARACTER UnabbreviatedTableName (pcTableName AS CHARACTER):

        DEFINE VARIABLE cTableName    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cTemp         AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER   NO-UNDO .

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcTableName "'pcTableName':U"} .

        CASE NUM-ENTRIES (pcTableName, ".":U):
            WHEN 1 THEN DO:
                DO i = 1 TO NUM-DBS ON ERROR UNDO, THROW:

                    ASSIGN cTemp = DatabaseHelper:UnabbreviatedTableName (LDBNAME (i), pcTableName) .

                    /* When we already had a match, we know it's ambigious */
                    IF cTableName > "":U THEN
                        UNDO, THROW NEW AmbiguousTableNameException ("Ambigious table name reference ~"&1~"."{&TRAN},
                                                                     pcTableName) .
                    ELSE
                    /* Rember this match */
                        ASSIGN cTableName = cTemp .

                    /* Catch InvalidTableNameException and ignore, but throw
                       AmbiguousTableNameException */
                    @SuppressUnusedWarnings.
                    CATCH e AS InvalidTableNameException:
                    END CATCH.
                END.

                IF cTableName > "":U THEN
                    RETURN cTableName .
                ELSE
                    UNDO, THROW NEW InvalidTableNameException ("Invalid table name reference ~"&1~"."{&TRAN},
                                                               pcTableName) .
            END.
            WHEN 2 THEN
                RETURN UnabbreviatedTableName (ENTRY (1, pcTableName, ".":U),
                                               ENTRY (2, pcTableName, ".":U)) .


            OTHERWISE
                UNDO, THROW NEW InvalidTableNameException ("Too many entries in table name reference ~"&1~"."{&TRAN},
                                                           pcTableName) .

        END CASE .

    END METHOD .

END CLASS.
