/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : GenericDataImporter
    Purpose     : Generic import routine for SmartDB data during upgrades
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 24 12:03:24 CEST 2014
    Notes       : Can also be used for other tasks that require to import
                  data into a Business Entity from a ProDataset supporting
                  updates and creates depending on the availability of
                  records with matching PUK values
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.Tools.Import.* FROM PROPATH .
USING Consultingwerk.Framework.*                   FROM PROPATH .
USING Consultingwerk.OERA.*                        FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:

    /**
     * Purpose: Returns if the GenericDataImporter is currently loading data
     * Notes:   Used in the Data Access classes to determine if the AssignSkipList
     *          needs to be set
     */
    DEFINE PUBLIC STATIC PROPERTY Importing AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Imports Business Entity Data from an XML file
     * Notes:
     * @param pcBusinessEntityName The name of the Business Entity to import data from
     * @param pcFileName The name of the source file name
     * @param pcSkipFields The name of the fields to skip during import
     * @return The number of records to export
     */
    METHOD PUBLIC ImportResult ImportBusinessEntityData (pcBusinessEntityName AS CHARACTER,
                                                         pcFileName           AS CHARACTER,
                                                         pcSkipFields         AS CHARACTER):

        DEFINE VARIABLE hDatasetImport AS HANDLE           NO-UNDO .

        ServiceInterface:FetchDataset (pcBusinessEntityName,
                                       OUTPUT DATASET-HANDLE hDatasetImport) .

        hDatasetImport:READ-XML ("file":U, pcFileName, ?, ?, ?) .

        RETURN THIS-OBJECT:ImportBusinessEntityData (pcBusinessEntityName, hDatasetImport, pcSkipFields).

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDatasetImport) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Imports Business Entity Data from an XML file
     * Notes:   When importing from XML files, the caller of this routine should
     *          retrieve the Business Entities ProDataset to import the XML data
     *          into, e.g.: ServiceInterface:FetchDataset (pcBusinessEntityName, OUTPUT DATASET-HANDLE hDatasetImport) .
     * @param pcBusinessEntityName The name of the Business Entity to import data from
     * @param phDatasetImport The handle of the ProDataset to import from
     * @param pcSkipFields The name of the fields to skip during import
     * @return The number of records to export
     */
    METHOD PUBLIC ImportResult ImportBusinessEntityData (pcBusinessEntityName AS CHARACTER,
                                                         phDatasetImport      AS HANDLE,
                                                         pcSkipFields         AS CHARACTER):

        DEFINE VARIABLE hDataset       AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hBufferImport  AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hBuffer        AS HANDLE           NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER          NO-UNDO .
        DEFINE VARIABLE hQuery         AS HANDLE           NO-UNDO .
        DEFINE VARIABLE cFindString    AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cErrors        AS CHARACTER        NO-UNDO .

        DEFINE VARIABLE iNew           AS INTEGER          NO-UNDO .
        DEFINE VARIABLE iUpdated       AS INTEGER          NO-UNDO .

        DEFINE VARIABLE oRequest       AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE oReturn        AS ImportResult     NO-UNDO .
        DEFINE VARIABLE lCompare       AS LOGICAL          NO-UNDO .

        ASSIGN GenericDataImporter:Importing = TRUE .

        DO i = 1 TO phDatasetImport:NUM-BUFFERS ON ERROR UNDO, THROW:

            ASSIGN hBufferImport = phDatasetImport:GET-BUFFER-HANDLE (i)
                   hQuery        = QueryHelper:CreatePreparedQuery (hBufferImport) .

            DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

                ASSIGN cFindString = "FOR EACH ":U + hBufferImport:NAME + " ":U + BufferHelper:UniqueFindPredicate (hBufferImport)
                       cErrors     = "":U
                       oRequest    = NEW FetchDataRequest (hBufferImport:NAME,
                                                           cFindString,
                                                           1)
                       hDataset    = ? .

                ServiceInterface:FetchData (pcBusinessEntityName,
                                            oRequest,
                                            OUTPUT DATASET-HANDLE hDataset) .

                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (hBufferImport:NAME) .

                hBuffer:FIND-FIRST () NO-ERROR .

                DatasetHelper:SetTrackingChanges (hDataset, TRUE) .

                IF hBuffer:AVAILABLE THEN DO:
                    hBuffer:BUFFER-COPY (hBufferImport, pcSkipFields) .

                    Consultingwerk.Util.BufferHelper:FindBeforeBuffer (hBuffer) .

                    lCompare = hBuffer:BUFFER-COMPARE (hBuffer:BEFORE-BUFFER, "binary":U) .

                    hBuffer:BUFFER-RELEASE () .

                    IF lCompare = FALSE THEN DO:

                        ServiceInterface:SaveChanges (pcBusinessEntityName,
                                                      INPUT-OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .

                        ASSIGN iUpdated = iUpdated + 1
                               cErrors  = DatasetHelper:DatasetErrorStrings (hDataset) .
                    END.
                END.
                ELSE DO:
                    hBuffer:BUFFER-CREATE () .
                    hBuffer:BUFFER-COPY (hBufferImport) .
                    hBuffer:BUFFER-RELEASE () .

                    ServiceInterface:SaveChanges (pcBusinessEntityName,
                                                  INPUT-OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .
                    ASSIGN iNew = iNew + 1
                           cErrors  = DatasetHelper:DatasetErrorStrings (hDataset) .
                END.

                IF cErrors > "":U THEN DO:
                    MESSAGE "Error processing: ":U cFindString .
                    MESSAGE "Error message:    ":U cErrors .
                    MESSAGE " ":U .
                END.

                hDataset:ACCEPT-CHANGES () .

                hQuery:GET-NEXT () .

                CATCH err AS Progress.Lang.Error:
                    MESSAGE "Unhandled error:"{&TRAN} SKIP (0)
                            ErrorHelper:FormattedErrorMessagesExt (err) SKIP (1).
                END CATCH.

                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hDataset) .
                    hDataset = ? .
                END FINALLY.
            END.

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
            END FINALLY.
        END.

        ASSIGN oReturn                 = NEW ImportResult ()
               oReturn:CreatedRecords  = iNew
               oReturn:ModifiedRecords = iUpdated .

        RETURN oReturn .

        FINALLY:
            ASSIGN GenericDataImporter:Importing = FALSE .
        END FINALLY.

    END METHOD .

END CLASS.

