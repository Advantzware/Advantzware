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
    File        : GenericDataExporter
    Purpose     : Generic export routine for SmartDB data during deployment
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 19 21:16:10 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Tools.Export.* FROM PROPATH .
USING Consultingwerk.Framework.*                   FROM PROPATH .
USING Consultingwerk.OERA.*                        FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Tools.Export.GenericDataExporter:

    /**
     * Purpose: Exports Business Entity Data to an XML file
     * Notes:
     * @param pcBusinessEntityName The name of the Business Entity to export data to
     * @param pcTables The name of the tables from the Business Entity to export
     * @param pcQueryString The query string to export
     * @param pcFileName The name of the target file name
     * @return The number of records exported
     */
    METHOD PUBLIC INTEGER ExportBusinessEntityData (pcBusinessEntityName AS CHARACTER,
                                                    pcTables AS CHARACTER,
                                                    pcQueryString AS CHARACTER,
                                                    pcFileName AS CHARACTER):

        DEFINE VARIABLE hDataset   AS HANDLE           NO-UNDO .
        DEFINE VARIABLE oRequest   AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE iRows      AS INTEGER          NO-UNDO .
        DEFINE VARIABLE cAllTables AS CHARACTER        NO-UNDO .

        ASSIGN oRequest = NEW FetchDataRequest (pcTables, pcQueryString, 0) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       pcBusinessEntityName,
                                                       oRequest,
                                                       OUTPUT DATASET-HANDLE hDataset) .

        hDataset:WRITE-XML ("file":U,
                            pcFileName,
                            TRUE, ?, ?) .

        ASSIGN cAllTables = ListHelper:ExpandListEntries (pcTables,
                                                          DatasetHelper:BufferNames (hDataset))

               iRows      = BufferHelper:NumRecords (hDataset:GET-BUFFER-HANDLE (ENTRY (1, cAllTables, ",":U))) .

        RETURN iRows .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .
        END FINALLY.

    END METHOD.

END CLASS.

