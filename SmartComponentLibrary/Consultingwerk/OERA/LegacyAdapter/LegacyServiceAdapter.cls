/*********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LegacyServiceAdapter
    Purpose     : A single class - with no dependencies to any SmartComponent Library
                  class (except some useful error types)
                  In the first version we will support:
                    FetchData (Business Entity Name, Tables, Query String), returning all records (no batching)
                    SaveChanges
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Fri Dec 02 08:21:37 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*  FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.LegacyAdapter.LegacyServiceAdapter:

    DEFINE VARIABLE hAppServerConnection AS HANDLE NO-UNDO.

    { Consultingwerk/OERA/Context/dsContext.i }

    /**
     * Purpose: Server Context Dataset
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ContextDatasetHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Relative path of service interface procedures on the AppServer
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ServiceInterfacePath AS CHARACTER NO-UNDO INITIAL "OERA/support":U
    GET.
    SET.

    /**
     * Purpose: Constructor for the LegacyServiceAdapter class
     * Notes:
     * @param phAppServerConnection Handle to a connected AppServer
     */
    CONSTRUCTOR PUBLIC LegacyServiceAdapter (phAppServerConnection AS HANDLE):

        HandleAssert:ValidHandle (phAppServerConnection, "phAppServerConnection":U).
        HandleAssert:IsServer (phAppServerConnection).

        Assert:Equals (phAppServerConnection:CONNECTED (), TRUE).

        hAppServerConnection = phAppServerConnection.

        InitContextDataset ().

    END CONSTRUCTOR.

    /**
     * Purpose: Initializes the Context Dataset with local settings
     * Notes:
     *
     */
    METHOD PRIVATE VOID InitContextDataset ():

        THIS-OBJECT:ContextDatasetHandle = DATASET dsContext:HANDLE.

        CREATE eSessionContext .
        ASSIGN eSessionContext.ClientProVersion = PROVERSION
               eSessionContext.ClientType       = SESSION:CLIENT-TYPE .

    END METHOD.

    /**
     * Purpose: Reads data from the Backend
     * Notes:
     * @param pcEntityName The name of the business entity
     * @param pcTables Comma delimeted list of tables to be populated ("" or "*" request all tables)
     * @param pcQueryString Query statements for each requested table (CHR (1) seperated)
     * @param phDataset OUTPUT DATASET-HANDLE The resulting dataset
     */
    METHOD PUBLIC VOID RetrieveData (pcEntityName AS CHARACTER,
                                     pcTables AS CHARACTER,
                                     pcQueryString AS CHARACTER,
                                     OUTPUT DATASET-HANDLE phDataset) :

        DEFINE VARIABLE cEntity         AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cTables         AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cQueries        AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cJoins          AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cPositions      AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cRequests       AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cBatchContext   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFillBatch      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cNumRecords     AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cContext        AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cPrevContext    AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cNextContext    AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE lcParameter     AS LONGCHAR  NO-UNDO EXTENT 1.
        DEFINE VARIABLE lcNamedQuery    AS LONGCHAR  NO-UNDO EXTENT 1.

        DEFINE VARIABLE hDataset        AS HANDLE    NO-UNDO EXTENT 10 .

        ASSIGN
            cEntity[1]       = pcEntityName
            cTables[1]       = IF pcTables = "":U OR pcTables = ? THEN "*":U ELSE pcTables
            cQueries[1]      = pcQueryString
            cJoins[1]        = "":U
            cPositions[1]    = "":U
            cRequests[1]     = "":U
            cBatchContext    = "":U
            cNumRecords[1]   = "0":U
            .

        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            RUN VALUE (ServiceInterfacePath + "/proSIretrieve.p":U) ON hAppServerConnection
                                         (cEntity,
                                          cTables,
                                          cQueries,
                                          cJoins,
                                          cPositions,
                                          cRequests,
                                          cBatchContext,
                                          lFillBatch,
                                          0,
                                          INPUT-OUTPUT cNumRecords,
                                          INPUT-OUTPUT lcParameter,
                                          lcNamedQuery,
                                          OUTPUT DATASET-HANDLE phDataset,
                                          OUTPUT DATASET-HANDLE hDataset[2],
                                          OUTPUT DATASET-HANDLE hDataset[3],
                                          OUTPUT DATASET-HANDLE hDataset[4],
                                          OUTPUT DATASET-HANDLE hDataset[5],
                                          OUTPUT DATASET-HANDLE hDataset[6],
                                          OUTPUT DATASET-HANDLE hDataset[7],
                                          OUTPUT DATASET-HANDLE hDataset[8],
                                          OUTPUT DATASET-HANDLE hDataset[9],
                                          OUTPUT DATASET-HANDLE hDataset[10],
                                          INPUT-OUTPUT cContext,
                                          OUTPUT cPrevContext,
                                          OUTPUT cNextContext,
                                          INPUT-OUTPUT DATASET-HANDLE ContextDatasetHandle BY-REFERENCE) .
        END. /* DO ON STOP UNDO, ...*/

        FINALLY:

            {Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}

            /* Delete datasets (delayed until this procedure terminates */
            IF VALID-HANDLE(hDataset[2]) THEN
                DELETE OBJECT hDataset[2] NO-ERROR .

            IF VALID-HANDLE(hDataset[3]) THEN
                DELETE OBJECT hDataset[3] NO-ERROR .

            IF VALID-HANDLE(hDataset[4]) THEN
                DELETE OBJECT hDataset[4] NO-ERROR .

            IF VALID-HANDLE(hDataset[5]) THEN
                DELETE OBJECT hDataset[5] NO-ERROR .

            IF VALID-HANDLE(hDataset[6]) THEN
                DELETE OBJECT hDataset[6] NO-ERROR .

            IF VALID-HANDLE(hDataset[7]) THEN
                DELETE OBJECT hDataset[7] NO-ERROR .

            IF VALID-HANDLE(hDataset[8]) THEN
                DELETE OBJECT hDataset[8] NO-ERROR .

            IF VALID-HANDLE(hDataset[9]) THEN
                DELETE OBJECT hDataset[9] NO-ERROR .

            IF VALID-HANDLE(hDataset[10]) THEN
                DELETE OBJECT hDataset[10] NO-ERROR .

        END FINALLY.

    END METHOD.

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     */
     METHOD PUBLIC VOID SubmitData (pcEntityName AS CHARACTER,
                                    INPUT-OUTPUT DATASET-HANDLE hChangesDataset) :

        DEFINE VARIABLE pcContext   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcParameter AS LONGCHAR  NO-UNDO.

        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            RUN VALUE (ServiceInterfacePath + "/proSIsubmit.p":U) ON hAppServerConnection
                                      (pcEntityName,
                                       INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                       INPUT-OUTPUT pcContext,
                                       INPUT-OUTPUT lcParameter,
                                       INPUT-OUTPUT DATASET-HANDLE ContextDatasetHandle BY-REFERENCE) .

        END.

        FINALLY:

            {Consultingwerk/OERA/delete-dataset.i hChangesDataset}

        END FINALLY.

     END METHOD.

END CLASS.