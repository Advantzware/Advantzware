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
    File        : proSIretrieve1.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 04 15:40:40 CEST 2009
    Notes       : This file is no longer used in the SmartComponent
                  Library. The ServiceAdapter now handles the array
                  parameters required for proSIretrieve.p.

                  This file is left part of the SmartComponent Library
                  in case it's still used by customers. However it is
                  recommended that customers change their code to access
                  the ServiceAdapter from code on the client and the
                  ServiceInterface or proSIretrieve.p for code run on the
                  server.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT         PARAMETER pcEntity       AS CHARACTER NO-UNDO .
DEFINE INPUT         PARAMETER pcTables       AS CHARACTER NO-UNDO .
DEFINE INPUT         PARAMETER pcQueries      AS CHARACTER NO-UNDO .
DEFINE INPUT         PARAMETER pcJoins        AS CHARACTER NO-UNDO .
DEFINE INPUT         PARAMETER pcPositions    AS CHARACTER NO-UNDO .
DEFINE INPUT         PARAMETER pcRequests     AS CHARACTER NO-UNDO .
DEFINE INPUT         PARAMETER pcBatchContext AS CHARACTER NO-UNDO .
DEFINE INPUT         PARAMETER plFillBatch    AS LOGICAL   NO-UNDO .
DEFINE INPUT         PARAMETER piStopAfter    AS INTEGER   NO-UNDO .
DEFINE INPUT-OUTPUT  PARAMETER pcNumRecords   AS CHARACTER NO-UNDO .
DEFINE INPUT-OUTPUT  PARAMETER plcParameter   AS LONGCHAR  NO-UNDO .
DEFINE INPUT         PARAMETER plcNamedQuery  AS LONGCHAR  NO-UNDO .

DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSet .

DEFINE INPUT-OUTPUT  PARAMETER pcContext      AS CHARACTER NO-UNDO .
DEFINE OUTPUT        PARAMETER pcPrevContext  AS CHARACTER NO-UNDO .
DEFINE OUTPUT        PARAMETER pcNextContext  AS CHARACTER NO-UNDO .

DEFINE VARIABLE cEntity       AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cTables       AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cQueries      AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cJoins        AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cPositions    AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cRequests     AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cNumRecords   AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE lcParameter   AS LONGCHAR  NO-UNDO EXTENT 1.
DEFINE VARIABLE lcNamedQuery  AS LONGCHAR  NO-UNDO EXTENT 1.
DEFINE VARIABLE cContext      AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cPrevContext  AS CHARACTER NO-UNDO EXTENT 1.
DEFINE VARIABLE cNextContext  AS CHARACTER NO-UNDO EXTENT 1.

DEFINE VARIABLE hDataset      AS HANDLE    NO-UNDO EXTENT 10.

DEFINE VARIABLE hContextDataset AS HANDLE NO-UNDO .

{Consultingwerk/products.i}

/* ***************************  Main Block  *************************** */

ASSIGN
    cEntity[1]      = pcEntity
    cTables[1]      = pcTables
    cQueries[1]     = pcQueries
    cJoins[1]       = pcJoins
    cPositions[1]   = pcPositions
    cRequests[1]    = pcRequests
    cNumRecords[1]  = pcNumRecords
    cContext[1]     = pcContext
    cPrevContext[1] = pcPrevContext
    cNextContext[1] = pcNextContext
    lcParameter[1]  = plcParameter
    lcNamedQuery[1] = plcNamedQuery .

RUN {&OERASI}/proSIretrieve.p
          (cEntity,
           cTables,
           cQueries,
           cJoins,
           cPositions,
           cRequests,
           pcBatchContext,
           plFillBatch,
           piStopAfter,
           INPUT-OUTPUT cNumRecords,
           INPUT-OUTPUT lcParameter,
           lcNamedQuery,
           OUTPUT DATASET-HANDLE hDataset[1] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[2] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[3] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[4] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[5] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[6] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[7] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[8] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[9] APPEND BY-REFERENCE,
           OUTPUT DATASET-HANDLE hDataset[10] APPEND BY-REFERENCE,
           INPUT-OUTPUT cContext,
           OUTPUT cPrevContext,
           OUTPUT cNextContext,
           INPUT-OUTPUT DATASET-HANDLE hContextDataset BY-REFERENCE)  NO-ERROR .

IF ERROR-STATUS:ERROR THEN
    RETURN ERROR RETURN-VALUE .


ASSIGN
    pcNumRecords  = cNumRecords[1]
    pcContext     = cContext[1]
    pcPrevContext = cPrevContext[1]
    pcNextContext = cNextContext[1]
    plcParameter  = lcParameter[1] .

ASSIGN phDataset = hDataset[1] .

/* Delete datasets (delayed until this procedure terminates */

IF VALID-HANDLE (phDataset) THEN
    DELETE OBJECT phDataset .

IF VALID-HANDLE(hDataset[2]) THEN
    DELETE OBJECT hDataset[2] .

IF VALID-HANDLE(hDataset[3]) THEN
    DELETE OBJECT hDataset[3] .

IF VALID-HANDLE(hDataset[4]) THEN
    DELETE OBJECT hDataset[4] .

IF VALID-HANDLE(hDataset[5]) THEN
    DELETE OBJECT hDataset[5] .

IF VALID-HANDLE(hDataset[6]) THEN
    DELETE OBJECT hDataset[6] .

IF VALID-HANDLE(hDataset[7]) THEN
    DELETE OBJECT hDataset[7] .

IF VALID-HANDLE(hDataset[8]) THEN
    DELETE OBJECT hDataset[8] .

IF VALID-HANDLE(hDataset[9]) THEN
    DELETE OBJECT hDataset[9] .

IF VALID-HANDLE(hDataset[10]) THEN
    DELETE OBJECT hDataset[10] .

FINALLY:
    IF VALID-HANDLE (hContextDataset) THEN
        DELETE OBJECT hContextDataset NO-ERROR .
END FINALLY.
