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
    File        : proSIinvokeTask.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 20 21:10:58 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT        PARAMETER pcTaskName    AS CHARACTER NO-UNDO .
DEFINE INPUT        PARAMETER pcMethodName  AS CHARACTER NO-UNDO .
DEFINE INPUT        PARAMETER piNumDatasets AS INTEGER   NO-UNDO .
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet1 .
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet2 .
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet3 .
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet4 .
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet5 .
DEFINE INPUT-OUTPUT PARAMETER plcParameter AS LONGCHAR NO-UNDO .
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phContextDataset .

DEFINE VARIABLE lContextDatasetAssigned AS LOGICAL NO-UNDO INITIAL FALSE .

{ Consultingwerk/products.i }

/* Mike Fechner, Consultingwerk Ltd. 08.05.2013
   Support for custom include files to the proSI... procedures.
   This allows adding SHARED variable definitions that may be
   required to execute legacy database triggers */
&IF "{&ProSIcustomIncludeDirectory}":U NE "":U &THEN
{ {&ProSIcustomIncludeDirectory}/proSIinvokeTaskCustom.i }
&ENDIF

/* ***************************  Main Block  *************************** */

IF VALID-HANDLE (phContextDataset) THEN
    ASSIGN Consultingwerk.Framework.Session.SessionManager:ContextDataset = phContextDataset
           lContextDatasetAssigned                                        = TRUE .

Consultingwerk.OERA.ServiceInterface:InvokeTask (pcTaskName,
                                                 pcMethodName,
                                                 piNumDatasets,
                                                 INPUT-OUTPUT DATASET-HANDLE phDataset1 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE phDataset2 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE phDataset3 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE phDataset4 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE phDataset5 BY-REFERENCE,
                                                 INPUT-OUTPUT plcParameter).

{ {&OERASI}/sicatch.i }

FINALLY:
    IF VALID-HANDLE (phDataset1) THEN
        DELETE OBJECT phDataset1 .
    IF VALID-HANDLE (phDataset2) THEN
        DELETE OBJECT phDataset2 .
    IF VALID-HANDLE (phDataset3) THEN
        DELETE OBJECT phDataset3 .
    IF VALID-HANDLE (phDataset4) THEN
        DELETE OBJECT phDataset4 .
    IF VALID-HANDLE (phDataset5) THEN
        DELETE OBJECT phDataset5 .
    IF VALID-HANDLE (phContextDataset) THEN
        DELETE OBJECT phContextDataset NO-ERROR .

    /* Mike Fechner, Consultingwerk Ltd. 23.10.2011
       Only reset the ContextDataset when it was set by this instance
       (avoid issues with call nesting) */
    IF lContextDatasetAssigned AND SESSION:REMOTE THEN
        Consultingwerk.Framework.Session.SessionManager:ContextDataset = ?  .
END FINALLY.
