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
    File        : sainvoketaskasync.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 08 20:10:49 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

        DEFINE VARIABLE lcParameter    AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE hAppServer     AS HANDLE   NO-UNDO .

        DEFINE VARIABLE hRequest       AS HANDLE   NO-UNDO .
        DEFINE VARIABLE hHandler       AS HANDLE   NO-UNDO .

        DEFINE VARIABLE lcDataset1     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcDataset2     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcDataset3     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcDataset4     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcDataset5     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcContext      AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcParameterOut AS LONGCHAR NO-UNDO .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcTaskName) .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        IF VALID-OBJECT (poParameter) THEN
            ASSIGN lcParameter = poParameter:Serialize () .

        RUN Consultingwerk/OERA/saasynctaskhandler.p
                PERSISTENT SET hHandler
                        (THIS-OBJECT,
                         poContext,
                         poParameter,
                         phDataset1,
                         phDataset2,
                         phDataset3,
                         phDataset4,
                         phDataset5) .

        FIX-CODEPAGE (lcDataset1) = "utf-8":U .
        FIX-CODEPAGE (lcDataset2) = "utf-8":U .
        FIX-CODEPAGE (lcDataset3) = "utf-8":U .
        FIX-CODEPAGE (lcDataset4) = "utf-8":U .
        FIX-CODEPAGE (lcDataset5) = "utf-8":U .
        FIX-CODEPAGE (lcContext) = "utf-8":U .

        IF VALID-HANDLE (phDataset1) THEN
            phDataset1:WRITE-XML ("LONGCHAR":U, lcDataset1, FALSE, "utf-8":U, ?, TRUE) .
        IF VALID-HANDLE (phDataset2) THEN
            phDataset2:WRITE-XML ("LONGCHAR":U, lcDataset2, FALSE, "utf-8":U, ?, TRUE) .
        IF VALID-HANDLE (phDataset3) THEN
            phDataset3:WRITE-XML ("LONGCHAR":U, lcDataset3, FALSE, "utf-8":U, ?, TRUE) .
        IF VALID-HANDLE (phDataset4) THEN
            phDataset4:WRITE-XML ("LONGCHAR":U, lcDataset4, FALSE, "utf-8":U, ?, TRUE) .
        IF VALID-HANDLE (phDataset5) THEN
            phDataset5:WRITE-XML ("LONGCHAR":U, lcDataset5, FALSE, "utf-8":U, ?, TRUE) .
         IF VALID-HANDLE (phContextDataset) THEN
            phContextDataset:WRITE-XML ("LONGCHAR":U, lcContext, FALSE, "utf-8":U, ?, TRUE) .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcTaskName), 0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIinvokeTaskAsync.p":U)
                    ON hAppServer
                    ASYNCHRONOUS
                    SET hRequest
                    EVENT-PROCEDURE "AsyncTaskCompleted":U IN hHandler

                                            (INPUT pcTaskName,
                                             INPUT pcMethodName,
                                             {1},
                                             lcDataset1,
                                             lcDataset2,
                                             lcDataset3,
                                             lcDataset4,
                                             lcDataset5,
                                             lcParameter,
                                             lcContext,
                                             OUTPUT lcDataset1,
                                             OUTPUT lcDataset2,
                                             OUTPUT lcDataset3,
                                             OUTPUT lcDataset4,
                                             OUTPUT lcDataset5,
                                             OUTPUT lcParameterOut) .
            {Consultingwerk/OERA/callloop-end.i}

            /* It's actually not expected, that this here is ever true - as
               Async calls require an actual AppServer connection */
            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        RETURN hRequest .

        CATCH err AS Progress.Lang.Error:
            /* Mike Fechner, Consultingwerk Ltd. 01.11.2014
               When we error here, the request will not be returned, so we won't need the handler */
            DELETE OBJECT hHandler .

            UNDO, THROW err .
        END CATCH.
