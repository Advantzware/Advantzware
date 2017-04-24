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
    File        : sainvoketask.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 08 20:10:49 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

        DEFINE VARIABLE lcParameter     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE hAppServer      AS HANDLE   NO-UNDO .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcTaskName) .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        IF VALID-OBJECT (poParameter) THEN
            ASSIGN lcParameter = poParameter:Serialize () .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcTaskName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIinvokeTask.p":U) ON hAppServer
                                            (INPUT pcTaskName,
                                             INPUT pcMethodName,
                                             {1},
                                             INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                             INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                             INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                             INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                             INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                             INPUT-OUTPUT lcParameter,
                                             INPUT-OUTPUT DATASET-HANDLE phContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        IF VALID-OBJECT (poParameter) AND lcParameter > "":U THEN
            poParameter:Deserialize (lcParameter) .
        ELSE IF lcParameter > "":U THEN
            poParameter = Consultingwerk.Serializable:DeserializeInstance (lcParameter) .

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

        /* Ignore error:
           Cannot delete a BY-REFERENCE PARAMETER dataset or table in the called procedure. (12327) */
        CATCH err AS Progress.Lang.SysError:
            IF err:GetMessageNum (1) = 12327 THEN .
            ELSE UNDO, THROW err .
        END CATCH.

        /* Mike Fechner, Consultingwerk Ltd. 08.09.2011
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .

            IF VALID-HANDLE (phDataset1) THEN
                DELETE OBJECT phDataset1 NO-ERROR .
            IF VALID-HANDLE (phDataset2) THEN
                DELETE OBJECT phDataset2 NO-ERROR .
            IF VALID-HANDLE (phDataset3) THEN
                DELETE OBJECT phDataset3 NO-ERROR .
            IF VALID-HANDLE (phDataset4) THEN
                DELETE OBJECT phDataset4 NO-ERROR .
            IF VALID-HANDLE (phDataset5) THEN
                DELETE OBJECT phDataset5 NO-ERROR .
        END FINALLY.