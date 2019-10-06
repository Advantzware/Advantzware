/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dynamic-invoke-workaround.i
    Purpose     : Workaround to DYNAMIC-INVOKE issue in OpenEdge 11.6
                  See SCL-1167

    Syntax      : {Consultingwerk/OERA/dynamic-invoke-workaround.i phDataset1 phDataset2 phDataset3 phDataset4 phDataset5}

    Description : DYNAMIC-INVOKE fails in OpenEdge 11.6 when an object parameter
                  in the callee is receiving ?. See SCL-1167 for details.

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 18 07:01:09 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

            /* Mike Fechner, Consultingwerk Ltd. 16.01.2016
               SCL-1167 - workaround ABL issue with DYNAMIC-INVOKE when the parameter object = ? */
            &IF DEFINED (AblReflection) NE 0 &THEN
            IF NOT VALID-OBJECT (poParameter) THEN DO:

                oMethod = AblReflectionHelper:GetInvokableMethod (oTask:GetClass(), pcMethodName, piNumDatasets) .

                IF NOT VALID-OBJECT (oMethod) THEN
                    UNDO, THROW NEW MethodInvokationException (SUBSTITUTE ("Invalid method &1 in &2"{&TRAN},
                                                                           pcMethodName,
                                                                           pcTaskName),
                                                               0,
                                                               pcMethodName) .

                oParameterList = NEW ParameterList (piNumDatasets + 1) .

                &IF "{1}"  NE "" &THEN oParameterList:SetParameter (1,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {1})  . &ENDIF
                &IF "{2}"  NE "" &THEN oParameterList:SetParameter (2,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {2})  . &ENDIF
                &IF "{3}"  NE "" &THEN oParameterList:SetParameter (3,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {3})  . &ENDIF
                &IF "{4}"  NE "" &THEN oParameterList:SetParameter (4,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {4})  . &ENDIF
                &IF "{5}"  NE "" &THEN oParameterList:SetParameter (5,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {5})  . &ENDIF
                &IF "{6}"  NE "" &THEN oParameterList:SetParameter (6,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {6})  . &ENDIF
                &IF "{7}"  NE "" &THEN oParameterList:SetParameter (7,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {7})  . &ENDIF
                &IF "{8}"  NE "" &THEN oParameterList:SetParameter (8,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {8})  . &ENDIF
                &IF "{9}"  NE "" &THEN oParameterList:SetParameter (9,  "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {9})  . &ENDIF
                &IF "{10}" NE "" &THEN oParameterList:SetParameter (10, "DATASET-HANDLE":U,             IoModeEnum:InputOutput, {10}) . &ENDIF
                oParameterList:SetParameter (piNumDatasets + 1, "CLASS Progress.Lang.Object":U, IoModeEnum:Input,       ?) .

                oMethod:Invoke (oTask, oParameterList).
            END.
            ELSE
            &ENDIF