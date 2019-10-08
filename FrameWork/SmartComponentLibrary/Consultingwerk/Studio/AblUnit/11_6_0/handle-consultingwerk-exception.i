/**********************************************************************
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
    File        : handle-consultingwerk-exception.i
    Purpose     : Handles Consultingwerk AssertException within ABLUnit
                  CATCH blocks

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 27 13:40:15 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

        CATCH consultingwerkaex AS Consultingwerk.Assertion.AssertException:
            testResult:testResult = TestTestResult:StatusFailed.
            testResult:ErrorMessage = IF consultingwerkaex:ReturnValue > "" THEN consultingwerkaex:ReturnValue ELSE consultingwerkaex:GetMessage(1).
            testResult:Error = consultingwerkaex.
            testResult:TestName = testMethod:getTestName().
            testResult:isIncomplete = TRUE.
        END CATCH.
