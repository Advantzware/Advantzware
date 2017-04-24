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
    File        : handle-consultingwerk-exception2.i
    Purpose     : Handles Consultingwerk AssertException within ABLUnit
                  CATCH blocks for Progress.Lang.Error IF THEN ELSE
                  blocks

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 27 13:40:15 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

                /* Use TYPE-OF rather than Progress' class _NAME_ matching */
                IF TYPE-OF (e, Consultingwerk.Assertion.AssertException) THEN
                DO:
                    {1}:TestResult  = TestTestResult:StatusFailed.
                    {1}:TestName = {2}:getTestName().
                    {1}:Error = e.
                    {1}:ErrorMessage = e:GetMessage(1).
                END.
                ELSE
