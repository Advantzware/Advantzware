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
    File        : ITestRunner
    Purpose     : Interface for TestRunner implementations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Mar 24 15:46:12 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartUnit.*            FROM PROPATH . 
USING Consultingwerk.SmartUnit.TestRunner.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

INTERFACE Consultingwerk.SmartUnit.TestRunner.ITestRunner:  

    {Consultingwerk/SmartUnit/dsTestsuites.i}
  
    /*------------------------------------------------------------------------------
        Purpose: Executes the test suite described by the ITestSuite instance
        Notes:   
        @param poTestSuite The ITestSuite to execute
        @return Logical value, when set to false halt will be signalled to the caller (ANT script)  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL Execute (poTestSuite AS ITestSuite).

END INTERFACE.