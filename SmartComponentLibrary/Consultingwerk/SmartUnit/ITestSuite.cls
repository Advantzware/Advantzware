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
    File        : ITestSuite
    Purpose     : Interface describing structures for the Test Suite
    Syntax      : 
    Description : This is the parameter class for the ITestRunner implementation
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Mar 24 15:48:56 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartUnit.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH.

INTERFACE Consultingwerk.SmartUnit.ITestSuite:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns if on Windows the XmlXref based ClassAnnotationParser 
                 should be used to evaluate the class annotations
        Notes:   By default on Windows the proparse based ClassAnnotationParser will 
                 be used    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ForceXmlXref AS LOGICAL NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the client logfile of the test run should be imported 
                 into the JUnit output
        Notes:   Allows turning this feature off, to avoid memory issues with very
                 large log files
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImportClientLog AS LOGICAL NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the std output logfile of the test run should be imported 
                 into the JUnit output
        Notes:   Allows turning this feature off, to avoid memory issues with very
                 large log files    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImportStdOut AS LOGICAL NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the current job should be stopped after an error has
                 occured 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY HaltOnError AS LOGICAL NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the output file
        Notes:      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Output AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of service definition files that should be 
                 loaded prior to the test run 
        Notes:   comma delimited list of relative XML file names
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Services AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the pattern for the test classes to execute
        Notes:   Comma delimited list of class names or package/class name matches   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tests AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Name of the Test Suite as it will appear in the JUnit output 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TestSuite AS CHARACTER NO-UNDO 
	GET.

END INTERFACE.