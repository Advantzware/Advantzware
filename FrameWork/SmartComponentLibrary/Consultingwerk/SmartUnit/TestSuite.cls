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
    File        : TestSuite
    Purpose     : Parameter Structure for the ITestRunner
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Mar 24 15:55:39 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH . 
USING Consultingwerk.SmartUnit.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.SmartUnit.TestSuite 
    INHERITS XmlSerializable
    IMPLEMENTS ITestSuite: 

    /*------------------------------------------------------------------------------
        Purpose: Returns if on Windows the XmlXref based ClassAnnotationParser 
                 should be used to evaluate the class annotations
        Notes:   By default on Windows the proparse based ClassAnnotationParser will 
                 be used    
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i ForceXmlXref LOGICAL}

    /*------------------------------------------------------------------------------
        Purpose: Returns if the client logfile of the test run should be imported 
                 into the JUnit output
        Notes:   Allows turning this feature off, to avoid memory issues with very
                 large log files
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i ImportClientLog LOGICAL "INIT TRUE"}

    /*------------------------------------------------------------------------------
        Purpose: Returns if the std output logfile of the test run should be imported 
                 into the JUnit output
        Notes:   Allows turning this feature off, to avoid memory issues with very
                 large log files    
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i ImportStdOut LOGICAL "INIT TRUE"}

    /*------------------------------------------------------------------------------
        Purpose: Returns if the current job should be stopped after an error has
                 occurred 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i HaltOnError LOGICAL}


    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the output file
        Notes:      
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i Output CHARACTER}


    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of service definition files that should be 
                 loaded prior to the test run 
        Notes:   comma delimited list of relative XML file names
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i Services CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Returns the pattern for the test classes to execute
        Notes:   Comma delimited list of class names or package/class name matches   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i Tests CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Returns the Name of the Test Suite as it will appear in the JUnit output 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i TestSuite CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TestSuite class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TestSuite ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
    END CONSTRUCTOR.

END CLASS.
