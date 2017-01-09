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
    File        : TestResult
    Purpose     : Result structrure for a single test run
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 25 20:34:40 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.SmartUnit.TestRunner.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.SmartUnit.TestRunner.TestResult 
    INHERITS XmlSerializable: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ErrorMessage of the Test Result 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i ErrorMessage CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Error Stack Trace of the Test Result 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i ErrorStacktrace CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ErrorType of the Test Result 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i ErrorType CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ErrorType of the Test Result 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i ExecutionTime DECIMAL "INIT ?"}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the SystemErr of the Test Result  
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i SystemErr LONGCHAR}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the SystemOut of the Test Result  
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/SerializableProperty.i SystemOut LONGCHAR}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TestSuite class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TestResult ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
    END CONSTRUCTOR.

END CLASS.
