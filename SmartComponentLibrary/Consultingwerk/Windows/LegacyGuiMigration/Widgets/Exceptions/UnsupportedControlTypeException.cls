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
    File        : UnsupportedControlTypeException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:51:09 CEST 2015
    Notes       : Non SERIALIZABLE, as the reference to the original .NET
                  Control is not serializable 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                                    FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .
USING System.Windows.Forms.*                                         FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Exceptions.UnsupportedControlTypeException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the .NET Control associated with this exception
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Control AS Control NO-UNDO 
    GET.
    PRIVATE SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException ():
    
        SUPER ().        
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param poControl The reference to the .NET Control that cause this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (poControl AS Control):
    
        SUPER ().        
    
        THIS-OBJECT:Control = poControl .
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (pcErrorString AS CHARACTER,
                                                        piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
                        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param poControl The reference to the .NET Control that cause this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (pcErrorString AS CHARACTER,
                                                        piMessageNumber AS INTEGER,
                                                        poControl AS Control):        
    
        SUPER (pcErrorString, piMessageNumber).
    
        THIS-OBJECT:Control = poControl . 
    
    END CONSTRUCTOR.
                        
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (pcErrorString AS CHARACTER):        
        
        SUPER (pcErrorString).

    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param poControl The reference to the .NET Control that cause this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (pcErrorString AS CHARACTER,
                                                        poControl AS Control):        
        
        SUPER (pcErrorString).

        THIS-OBJECT:Control = poControl . 

    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (poInnerException AS Error):
    
        SUPER (poInnerException).        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param poControl The reference to the .NET Control that cause this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (poInnerException AS Error,
                                                        poControl AS Control):
    
        SUPER (poInnerException).        
        
        THIS-OBJECT:Control = poControl . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (poInnerException AS Error,
                                                        pcErrorString AS CHARACTER,
                                                        piMessageNumber AS INTEGER):        
        
        THIS-OBJECT (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param poControl The reference to the .NET Control that cause this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (poInnerException AS Error,
                                                        pcErrorString AS CHARACTER,
                                                        piMessageNumber AS INTEGER,
                                                        poControl AS Control):        
        
        THIS-OBJECT (poInnerException, pcErrorString, piMessageNumber).
        
        THIS-OBJECT:Control = poControl .
                
    END CONSTRUCTOR.
                                       
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (poInnerException AS Error,
                                                        pcErrorString AS CHARACTER):        
        
        SUPER (poInnerException, pcErrorString).
        
    END CONSTRUCTOR.           
 
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedControlTypeException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param poControl The reference to the .NET Control that cause this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedControlTypeException (poInnerException AS Error,
                                                        pcErrorString AS CHARACTER,
                                                        poControl AS Control):        
        
        SUPER (poInnerException, pcErrorString).
        
        THIS-OBJECT:Control = poControl . 
        
    END CONSTRUCTOR.           

END CLASS.
