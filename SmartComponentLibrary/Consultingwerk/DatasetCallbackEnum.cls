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
    File        : DatasetCallback
    Purpose     : Enum for ProDataset Callback Names
    Syntax      : 
    Description : Reduce risk of typos in ProDataset SET-CALLBACK method 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 09 08:31:24 CEST 2011
    Notes       : Enum names are without - for consistency with coding stlye
                  However the values will return the proper form ABL constant
                  including the dash ("-") 
                  
                  This Enum type is purposely implemented using CHARACTER 
                  static properties and not our Enum base class / EnumMember.i
                  include file - because the SET-CALLBACK method does implicitely
                  use the ToString() method and thus would cause compilation 
                  issues.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*.

CLASS Consultingwerk.DatasetCallbackEnum: 

    /*------------------------------------------------------------------------------
        Purpose: This event occurs at the very end of a FILL, and can be used to 
                 adjust the contents of the ProDataSet object or Buffer object, reject 
                 the FILL operation, or disconnect from a server or database. For a 
                 child table, the event occurs once for each parent record that is 
                 created.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY AfterFill AS CHARACTER NO-UNDO INIT "AFTER-FILL":U 
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: This event occurs at the very beginning of a FILL, before anything 
                 is read or created.                                                                      
        Notes:   For a ProDataSet object, this event allows the developer to make a 
                 server or database connection, or do other preparatory work. 
                 Alternatively, it allows the developer to intercept and fully 
                 replace the default behavior.                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY BeforeFill AS CHARACTER NO-UNDO INIT "BEFORE-FILL":U
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DatasetCallbackEnum ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the specified callback name is valid                                                                     
        Notes:                                                  
        @param pcValue The value to verify
        @return Logical value indicating if the value is valid                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsValid (pcValue AS CHARACTER):
        
        IF INDEX ("AFTER-FILL,BEFORE-FILL":U, pcValue) > 0 THEN 
            RETURN TRUE . 
            
        RETURN FALSE .     

    END METHOD.

END CLASS.