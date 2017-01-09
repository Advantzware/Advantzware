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
    File        : SmartException
    Purpose     : Exception class that retrieves the message text from 
                  the IMessageProvider service
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 05 13:35:34 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.Exceptions.* FROM PROPATH . 
USING Progress.Lang.*                            FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH.

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Exceptions.SmartException
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     "":U, "":U, "":U, "":U, "":U,
                     "":U, "":U, "":U, "":U) .
        
    END CONSTRUCTOR .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, "":U, "":U, "":U, "":U,
                     "":U, "":U, "":U, "":U) .
        
    END CONSTRUCTOR .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, pcParameter2, "":U, "":U, "":U,
                     "":U, "":U, "":U, "":U) .
        
    END CONSTRUCTOR .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER,
                                       pcParameter3 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, pcParameter2, pcParameter3, "":U, "":U,
                     "":U, "":U, "":U, "":U) .
        
    END CONSTRUCTOR .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER,
                                       pcParameter3 AS CHARACTER,
                                       pcParameter4 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, pcParameter2, pcParameter3, pcParameter4, "":U,
                     "":U, "":U, "":U, "":U) .
        
    END CONSTRUCTOR .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER,
                                       pcParameter3 AS CHARACTER,
                                       pcParameter4 AS CHARACTER,
                                       pcParameter5 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5,
                     "":U, "":U, "":U, "":U) .
        
    END CONSTRUCTOR .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER,
                                       pcParameter3 AS CHARACTER,
                                       pcParameter4 AS CHARACTER,
                                       pcParameter5 AS CHARACTER,
                                       pcParameter6 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5,
                     pcParameter6, "":U, "":U, "":U) .
        
    END CONSTRUCTOR .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER,
                                       pcParameter3 AS CHARACTER,
                                       pcParameter4 AS CHARACTER,
                                       pcParameter5 AS CHARACTER,
                                       pcParameter6 AS CHARACTER,
                                       pcParameter7 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5,
                     pcParameter6, pcParameter7, "":U, "":U) .
        
    END CONSTRUCTOR .
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER,
                                       pcParameter3 AS CHARACTER,
                                       pcParameter4 AS CHARACTER,
                                       pcParameter5 AS CHARACTER,
                                       pcParameter6 AS CHARACTER,
                                       pcParameter7 AS CHARACTER,
                                       pcParameter8 AS CHARACTER):
        
        THIS-OBJECT (pcMessageGroup, piMessageNumber,
                     pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5,
                     pcParameter6, pcParameter7, pcParameter8, "":U) .
        
    END CONSTRUCTOR .
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartException class
        Notes:
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
        @param pcParameter9 The nineth message parameter            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartException (pcMessageGroup AS CHARACTER,
                                       piMessageNumber AS INTEGER,
                                       pcParameter1 AS CHARACTER,
                                       pcParameter2 AS CHARACTER,
                                       pcParameter3 AS CHARACTER,
                                       pcParameter4 AS CHARACTER,
                                       pcParameter5 AS CHARACTER,
                                       pcParameter6 AS CHARACTER,
                                       pcParameter7 AS CHARACTER,
                                       pcParameter8 AS CHARACTER,
                                       pcParameter9 AS CHARACTER):
        
        SUPER () .

        DEFINE VARIABLE oMessageProvider AS IMessageProvider NO-UNDO .
        
        &IF DEFINED (SmartFramework) NE 0 &THEN
        ASSIGN oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider 
                                                                "NEW MessageProvider ()"} .
        &ELSE
        ASSIGN oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .
        &ENDIF
        
        IF NOT VALID-OBJECT (oMessageProvider) THEN 
            THIS-OBJECT:AddMessage (MessageFormatter:GetMessage (pcMessageGroup, piMessageNumber,
                                                                 pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5,
                                                                 pcParameter6, pcParameter7, pcParameter8, pcParameter9), piMessageNumber) .
        ELSE 
            THIS-OBJECT:AddMessage (oMessageProvider:GetMessageText (pcMessageGroup, piMessageNumber,
                                                                     pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5,
                                                                     pcParameter6, pcParameter7, pcParameter8, pcParameter9), piMessageNumber) .
        
    END CONSTRUCTOR.


END CLASS.