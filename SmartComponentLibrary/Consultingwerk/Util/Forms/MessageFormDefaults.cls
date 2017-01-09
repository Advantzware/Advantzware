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
    File        : MessageFormDefaults
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 31 12:45:59 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.Enum.* FROM PROPATH .
USING Consultingwerk.Util.Forms.*     FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Util.Forms.MessageFormDefaults: 

    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the labels for an Ok/Cancel message                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LabelOkCancel AS CHARACTER EXTENT 2 NO-UNDO 
    GET (i AS INTEGER):
        CASE i:
            WHEN 1 THEN 
               RETURN MessageFormResources:MessageForButtonOk .
            WHEN 2 THEN 
               RETURN MessageFormResources:MessageForButtonCancel . 
        END CASE .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the labels for an Retry/Cancel message                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LabelRetryCancel AS CHARACTER EXTENT 2 NO-UNDO 
    GET (i AS INTEGER):
        CASE i:
            WHEN 1 THEN 
               RETURN MessageFormResources:MessageForButtonRetry .
            WHEN 2 THEN 
               RETURN MessageFormResources:MessageForButtonCancel . 
        END CASE .      
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the labels for an Yes/No message                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LabelYesNo AS CHARACTER EXTENT 2 NO-UNDO 
    GET (i AS INTEGER):
        CASE i:
            WHEN 1 THEN 
               RETURN MessageFormResources:MessageForButtonYes .
            WHEN 2 THEN 
               RETURN MessageFormResources:MessageForButtonNo . 
        END CASE .      
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the labels for an Yes/No/Cancel message                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LabelYesNoCancel AS CHARACTER EXTENT 3 NO-UNDO 
    GET (i AS INTEGER):
        CASE i:
            WHEN 1 THEN 
               RETURN MessageFormResources:MessageForButtonYes .
            WHEN 2 THEN 
               RETURN MessageFormResources:MessageForButtonNo . 
            WHEN 3 THEN 
               RETURN MessageFormResources:MessageForButtonCancel .
        END CASE .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE MessageFormDefaults ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the DialogResultEnum values for an 
                 Ignore/Cancel message                                                                      
        Notes:                 
        @return The DialogResultEnum array for Ignore, Cancel 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DialogResultEnum EXTENT 2 GetDialogResultIgnoreCancel ():
        
        DEFINE VARIABLE oReturn AS DialogResultEnum EXTENT 2 . 

        ASSIGN oReturn [1] = DialogResultEnum:DialogResultIgnore 
               oReturn [2] = DialogResultEnum:DialogResultCancel . 

        RETURN oReturn . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the DialogResultEnum values for a
                 Ok/Cancel message                                                                      
        Notes:
        @return The DialogResultEnum array for Ok, Cancel                                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DialogResultEnum EXTENT 2 GetDialogResultOkCancel ():
        
        DEFINE VARIABLE oReturn AS DialogResultEnum EXTENT 2 . 

        ASSIGN oReturn [1] = DialogResultEnum:DialogResultOk 
               oReturn [2] = DialogResultEnum:DialogResultCancel . 

        RETURN oReturn . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the DialogResultEnum values for a 
                 Yes/No message                                                                      
         Notes:                  
        @return The DialogResultEnum array for Yes, No
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DialogResultEnum EXTENT 2 GetDialogResultYesNo ():
        
        DEFINE VARIABLE oReturn AS DialogResultEnum EXTENT 2 . 

        ASSIGN oReturn [1] = DialogResultEnum:DialogResultYes 
               oReturn [2] = DialogResultEnum:DialogResultNo . 

        RETURN oReturn . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an array containing the DialogResultEnum values for an 
                 Yes/No/Cancel message                                                                       
        Notes:                                                                        
        @return The DialogResultEnum array for Yes, No, Cancel
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DialogResultEnum EXTENT 3 GetDialogResultYesNoCancel ():
        
        DEFINE VARIABLE oReturn AS DialogResultEnum EXTENT 3 . 

        ASSIGN oReturn [1] = DialogResultEnum:DialogResultYes 
               oReturn [2] = DialogResultEnum:DialogResultNo 
               oReturn [3] = DialogResultEnum:DialogResultCancel . 

        RETURN oReturn . 

    END METHOD.

END CLASS.
