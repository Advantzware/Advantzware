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
    File        : AttachmentBusinessEntity
    Purpose     : Business Entity for Attachment
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 13.01.2013 11:21:55
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH . 
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.AttachmentBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/System/dsAttachment.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AttachmentBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttachmentBusinessEntity ():
        SUPER (DATASET dsAttachment:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.AttachmentDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Attachment record including large objects 
        Notes:   Default of this business etntiy is not to return large objects
        @param dsAttachment INPUT-OUTPUT DATASET The Dataset
        @param poAttachmentKey The CharacterHolder with the Attachment Key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetLargeObjects (INPUT-OUTPUT DATASET dsAttachment,
                                        poAttachmentKey AS CharacterHolder):
        
        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO . 
        
        DATASET dsAttachment:EMPTY-DATASET () .

        ASSIGN oRequest = NEW FetchDataRequest ("eSmartAttachment":U,
                                                SUBSTITUTE ("for each eSmartAttachment where eSmartAttachment.AttachmentGUID = &1":U,
                                                            QUOTER (poAttachmentKey:Value)),
                                                1) 
                                                
               oRequest:CustomContext = "CompleteBinding":U. 

        THIS-OBJECT:FetchData (oRequest) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and 
                 Update operations (i.e. population of aggregated values)                                                                     
        Notes:   Invoked during FetchData () and SaveChanges ()                                                                     
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update 
                 operations                                                                     
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object                                                                      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():
        
    END METHOD.

END CLASS.
