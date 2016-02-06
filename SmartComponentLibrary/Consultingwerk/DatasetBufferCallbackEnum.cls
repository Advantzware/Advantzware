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
    Purpose     : Enum for ProDataset Buffer Callback Names
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

CLASS Consultingwerk.DatasetBufferCallbackEnum: 

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
        Purpose: This event occurs after a record is created in the temp-table. The 
                 procedure can, for example, modify field values in the record by 
                 supplying values for calculated fields, or perform filtering and 
                 reject a record by deleting it. The procedure cannot modify record 
                 currency using the ProDataSet object buffers in any other way. It 
                 can use separately defined buffers to modify the ProDataSet object 
                 in other ways. The procedure can RETURN ERROR to abort the entire 
                 FILL, or RETURN NO-APPLY to cancel the cascading of the FILL to 
                 child buffers, if any.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY AfterRowFill AS CHARACTER NO-UNDO INIT "AFTER-ROW-FILL":U  
    GET. 


    /*------------------------------------------------------------------------------
        Purpose: This event occurs at the very beginning of a FILL, before anything 
                 is read or created.
        Notes:   For a Buffer object, this event allows the developer to do 
                 preparatory work for an individual table. For the parent table in a 
                 set of related tables, where the FILL event is applied to this 
                 top-level table, it could be the same kind of connection code as 
                 for the ProDataSet object as a whole. For a child table, the event 
                 occurs once for each parent record that is created, and allows the 
                 developer to adjust the query for the child table, or cancel the 
                 FILL for that parent altogether.                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY BeforeFill AS CHARACTER NO-UNDO INIT "BEFORE-FILL":U
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: This event occurs before a record is created in the temp-table, but 
                 after the data source record(s) for it are read. For example, this 
                 procedure could examine the database buffers or other information 
                 and decide not to create the record, using a RETURN NO-APPLY statement.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY BeforeRowFill AS CHARACTER NO-UNDO INIT "BEFORE-ROW-FILL":U 
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: This event occurs immediately after the record is created in the 
                 temp-table. The current buffer for the temp-table is available and 
                 contains initial values as defined in the temp-table definition 
                 (or inherited from the schema). You can use this event to calculate 
                 initial values for fields, make changes to other records, or reject 
                 the creation by deleting the new temp-table record.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RowCreate AS CHARACTER NO-UNDO INIT "ROW-CREATE":U 
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: This event occurs when you delete a temp-table record, immediately 
                 before the record is deleted. The event procedure can use this event 
                 to RETURN ERROR to cancel the delete, or to make adjustments to 
                 other records based on the delete. Since the record has not yet been 
                 deleted, the record is in the temp-table buffer and the code can 
                 look at its values. Because the code can assume that the DELETE will 
                 go through unless cancelled by the event procedure itself, it can 
                 take actions based on the record deletion while the record is still 
                 there to be looked at.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RowDelete AS CHARACTER NO-UNDO INIT  "ROW-DELETE":U 
    GET.   
  
    /*------------------------------------------------------------------------------
        Purpose: This event occurs immediately before the record is updated in the 
                 temp-table. It typically occurs when: 
                 - The buffer scope ends
                 - The transaction scope ends
                 - The RELEASE statement or BUFFER-RELEASE() method is run on the buffer
                 - The buffer is needed for another record
        Notes:   The AVM sets the SELF system handle to the handle of the buffer on 
                 which the event procedure is running before calling the event handler. 
                 If the event handler returns NO-APPLY or ERROR, the return is ignored. 
                 The handler has run, and it is too late to undo any changes to the 
                 record. 
                 You can use this event to determine if and how a record has changed 
                 by reading the buffer in the before-image table (using the SELF:BEFORE-ROWID 
                 attribute) and comparing it to the updated buffer. You can also use 
                 this event in the event handler to update fields in the record (for 
                 example, to supply a calculated field). 
                 You cannot read another record into the buffer on which the event 
                 procedure is running in the event handler. If you need to read 
                 another record, use a different buffer to avoid disturbing the record 
                 you are currently updating.  
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RowUpdate AS CHARACTER NO-UNDO INIT "ROW-UPDATE":U
    GET.   
    
    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DatasetBufferCallbackEnum ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the specified callback name is valid                                                                      
        Notes:                                                  
        @param pcValue The value to verify
        @return Logical value indicating if the value is valid                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsValid (pcValue AS CHARACTER):
        
        IF INDEX ("AFTER-FILL,AFTER-ROW-FILL,BEFORE-FILL,BEFORE-ROW-FILL,ROW-CREATE,ROW-DELETE,ROW-UPDATE":U, pcValue) > 0 THEN 
            RETURN TRUE . 
            
        RETURN FALSE .     

    END METHOD.

END CLASS.