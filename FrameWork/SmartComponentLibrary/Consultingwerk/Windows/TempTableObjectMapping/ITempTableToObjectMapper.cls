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
    File        : ITempTableToObjectMapper
    Purpose     : Interface for classes that allow to copy from temp-table 
                  fields to .NET value objects
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 19 08:53:23 CET 2014
    Notes       : SCL-591
                  http://jira.consultingwerkcloud.com:8090/display/SCL/.NET+Value+object+to+ABL+Temp-Table+mapping
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

INTERFACE Consultingwerk.Windows.TempTableObjectMapping.ITempTableToObjectMapper:  
  
    /*------------------------------------------------------------------------------
        Purpose: Copies the field values form the buffer to the value object
        Notes:
        @param phBuffer The handle of the source buffer
        @param poObject The reference to the target object
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CopyFromBuffer (phBuffer AS HANDLE, 
                                       poObject AS System.Object,
                                       pcProperties AS CHARACTER) .  

    /*------------------------------------------------------------------------------
        Purpose: Copies the field values form the buffer to the value object
        Notes:   Also for new records (plNewRecord = TRUE) we expect that in phBuffer 
                 a record has already been created
                 On 10.2B, the 4th parameter, is a IDictionary, as the IReadOnlyDictionary 
                 is not available on .NET 2.0. On OE11 the IReadOnlyDictionary provides
                 wider access to this method 
        @param poObject The reference to the target object
        @param phBuffer The handle of the source buffer
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
        @param poBeforeValues An optional dictionary with values to be assigned to the before temp-table row 
        @param plNewRecord Logical value indicating if this routine will be used to create a new record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CopyFromObject (poObject AS System.Object,
                                       phBuffer AS HANDLE, 
                                       pcProperties AS CHARACTER,
&IF PROVERSION BEGINS "10.2":U &THEN
                                       poBeforeValues AS "System.Collections.Generic.IDictionary<CHARACTER,System.Object>",
&ELSE
                                       poBeforeValues AS "System.Collections.Generic.IReadOnlyDictionary<CHARACTER,System.Object>",
&ENDIF                                       
                                       plNewRecord AS LOGICAL) .  

    /*------------------------------------------------------------------------------
        Purpose: Finds the temp-table buffer based on the field values from the value object
        Notes:
        @param phBuffer The handle of the source buffer
        @param poObject The reference to the target object
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
        @return Logical value indicating if the record was found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FindBuffer (phBuffer AS HANDLE, 
                                      poObject AS System.Object,
                                      pcProperties AS CHARACTER) .  
                                      
    /*------------------------------------------------------------------------------
        Purpose: Finds the temp-table buffer based on the field values from the value object
        Notes:
        @param phBuffer The handle of the source buffer
        @param poObject The reference to the target object
        @param poMissingValues The dictionary of character values for index fields from the phBuffer not present in phValueBuffer
        @param pcProperties The field mapping in the same form as expected by the TempTableFromTypeFactory:CreateTempTable() method
        @return Logical value indicating if the record was found        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FindBuffer (phBuffer AS HANDLE, 
                                      poObject AS System.Object,
                                      poMissingValues AS CharacterDictionary, 
                                      pcProperties AS CHARACTER) . 
                                       
END INTERFACE.
