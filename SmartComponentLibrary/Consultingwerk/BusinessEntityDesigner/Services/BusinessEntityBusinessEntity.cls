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
    File        : BusinessEntityBusinessEntity
    Purpose     : Business Entity provides access to business entity designs
                  using bedgm files
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 19:45:41 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Services.* .
USING Consultingwerk.OERA.BusinessEntity.
USING Progress.Lang.*.
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH.

CLASS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity INHERITS BusinessEntity: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityBusinessEntity class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityBusinessEntity ():
        SUPER (DATASET dsBusinessEntity:HANDLE).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and 
                 Update operations (i.e. population of aggregated values)                                                                     
        Notes:   Invoked during FetchData () and SaveChanges ()                                                                     
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():
        
        FOR EACH eField:
            ASSIGN eField.FieldDataType = UPPER (eField.FieldDataType) .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the business entity to a file                                                                        
        Notes:   
        @param dsBusinessEntity The Business Entity dataset
        @param poOpenSaveParameter The OpenSaveParameter with the data for this mether                                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Save (INPUT-OUTPUT DATASET dsBusinessEntity,
                             poOpenSaveParameter AS OpenSaveParameter):
        
        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO . 
        
        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Business Entity":U, poOpenSaveParameter:FileName) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN 
                RETURN .
                
            IF oWriteFileEventArgs:NewFileName > "":U THEN 
                poOpenSaveParameter:FileName = oWriteFileEventArgs:NewFileName . 
        END.

        /* Mike Fechner, Consultingwerk Ltd. 25.11.2010
           Check for write protected file (SCM) */
        FILE-INFO:FILE-NAME = poOpenSaveParameter:FileName .
        
        IF FILE-INFO:FULL-PATHNAME > "":U AND NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to write to protected file: &1"{&TRAN}, FILE-INFO:FULL-PATHNAME), 0) .        
        
        DATASET dsBusinessEntity:WRITE-XML ("FILE":U, poOpenSaveParameter:FileName, TRUE) .

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
