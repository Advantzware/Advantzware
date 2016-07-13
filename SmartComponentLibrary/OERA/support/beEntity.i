/*------------------------------------------------------------------------
    File        : beEntity.i
    Purpose     : define static instance that loads beSupport super
    Syntax      : {support/beEntity.i <EntityName>}

    Description : Include for super bootstrapping 

    Author(s)   : hdaniels
    Created     : Wed May 23 09:28:14 EDT 2007
    Notes       : There must be a dataset include in propath of name 
                 {&schemapath}/{&datasetprefix}{&entityname}.i  
                 default = schema/ds<Entityname>.i
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ Consultingwerk/products.i }
{ {&OERASI}/startsuper.i } 

DEFINE VARIABLE cEntityContext AS CHARACTER NO-UNDO.

/* Allow external named preprocessor, but let numbered arguments override */ 
&if "{1}" <> "":U &then 
  &scop entityname {1}
&endif 

&if "{2}" <> "":U &then 
  &scop schemapath {2}
&elseif defined(schemapath) = 0 &then
  &scop schemapath schema 
&endif 

&if "{3}" <> "":U &then 
  &scop datasetprefix {3}  
&elseif defined(datasetprefix) = 0 &then 
  &scop datasetprefix ds
&endif

&if defined(entityname) <> 0 &then
{{&schemapath}/{&datasetprefix}{&entityname}.i}

/* **************************  Main Block  *************************** */
startSuper('beSupport').

run createObject.
/* ************ Functions ****************************************/   

FUNCTION getContext RETURNS CHARACTER 
	() FORWARD.
	
FUNCTION setContext RETURNS CHARACTER 
	(pcContext AS CHARACTER) FORWARD.

/* ************************  Function Implementations ***************** */

FUNCTION getContext RETURNS CHARACTER ():
/*------------------------------------------------------------------------------
	Purpose: Returns the Buisiness Entities request context  																	  
	Notes:  																	  
------------------------------------------------------------------------------*/	

    RETURN cEntityContext . 
		
END FUNCTION.

function getStaticDataset returns handle():
  return dataset {&datasetprefix}{&entityname}:handle.
end.

function getEntityName returns character():
  return "{&entityname}".
end.

&endif


FUNCTION setContext RETURNS CHARACTER (pcContext AS CHARACTER):
/*------------------------------------------------------------------------------
	Purpose: Set's the request context in the Business Entity  																	  
	Notes:  																	  
------------------------------------------------------------------------------*/	

    ASSIGN cEntityContext = pcContext . 
		
END FUNCTION.
