/*------------------------------------------------------------------------
    File        : daEntity.i
    Purpose     : define static instance that loads daSupport super
    Syntax      : {support/daentity.i <EntityName>}

    Description : Include for super bootstrapping 
    Notes       : There must be a dataset include in propath of name 
                 {&schemapath}/{&datasetprefix}{&entityname}.i  
                 default = schema/ds<Entityname>.i
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ Consultingwerk/products.i }
{ {&OERASI}/startsuper.i} 

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
startSuper('daSupport').

run createObject.
/* ************ Functions ****************************************/   
function getEntityName returns character():
  return "{&entityname}".
end.

&endif
