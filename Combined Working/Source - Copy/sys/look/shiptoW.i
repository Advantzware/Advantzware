/* --------------------------------------------------- look/shipto.w 2/92 cd  */
/*                                                                            */
/* WHERE STATEMENT - SHIPTO FILE                                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

where (shipto.company eq cocode
  and  shipto.cust-no eq cust.cust-no)
  
/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

