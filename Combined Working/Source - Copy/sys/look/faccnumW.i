/* --------------------------------------------- sys/look/faccnumW.i 10/92 cd  */
/*                                                                            */
/* Where Statement - Find account by account number                           */
/*                                                                            */
/* -------------------------------------------------------------------------- */

where (account.company eq cocode
  and  account.type    ne "T")

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
