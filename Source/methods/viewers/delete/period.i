/* period.i */

If can-find(first gltrans where gltrans.company = period.company
                            and gltrans.tr-date >= period.pst
                            AND gltrans.tr-date <= period.pend
            )
    OR
    can-find(first glhist where glhist.company = period.company
                            and glhist.tr-date >= period.pst
                            AND glhist.tr-date <= period.pend
            )
then DO:
   message 'Cannot delete this period with outstanding transactions' view-as alert-box error.
   return error.
end.
