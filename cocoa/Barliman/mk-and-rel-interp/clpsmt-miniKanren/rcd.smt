(set-logic ALL_SUPPORTED)

(declare-datatypes
 ((Pair 2))
 ((par (A B) ((pair (fst A) (snd B))))))

(declare-datatypes
 ((Typ 0))
 (((arr (arr_param Typ) (arr_return Typ))
   (rcd (rcd_set (Set (Pair Int Typ)))))))
