module Estructura where 

type HLista = [HPersona]

data HPersona 
    = HPersona { nombre      :: Info
               , universidad :: Info
               , email       :: Info
               , nivel       :: Info
               }

type Info = Maybe Field

type Field = (String, String)

getNombre      = maybe "" (id.snd) . nombre 
getUniversidad = maybe "" (id.snd) . universidad 
getEmail       = maybe "" (id.snd) . email
getNivel       = maybe "" (id.snd) . nivel

instance Show HPersona where
    show (HPersona nm un em ni) = showField nm ++
                                  showField un ++
                                  showField em ++
                                  showField ni ++ "\n"
        where showField mb = maybe "" showC mb ++ "\n"
              showC (nm,vl) = nm ++ " : " ++ vl

