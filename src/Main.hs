-- | El GUI del programa
module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX
import qualified Control.Exception as C

import Semantica

about = "San Simon Haskell Hackathone [s2hk]"

main :: IO()
main = start gui

-- ==============================================================
-- | construye el GUI
-- ==============================================================
gui :: IO()
gui = do dbfile  <- variable [value := "./db"]
         db      <- variable [value := ([],UnLoad)]
         lcItem  <- variable [value := -1]
         f       <- frame [text := "San Simon Haskel Hackathone"]
         (barraFile, barraDB) <- crearBarraEstado f dbfile db
         listBox <- crearLista f dbfile db lcItem  barraFile barraDB
         crearMenuToolBar f dbfile db listBox lcItem barraFile barraDB
         set f [ layout  := fill $ widget listBox
               , on paint := pintarBG ]

pintarBG dc (Rect x y w h)
    = do img <- imageCreateFromFile "img/bg.jpg"
         wi  <- imageGetWidth img
         hi  <- imageGetHeight img
         let px = md w - md wi
         let py = md h - md hi
         drawImage dc img (pt px py) []
    where md v = v `div` 2

-- ==============================================================
-- list control
-- ==============================================================
crearLista f dbfile db lcItem barraFile barraDB
    = do listBox <- listCtrl f [ style :=  wxLC_REPORT
                                       .+. wxLC_SINGLE_SEL
                                       .+. wxLC_HRULES
                                       .+. wxLC_VRULES
                               , columns := [ ("Numero"     , AlignLeft, -1)
                                            , ("Nombre"     , AlignLeft, 300)
                                            , ("Universidad", AlignLeft, 300)
                                            , ("EMail"      , AlignLeft, 300)
                                            , ("Nivel"      , AlignLeft, 100)
                                            ]
                               , visible := False
                               ]
         updateLista listBox db
         set listBox [ on listEvent := onListEvent f listBox dbfile db lcItem barraFile barraDB]
         return listBox

updateLista lb db
    = do (hlist,_) <- get db value
         let lista = formatoListCtrl hlist
         set lb [items := lista]

-- list control events
onListEvent f lb dbfile db lcItem barraFile barraDB evt
    = case evt of
        ListKeyDown (KeyChar 'e') -> editar   f lb dbfile db lcItem barraFile barraDB
        ListKeyDown (KeyChar 'd') -> eliminar f lb dbfile db lcItem barraFile barraDB
        ListKeyDown (KeyChar 'i') -> insertar f lb dbfile db        barraFile barraDB
        ListItemRightClick ind    -> infoDialog f "click" (show ind)
        ListItemSelected ind      -> set lcItem [value := ind]
        anyEvent                  -> return ()

eliminar f lb dbfile db lcItem barraFile barraDB
    = catch eliminar error
    where eliminar = do ind       <- get lcItem value
                        (hlist,_) <- get db value
                        if ind < 0 || ind >= length hlist
                         then C.throw (userError "Seleccione un indice valido para Eliminar")
                         else do bool <- confirmDialog f "Confirmar" (msg ind) True
                                 if bool
                                  then do let newHList = myDeleteF hlist ind
                                          set db [value := (newHList, Changed)]
                                          updateLista lb db
                                          updateBarraEstado dbfile db barraFile barraDB
                                  else return ()
          error e = errorDialog f "Error" (show e)
          myDeleteF (x:xs) 0 = xs
          myDeleteF (x:xs) n = x : myDeleteF xs (n-1)
          msg ind = "Estas seguro que quieres eliminar todo el item " ++ show (ind + 1) ++ " ?"

editar f lb dbfile db lcItem barraFile barraDB
    = catch edit error
    where edit :: IO ()
          edit = do (hlist,_) <- get db value
                    ind       <- get lcItem value
                    let hper = if ind < 0
                               then C.throw (userError "No selecciono un item")
                               else hlist !! ind
                    d      <- dialog f [text := "Editar"]
                    [nm,un,em,ni] <- mapM (toWidget d) [ getNombre hper
                                                       , getUniversidad hper
                                                       , getEmail hper
                                                       , getNivel hper
                                                       ]
                    cancel <- button d [text := "Cancel"]
                    ok     <- button d [text := "Ok"]
                    set d [ layout := column 10 [ grid 5 5 [ [label "Numero:"       , hfill $ label (show $ ind + 1)]
                                                           , [label "Nombre:"       , hfill $ widget nm]
                                                           , [label "Universidad:"  , hfill $ widget un]
                                                           , [label "EMail:"        , hfill $ widget em]
                                                           , [label "Nivel:"        , hfill $ widget ni]
                                                           ]
                                                , row 5 [hspace 200, widget cancel, widget ok]
                                                ]
                          ]
                    let endFunction stop =  set ok     [ on command := obtenerInfo stop nm un em ni] 
                                         >> set cancel [ on command := stop Nothing]
                    result <- showModal d endFunction
                    case result of
                         Nothing -> return ()
                         Just hp -> do let newHList = myInsertF hlist hp ind
                                       set db [value := (newHList, Changed)]
                                       updateLista lb db
                                       updateBarraEstado dbfile db barraFile barraDB
          error e = errorDialog f "Error" (show e)
          myInsertF []     hp 0 = [hp]
          myInsertF (x:xs) hp 0 = hp : xs
          myInsertF (x:xs) hp n = x  : myInsertF xs hp (n-1)


toWidget f str = entry f [text := str, alignment := AlignLeft]

obtenerInfo stop nm un em ni
    = do vnm <- get nm text >>= (\v -> return ("Nombre_Completo",v))
         vun <- get un text >>= (\v -> return ("Universidad",v))
         vem <- get em text >>= (\v -> return ("Correo_Electronico",v))
         vni <- get ni text >>= (\v -> return ("Conocimiento_en_Programación_Funcional",v))
         stop (Just $ HPersona (Just vnm) (Just vun) (Just vem) (Just vni))

insertar f lb dbfile db barraFile barraDB
    = catch inst error
    where inst :: IO ()
          inst = do d             <- dialog f [text := "Insetar"]
                    [nm,un,em,ni] <- mapM (toWidget d) [ "", "", "", ""]
                    cancel        <- button d [text := "Cancel"]
                    ok            <- button d [text := "Ok"]
                    set d [ layout := column 10 [ grid 5 5 [ [label "Nombre:"       , hfill $ widget nm]
                                                           , [label "Universidad:"  , hfill $ widget un]
                                                           , [label "EMail:"        , hfill $ widget em]
                                                           , [label "Nivel:"        , hfill $ widget ni]
                                                           ]
                                                , row 5 [hspace 200, widget cancel, widget ok]
                                                ]
                          ] 
                    let endFunction stop =  set ok     [ on command := obtenerInfo stop nm un em ni] 
                                         >> set cancel [ on command := stop Nothing]
                    result <- showModal d endFunction
                    case result of
                         Nothing -> return ()
                         Just hp -> do (hlist,_) <- get db value
                                       let newHList = hlist ++ [hp]
                                       set db [value := (newHList, Changed)]
                                       updateLista lb db
                                       updateBarraEstado dbfile db barraFile barraDB
          error e = errorDialog f "Error" (show e)

-- ==============================================================
-- data base
-- ==============================================================
data EstadoDB
    = UnLoad | Loaded | Changed | OK

instance Show EstadoDB where
    show UnLoad  = "Estado DB: No Cargado"
    show Loaded  = "Estado DB: Cargado"
    show Changed = "Estado DB: Modificado"
    show OK      = "Estado DB: OK"

-- | Cargar archivo de base de datos
loadDB f dbfile db barraFile barraDB
    = catch load error
    where load = do file  <- get dbfile value
                    hlist <- readHLista file
                    set db [value := (hlist, Loaded)]
                    updateBarraEstado dbfile db barraFile barraDB
                    return ()
          error e = errorDialog f "Error" (show e)

-- | Guardar la base de datos
saveDB f dbfile db barraFile barraDB
    = do catch save error
    where save = do file      <- get dbfile value
                    (hlist,_) <- get db value
                    writeHLista hlist file
                    set db [value := (hlist,OK)]
                    updateBarraEstado dbfile db barraFile barraDB
          error e = errorDialog f "Error" (show e)

-- ==============================================================
-- | Barras de Estado
-- ==============================================================
crearBarraEstado f dbfile db
    = do (_,state) <- get db value
         barraDB   <- statusField [statusWidth := -2, text := show state]
         file      <- get dbfile value
         barraFile <- statusField [text := "Data Base File: " ++ file]
         set f [statusBar := [barraDB,barraFile]]
         return (barraFile, barraDB)

updateBarraEstado dbfile db barraFile barraDB
    = do (_,state) <- get db value
         file      <- get dbfile value
         set barraDB [text := show state]
         set barraFile [text := "Data Base File: " ++ file]

-- ==============================================================
-- | Crea los menus and tool bar
-- ==============================================================
crearMenuToolBar f dbfile db lb lcItem barraFile barraDB
    = do -- panel mfile
         mfile  <- menuPane [text := "&Archivo"]
         msave  <- menuItem mfile [ text := "&Guardar\tCtrl+G"
                                  , help := "Guardar los cambios a la base de datos"
                                  , on command := saveDB f dbfile db barraFile barraDB
                                  ]
         mload  <- menuItem mfile [ text := "Cargar Archivo DB\tCtrl+L"
                                  , on command := loadDB f dbfile db barraFile barraDB
                                  , help := "Cargar la base de datos"
                                  ]
         menuLine mfile
         mclose <- menuItem mfile [ text := "&Cerrar\tCtrl+C"
                                  , help := "Cerrar s2hk"
                                  , on command := close f
                                  ]
         -- panel mlist
         mlist  <- menuPane [text := "&Lista"]
         mshow  <- menuItem mlist [ text := "Mo&strar Lista\tCtrl+s"
                                  , help := "Mostrar la Lista"
                                  , checkable := True
                                  ]
         set mshow [on command := showLista mshow]
         mupls  <- menuItem mlist [ text := "Actualizar Lista\tF5"
                                  , on command := updateLista lb db]
         minst  <- menuItem mlist [ text := "&Añadir Participante"
                                  , on command := insertar f lb dbfile db barraFile barraDB
                                  , help := "Insertar un participante"
                                  ]
         melim  <- menuItem mlist [text := "&Eliminar Participante"
                                  , on command := eliminar f lb dbfile db lcItem barraFile barraDB
                                  , help := "eleminar un participante"
                                  ]
         -- panel mhelp
         mhelp  <- menuHelp []
         mabout <- menuAbout mhelp [ on command := infoDialog f "About" about
                                   , help := "Informacion sobre s2hk"
                                   ]
         -- set the menu on frame
         set f [ menuBar := [mfile, mlist, mhelp]]
         {-
         -- toolbar
         tbar <- toolBar f []
         toolMenu tbar msave "Guardar" "img/guardar.png" []
         toolItem tbar mshow "Cargar"  "img/cargar.png"  []
         toolMenu tbar mshow "Mostrar" "img/mostrar.png" []
         --toolMenu tbar dklasj  "Editar"   "open.png"  []
         toolMenu tbar minst  "Insetar"  "img/insertar.png" []
         toolMenu tbar melim  "Eliminar" "img/eliminar.png" []
         -}
    where showLista ms = do bool <- get ms checked
                            set lb [visible := bool ]
                            set f [layout := fill $ widget lb]
                            repaint f

