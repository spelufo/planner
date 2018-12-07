module Decode exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Udelar exposing (..)



curriculum : Decoder Curriculum
curriculum =
    map3 Curriculum
        (field "asigs" asigs)
        (field "groups" groups)
        (field "area" area)


asigs : Decoder (Dict AsigId Asig)
asigs =
    dictFromList (list asig)


groups : Decoder (Dict GroupId Group)
groups =
    dictFromList (list group)


area : Decoder Area
area =
    let
        decodeArea (id, name, min) entries =
            { id = id
            , name = name
            , min = min
            , entries = entries
            }
    in
    map2 decodeArea
        (field "content" areaContent)
        (field "children" (list areaEntry))


areaEntry : Decoder AreaEntry
areaEntry =
    let
        dispatchEntry t =
            if t == "Materia" then
                map (\(id, name, creds) -> AreaAsig {id = id, name = name, creds = creds})
                    (field "content" areaContent)
            else
                map SubArea area
    in
    andThen dispatchEntry (field "type" string)

    
areaContent : Decoder (String, String, Creds)
areaContent =
    let
        decodeContents content =
            case (String.split " - " content) of
                id :: name :: credStr :: _ ->
                    let
                        creds =
                            case (String.split " " credStr) of
                                _ :: credStr_ :: _ ->
                                    case (String.toInt credStr_) of
                                        Just c ->
                                            c
                                        Nothing ->
                                            Debug.todo "expected a number of creds"
                                _ ->
                                    Debug.todo "unexpected creds string"
                    in
                    (id, name, creds)
                _ ->
                    Debug.todo "invalid area content string"
    in
    map decodeContents string


dictFromList =
    map (\xs -> Dict.fromList (List.map (\a -> (a.id, a)) xs))


asig : Decoder Asig
asig =
    map4 Asig
        (field "id" asigId)
        (field "nombre" string)
        (field "pcurso" (list req))
        (field "pexamen" (list req))


asigId : Decoder AsigId
asigId =
    string


group : Decoder Group
group =
    map4 Group
        (field "id" groupId)
        (field "min" int)
        (field "max" int)
        (field "previas" (list groupReqEntry))


groupId : Decoder GroupId
groupId =
    string


groupReqEntry : Decoder GroupReqEntry
groupReqEntry =
    map2 GroupReqEntry req (field "puntaje" int)


req : Decoder Req
req =
    (map2 (\x y -> (x, y))
        (field "actividad" string)
        (map (Maybe.withDefault False) (maybe (field "obs" bool))))
        |> andThen reqHelp


reqHelp : (String, Bool) -> Decoder Req
reqHelp (kind, obs) =
    if obs then
        map NotReq (reqHelp (kind, False))
    else
        case kind of
            "Créditos" ->
                map CredReq credReqInfo

            "Inscripción a curso" ->
                map InscriptionReq inscriptionReqInfo

            "Curso aprobado" ->
                map CourseReq courseReqInfo

            "Examen aprobado" ->
                map ExamReq examReqInfo

            "Grupo" ->
                map GroupReq groupReqInfo

            _ ->
                fail "Invalid req kind."


credReqInfo =
    map2 CredReqInfo
        (field "cantidad" int)
        (field "texto" string)


inscriptionReqInfo =
    map2 InscriptionReqInfo
        (field "id" asigId)
        (field "nombre" string)


courseReqInfo =
    map2 CourseReqInfo
        (field "id" asigId)
        (field "nombre" string)


examReqInfo =
    map2 ExamReqInfo
        (field "id" asigId)
        (field "nombre" string)


groupReqInfo =
    map2 GroupReqInfo
        (field "id" groupId)
        (field "nombre" string)
