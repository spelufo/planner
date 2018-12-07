module Udelar exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode



-- MODEL


type alias Model =
    { curriculum : Curriculum
    , plan : Plan
    , selection : Maybe AsigId
    , query : String
    , showNext : Bool
    , hidePlanned : Bool
    }


init curriculum =
    { curriculum = curriculum
    , plan = []
    , selection = Nothing
    , query = ""
    , showNext = False
    , hidePlanned = False
    }


type alias Curriculum =
    { asigs : Dict AsigId Asig
    , groups : Dict GroupId Group
    , area : Area
    }


type alias Asig =
    { id : AsigId
    , name : String
    , courseReqs : List Req
    , examReqs : List Req
    }


type alias Group =
    { id : GroupId
    , min : Int
    , max : Int
    , entries : List GroupReqEntry
    }

type alias Area =
    { id : String
    , name : String
    , min : Creds
    , entries : List AreaEntry}


type AreaEntry
    = SubArea Area
    | AreaAsig { id : AsigId, name : String, creds : Creds }


type alias AsigId =
    String


type alias GroupId =
    String


type alias Creds =
    Int


areaCreds : Plan -> Area -> Int
areaCreds plan area =
    List.sum (List.map (areaEntryCreds plan) area.entries)


areaEntryCreds : Plan -> AreaEntry -> Int
areaEntryCreds plan areaEntry =
    case areaEntry of
        SubArea a ->
            areaCreds plan a
        AreaAsig {id, name, creds} ->
            if (List.member id (List.map .id (examsDone plan))) then
                creds
            else
                0


getAsig asigId curriculum =
    case Dict.get asigId curriculum.asigs of
        Just asig ->
            asig

        Nothing ->
            Debug.todo ("getAsig: Asig not found: " ++ asigId)


getGroup groupId curriculum =
    case Dict.get groupId curriculum.groups of
        Just group ->
            group

        Nothing ->
            Debug.todo ("getAsig: Group not found: " ++ groupId)


type Req
    = CredReq CredReqInfo
    | InscriptionReq InscriptionReqInfo
    | CourseReq CourseReqInfo
    | ExamReq ExamReqInfo
    | GroupReq GroupReqInfo
    | NotReq Req


type alias CredReqInfo =
    { creds : Creds, area : String }


type alias InscriptionReqInfo =
    { id : AsigId, name : String }


type alias CourseReqInfo =
    { id : AsigId, name : String }


type alias ExamReqInfo =
    { id : AsigId, name : String }


type alias GroupReqInfo =
    { id : GroupId, name : String }


type alias GroupReqEntry =
    { req : Req, points : Int }


type alias Plan =
    List Activity


type alias Activity =
    { kind : ActivityKind
    , period : Period
    , asig : Asig
    }


type ActivityKind
    = TakeCourse
    | TakeExam


type alias Period =
    ( Year, Month )


type alias Year =
    Int


type alias Month =
    Int


isCoursePeriod ( y, m ) =
    m == 3 || m == 8


isExamPeriod ( y, m ) =
    m == 2 || m == 7 || m == 12


periodActivityKind period =
    if isCoursePeriod period then
        TakeCourse

    else
        TakeExam


addToPlan plan period asig =
    let
        activity =
            { kind = periodActivityKind period
            , period = period
            , asig = asig
            }
    in
    activity :: removeFromPlan plan activity


-- Even though we pass in an activity, its period is disregarded.   
removeFromPlan : Plan -> Activity -> Plan
removeFromPlan plan { kind, asig } =
    List.filter
        (\activity ->
            not (activity.kind == kind && activity.asig == asig)
        )
        plan


periodActivities plan period =
    List.filter ((==) period << .period) plan


planUpTo plan period =
    List.filter ((>) period << .period) plan



maxPeriodPlanned plan =
    case (List.maximum (List.map .period plan)) of
        Just x ->
            x
        Nothing ->
            (99999, 99999)
    

coursesDone plan =
    List.map .asig (List.filter ((==) TakeCourse << .kind) plan)


examsDone plan =
    List.map .asig (List.filter ((==) TakeExam << .kind) plan)


asigDone plan asigId =
    List.member asigId (List.map .id (examsDone plan))

canDoActivity : Curriculum -> Plan -> Activity -> Bool
canDoActivity curriculum plan activity =
    let
        asig =
            activity.asig

        getReqs =
            case activity.kind of
                TakeCourse ->
                    .courseReqs

                TakeExam ->
                    .examReqs
    in
    List.all (reqSatisfied curriculum (planUpTo plan activity.period)) (getReqs asig)


reqSatisfied : Curriculum -> Plan -> Req -> Bool
reqSatisfied curriculum plan req =
    case req of
        CredReq { creds, area } ->
            True

        -- TODO: Not enough info on asigs.
        InscriptionReq { id, name } ->
            True

        CourseReq { id } ->
            List.member id (List.map .id (coursesDone plan))

        ExamReq { id } ->
            List.member id (List.map .id (examsDone plan))

        GroupReq { id } ->
            groupSatisfied curriculum plan (getGroup id curriculum)

        NotReq req_ ->
            not (reqSatisfied curriculum plan req_)


groupSatisfied curriculum plan g =
    let
        countEntry reqEntry =
            if reqSatisfied curriculum plan reqEntry.req then
                reqEntry.points

            else
                0

        count =
            List.sum (List.map countEntry g.entries)
    in
    g.min <= count && count <= g.max



-- UPDATE


type Msg
    = NoOp
    | UpdateQuery String
    | SelectAsig AsigId
    | UpdatePlan Period
    | RemoveFromPlan Activity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Msg" msg
    in
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        UpdateQuery q ->
            ( { model | query = String.toUpper q }
            , Cmd.none
            )

        SelectAsig a ->
            ( { model
                | selection =
                    case model.selection of
                        Just s ->
                            if s == a then
                                Nothing
                            else
                                Just a
                        Nothing ->
                            Just a }
            , Cmd.none
            )

        UpdatePlan period ->
            ( case model.selection of
                Just asigId ->
                    { model
                        | selection = Nothing
                        , plan = addToPlan model.plan period (getAsig asigId model.curriculum)
                    }

                Nothing ->
                    model
            , Cmd.none
            )

        RemoveFromPlan activity ->
            ( case model.selection of
                Just asigId ->
                    { model
                        | selection = Nothing
                        , plan = removeFromPlan model.plan activity
                    }
                Nothing ->
                    model
            , Cmd.none
            )


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "udelar" ]
        [ sidebar model
        , viewPlan model
        , div [ class "main-area"] [ viewArea model model.curriculum.area ]
        ]


viewArea : Model -> Area -> Html Msg
viewArea model area =
    let
        creds =
            areaCreds model.plan area
    in
    div [ class "area" ]
        [ div [ classList [("area-done", area.min <= creds)] ]
            [ text
                (area.id
                    ++ " - "
                    ++ area.name
                    ++ " - "
                    ++ (String.fromInt creds)
                    ++  "/"
                    ++ (String.fromInt area.min)) ]
        , ul [] (List.map (viewAreaEntry model) area.entries)
        ]


viewAreaEntry model areaEntry =
    let
        areaEntryDone =
            case areaEntry of
                AreaAsig {id} ->
                    asigDone model.plan id
                SubArea _ ->
                    False
    in
    li [ classList [("area-entry-done", areaEntryDone)] ]
        [ case areaEntry of
            SubArea a ->
                viewArea model a
            AreaAsig {id, name, creds} ->
                text (id ++ " - " ++ name ++ " - " ++ (String.fromInt creds))
        ]


sidebar model =
    let
        plan =
            model.plan

        isntPlanned asig =
            if model.hidePlanned then
                not (List.member asig (coursesDone plan) && List.member asig (examsDone plan))
            else
                True

        canDo asig =
            if model.showNext then
                canDoActivity model.curriculum plan
                    {kind = TakeCourse, asig = asig, period = maxPeriodPlanned plan}
            else
                True

        asigs =
            model.curriculum.asigs
                |> Dict.values
                |> List.sortBy .name
                |> List.filter (String.contains model.query << .name)
                |> List.filter canDo
                |> List.filter isntPlanned
    in
    div [ class "sidebar" ]
        [ input [ value model.query, onInput UpdateQuery ] []
        , div [ class "asigs-list" ] (List.map (viewAsig model True) asigs)
        ]


viewAsig model showReqs asig =
    let
        selected =
            case model.selection of
                Just asigId ->
                    asig.id == asigId

                Nothing ->
                    False
    in
    div
        [ class "asig"
        , classList [ ( "selected", selected ) ]
        , onClickStopPropagation (SelectAsig asig.id)
        ]
        ([ text asig.name ]
            ++ (if showReqs then
                    [ viewReqs model asig ]

                else
                    []
               )
        )


viewReqs model asig =
    div [ class "reqs" ]
        [ text "Course reqs:"
        , ul [ class "course-reqs" ] (List.map (viewCourseReq model) asig.courseReqs)
        , text "Exam reqs:"
        , ul [ class "exam-reqs" ] (List.map (viewExamReq model) asig.examReqs)
        ]


viewCourseReq model req =
    li [ classList [ ( "req-not-satisfied", not (reqSatisfied model.curriculum model.plan req) ) ] ]
        [ text (Debug.toString req) ]


viewExamReq model req =
    li [ classList [ ( "req-not-satisfied", not (reqSatisfied model.curriculum model.plan req) ) ] ]
        [ text (Debug.toString req) ]


viewPlan model =
    div [ class "plan" ]
        (List.map (viewYear model) (List.range 2011 2022))


viewYear model year =
    div [ class "year" ]
        [ viewExamPeriod model ( year, 2 )
        , viewSemester model ( year, 3 )
        , viewExamPeriod model ( year, 7 )
        , viewSemester model ( year, 8 )
        , viewExamPeriod model ( year, 12 )
        ]


viewSemester model period =
    let
        activities =
            periodActivities model.plan period
    in
    div [ class "semester", onClick (UpdatePlan period) ]
        [ text (Debug.toString period)
        , div [] (List.map (viewActivity model) activities)
        ]


viewExamPeriod model period =
    let
        activities =
            periodActivities model.plan period
    in
    div [ class "exam-period", onClick (UpdatePlan period) ]
        [ text (Debug.toString period)
        , div [] (List.map (viewActivity model) activities)
        ]


viewActivity model activity =
    let
        activityClass =
            case activity.kind of
                TakeCourse ->
                    "course"

                TakeExam ->
                    "exam"

        missingReqs =
            not (canDoActivity model.curriculum model.plan activity)
    in
    div [ class "activity"
        , class activityClass
        , classList [ ( "missing-reqs", missingReqs ) ]
        ]
        [ viewAsig model False activity.asig ]



-- UTILS


onClickStopPropagation : msg -> Html.Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click" (Json.Decode.succeed ( msg, True ))
