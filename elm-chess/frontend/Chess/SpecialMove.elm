module Chess.SpecialMove where

import Chess.Board exposing (..)

type SpecialMove
  -- @Position:  the position behind the enemy pawn,
  --             where the attacking pawn will land
  --             after taking another pawn with EnPassant
  = EnPassant Position

  -- @Position:  the left or right position
  --             King will land after a Castling move
  | Castling ( Maybe Position, Maybe Position )

  -- @Position: position of the pawn which will be replaced for a new piece
  | Promotion Position


getPawnSpecialDestinations : Game -> Position -> (List Position, Maybe SpecialMove)
getPawnSpecialDestinations game origin =
  let
    adjacentPositions =
      Board.getHorizontalAdjacentPositions origin
        --positionOfPawnWhichMoved2Squares

    left = fst adjacentPositions

    right = snd adjacentPositions

    nextPosition = Board.positionAhead game.turn origin

    nextRow = snd <| nextPosition

    ( specialDestinations, specialMove ) =
      if | nextRow == 1 || nextRow == 8 ->
             ([], Just <| Promotion nextPosition)
         | otherwise ->
             case game.state of
               Origin (Just pawnPosition) ->
                 if pawnPosition == left || pawnPosition == right
                 then
                   let
                     enPassantDestination : Position
                     enPassantDestination =
                         Board.shift pawnPosition <|
                            case game.turn of
                              White ->
                                (0, 1)

                              Black ->
                                (0, -1)
                   in
                     ( [enPassantDestination]
                     , Just <| EnPassant enPassantDestination
                     )
                 else
                  noSpecialMove

               _ ->
                  noSpecialMove

    destinations = (getPawnValidTakes game origin) ++ specialDestinations

  in
    (destinations, specialMove)

--getCastlingDestinations : Game -> Position -> Piece -> (List Position, Maybe SpecialMove)
--getCastlingDestinations game origin king =
--  if king.moved
--  then
--    noSpecialMove
--  else
--    let
--      getSquareContent' = Board.getSquareContent game.board
--
--      ( leftRookPosition, rightRookPosition ) =
--        Board.getRookInitialPosition game.turn
--
--      ( leftCastlingIntermediatePositions
--      , rightCastlingIntermediatePositions
--      ) =
--        Board.getCastlingIntermediatePositions game.turn
--
--      breno : (Position, Position)
--      breno = leftCastlingIntermediatePositions
--
--      kingLandingPoint : Position
--      kingLandingPoint = snd leftCastlingIntermediatePositions
--
--      getKingCastlingArrivalPosition : (Position, Position) -> Maybe Position
--      getKingCastlingArrivalPosition castlingIntermediatePositions =
--        let
--          leftRookSquare : Maybe Piece
--          leftRookSquare = getSquareContent' leftRookPosition
--
--        in
--        case leftRookSquare of
--          -- FIXME: looks like applicative functions would be nice here
--          Nothing -> Nothing
--          Just piece ->
--            if not (piece.figure == Rook && piece.moved == False)
--            then
--              Nothing
--            else
--              let
--                isIntermediateCastlingSquaresEmpty : Bool
--                isIntermediateCastlingSquaresEmpty =
--                  List.all Maybe.Extra.isNothing <|
--                    [ fst castlingIntermediatePositions
--                    , snd castlingIntermediatePositions
--                    ]
--              in
--                if not isIntermediateCastlingSquaresEmpty
--                then
--                  Nothing
--                else
--                  Just <| kingLandingPoint
--
--      leftKingCastlingArrivalPosition =
--        getKingCastlingArrivalPosition leftCastlingIntermediatePositions
--
--      rightKingCastlingArrivalPosition =
--        getKingCastlingArrivalPosition rightCastlingIntermediatePositions
--
--      castlingPositions =
--        (Maybe.Extra.maybeToList
--          leftKingCastlingArrivalPosition)
--        ++
--        (Maybe.Extra.maybeToList
--          rightKingCastlingArrivalPosition)
--
--      noCastling =
--        castlingPositions == []
--
--    in
--      if noCastling
--      then
--        noSpecialMove
--      else
--        ( castlingPositions
--        , Just <| Castling
--           ( leftKingCastlingArrivalPosition
--           , rightKingCastlingArrivalPosition
--           )
--        )


getSpecialDestinations : Game -> Position -> Piece ->
  ( List Position, Maybe SpecialMove )
getSpecialDestinations game origin piece =
  case piece.figure of
    Pawn ->
      getPawnSpecialDestinations game origin

    King ->
      --getCastlingDestinations game origin piece
      noSpecialMove
    _ ->
      noSpecialMove
