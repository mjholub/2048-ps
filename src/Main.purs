--    2048-ps. PureScript implementation of the 2048 game
--    Copyright (C) 2024  Marcelina Hołub
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU Affero General Public License as
--    published by the Free Software Foundation, either version 3 of the
--    License, or (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU Affero General Public License for more details.
--
--    You should have received a copy of the GNU Affero General Public License
--    along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Main where

import Prelude

import Control.Monad.Free (wrap)
import Effect (Effect)
import Effect.Console (logShow)
import Halogen (HalogenQ(..), mkComponent)
import Halogen as H
import Halogen.Aff as Haff
import Halogen.HTML as Html
import Halogen.HTML.Events as HEv
import Halogen.VDom.Driver (runUI)

type Board = [[Int]]

-- initialize a game board with 2 random tiles
initBoard :: Board
initBoard = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]

-- merges two tiles in an axis
merge :: [Int] -> [Int]
merge = merge' []
  where
    merge' acc [] = reverse acc
    merge' acc [x] = reverse (x : acc)
    merge' acc (x : y : xs)
      | x == y = merge' (x * 2 : acc) xs
      | otherwise = merge' (x : acc) (y : xs)

moveLeft :: [Int] -> [Int]
moveLeft row = merge (filter ( _ /= 0))

moveRight :: [Int] -> [Int]
moveRight row = reverse (moveLeft (reverse row))

-- transpose a matrix
trasnpose :: Board -> Board
transpose = map reverse . foldl (zipWith (:)) (replicate 4 [])

moveUp :: Board -> Board
moveUp = map moveLeft . transpose

moveDown :: Board -> Board
moveDown = map moveRight . transpose

type State = { board :: Board }

data Action = MoveLeft | MoveRight | MoveUp | MoveDown

component :: forall q i o. H.Component Html.HTML q i o Haff
component =
  H.mkComponent 
    { initialState
    , render
    , eval: Haff.mkEval $ Haff.defaultEval { handleAction = handleAction }
    }
  where
    initialState :: i -> State
    initialState _ = { board: initBoard }

render :: forall m. State -> H.ComponentHTML Action () m
  render state = 
    Html.div_
      [ Html.button [ HEv.onClick MoveLeft ] [ Html.text "Move Left" ]
      , Html.button [ HEv.onClick MoveRight ] [ Html.text "Move Right" ]
      , Html.button [ HEv.onClick MoveUp ] [ Html.text "Move Up" ]
      , Html.button [ HEv.onClick MoveDown ] [ Html.text "Move Down" ]
      , Html.div_ (map renderRow state.board)
      ]

renderRow :: [Int] -> H.ComponentHTML Action () m
renderRow row = Html.div_ (map renderTile row)

renderTile :: Int -> H.ComponentHTML Action () m
renderTile value = Html.div_ [Html.class_ (Html.ClassName "tile")] [Html.text (show value)]

handleAction :: forall o. Action -> H.HalogenM State Action () o Haff Unit
handleAction = case _ of
  MoveUp -> H.modify_ \state -> state { board = moveUp state.board }
  MoveDown -> H.modify_ \state -> state { board = moveDown state.board }
  MoveLeft -> H.modify_ \state -> state { board = map moveLeft state.board }
  MoveRight -> H.modify_ \state -> state { board = map moveRight state.board }

main :: Effect Unit
main = Haff.runHalogenAff do
  body <- Haff.awaitBody
  runUI component unit body
