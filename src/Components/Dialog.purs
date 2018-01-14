-- |
module Example.Component.Dialog
  ( component
  , Query(..)
  , DialogResult(..)
  , DialogOptionsLite
  ) where

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), const, discard, id, pure, ($), (<<<))

type DialogOptionsLite = 
  { title   :: String
  , message :: String
  , actions :: Array String
  }

data Query a = CloseDialog Int a

data DialogResult = DialogResult Int

type State = DialogOptionsLite

component :: forall m. H.Component HH.HTML Query DialogOptionsLite DialogResult m
component
  = H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing 
    }
  
  where

  render :: State -> H.ComponentHTML Query
  render dialogOptions =
    HH.div_
      [ HH.div
        [ HP.class_ (H.ClassName "ps-modal") ]
        [ HH.div
          [ HP.class_ (H.ClassName "guts") ]
          [ HH.h2_ [ HH.text dialogOptions.title ]
          , HH.p_  [ HH.text dialogOptions.message ]
          , HH.ul_ (mapWithIndex renderAction dialogOptions.actions)
          ]
        ]
      , HH.div
        [ HP.class_ (H.ClassName "ps-modal-overlay") ]
        [ ]
      ]

      where

      renderAction :: Int -> String -> H.ComponentHTML Query
      renderAction i a = 
        HH.li_ 
          [ HH.button
            [ HE.onClick (HE.input_ (CloseDialog i))
            , HP.class_ (H.ClassName "ps-btn")
            ]
            [ HH.text a ]
          ]

  eval :: Query ~> H.ComponentDSL State Query DialogResult m
  eval (CloseDialog idx next) = do
    H.raise <<< DialogResult $ idx
    pure next