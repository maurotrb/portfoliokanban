#!/usr/bin/env stack
{- stack
  script
  --resolver lts-8.13
  --package containers
  --package lucid
  --package text
-}

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, GeneralizedNewtypeDeriving #-}

import           Data.Monoid
import           Data.String

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Lucid


{- Configuration -}

{- Demo board -}

pk :: PortfolioKanban
pk = PK { title = "Project Portfolio Kanban Demo"
        , subtitle = "Period from January to June, 2017"
                     -- Backlog: list of ProjectID
        , backlog = [ "PI", "RHO", "SIGMA" ]
                    -- Timeline: list of month. Must be 10 months. Columns of the board
        , timeline = [ January, February, March, April, May, June, July, August, September, October ]
                     -- Swimlane: list of ResourceID. Rows of the board
        , swimlane = [ "A", "B", "C", "D", "E", "F", "G" ]
                     -- Checkers: ProjectID and Tag list per ResourceID and Month. Where timelines and swimlanes cross.
        , checkers = Map.fromList [ (("A", January), [ ("ALPHA", Maintenance), ("BETA", Active) ])
                                  , (("A", February), [ ("ALPHA", Maintenance), ("BETA", Active) ])
                                  , (("A", March), [ ("ALPHA", Maintenance) ])
                                  , (("A", April), [ ("ALPHA", Maintenance) ])
                                  , (("A", May), [ ("ALPHA", Maintenance) ])
                                  , (("A", June), [ ("ALPHA", Maintenance) ])
                                  , (("A", July), [ ("ALPHA", Maintenance) ])
                                  , (("A", August), [ ("ALPHA", Maintenance) ])
                                  , (("A", September), [ ("ALPHA", Maintenance) ])
                                  , (("A", October), [ ("ALPHA", Maintenance) ])
                                  , (("B", January), [ ("GAMMA", Maintenance) ])
                                  , (("B", February), [ ("GAMMA", Maintenance) ])
                                  , (("B", March), [ ("GAMMA", Maintenance) ])
                                  , (("B", April), [ ("GAMMA", Maintenance) ])
                                  , (("B", May), [ ("GAMMA", Maintenance) ])
                                  , (("B", June), [ ("GAMMA", Maintenance) ])
                                  , (("B", July), [ ("GAMMA", Maintenance) ])
                                  , (("B", August), [ ("GAMMA", Maintenance) ])
                                  , (("B", September), [ ("GAMMA", Maintenance) ])
                                  , (("B", October), [ ("GAMMA", Maintenance) ])
                                  , (("C", January), [ ("DELTA", Active) ])
                                  , (("C", February), [ ("DELTA", Active) ])
                                  , (("C", March), [ ("DELTA", Active) ])
                                  , (("C", April), [ ("DELTA", Active) ])
                                  , (("C", May), [ ("DELTA", Active) ])
                                  , (("C", June), [ ("DELTA", Active) ])
                                  , (("D", January), [ ("EPSILON", Maintenance), ("ZETA", Active), ("ETA", Active) ])
                                  , (("D", February), [ ("EPSILON", Maintenance), ("ETA", Active) ])
                                  , (("D", March), [ ("EPSILON", Maintenance) ])
                                  , (("D", April), [ ("EPSILON", Maintenance) ])
                                  , (("D", May), [ ("EPSILON", Maintenance) ])
                                  , (("D", June), [ ("EPSILON", Maintenance) ])
                                  , (("D", July), [ ("EPSILON", Maintenance) ])
                                  , (("D", August), [ ("EPSILON", Maintenance) ])
                                  , (("D", September), [ ("EPSILON", Maintenance) ])
                                  , (("D", October), [ ("EPSILON", Maintenance) ])
                                  , (("E", January), [ ("THETA", Maintenance), ("IOTA", Active) ])
                                  , (("E", February), [ ("THETA", Maintenance), ("IOTA", Late) ])
                                  , (("E", March), [ ("THETA", Maintenance), ("IOTA", Late) ])
                                  , (("E", April), [ ("THETA", Maintenance) ])
                                  , (("E", May), [ ("THETA", Maintenance) ])
                                  , (("E", June), [ ("THETA", Maintenance) ])
                                  , (("E", July), [ ("THETA", Maintenance) ])
                                  , (("E", August), [ ("THETA", Maintenance) ])
                                  , (("E", September), [ ("THETA", Maintenance) ])
                                  , (("E", October), [ ("THETA", Maintenance) ])
                                  , (("F", January), [ ("KAPPA", Active), ("LAMBDA", Active) ])
                                  , (("F", February), [ ("KAPPA", Active), ("LAMBDA", Active) ])
                                  , (("F", March), [ ("KAPPA", Active), ("LAMBDA", Active) ])
                                  , (("F", April), [ ("KAPPA", Active) ])
                                  , (("F", May), [ ("KAPPA", Active) ])
                                  , (("F", June), [ ("KAPPA", Active) ])
                                  , (("F", July), [ ("KAPPA", Active) ])
                                  , (("F", August), [ ("KAPPA", Active) ])
                                  , (("F", September), [ ("KAPPA", Active) ])
                                  , (("F", October), [ ("KAPPA", Active) ])
                                  , (("G", January), [ ("MU", Maintenance) ])
                                  , (("G", February), [ ("MU", Maintenance) ])
                                  , (("G", March), [ ("MU", Maintenance) ])
                                  , (("G", April), [ ("MU", Maintenance) ])
                                  , (("G", May), [ ("MU", Maintenance), ("NU", Active) ])
                                  , (("G", June), [ ("MU", Maintenance), ("NU", Active) ])
                                  , (("G", July), [ ("MU", Maintenance), ("NU", Active) ])
                                  , (("G", August), [ ("MU", Maintenance) ])
                                  , (("G", September), [ ("MU", Maintenance) ])
                                  , (("G", October), [ ("MU", Maintenance) ])
                                  ]
                     -- Projects:
        , projects = Map.fromList [ ("ALPHA", Project { pid = "ALPHA"
                                                      , headline = "Project ALPHA"
                                                      , summary = "Project ALPHA description"
                                                      , symbols = [ "fa-exclamation"
                                                                  ]})
                                  , ("BETA", Project { pid = "BETA"
                                                     , headline = "Project BETA"
                                                     , summary = "Project BETA description"
                                                     , symbols = [
                                                                 ]})
                                  , ("GAMMA", Project { pid = "GAMMA"
                                                      , headline = "Project GAMMA"
                                                      , summary = "Project GAMMA description"
                                                      , symbols = [
                                                                  ]})
                                  , ("DELTA", Project { pid = "DELTA"
                                                      , headline = "Project DELTA"
                                                      , summary = "Project DELTA description"
                                                      , symbols = [
                                                                  ]})
                                  , ("EPSILON", Project { pid = "EPSILON"
                                                        , headline = "Project EPSILON"
                                                        , summary = "Project EPSILON description"
                                                        , symbols = [
                                                                    ]})
                                  , ("ZETA", Project { pid = "ZETA"
                                                     , headline = "Project ZETA"
                                                     , summary = "Project ZETA description"
                                                     , symbols = [
                                                                 ]})
                                  , ("ETA", Project { pid = "ETA"
                                                    , headline = "Project ETA"
                                                    , summary = "Project ETA description"
                                                    , symbols = [
                                                                ]})
                                  , ("THETA", Project { pid = "THETA"
                                                      , headline = "Project THETA"
                                                      , summary = "Project THETA description"
                                                      , symbols = [
                                                                  ]})
                                  , ("IOTA", Project { pid = "IOTA"
                                                     , headline = "Project IOTA"
                                                     , summary = "Project IOTA description"
                                                     , symbols = [
                                                                 ]})
                                  , ("KAPPA", Project { pid = "KAPPA"
                                                      , headline = "Project KAPPA"
                                                      , summary = "Project KAPPA description"
                                                      , symbols = [
                                                                  ]})
                                  , ("LAMBDA", Project { pid = "LAMBDA"
                                                       , headline = "Project LAMBDA"
                                                       , summary = "Project LAMBDA description"
                                                       , symbols = [
                                                                   ]})
                                  , ("MU", Project { pid = "MU"
                                                   , headline = "Project MU"
                                                   , summary = "Project MU description"
                                                   , symbols = [
                                                               ]})
                                  , ("NU", Project { pid = "NU"
                                                   , headline = "Project NU"
                                                   , summary = "Project NU description"
                                                   , symbols = [
                                                               ]})
                                  , ("PI", Project { pid = "PI"
                                                   , headline = "Project PI"
                                                   , summary = "Project PI description"
                                                   , symbols = [
                                                               ]})
                                  , ("RHO", Project { pid = "RHO"
                                                    , headline = "Project RHO"
                                                    , summary = "Project RHO description"
                                                    , symbols = [
                                                                ]})
                                  , ("SIGMA", Project { pid = "SIGMA"
                                                      , headline = "Project SIGMA"
                                                      , summary = "Project SIGMA description"
                                                      , symbols = [
                                                                  ]})
                                  ]
                     -- Resources:
        , resources = Map.fromList [ ("A", Resource { rid  = "A"
                                                    , name = "Team A"
                                                    , description = "Team A description"
                                                    })
                                   , ("B", Resource { rid  = "B"
                                                    , name = "Team B"
                                                    , description = "Team B description"
                                                    })
                                   , ("C", Resource { rid  = "C"
                                                    , name = "Team C"
                                                    , description = "Team C description"
                                                    })
                                   , ("D", Resource { rid  = "D"
                                                    , name = "Team D"
                                                    , description = "Team D description"
                                                    })
                                   , ("E", Resource { rid  = "E"
                                                    , name = "Team E"
                                                    , description = "Team E description"
                                                    })
                                   , ("F", Resource { rid  = "F"
                                                    , name = "Team F"
                                                    , description = "Team F description"
                                                    })
                                   , ("G", Resource { rid  = "G"
                                                    , name = "Team G"
                                                    , description = "Team G description"
                                                    })
                                   ]
        }



{- End of configuration. Do not modify below unless you know what you are doing :-) -}


{- Create HTML Portfolio Kanban Board  -}

main = do
  renderToFile "../index.html" $ portfolioPage pk

{- Board elements -}

data PortfolioKanban = PK
                       { title       :: Text
                       , subtitle    :: Text
                       , backlog     :: [ProjectID]
                       , timeline    :: [Month]
                       , swimlane    :: [ResourceID]
                       , checkers    :: Checkers
                       , projects    :: Projects
                       , resources   :: Resources
                       }
                     deriving (Show)

data Project = Project
               { pid      :: ProjectID
               , headline :: Text
               , summary  :: Text
               , symbols   :: [Text]
               }
             | EmptyProject
             deriving (Eq, Show)

newtype ProjectID  = PID Text deriving (Eq, Ord, Show, IsString)

data Resource = Resource
                { rid         :: ResourceID
                , name        :: Text
                , description :: Text
                }
              | EmptyResource
              deriving (Eq, Show)

newtype ResourceID = RID Text deriving (Eq, Ord, Show, IsString)

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq, Ord, Show)

type Checkers = Map (ResourceID, Month) [(ProjectID, Tag)]
type Projects = Map ProjectID Project
type Resources = Map ResourceID Resource
data Tag = Waiting | Active | Late | Maintenance deriving (Eq, Show)


{- HTML page -}

portfolioPage :: PortfolioKanban -> Html ()
portfolioPage pk =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ (toHtml $ title pk)
      meta_ [name_ "description", content_ (subtitle pk)]
      link_ [rel_ "stylesheet", href_ "css/portfoliokanban.min.css"]
      link_ [rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"]
    body_ $ do
      section_ [class_ "section"] $ do
        tile_ancestor_ [class_ "is-vertical"] $ do
          -- Portfolio header
          tile_parent_ $ do
            tile_child_ [class_ "box"] $ do
              h1_ [class_ "title is-1"] (strong_ (toHtml $ title pk))
              h2_ [class_ "subtitle is-3"] (toHtml $ subtitle pk)
          -- Portfolio body
          tile_ [class_ "is-vertical"] $ do
            -- Portfolio sub-header
            tile_ $ do
              tile_parent_ [class_ "is-1"] $ do
                tile_child_ [class_ "box has-text-centered"] $ do
                  h3_ [class_ "title is-6"] (strong_ "Backlog")
              tile_ [class_ "is-11"] $ do
                tile_parent_ [class_ "is-2"] $ do
                  tile_child_ [class_ "box has-text-centered"] $ do
                    h3_ [class_ "title is-6"] (strong_ "Resource")
                mconcat $ map timelineColumnHeader (timeline pk)
            -- Portfolio content
            tile_ $ do
              -- Backlog content
              tile_parent_ [class_ "is-vertical is-1"] $ do
                projectStack (projects pk) $ map (\pid -> (pid, Waiting)) (backlog pk)
              -- Active
              tile_ [class_ "is-vertical is-11"] $ do
                -- Active body
                mconcat $
                  map (swimlaneSection (timeline pk) (checkers pk) (projects pk)) $
                  map (\rid -> Map.findWithDefault EmptyResource rid (resources pk)) (swimlane pk)


swimlaneSection :: [Month] -> Checkers -> Projects -> Resource -> Html ()
swimlaneSection _ _   _   EmptyResource                = ""
swimlaneSection t cks ps (Resource rid@(RID rid') n d) =
  tile_ $ do
    tile_parent_ [class_ "is-vertical is-2"] $ do
      tile_child_ [class_ "card"] $ do
        div_ [class_ "card-header"] $ do
          p_ [class_ "card-header-title"] (toHtml rid')
        div_ [class_ "card-content"] $ do
          div_ [class_ "content is-small"] $ do
            p_ $ strong_ (toHtml n)
            p_ (toHtml d)
    mconcat $ map (\m -> checkerSection ps (Map.findWithDefault [] (rid, m) cks)) t

checkerSection :: Projects -> [(ProjectID, Tag)] -> Html ()
checkerSection ps ckps = tile_parent_ [class_ "is-vertical is-1"] $ projectStack ps ckps

projectStack :: Projects -> [(ProjectID, Tag)] -> Html ()
projectStack _  []   = tile_child_ ""
projectStack ps pids = mconcat $ map (\(pid, tag) -> projectCard (Map.findWithDefault EmptyProject pid ps) tag) pids

projectCard :: Project -> Tag -> Html ()
projectCard EmptyProject                    _   = tile_child_ ""
projectCard (Project (PID pid) hl sum syms) tag =
  tile_child_ [class_ (T.append "card " (tagHtmlClass tag))] $ do
    div_ [class_ "card-header"] $ do
      p_ [class_ "card-header-title"] (toHtml pid)
    div_ [class_ "card-content"] $ do
      div_ [class_ "content is-small"] $ do
        p_ $ strong_ (toHtml hl)
        p_ (toHtml sum)
        projectCardSymbols syms

tagHtmlClass :: Tag -> Text
tagHtmlClass Waiting     = "pwaiting"
tagHtmlClass Active      = "pactive"
tagHtmlClass Late        = "plate"
tagHtmlClass Maintenance = "pmaintenance"

projectCardSymbols :: [Text] -> Html ()
projectCardSymbols []   = ""
projectCardSymbols syms =
  p_ $ do
    mconcat $ map (\sym -> icon sym) syms
      where
        icon :: Text -> Html ()
        icon sym =
          span_ [class_ "icon"] $ do
            i_ [class_ (T.append "fa " sym)] ""


monthToText :: Month -> Text
monthToText January   = "January"
monthToText February  = "February"
monthToText March     = "March"
monthToText April     = "April"
monthToText May       = "May"
monthToText June      = "June"
monthToText July      = "July"
monthToText August    = "August"
monthToText September = "September"
monthToText October   = "October"
monthToText November  = "November"
monthToText December  = "December"

timelineColumnHeader :: Month -> Html ()
timelineColumnHeader m =
  tile_parent_ [class_ "is-1"] $ do
    tile_child_ [class_ "box has-text-centered"] $ do
      h3_ [class_ "title is-6"] (strong_ (toHtml $ monthToText m))

-- Bulma custom class

tile_ :: Term arg result => arg -> result
tile_ = termWith "div" [class_ " tile "]

tile_ancestor_ :: Term arg result => arg -> result
tile_ancestor_ = termWith "div" [class_ " tile ancestor "]

tile_parent_ :: Term arg result => arg -> result
tile_parent_ = termWith "div" [class_ " tile is-parent "]

tile_child_ :: Term arg result => arg -> result
tile_child_ = termWith "div" [class_ " tile is-child "]
