-----------------------------------------------------------------------------
--
-- Module      :  Data.RDF.MTriples
-- Copyright   :  (c) Calvin Smith, Rob Stewart
-- License     :  BSD3
--
-- Maintainer  :  Rob Stewart <robstewart@gmail.com>
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Data.RDF.MTriples (
    MTriples,
    empty,
    mkRdf,
    triplesOf,
    select,
    query)
where
import Data.RDF.Types
       (Predicate, NodeSelector, Triple(..), Object, Triples, Subject,
        PrefixMappings(..), BaseUrl(..), RDF(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.RDF.Namespace (mergePrefixMappings)
import Data.RDF.Query (objectOf, subjectOf)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

-- |Another map-based graph implementation.

newtype MTriples = MTriples (TMaps, Maybe BaseUrl, PrefixMappings)

type TMaps = (Map Subject Triples, Map Object Triples)

instance RDF MTriples where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = triplesOf'
  select            = select'
  query             = query'

instance Show MTriples where
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

baseUrl' :: MTriples -> Maybe BaseUrl
baseUrl' (MTriples (_, baseURL, _)) = baseURL

prefixMappings' :: MTriples -> PrefixMappings
prefixMappings' (MTriples (_, _, pms)) = pms

addPrefixMappings' :: MTriples -> PrefixMappings -> Bool -> MTriples
addPrefixMappings' (MTriples (ts, baseURL, pms)) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  MTriples (ts, baseURL, merge pms pms')

empty' :: MTriples
empty' = MTriples ((Map.empty, Map.empty), Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> MTriples
mkRdf' ts baseURL pms = MTriples (mergeTs (Map.empty, Map.empty) (reverse ts), baseURL, pms)

mergeTs :: TMaps -> [Triple] -> TMaps
mergeTs = foldl' mergeT
  where
    mergeT :: TMaps -> Triple -> TMaps
    mergeT (sm,om) t = (Map.insertWith insertFunc (subjectOf t) [t] sm,
                        Map.insertWith insertFunc (objectOf t) [t] om)
    insertFunc [n] o = n:o
    insertFunc _ _ = error "MTriple>>mergeTs: impossible"

triplesOf' :: MTriples -> Triples
triplesOf' (MTriples ((sm,_),_,_)) = concat (Map.elems sm)

select' :: MTriples -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' mt cfs cfp cfo = filter selectIt (triplesOf' mt)
  where
    selectIt (Triple s p o) = (case cfs of
                                Nothing -> True
                                Just fs -> fs s)
                                                  &&
                              (case cfp of
                                Nothing -> True
                                Just fp -> fp p)
                                                  &&
                              (case cfo of
                                Nothing -> True
                                Just fo -> fo o)

query' :: MTriples -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' (MTriples ((sm,_),_,_)) (Just s) Nothing Nothing =
    fromMaybe [] (Map.lookup s sm)
query' (MTriples ((sm,_),_,_)) (Just s) Nothing (Just o) =
    maybe [] (filter (\ (Triple _ _ o') -> o == o')) (Map.lookup s sm)
query' (MTriples ((sm,_),_,_)) (Just s) (Just p) Nothing =
    maybe [] (filter (\ (Triple _ p' _) -> p == p')) (Map.lookup s sm)
query' (MTriples ((sm,_),_,_)) (Just s) (Just p) (Just o) =
    maybe [] (filter (\ (Triple _ p' o') -> p == p' && o == o')) (Map.lookup s sm)
query' (MTriples ((_,om),_,_)) Nothing Nothing (Just o) =
    fromMaybe [] (Map.lookup o om)
query' (MTriples ((_,om),_,_)) Nothing (Just p) (Just o) =
    maybe [] (filter (\ (Triple _ p' _) -> p == p')) (Map.lookup o om)
-- and now the slow case:
query' mt Nothing (Just p) Nothing =
    select' mt Nothing (Just (== p)) Nothing
query' mt Nothing Nothing Nothing =
    triplesOf' mt


