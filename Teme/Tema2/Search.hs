{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    nodeStateX :: s,
    nodeParentX :: Maybe (Node s a),
    nodeDepthX :: Int,
    nodeChildrenX :: [Node s a],
    nodeHeuristicX :: Float,
    nodeActionX :: Maybe a
}

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    node1 == node2 = (nodeStateX node1) == (nodeStateX node2)

instance Ord s => Ord (Node s a) where
    node1 <= node2 = (nodeStateX node1) <= (nodeStateX node2)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState node = (nodeStateX node)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = (nodeParentX node)

nodeDepth :: Node s a -> Int
nodeDepth node = (nodeDepthX node)

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = (nodeChildrenX node)

nodeHeuristic :: Node s a -> Float
nodeHeuristic node = (nodeHeuristicX node)

nodeAction :: Node s a -> Maybe a
nodeAction node = (nodeActionX node)

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpaceHelper :: (ProblemState s a, Eq s) => s -> Maybe a -> Maybe (Node s a) -> Int -> Node s a
createStateSpaceHelper initialState action parent depth = n
    where 
        n 
            | (isGoal initialState) = (Node initialState parent depth [] (h initialState) action)
            | otherwise = (Node initialState parent depth (map function (successors initialState)) (h initialState) action)
        function = (\p -> (createStateSpaceHelper (snd p) (Just (fst p)) (Just n) (depth + 1)))

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createStateSpaceHelper initialState Nothing Nothing 0
{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = (filter (\n -> (S.notMember (nodeState n) visited)) (nodeChildren node))

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = (PQ.insertWith min node cost frontier)
    where
        cost = (nodeHeuristic node) + (fromIntegral (nodeDepth node) :: Float)

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = (foldl insertSucc frontier (suitableSuccs node visited))

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = goal
    where
        result = (deleteFindMin frontier)
        node = (fst result)
        newFrontier = (snd result)
        newVisited = (S.insert (nodeState node) visited)
        thenewFrontier = (insertSuccs node newFrontier newVisited)
        goal
            | (isGoal (nodeState node)) = node
            | otherwise = (astar' newVisited thenewFrontier)

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty  (insertSucc PQ.empty initialNode)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPathHelper :: (Eq s, Eq a) => Maybe (Node s a) -> [Maybe (Node s a)] -> [Maybe (Node s a)]
extractPathHelper goalNode list
    | goalNode == Nothing = list
    | otherwise = extractPathHelper (nodeParent (fromJust goalNode)) (goalNode:list)

extractPath :: (Eq s, Eq a) => Node s a -> [(a, s)]
extractPath goalNode = list_of_good_pairs
    where
        list_of_nodes = (map fromJust (extractPathHelper (Just goalNode) []))
        list_of_bad_pairs = (map (\n -> ((nodeAction n), (nodeState n))) list_of_nodes)
        list_of_better_pairs = (filter (\p -> (not ((fst p) == Nothing))) list_of_bad_pairs)
        list_of_good_pairs = (map (\p -> ((fromJust (fst p)), (snd p))) list_of_better_pairs)