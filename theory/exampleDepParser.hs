data Category = Nn | Na | V deriving (Eq, Ord)

data Role = Subj | Cmpl deriving (Eq, Ord)

dic "Joannes" = [Nn]
dic "Mariam"  = [Na]
dic "Amat"    = [V]

-- to connect a dependent to its heads
heads d = [ (r, h) | h <- preds d,
                     r <- link (getCategory h) (getCategory d)]


-- to connect a head to its dependents
deps h = [ (r, d) | d <- preds h,
                    r <- link (category h) (category d)]       

-- to show the role of an arc
link V Nn = [Subj]
link V Na = [Cmpl]
link _ _  = []

(>=>) :: (a -> [a]) -> (a -> [a]) -> (a -> [a])
f >=> g = g . f

clo, mclo, rclo, mrclo :: (a -> [a]) -> a -> [a]
clo f = f >=> (pure `mappend` clo f)
rclo f = pure `mappend` clo f
mclo f = f >=> mrclo f
mrclo f x = let
              fx = f x in
                 if null fx
                 then pure x
                 else fx >>= mrclo f

type Index = Int

type Arc = (Role, Index)

data Step = Step {
                 index :: Index,
                 category :: Category,
                 headArc :: [Arc],
                 depArcs :: [Arc]
                 } deriving (Eq, Ord)


type Parse = [Step]

data Node = Node {
                 history :: [Step],
                 future :: [Step]
                 } deriving (Eq, Ord)

infixl 4 <<, +->, +<-
  
(<<) :: Parse -> (Index, Category) -> Parse
p << (i, c) = Step i c [] [] : p

(+->),(+<-) :: Parse -> (Role, Index) -> Parse
(Step i c [] d:p) +-> (r, j) = Step i c [(r, j)] d:p
(Step i c h d:p)  +<- (r, j) = Step i c h ((r,j) : d) : p

getIndex :: Node -> Index
getIndex (Node (Step index category headArc depArcs : _) _) = index

getCategory :: Node -> Category
getCategory (Node (Step index category headArc depArcs : _) _) = category

getHeadArc :: Node -> [Arc]
getHeadArc (Node (Step index category headArc depArcs : _) _) = headArc

getDepArcs :: Node -> [Arc]
getDepArcs (Node (Step index category headArc depArcs : _) _) = depArcs

leftNeighbour, rightNeighbour :: Node -> [Node]
leftNeighbour (Node (s : s' : p) q) = [Node (s' : p) (s : q)]
leftNeighbour _ = []
rightNeighbour (Node p (s : q)) = [Node (s : p) q]
rightNeighbour _ = []

preds, succs :: Node -> [Node]
preds = clo leftNeighbour
succs = clo rightNeighbour

-- argNode : argument Node

leftDepHelper, rightDepHelper, depHelper :: Node ->  [(Role, Node)]
leftDepHelper argNode = [(role, node) | node <- preds argNode,
                          (role, index) <- getDepArcs argNode,
                         (getIndex node) == index]
rightDepHelper argNode = [(role, node) | node <- succs argNode,
                          (role, index) <- getHeadArc node,
                          (getIndex argNode) == index]
depHelper = leftDepHelper `mappend` rightDepHelper


leftHeadHelper, rightHeadHelper, headHelper :: Node -> [(Role, Node)]
leftHeadHelper argNode = [(role, node) | node <- preds argNode,
                          (role, index) <- getHeadArc argNode,
                          (getIndex node) == index]
rightHeadHelper argNode = [(role, node) | node <- succs argNode,
                           (role, index) <- getDepArcs node,
                           (getIndex argNode) == index]
headHelper = leftHeadHelper `mappend` rightHeadHelper

leftDep, rightDep, dep, leftHead, rightHead, parseHead :: Node -> [Node]
leftDep   = snd <*> leftDepHelper
rightDep  = snd <*> rightDepHelper
dep       = snd <*> depHelper
leftHead  = snd <*> leftHeadHelper
rightHead = snd <*> rightHeadHelper
parseHead      = snd <*> headHelper

leftDepRoles, rightDepRoles, depRoles, leftHeadRoles, rightHeadRoles, headRoles ::Node -> [Role]
leftDepRoles   = fst <*> leftDepHelper
rightDepRoles  = fst <*> rightDepHelper
depRoles       = fst <*> depHelper
leftHeadRoles  = fst <*> leftHeadHelper
rightHeadRoles = fst <*> rightHeadHelper
headRoles      = fst <*> headHelper

leftDepBy, rightDepBy, depBy :: Role -> Node -> [Node]
leftDepBy argRole argNode  = [ node | (argRole, node) <- leftDepHelper argNode ]
rightDepBy argRole argNode = [ node | (argRole, node) <- rightDepHelper argNode]
depBy argRole = leftDepBy argRole `mappend` rightDepBy argRole


leftHeadBy, rightHeadBy, headBy :: Role -> Node -> [Node]
leftHeadBy argRole argNode  = [ node | (argRole, node) <- leftHeadHelper argNode ] 
rightHeadBy argRole argNode = [ node | (argRole, node) <- rightHeadHelper argNode]
headBy argRole = leftHeadBy argRole `mappend` rightHeadBy argRole

leftmost, rightmost :: [Node] -> [Node]
leftmost [] = []
leftmost xs = [minimum xs]
rightmost [] = []
rightmost xs = [minimum xs]

headless :: Node -> Bool
headless = null . parseHead


lastNode :: Parse -> Node
lastNode p = Node p []

type ParseWord = String

shift :: ParseWord -> Parse -> [Parse]
shift word parse = [ parse << (nextId parse, cat) | cat <- dic word ]
                   where
                     nextId [] = 1
                     nextId (Step index category headArc depArcs : _) = index + 1

addHead, addDep :: Parse -> [Parse]
addHead parse = [ parse +-> (role, getIndex node) | let currentNode = lastNode parse,
                                                    headless currentNode,
                                                    (role, node) <- heads currentNode]
addDep parse  = [ parse +<-(role, getIndex node) | let currentNode = lastNode parse,
                                                   (role, node) <- deps currentNode,
                                                   headless node]

connect :: Parse -> [Parse]
connect = (addDep >=> connect) `mappend` addHead `mappend` pure

step :: ParseWord -> Parse -> [Parse]
step word = shift word >=> connect