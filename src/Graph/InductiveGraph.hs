module Graph.InductiveGraph where 

type Node = Int 
type Adj b = [(b, Node)]
type Context a b = (Adj b, Node, a , Adj b)

data Graph a b = Empty | (Context a b) :&: (Graph a b)  deriving Show
--    deriving stock instance (Show a, Show b) => Show (Graph a b)
{-
instance (Show a, Show b) => Show (Graph a b) where 
    show Empty = ""
    -- show ( ([(b,n1), n2, a, [(b2,n3)]]   ) :&: g) = show c' <> show g where 
    show ( (adj1, n, a, adj2   ) :&: g) = show adj1 <> ", " <> show n <> ", " <> show a <> ", " <> show adj2 <> " :&: " <> show g 
-}
c1, c2 :: Adj String 
c1 = [ ("left", 2), ("up", 3) ]
c2 = [ ("right", 4)]  

cntx :: Context String String
cntx =  (c1, 1, "some label", c2)
graphs :: Graph String String
graphs = cntx :&: Empty

adj :: Adj String
adj = []

g,g2 :: Graph Char String 
g  =  (adj, 1, 'a', adj) :&: 
       ( (adj, 2, 'b', adj) :&: 
       Empty )
g2 = ( 
    [ ("left", 21), ("up", 3) ] , 1, 'a', [ ("right", 4)])  
    :&: ( ( [] , 2, 'b', [("doom", 5)]) 
    :&: ( ( adj , 3, 'c', adj) 
    :&: Empty ) )


--  >>> g2
-- ([("left",20),("up",3)],1,'a',[("right",4)]) :&: (([],2,'b',[("doom",5)]) :&: (([],3,'c',[]) :&: Empty))

