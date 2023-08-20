

-- {Ejercicio 1: Agrupamiento de anagramas completo}


subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets [x] = [[], [x]]
subsets [1 ,2 ,3]
subsets ['a', 'b', 'c', 'd']

majorityElem :: Eq a => [a] -> a
majorityElem [3 ,2 ,3]
majorityElem [2 ,2 ,1 ,1 ,1 ,2 ,2]


coins :: [Int] -> Int -> Bool
coins [] n = n == 0
coins [2 ,5] 8
coins [2 ,4 ,6] 21


data BST a = Empty | Node a (BST a) (BST a) deriving Show
isBST :: BST Int -> Bool
isBST Empty = True
isBST (Node n left right) = (isSubBST n left) && (isSubBST n right)
    where 
        isSubBST :: Int -> BST Int -> Bool
        isSubBST _ Empty = True
        isSubBST val (Node v l r) = (val >= v) && (isSubBST v l) && (isSubBST v r)
        isSubBST o re = False

isBST (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty))
isBST (Node 3 (Node 1 Empty (Node 3 Empty Empty)) (Node 4 Empty Empty))


kthElem :: BST a -> Int -> a
kthElem (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)) 2
kthElem (Node 5 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 Empty Empty)) (Node 6 Empty Empty)) 4